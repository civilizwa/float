#include <algorithm>
#include "LinearScan.h"
#include "SymbolTable.h"
#include "AsmBuilder.h"
#include "MachineCode.h"
#include "LiveVariableAnalysis.h"
#include <limits.h>

LinearScan::LinearScan(MachineUnit *unit)
{
    this->unit = unit;
    for (int i = 4; i < 11; i++) //可分配寄存器为4-10
        regs.push_back(i);
    for (int i = 16; i <= 31; i++) // s0-s15好像都可以用来传参
        fpuRegs.push_back(i);
}

//遍历每个函数 获得虚拟寄存器对应的物理寄存器
void LinearScan::allocateRegisters()
{
    //遍历unit下的每个func
    for (auto &f : unit->getFuncs())
    {
        func = f;
        //用success判断何时完成分配
        bool success = false;
        while (!success) // repeat until all vregs can be mapped
        {
            //旧区间重构，重分配原来的寄存器
            computeLiveIntervals();
            spillIntervals.clear();
            success = linearScanRegisterAllocation();
            //有溢出情况 生成溢出代码 否则直接调整寄存器
            if (success) // all vregs can be mapped to real regs
                modifyCode();//没有溢出 修改寄存器映射即可退出
            else // spill vregs that can't be mapped to real regs
                genSpillCode();//生成溢出代码 然后继续循环
        }
    }
}

void LinearScan::makeDuChains()
{
    LiveVariableAnalysis lva;
    lva.pass(func);//遍历函数 添加使用和定义变量，分配寄存器
    du_chains.clear();
    int i = 0;
    std::map<MachineOperand, std::set<MachineOperand *>> liveVar;
    //遍历函数每个MachineBlock
    for (auto &bb : func->getBlocks())
    {
        liveVar.clear();
        //放入不活跃的变量
        for (auto &t : bb->getLiveOut())
            liveVar[*t].insert(t);
        int no;
        no = i = bb->getInsts().size() + i;
        //为了对于标号 逆序遍历
        for (auto inst = bb->getInsts().rbegin(); inst != bb->getInsts().rend(); inst++)
        {
            (*inst)->setNo(no--);
            //遍历指令定义的操作数
            //将虚拟寄存器的变量
            for (auto &def : (*inst)->getDef())
            {
                if (def->isVReg())
                {
                    //获取定义变量的活跃寄存器
                    auto &uses = liveVar[*def];//对应变量的集合
                    //集合插入du_chains
                    du_chains[def].insert(uses.begin(), uses.end());
                    auto &kill = lva.getAllUses()[*def];
                    std::set<MachineOperand *> res;
                    set_difference(uses.begin(), uses.end(), kill.begin(), kill.end(), inserter(res, res.end()));
                    liveVar[*def] = res;
                }
            }
            for (auto &use : (*inst)->getUse())
            {
                if (use->isVReg())
                    liveVar[*use].insert(use);
            }
        }
    }
}

void LinearScan::computeLiveIntervals()
{
    makeDuChains();
    intervals.clear();
    // compute live intervals
    for (auto &du_chain : du_chains) {
        int t = -1;
        for (auto &use : du_chain.second)
            t = std::max(t, use->getParent()->getNo());
        Interval *interval = new Interval({du_chain.first->getParent()->getNo(), 
            t, false, 0, 0, {du_chain.first}, du_chain.second, du_chain.first->isFReg()});
        intervals.push_back(std::move(interval));
    }
    // compute start and end of live intervals
    for (auto &interval : intervals) {
        auto uses = interval->uses;
        auto begin = interval->start;
        auto end = interval->end;
        // find the first and last operand
        for (auto block : func->getBlocks()) {
            auto liveIn = block->getLiveIn();
            auto liveOut = block->getLiveOut();
            bool in = false;
            bool out = false;
            // check if the vreg is live in or live out
            for (auto use : uses)
                if (liveIn.count(use)) {
                    in = true; // the vreg is live in
                    break;
                }
            for (auto use : uses)
                if (liveOut.count(use)) {
                    out = true;
                    break;
                }
            // compute the start and end of the interval 
            if (in && out) {
                // if the vreg is live in and live out, the interval is the whole block
                begin = std::min(begin, (*(block->begin()))->getNo());
                end = std::max(end, (*(block->rbegin()))->getNo());
            }
            else if (!in && out) {
                for (auto i : block->getInsts())
                    if (i->getDef().size() > 0 && i->getDef().front() == *(uses.begin())) {
                        begin = std::min(begin, i->getNo());
                        break;
                    }
                end = std::max(end, (*(block->rbegin()))->getNo());
            }
            else if (in && !out) {
                begin = std::min(begin, (*(block->begin()))->getNo());
                int temp = 0;
                for (auto use : uses)
                    if (use->getParent()->getParent() == block)
                        temp = std::max(temp, use->getParent()->getNo());
                end = std::max(temp, end);
            }
        }
        // set the start and end of the interval
        interval->start = begin;
        interval->end = end;
    }
    bool change = true;// if there is a change in the intervals
    while (change) {
        change = false;
        std::vector<Interval* >t;
        t.insert(t.begin(), intervals.begin(), intervals.end());
        // merge intervals that have the same def and use
        for (size_t i = 0; i < t.size(); i++)
            for (size_t j = i + 1; j < t.size(); j++) {
                Interval *w1 = t[i];
                Interval *w2 = t[j];
                if (**w1->defs.begin() == **w2->defs.begin()) {
                    std::set<MachineOperand *> temp;
                    set_intersection(w1->uses.begin(), w1->uses.end(),
                         w2->uses.begin(), w2->uses.end(), inserter(temp, temp.end()));
                    if (!temp.empty()) {
                        change = true;
                        w1->defs.insert(w2->defs.begin(), w2->defs.end());
                        w1->uses.insert(w2->uses.begin(), w2->uses.end());
                        auto w1Min = std::min(w1->start, w1->end);
                        auto w1Max = std::max(w1->start, w1->end);
                        auto w2Min = std::min(w2->start, w2->end);
                        auto w2Max = std::max(w2->start, w2->end);
                        w1->start = std::min(w1Min, w2Min);
                        w1->end = std::max(w1Max, w2Max);
                        auto it = std::find(intervals.begin(), intervals.end(), w2);
                        if (it != intervals.end())
                            intervals.erase(it);
                    }
                }
            }
    }
    sort(intervals.begin(), intervals.end(), compareStart);
}

// 【重点函数】扫描 分配寄存器
bool LinearScan::linearScanRegisterAllocation()
{
    // Todo
    bool success = true;//用于判断能否分配成功
    //初始化
    active.clear();
    regs.clear();
    fpuRegs.clear();
    //初始放入可用分配寄存器 4-10
    for (int i = 4; i < 11; i++)
        regs.push_back(std::move(i));
    for (int i = 16; i <= 31; i++)
        fpuRegs.push_back(std::move(i));
    //遍历每个unhandled interval没有分配寄存器的活跃区间
    for (auto interval : intervals) {
        //遍历 active 列表，看该列表中是否存在结束时间早于unhandled interval 的 interval
        //主要用于回收可用寄存器
        expireOldIntervals(interval);
        //没有可分配的寄存器 溢出
        if ((interval->fpu && fpuRegs.empty()) || (!interval->fpu && regs.empty())) {
            spillAtInterval(interval);//溢出操作
            success = false;
        }
        else {
            //分配寄存器 同时删去已经分配的
            if (interval->fpu) { //如果是浮点数
                interval->rreg = fpuRegs.front();
                fpuRegs.erase(fpuRegs.begin());
            }
            else {
                interval->rreg = regs.front();
                regs.erase(regs.begin());
            }
            //放入已经分配的向量中
            active.push_back(std::move(interval));//右值引用 可以直接用interval
            //对活跃区间按照结束时间升序排序
            sort(active.begin(), active.end(), compareEnd);
        }
    }
    return success;
}

void LinearScan::expireOldIntervals(Interval *interval)
{
    // Todo
    //查看active中是否有结束时间早于interval起始时间
    //active按照end时间升序排列，所以只用看头部
    //头部如果大于 那么直接返回
    //头部小于 那么active的寄存器可以回收
    std::vector<Interval *>::iterator it = active.begin();
    // for(auto it = active.begin(); it != active.end(); )
    //     if((*it)->end >= interval->start)
    //         break;
    if (it != active.end()) {
        if ((*it)->end >= interval->start) //只用比较第一个
            return;
        if ((*it)->fpu)
            fpuRegs.push_back(std::move((*it)->rreg));
        else
            regs.push_back(std::move((*it)->rreg));
        it = active.erase(find(active.begin(), active.end(), *it));
        sort(regs.begin(), regs.end());
    }

}

// 寄存器溢出操作
// 在 active列表中最后一个 interval 和当前 unhandled interval 中选择一个 interval 将其溢出到栈中，
// 选择策略就是看谁的活跃区间结束时间更晚，如果是 unhandled interval 的结束时间更晚，
// 只需要置位其 spill 标志位即可，如果是 active 列表中的 interval 结束时间更晚，需要置位
// 其 spill 标志位，并将其占用的寄存器分配给 unhandled interval，再按照 unhandled interval
// 活跃区间结束位置，将其插入到 active 列表中。
void LinearScan::spillAtInterval(Interval *interval)
{
    // Todo
    //选择active列表末尾与当前unhandled的一个溢出到栈中
    auto it = active.rbegin();
    for (; it != active.rend(); it++) {
        if ((*it)->fpu == interval->fpu)
            break;
    }
    //将结束时间更晚的溢出
    if ((*it)->end > interval->end) {
        //选择active列表末尾溢出，并回收其寄存器
        interval->rreg = (*it)->rreg;
        (*it)->spill = true;
        //额外添加 处理寄存器
        spillIntervals.push_back(*it);
        active.erase((++it).base());
        //再按照 unhandled interval活跃区间结束位置，将其插入到 active 列表中。
        active.push_back(interval);
        //插入后再次按照结束时间对活跃区间进行排序
        sort(active.begin(), active.end(), compareEnd);
    }
    else {
        //unhandle溢出更晚只需置位spill标志位
        interval->spill = true;
        spillIntervals.push_back(interval);
    }
}

void LinearScan::modifyCode()
{
    for (auto &interval : intervals)
    {
        func->addSavedRegs(interval->rreg);
        for (auto def : interval->defs)
            def->setReg(interval->rreg);
        for (auto use : interval->uses)
            use->setReg(interval->rreg);
    }
}

void LinearScan::genSpillCode()
{
    for (auto &interval : spillIntervals) {
        if (!interval->spill)//只对spill置位的操作 没有溢出
            continue;
        // TODO
        /* HINT:
         * The vreg should be spilled to memory.
         * 1. insert ldr inst before the use of vreg
         * 2. insert str inst after the def of vreg
         */
        //获取栈内相对偏移
        //注意要是负的 以FP为基准
        interval->disp = -func->AllocSpace(4);
        //遍历其 USE 指令的列表，在 USE 指令前插入 LoadMInstruction，将其从栈内加载到目前的虚拟寄存器中
        for (auto use : interval->uses) {
            int op = INT_MIN;
                if (interval->fpu) // 区分操作码
                    op = LoadMInstruction::VLDR;
                else
                    op = LoadMInstruction::LDR;
            //在use之前插入load指令 将其从栈内加载到目的虚拟寄存器中
            auto cur_bb = use->getParent()->getParent();
            if (interval->disp > -254) {//正常情况，直接从fp-off的地方加载
                MachineInstruction *cur_inst = nullptr;
                cur_inst = new LoadMInstruction(cur_bb, op, new MachineOperand(*use),
                    new MachineOperand(MachineOperand::REG, 11), 
                    new MachineOperand(MachineOperand::IMM, interval->disp));
                //USE指令前插入Load指令
                cur_bb->insertBefore(cur_inst, use->getParent());
            }
            //判断当前数据地址是否超过寻址空间 
            //超出寻址空间 不能直接加载 要分两步
            //首先加载到虚拟寄存器 ldr v1, off; off是相对偏移
            //范围超过255就load，最后load范围256*4kb
            else {
                auto internal_reg = new MachineOperand(MachineOperand::VREG, SymbolTable::getLabel());
                MachineInstruction *cur_inst = new LoadMInstruction(cur_bb, LoadMInstruction::LDR,
                    internal_reg, new MachineOperand(MachineOperand::IMM, interval->disp));
                cur_bb->insertBefore(cur_inst, use->getParent());
                cur_inst = new BinaryMInstruction(cur_bb, BinaryMInstruction::ADD,
                    new MachineOperand(*internal_reg), new MachineOperand(*internal_reg),
                    new MachineOperand(MachineOperand::REG, 11));
                cur_bb->insertBefore(cur_inst, use->getParent());
                //超出寻址空间的话 第二步ldr r0,[fp,v1]
                cur_inst = new LoadMInstruction(cur_bb, op,
                    new MachineOperand(*use), new MachineOperand(*internal_reg));
                cur_bb->insertBefore(cur_inst, use->getParent());
            }
        }

        //遍历其 DEF 指令的列表，在 DEF 指令后插入 StoreMInstruction，将其从目前的虚拟寄存器中存到栈内
        for (auto def : interval->defs) {
            //在def之后插入store指令
            auto cur_bb = def->getParent()->getParent();
            int op = INT_MIN;
                if (interval->fpu) // 区分操作码
                    op = StoreMInstruction::VSTR;
                else
                    op = StoreMInstruction::STR;
            //同样要考虑寻址空间 ldr v1, off
            if (interval->disp > -254) {//正常store
                MachineInstruction *cur_inst = nullptr;
                cur_inst = new StoreMInstruction(cur_bb, op,
                    new MachineOperand(*def), new MachineOperand(MachineOperand::REG, 11),
                    new MachineOperand(MachineOperand::IMM, interval->disp));
                //StoreMInstruction要插入DEF指令之后
                cur_bb->insertAfter(cur_inst, def->getParent());
            }
            else {
                auto internal_reg = new MachineOperand(MachineOperand::VREG, SymbolTable::getLabel());
                auto cur_inst = new LoadMInstruction(cur_bb, LoadMInstruction::LDR, internal_reg,
                    new MachineOperand(MachineOperand::IMM, interval->disp));
                cur_bb->insertAfter(cur_inst, def->getParent());
                auto cur_inst1 = new BinaryMInstruction(cur_bb, BinaryMInstruction::ADD,
                    new MachineOperand(*internal_reg), new MachineOperand(*internal_reg),
                    new MachineOperand(MachineOperand::REG, 11));
                cur_bb->insertAfter(cur_inst1, cur_inst);
                MachineInstruction *cur_inst2 = nullptr;
                cur_inst2 = new StoreMInstruction(cur_bb, op,
                    new MachineOperand(*def), new MachineOperand(*internal_reg));
                cur_bb->insertAfter(cur_inst2, cur_inst1);
            }
        }
    }
}

bool LinearScan::compareStart(Interval *a, Interval *b)
{
    return a->start < b->start;
}

bool LinearScan::compareEnd(Interval *a, Interval *b)
{
    return a->end < b->end;
}