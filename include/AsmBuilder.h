#ifndef __ASMBUILDER_H__
#define __ASMBUILDER_H__

#include "MachineCode.h"

/*AsmBuilder.h 中为汇编代码构造辅助类。其主要作用就是在中间代码向目标代码进行自顶向下
的转换时，记录当前正在进行转换操作的对象，以进行函数、基本块及指令的插入。*/
class AsmBuilder
{
private:
    MachineUnit *mUnit;         // mahicne unit
    MachineFunction *mFunction; // current machine code function;
    MachineBlock *mBlock;       // current machine code block;
    int cond;              // CmpInstruction opcode, for CondInstruction;
public:
    void setUnit(MachineUnit *unit) { this->mUnit = unit; };
    void setFunction(MachineFunction *func) { this->mFunction = func; };
    void setBlock(MachineBlock *block) { this->mBlock = block; };
    void setCond(int opcode) { this->cond = opcode; };
    MachineUnit *getUnit() { return this->mUnit; };
    MachineFunction *getFunction() { return this->mFunction; };
    MachineBlock *getBlock() { return this->mBlock; };
    int getCond() { return this->cond; };
    static unsigned int rightmove(unsigned int num,int bit){
        return num=(num<<(32-bit)|(num>>bit));
    }
    static bool judge(int num){
        num = (unsigned int)num;
        unsigned int num_temp;
        int i;
        for(i=0;i<32;i++){
            num_temp=rightmove(num,i);
            if(num_temp<=0x00ff&&i%2==0){
                return true;
            }
        }
        return false;
    }
};

#endif