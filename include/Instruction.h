#ifndef __INSTRUCTION_H__
#define __INSTRUCTION_H__

#include "Operand.h"
#include "AsmBuilder.h"
#include <vector>
#include <map>
#include <sstream>

class BasicBlock;

class Instruction
{
public:
    Instruction(unsigned instType, BasicBlock *insert_bb = nullptr);
    virtual ~Instruction();
    BasicBlock *getParent();
    bool isUncond() const { return instType == UNCOND; };
    bool isCond() const { return instType == COND; };
    bool isRet() const { return instType == RET; };
    bool isAlloc() const { return instType == ALLOCA; };
    void setParent(BasicBlock *);
    void setNext(Instruction *);
    void setPrev(Instruction *);
    Instruction *getNext();
    Instruction *getPrev();
    virtual void output() const = 0;
    MachineOperand *genMachineOperand(Operand *, AsmBuilder *);
    MachineOperand *genMachineReg(int reg, bool fpu);
    MachineOperand *genMachineVReg(bool fpu);
    MachineOperand *genMachineImm(int val);
    MachineOperand *genMachineLabel(int block_no);
    void DeuLegal(int imm,MachineOperand *, MachineBlock *);
    virtual void genMachineCode(AsmBuilder *) = 0;

protected:
    unsigned instType;
    unsigned opcode;
    Instruction *prev;
    Instruction *next;
    BasicBlock *parent;
    std::vector<Operand *> operands;
    enum { BINARY, UNARY, COND, UNCOND, RET, CALL, LOAD, STORE, CMP, ALLOCA, XOR, ZEXT, GEP, FPTSI, SITFP};
};

// meaningless instruction, used as the head node of the instruction list.
class DummyInstruction : public Instruction
{
public:
    DummyInstruction() : Instruction(-1, nullptr){};
    void output() const {};
    void genMachineCode(AsmBuilder *){};
};

class AllocaInstruction : public Instruction
{
public:
    AllocaInstruction(Operand *dst, SymbolEntry *se, BasicBlock *insert_bb = nullptr);
    ~AllocaInstruction();
    void output() const;
    void genMachineCode(AsmBuilder *);

private:
    SymbolEntry *se;
};

class LoadInstruction : public Instruction
{
public:
    LoadInstruction(Operand *dst, Operand *src_addr, BasicBlock *insert_bb = nullptr);
    ~LoadInstruction();
    void output() const;
    void genMachineCode(AsmBuilder *);
};

class StoreInstruction : public Instruction
{
    int paramno;
public:
    StoreInstruction(Operand *dst_addr, Operand *src, BasicBlock *insert_bb = nullptr, int paramno = -1);
    ~StoreInstruction();
    void output() const;
    void genMachineCode(AsmBuilder *);
};

class BinaryInstruction : public Instruction
{
public:
    BinaryInstruction(unsigned opcode, Operand *dst, Operand *src1, Operand *src2, BasicBlock *insert_bb = nullptr);
    ~BinaryInstruction();
    void output() const;
    void genMachineCode(AsmBuilder *);
    enum
    {
        ADD = 0,
        SUB,
        MUL,
        DIV,
        AND,
        OR,
        MOD,
        XOR,
        FADD,
        FSUB,
        FMUL,
        FDIV
    };
    
};

class CmpInstruction : public Instruction
{
public:
    CmpInstruction(unsigned opcode, Operand *dst, Operand *src1, Operand *src2, BasicBlock *insert_bb = nullptr);
    ~CmpInstruction();
    void output() const;
    void genMachineCode(AsmBuilder *);
    enum { E, NE, GE, L, LE, G,FE,FNE,FGE,FL,FLE,FG };
};

// unconditional branch
class UncondBrInstruction : public Instruction
{
public:
    UncondBrInstruction(BasicBlock *, BasicBlock *insert_bb = nullptr);
    void output() const;
    void setBranch(BasicBlock *);
    BasicBlock *getBranch();
    void genMachineCode(AsmBuilder *);

protected:
    BasicBlock *branch;
};

// conditional branch
class CondBrInstruction : public Instruction
{
public:
    enum {E, NE, L, GE, G, LE};
    CondBrInstruction(BasicBlock *, BasicBlock *, Operand *, BasicBlock *insert_bb = nullptr);
    ~CondBrInstruction();
    void output() const;
    void setTrueBranch(BasicBlock *);
    BasicBlock *getTrueBranch();
    void setFalseBranch(BasicBlock *);
    BasicBlock *getFalseBranch();
    void genMachineCode(AsmBuilder *);

protected:
    BasicBlock *true_branch;
    BasicBlock *false_branch;
};

class CallInstruction : public Instruction // 函数调用
{
public:
    CallInstruction(Operand *dst, SymbolEntry *func, std::vector<Operand *> params, BasicBlock *insert_bb = nullptr);
    ~CallInstruction();
    void output() const;
    void genMachineCode(AsmBuilder *);

private:
    SymbolEntry *func;
};

class RetInstruction : public Instruction
{
public:
    RetInstruction(Operand *src, BasicBlock *insert_bb = nullptr);
    ~RetInstruction();
    void output() const;
    void genMachineCode(AsmBuilder *);
};

class UnaryInstruction : public Instruction
{
public:
    UnaryInstruction(unsigned opcode, Operand* dst, Operand *src, BasicBlock *insert_bb=nullptr);    
    ~UnaryInstruction();
    void output() const;
    enum{SUB, ADD, NOT,FSUB,FADD,FNOT};
    void genMachineCode(AsmBuilder*);
};

//xor instruction, 用于not取反情况
//if a is a bool var,i wanna !a, we use a xor 1.
class XorInstruction : public Instruction // not指令
{
public:
    XorInstruction(Operand *dst, Operand *src, BasicBlock *insert_bb = nullptr);
    ~XorInstruction();
    void output() const;
    void genMachineCode(AsmBuilder *);
};

//零扩展指令，将一种类型的变量拓展为另一种类型的变量，高位补0
class ZextInstruction : public Instruction // bool转为int
{
public:
    ZextInstruction(Operand *dst, Operand *src, bool b2i = false, BasicBlock *insert_bb = nullptr);
    ~ZextInstruction();
    void output() const;
    void genMachineCode(AsmBuilder *);

private:
    bool b2i;
};

//GetElementPtr
class GepInstruction : public Instruction
{
public:
    GepInstruction(Operand *dst, Operand *base, std::vector<Operand *> offs, BasicBlock *insert_bb = nullptr, bool type2 = false);
    ~GepInstruction();
    void output() const;
    void genMachineCode(AsmBuilder *);

private:
    bool type2 = false;
};

class F2IInstruction : public Instruction
{
public:
    F2IInstruction(Operand *dst, Operand *src, BasicBlock *insert_bb = nullptr);
    void output() const;
    void genMachineCode(AsmBuilder *);
};

class I2FInstruction : public Instruction
{
public:
    I2FInstruction(Operand *dst, Operand *src, BasicBlock *insert_bb = nullptr);
    void output() const;
    void genMachineCode(AsmBuilder *);
};

#endif