#ifndef __ASMBUILDER_H__
#define __ASMBUILDER_H__

#include "MachineCode.h"

class AsmBuilder
{
private:
    MachineUnit *mUnit;         // mahicne unit
    MachineFunction *mFunction; // current machine code function;
    MachineBlock *mBlock;       // current machine code block;
    int cmpOpcode;              // CmpInstruction opcode, for CondInstruction;
public:
    void setUnit(MachineUnit *unit) { this->mUnit = unit; };
    void setFunction(MachineFunction *func) { this->mFunction = func; };
    void setBlock(MachineBlock *block) { this->mBlock = block; };
    void setCmpOpcode(int opcode) { this->cmpOpcode = opcode; };
    MachineUnit *getUnit() { return this->mUnit; };
    MachineFunction *getFunction() { return this->mFunction; };
    MachineBlock *getBlock() { return this->mBlock; };
    int getCmpOpcode() { return this->cmpOpcode; };
    static bool isLegalImm(int imm)
    {
        /*1.如十六进制数在0x00到0xFF之间，则它一定是合法的

         2.若十六进制数>0XFF则将十六进制的数转化成十进制

         3.如果这个十进制数能够被4整除则这个十六进制的立即数和合法的，否则是非法的
         如果一个立即数小于 0xFF（255）那么直接用前 7～0 位表示即可，此时不用移位，11～8 位的 Rotate_imm 等于 0。
        如果前八位 immed_8 的数值大于 255，那么就看这个数是否能有 immed_8 中的某个数移位 2*Rotate_imm 位形成的。如果能，那么就是合法立即数；否则非法。
        原文链接：https://blog.csdn.net/sinat_41104353/article/details/83097466
         */
        unsigned int num = (unsigned int)imm;
        for (int i = 0; i < 16; i++)
        {
            if (num <= 0xff)
            {
                return true;
            }
            num = ((num << 2) | (num >> 30));
        }
        return false;
    }
};

#endif