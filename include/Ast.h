#ifndef __AST_H__
#define __AST_H__

#include <fstream>
#include <iostream>
#include <stack>
#include "Operand.h"
#include "Type.h"

class SymbolEntry;
class Unit;
class Function;
class BasicBlock;
class Instruction;
class IRBuilder;

class Node
{
private:
    static int counter;
    int seq;
    Node *next;

protected:
    std::vector<Instruction *> true_list;
    std::vector<Instruction *> false_list;
    static IRBuilder *builder;
    void backPatch(std::vector<Instruction *> &list, BasicBlock *bb);
    std::vector<Instruction *> merge(std::vector<Instruction *> &list1, std::vector<Instruction *> &list2);

public:
    Node();
    int getSeq() const { return seq; };
    static void setIRBuilder(IRBuilder *ib) { builder = ib; };
    virtual void output(int level) = 0;
    virtual void typeCheck() = 0;
    virtual void genCode() = 0;
    void setNext(Node *node);
    Node *getNext() { return next; }
    std::vector<Instruction *> &trueList() { return true_list; }
    std::vector<Instruction *> &falseList() { return false_list; }
};

class ExprNode : public Node
{
private:
    int kind;
protected:
    enum { EXPR, IMPLICTCASTEXPR, UNARYEXPR };
    
    SymbolEntry *symbolEntry;
    Operand *dst; // The result of the subtree is stored into dst.
    Type *type;
    bool isCond = 0;
    bool isAnd_Or = 0;
    bool isFir = 1;
public:
    ExprNode(SymbolEntry *symbolEntry, int kind = EXPR) : kind(kind), symbolEntry(symbolEntry), isBr(false){};
    Operand *getOperand() { return dst; };
    SymbolEntry *getSymPtr() { return symbolEntry; };
    bool isBr;
    // bool isConde() const { return isCond; };
    // void setIsCond(bool isCond) { this->isCond = isCond; };
    void output(int level);
    void genCode(){};
    void typeCheck(){};
    virtual double getValue() { return -1; };
    virtual Type *getType() { return type; };
    bool IsFir(){ return isFir; };
    void setnotfir(){ isFir = 0; };
    bool IsCond() {return isCond; };
    bool IsAnd_Or() {return isAnd_Or; };
    void setOperand(Operand* op){ dst = op; }; 
    bool isExpr() const { return kind == EXPR; };
    bool isImplictCastExpr() const { return kind == IMPLICTCASTEXPR; };
    bool isUnaryExpr() const { return kind == UNARYEXPR; };
    Type* getOriginType() { return type; };
};

class BinaryExpr : public ExprNode
{
private:
    int op;
    ExprNode *expr1, *expr2;

public:
    enum{ADD, SUB, MUL, DIV, MOD, AND, OR, LESS, LESSEQUAL, GREATER, GREATEREQUAL, EQUAL, NOTEQUAL};
    BinaryExpr(SymbolEntry *se, int op, ExprNode *expr1, ExprNode *expr2);
    void output(int level);
    double getValue();
    void typeCheck();
    void genCode();
};

class UnaryExpr : public ExprNode
{
private:
    int op;
    ExprNode *expr;

public:
    enum{ NOT, SUB, ADD};
    UnaryExpr(SymbolEntry *se, int op, ExprNode *expr);
    void output(int level);
    double getValue();
    void typeCheck();
    void genCode();
    int getOp() const { return op; };
    void setType(Type* type) { this->type = type; }
};

class CallExpr : public ExprNode
{
private:
    std::vector<ExprNode *> params; // ??????
    ExprNode *param;

public:
    CallExpr(SymbolEntry *se, ExprNode *param = nullptr);
    void output(int level);
    void typeCheck();
    void genCode();
};

class Constant : public ExprNode
{
public:
    Constant(SymbolEntry *se) : ExprNode(se) {
        dst = new Operand(se);
        type = se->getType();
    };
    void output(int level);
    void typeCheck();
    void genCode();
    double getValue();
};

class Id : public ExprNode
{
private:
    ExprNode *index;
    bool isPointer = false;

public:
    Id(SymbolEntry *se, ExprNode *index = nullptr) : ExprNode(se), index(index) {
        this->type = se->getType();
        ArrayType* arrType;
        if (se->getType()->isArray()) {
            std::vector<int> indexs = dynamic_cast<ArrayType* >(se->getType())->getIndexs();
            for (ExprNode *expr = index; expr; expr = dynamic_cast<ExprNode* >(expr->getNext()))
                indexs.erase(indexs.begin()); // ????????????????????????????????????
            if (indexs.size() <= 0) { 
                // ??????????????????????????????????????????????????????????????????????????????
                arrType = dynamic_cast<ArrayType* >(se->getType());
                goto judge;
            }
            else{ 
                // ?????????????????????????????????????????????????????????????????????????????????????????????????????????
                // indexs.erase(indexs.begin());
                isPointer = true;
                arrType = dynamic_cast<ArrayType* >(se->getType());
                if (arrType->getBaseType()->isInt())
                    this->type = new PointerType(new ArrayType(indexs, TypeSystem::intType));
                else if (arrType->getBaseType()->isFloat())
                    this->type = new PointerType(new ArrayType(indexs, TypeSystem::floatType));
            }
        }
        else if (se->getType()->isPtr()) {
            arrType = dynamic_cast<ArrayType* >((dynamic_cast<PointerType* >(se->getType()))->getType());
            if (index == nullptr) // ????????????????????????
                this->type = se->getType();
            else { // ????????????
                judge :
                if (arrType->getBaseType()->isInt()) 
                    this->type = TypeSystem::intType;
                else if (arrType->getBaseType()->isFloat()) 
                    this->type = TypeSystem::floatType;
            }
        }
        else
            this->type = se->getType();
        dst = new Operand(new TemporarySymbolEntry(this->type, SymbolTable::getLabel()));
    };
    void output(int level);
    void typeCheck();
    void genCode();
    ExprNode *getIndex() { return index; };
    double getValue();
};

// ????????????bool???int?????????????????????int???float??????????????????bool???float?????????
class ImplictCastExpr : public ExprNode
{
private:
    ExprNode *expr;
    int op; // ????????????

public:
    enum
    {
        BTI, // bool???int
        ITB, // int???bool
        FTI, // float???int
        ITF, // int???float
        BTF, // bool???float
        FTB  // float???bool
    };
    // bool b2i = false
    ImplictCastExpr(ExprNode *expr, int op) : ExprNode(nullptr, IMPLICTCASTEXPR), expr(expr), op(op) {
        switch (op)
        {
        case ITB:
        case FTB:
            type = TypeSystem::boolType;
            break;
        case BTI:
        case FTI:
            type = TypeSystem::intType;
            break;
        case BTF:
        case ITF:
            type = TypeSystem::floatType;
            break;
        default:
            type = TypeSystem::intType;
            break;
        }
        dst = new Operand(new TemporarySymbolEntry(type, SymbolTable::getLabel()));
        if (expr->isBr)
        {
            //???????????????????????????????????????????????????????????????????????????
            expr->isBr=false;
            this->isBr = true;
        }
    };
    double getValue();
    void output(int level);
    void typeCheck(){};
    void genCode();
};

class StmtNode : public Node
{
private:
    int kind;

protected:
    enum { IF, IFELSE, WHILE, COMPOUND, RETURN };

public:
    StmtNode(int kind = -1) : kind(kind){};
    bool isIf() const { return kind == IF; };
    void typeCheck(){};
    void genCode(){};
};

class CompoundStmt : public StmtNode
{
private:
    StmtNode *stmt;

public:
    CompoundStmt(StmtNode *stmt = nullptr) : stmt(stmt){};
    void output(int level);
    void typeCheck();
    void genCode();
};

class SeqNode : public StmtNode
{
private:
    StmtNode *stmt1, *stmt2;

public:
    SeqNode(StmtNode *stmt1, StmtNode *stmt2) : stmt1(stmt1), stmt2(stmt2){};
    void output(int level);
    void typeCheck();
    void genCode();
};

class DeclStmt : public StmtNode
{
private:
    Id *id;
    ExprNode *expr;
    ExprNode **exprArray; // ???Id??????????????????????????????????????????????????????????????????

public:
    DeclStmt(Id *id, ExprNode *expr = nullptr) : id(id), expr(expr) {
        this->exprArray = nullptr;
        if (expr) {
            // ??????id????????????expr????????????????????????????????????????????????id???????????????
            if (id->getType()->isFloat() && expr->getType()->isInt())
                this->expr = new ImplictCastExpr(expr, ImplictCastExpr::ITF);
            if (id->getType()->isInt() && expr->getType()->isFloat())
                this->expr = new ImplictCastExpr(expr, ImplictCastExpr::FTI);
        }
    };
    void output(int level);
    void typeCheck();
    void genCode();
    void setInitArray(ExprNode **exprArray) ;
    Id *getId() { return id; };
};

class BlankStmt : public StmtNode
{
public:
    BlankStmt(){}; // do nothing
    void output(int level);
    void typeCheck(){};
    void genCode();
};

class IfStmt : public StmtNode
{
private:
    ExprNode *cond;
    StmtNode *thenStmt;

public:
    IfStmt(ExprNode *cond, StmtNode *thenStmt) : cond(cond), thenStmt(thenStmt) {
        // cond?????????bool?????????
        if (cond->getType()->isInt())
            this->cond = new ImplictCastExpr(cond, ImplictCastExpr::ITB);
        else if (cond->getType()->isFloat())
            this->cond = new ImplictCastExpr(cond, ImplictCastExpr::FTB);
    };
    void output(int level);
    void typeCheck();
    void genCode();
};

class IfElseStmt : public StmtNode
{
private:
    ExprNode *cond;
    StmtNode *thenStmt;
    StmtNode *elseStmt;

public:
    IfElseStmt(ExprNode *cond, StmtNode *thenStmt, StmtNode *elseStmt) 
        : cond(cond), thenStmt(thenStmt), elseStmt(elseStmt) {
        // cond?????????bool?????????
        if (cond->getType()->isInt())
            this->cond = new ImplictCastExpr(cond, ImplictCastExpr::ITB);
        else if (cond->getType()->isFloat())
            this->cond = new ImplictCastExpr(cond, ImplictCastExpr::FTB);
    };
    void output(int level);
    void typeCheck();
    void genCode();
};

class WhileStmt : public StmtNode
{
private:
    ExprNode *cond;
    StmtNode *stmt;
    BasicBlock *cond_bb;
    BasicBlock *end_bb;

public:
    WhileStmt(ExprNode *cond, StmtNode *stmt = nullptr) : cond(cond), stmt(stmt) {
        // cond?????????bool?????????
        if (cond->getType()->isInt())
            this->cond = new ImplictCastExpr(cond, ImplictCastExpr::ITB);
        else if (cond->getType()->isFloat())
            this->cond = new ImplictCastExpr(cond, ImplictCastExpr::FTB);
    };
    void setStmt(StmtNode *stmt) { this->stmt = stmt; };
    void output(int level);
    void typeCheck();
    void genCode();
    BasicBlock *get_cond_bb() { return this->cond_bb; };
    BasicBlock *get_end_bb() { return this->end_bb; };
};

class BreakStmt : public StmtNode
{
private:
    StmtNode *whileStmt;

public:
    BreakStmt(StmtNode *whileStmt) {
        this->whileStmt = whileStmt;
    }
    void output(int level);
    void typeCheck(){};
    void genCode();
};

class ContinueStmt : public StmtNode
{
private:
    StmtNode *whileStmt;

public:
    ContinueStmt(StmtNode *whileStmt) {
        this->whileStmt = whileStmt;
    }
    void output(int level);
    void typeCheck(){};
    void genCode();
};

class ReturnStmt : public StmtNode
{
private:
    ExprNode *retValue;

public:
    ReturnStmt(ExprNode *retValue = nullptr, Type *funcRetType = nullptr) 
        : retValue(retValue) {
        // ?????????????????????????????????????????????
        Type *retType;
        if (retValue == nullptr)
            retType = TypeSystem::voidType;
        else
            retType = retValue->getType();
        // ????????????????????????????????????
        if (funcRetType->isFloat() && retType->isInt())
            this->retValue = new ImplictCastExpr(this->retValue, ImplictCastExpr::ITF);
        else if (funcRetType->isInt() && retType->isFloat())
            this->retValue = new ImplictCastExpr(this->retValue, ImplictCastExpr::FTI);
    }
    void output(int level);
    void typeCheck();
    void genCode();
};

class AssignStmt : public StmtNode
{
private:
    ExprNode *lval;
    ExprNode *expr;

public:
    AssignStmt(ExprNode *lval, ExprNode *expr) : lval(lval), expr(expr) {
        Type *type = dynamic_cast<Id* >(lval)->getType();
        // ????????????????????????????????????
        if (type->isInt() && expr->getType()->isFloat())
            this->expr = new ImplictCastExpr(this->expr, ImplictCastExpr::FTI);
        else if (type->isFloat() && expr->getType()->isInt()) 
            this->expr = new ImplictCastExpr(this->expr, ImplictCastExpr::ITF);
    }
    void output(int level);
    void typeCheck();
    void genCode();
};

class ExprStmt : public StmtNode
{
private:
    ExprNode *expr;

public:
    ExprStmt(ExprNode *expr) : expr(expr) {};
    void output(int level);
    void typeCheck();
    void genCode();
};

class FunctionDef : public StmtNode
{
private:
    SymbolEntry *se;
    // ??????????????? next??????
    DeclStmt *decl;
    StmtNode *stmt;

public:
    FunctionDef(SymbolEntry *se, DeclStmt *decl, StmtNode *stmt) : se(se), decl(decl), stmt(stmt){};
    FunctionDef(SymbolEntry* se, StmtNode* decl) : se(se), stmt(decl){};
    FunctionDef(Id* id, StmtNode* decl, StmtNode* stmt) : se(id->getSymPtr()), decl((DeclStmt*)decl), stmt(stmt){};
    void output(int level);
    void typeCheck();
    void genCode();
    void setStmt(StmtNode *stmt) { this->stmt = stmt; };
    void setSymbolEntry(SymbolEntry* se){ this->se = se;};
    SymbolEntry *getSymbolEntry() { return se; };
    DeclStmt* getDecl() { return decl; };
};

class Ast
{
private:
    Node *root;

public:
    Ast() { root = nullptr; }
    void setRoot(Node *n) { root = n; }
    void output();
    void typeCheck();
    void genCode(Unit *unit);
};

#endif
