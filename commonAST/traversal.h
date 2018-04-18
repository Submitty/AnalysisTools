#ifndef __TRAVERSAL_H__
#define __TRAVERSAL_H__

#include <iostream>
#include <map>
#include <vector>
#include <stdlib.h>

using namespace std;

class Module;
class Identifier;
class FunctionDef;
class For;
class DoWhile;
class While;
class Args;
class VariableDecl;
class If;
class Bases;
class ClassDef;
class Import;
class CompoundStmt;
class Return;
class Call;
class Assign;
class AugAssign;
class Raise;
class Exec;
class Try;
class Except;
class BinOp;
class UnaryOp;
class Comparison;
class Switch;
class Case;
class List;
class Dict;
class Set;
class Tuple;




class CounterVisitor{
	public:
		CounterVisitor();
		CounterVisitor(map<string, vector<string> > nodesToCount);

		int getFor() const;
		int getModule() const;
		int getIdentifier() const;
		int getFunctionDef() const;
		int getVariableDecl() const;
		int getWhile() const;
		int getDoWhile() const;
		int getSwitch() const;
		int getArgs() const;
		int getCase() const;
		int getIf() const;
		int getClassDef() const;
		int getImport() const;
		int getReturn() const;
		int getCompoundStmt() const;
		int getAssign() const;
		int getRaise() const;
		int getExec() const;
		int getTry() const;
		int getExcept() const;
		int getBases() const;
		int getCall() const;
		int getBinOp() const;
		int getUnaryOp() const;
		int getComparison() const;
		int getComplexity() const;
		string getClassesAndBases() const;
		
		void visit(For* f);
		void visit(Module* m);
		void visit(Identifier* i);
		void visit(FunctionDef* f);
		void visit(VariableDecl* vd);
		void visit(While* w);
		void visit(DoWhile* dw);
		void visit(Switch* s);
		void visit(Args* a);
		void visit(Case* c);
		void visit(If* i);
		void visit(ClassDef* cd);
		void visit(Import* i);
		void visit(Return* r);
		void visit(CompoundStmt* cs);
		void visit(Assign* a);
		void visit(AugAssign* a);
		void visit(Raise* r);
		void visit(Exec* e);
		void visit(Try* t);
		void visit(Except* e);
		void visit(Bases* b);
		void visit(Call* c);
		void visit(BinOp* bo);
		void visit(UnaryOp* uo);
		void visit(Comparison* c);
		void visit(List* l);
		void visit(Dict* d);
		void visit(Set* s);
		void visit(Tuple* t);

private:

		int countModule;
		int countIdentifier;
		int countFunctionDef;
		int countVariableDecl;
		int countFor;
		int countWhile;
		int countDoWhile;
		int countIf;
		int countClassDef;
		int countImport;
		int countCompoundStmt;
		int countReturn;
		int countAssign;
		int countAugAssign;
		int countRaise;
		int countExec;
		int countTry;
		int countExcept;
		int countBases;
		int countCall;
		int countBinOp;
		int countUnaryOp;
		int countComparison;
		int countCase;
		int countSwitch;
		int countArgs;
		int complexity;
		int countList;
		int countSet;
		int countTuple;
		int countDict;

		
		bool findingComplexity;
	
		map<string, vector<string> > nodesToCount;


};

#endif
