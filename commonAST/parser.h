#ifndef __PARSER_H__
#define __PARSER_H__

#include <iostream>
#include <list>
#include <fstream>
#include <stdlib.h>
#include <vector>
#include "traversal.h"


class Stmt;
class CompoundStmt;
class Except;

string getIndentation(int level);


class Token{
	public:
		string value;
		int level;
};

class ASTNode{
	public:
		virtual string getType() = 0;
		virtual list<ASTNode*> getChildren() = 0;
		virtual void printNode(int level){
			cout << getIndentation(level);
			cout << "-----------------" << endl;
			cout << getIndentation(level);
			cout << "- " << getType() << ": " << endl;
			cout << getIndentation(level);
			cout << "-----------------" << endl;

		}

		virtual void printNodeAsJSON(int level){
			cout << getIndentation(level);
			cout << "\"type\": \"node\"," << endl;
			cout << getIndentation(level);
			cout << "\"tags\":[" << endl;
			cout << getIndentation(level+1);
			cout << "\"" << getType() << "\"\n";
			cout << getIndentation(level);
			cout << "]" << endl;
		}

		virtual void accept(CounterVisitor& v) = 0;

		int complexity;
};



class Expr : public ASTNode{
	public:
		string getType(){
			return "Expr";
		}

		list<ASTNode*> getChildren(){
			return children;
		}

		void accept(CounterVisitor& v){
			v.visit(this);
		}

		list<ASTNode*> children;
};

class Stmt : public ASTNode{
	virtual string getType() = 0;
	virtual list<ASTNode*> getChildren() = 0;
	virtual void accept(CounterVisitor& v) = 0;
};


class Call: public Expr{
	public:
		string getType(){
			return "Call";
		}

		list<ASTNode*> getChildren(){
			list<ASTNode*> children;
			children.push_back((ASTNode*)argsList);
			list<ASTNode*>::iterator itr;
			for(itr=otherChildren.begin(); itr != otherChildren.end(); itr++){
				if((*itr) != NULL){
					children.push_back((ASTNode*)*itr);
				}
			}

			return children;
		}

		void printNode(int level);

		void accept(CounterVisitor &v);

		string func;
		Args* argsList;
		list<ASTNode*> otherChildren;

		string obj;
		int level;
};



class Module : public ASTNode{
	public:
		string getType(){
			return "Module";
		}

		list<ASTNode*> getChildren(){
			list<ASTNode*> children;
			list<ASTNode*>::iterator itr;
			for(itr=body.begin(); itr != body.end(); itr++){
				if((*itr) != NULL){
					children.push_back((ASTNode*)*itr);
				}
			}
			return children;
		}


		void accept(CounterVisitor& v){
			list<ASTNode*> children = getChildren();
			list<ASTNode*>::iterator itr;
			for(itr=children.begin(); itr != children.end(); itr++){
				(*itr)->accept(v);	
				if(this->complexity <= (*itr)->complexity){
					this->complexity = (*itr)->complexity; 
				}
			}

			v.visit(this);
		}

		list<ASTNode*> body;
};


class Dict: public Expr{
	public:
		string getType(){
			return "Dict";
		}


		list<ASTNode*> getChildren(){
			return values;
		}


		void accept(CounterVisitor& v){
			v.visit(this);
		}


		void printNode(int level){
			cout << getIndentation(level);
			cout << "-----------------" << endl;
			cout << getIndentation(level);
			cout << "- Dict: " << endl;
			cout << getIndentation(level);
			cout << "-----------------" << endl;
		}

		list<ASTNode*> values;
};




class Set: public Expr{
	public:
		string getType(){
			return "Set";
		}


		list<ASTNode*> getChildren(){
			return values;
		}


		void accept(CounterVisitor& v){
			v.visit(this);
		}


		void printNode(int level){
			cout << getIndentation(level);
			cout << "-----------------" << endl;
			cout << getIndentation(level);
			cout << "- Set: " << endl;
			cout << getIndentation(level);
			cout << "-----------------" << endl;
		}

		list<ASTNode*> values;
};


class Tuple: public Expr{
	public:
		string getType(){
			return "Tuple";
		}


		list<ASTNode*> getChildren(){
			return values;
		}


		void accept(CounterVisitor& v){
			v.visit(this);
		}


		void printNode(int level){
			cout << getIndentation(level);
			cout << "-----------------" << endl;
			cout << getIndentation(level);
			cout << "- Tuple: " << endl;
			cout << getIndentation(level);
			cout << "-----------------" << endl;
		}

		list<ASTNode*> values;
};

class List: public Expr{
	public:
		string getType(){
			return "List";
		}


		list<ASTNode*> getChildren(){
			return values;
		}


		void accept(CounterVisitor& v){
			v.visit(this);
		}


		void printNode(int level){
			cout << getIndentation(level);
			cout << "-----------------" << endl;
			cout << getIndentation(level);
			cout << "- List: " << endl;
			cout << getIndentation(level);
			cout << "-----------------" << endl;
		}

		list<ASTNode*> values;
};

class Identifier : public Expr{
	public:
		string getType(){
			return "Identifier";
		}

		list<ASTNode*> getChildren(){
			list<ASTNode*> children;
			return children;
		}	

		void accept(CounterVisitor& v){
			v.visit(this);
		}


		void printNode(int level){
			cout << getIndentation(level);
			cout << "-----------------" << endl;
			cout << getIndentation(level);
			cout << "- Indentifier: " << endl;
			cout << getIndentation(level);
			cout << "- Name: " << name << endl;
			cout << getIndentation(level);
			cout << "-----------------" << endl;
		}	

		string name;
};

class Args : public ASTNode{
	public:
		string getType(){
			return "args";
		}

		list<ASTNode*> getChildren(){
			if(argList.size() == 0){
				return list<ASTNode*>(); 
			}

			list<ASTNode*> children;
			list<Expr*>::iterator itr;
			for(itr= argList.begin(); itr != argList.end(); itr++){
				if(*itr){
					children.push_back(*itr);
				}
			}

			return children;
		}

		void accept(CounterVisitor& v){
			list<ASTNode*> children = getChildren();

			list<ASTNode*>::iterator itr;
			for(itr=children.begin(); itr != children.end(); itr++){
				(*itr)->accept(v);	
				if(this->complexity <= (*itr)->complexity){
					this->complexity = (*itr)->complexity;
				}
			}

			v.visit(this);
		}

		list<Expr*> argList;
};

class FunctionDef : public Stmt{
	public:
		string getType(){
			return "FunctionDef";
		}

		list<ASTNode*> getChildren(){
			list<ASTNode*> children;
			children.push_back(name);
			if(compoundStmt){
				children.push_back((ASTNode*)compoundStmt);
			}

			list<ASTNode*>::iterator itr;
			for(itr=initList.begin(); itr != initList.end(); itr++){
				if(*itr){
					children.push_back(*itr);	
				}	

			}


			return children;	
		}

		virtual void printNode(int level){
			cout << getIndentation(level);
			cout << "-----------------" << endl;
			cout << getIndentation(level);
			if(compoundStmt){
				cout << "- FunctionDef: " << endl;
			}else{
				cout << "- FunctionPrototype: " << endl;
			}

			cout << getIndentation(level);
			cout << "-----------------" << endl;

		}


		void accept(CounterVisitor& v){
			list<ASTNode*> children = getChildren();

			list<ASTNode*>::iterator itr;
			for(itr=children.begin(); itr != children.end(); itr++){
				(*itr)->accept(v);	
				if((*itr)->complexity >= this->complexity){

					this->complexity = (*itr)->complexity;
				}
			}

			v.visit(this);
		}



		Identifier* name;
		CompoundStmt* compoundStmt;
		list<ASTNode*> initList;	
};

class VariableDecl: public Stmt{
	public:
		string getType(){
			return "VariableDecl";
		}	

		list<ASTNode*> getChildren(){
			list<ASTNode*> children;

			list<Expr*>::iterator itr;
			for(itr= right.begin(); itr != right.end(); itr++){
				if(*itr){
					children.push_back(*itr);
				}
			}

			return children;
		}

		void accept(CounterVisitor& v){
			list<ASTNode*> children = getChildren();

			list<ASTNode*>::iterator itr;
			for(itr=children.begin(); itr != children.end(); itr++){
				(*itr)->accept(v);	
				if(this->complexity <= (*itr)->complexity){
					this->complexity += (*itr)->complexity;
				}
			}

			v.visit(this);
		}


		list<Expr*> right;
};

class For : public Stmt{
	public:
		string getType(){
			return "For";
		}

		list<ASTNode*> getChildren(){
			list<ASTNode*> children;
			children.push_back((ASTNode*)compoundStmt);

			list<ASTNode*>::iterator itr;
			for(itr=stopCond.begin(); itr != stopCond.end(); itr++){
				if(*itr){
					children.push_back(*itr);
				}
			}

			return children;
		}

		void accept(CounterVisitor& v){
			list<ASTNode*> children = getChildren();

			this->complexity = 1;
			list<ASTNode*>::iterator itr;
			for(itr=children.begin(); itr != children.end(); itr++){
				(*itr)->accept(v);	
				if(this->complexity <= ((*itr)->complexity)){
					this->complexity = (*itr)->complexity + 1;
				}
			}

			v.visit(this);
		}


		list<ASTNode*> stopCond;
		CompoundStmt* compoundStmt;
};

class While: public Stmt{
	public:
		string getType(){
			return "While";
		}

		list<ASTNode*> getChildren(){
			list<ASTNode*> children;
			list<Expr*>::iterator itr;
			for(itr=test.begin(); itr != test.end(); itr++){
				if(*itr){
					children.push_back(*itr);
				}
			}

			children.push_back((ASTNode*)compoundStmt);
			return children;
		}

		void accept(CounterVisitor &v){
			list<ASTNode*> children = getChildren();

			this->complexity = 1;

			list<ASTNode*>::iterator itr;
			for(itr=children.begin(); itr != children.end(); itr++){
				(*itr)->accept(v);	
				if(this->complexity <= (*itr)->complexity){
					this->complexity = (*itr)->complexity + 1;
				}
			}

			v.visit(this);
		}


		list<Expr*> test;
		CompoundStmt* compoundStmt;
};

class DoWhile: public Stmt{
	public:
		string getType(){
			return "Do While";
		}

		list<ASTNode*> getChildren(){
			list<ASTNode*> children;
			list<Expr*>::iterator itr;
			for(itr=test.begin(); itr != test.end(); itr++){
				if(*itr){
					children.push_back(*itr);
				}
			}

			children.push_back((ASTNode*)compoundStmt);
			return children;
		}

		void accept(CounterVisitor &v){
			list<ASTNode*> children = getChildren();

			this->complexity = 1;

			list<ASTNode*>::iterator itr;
			for(itr=children.begin(); itr != children.end(); itr++){
				(*itr)->accept(v);	
				if(this->complexity <= (*itr)->complexity){
					this->complexity = (*itr)->complexity + 1;
				}
			}

			v.visit(this);
		}


		list<Expr*> test;
		CompoundStmt* compoundStmt;
};

class IfBlock: public Stmt{
	public: 
	
		string getType(){
			return "IfBlock";
		}

		list<ASTNode*> getChildren(){
			list<ASTNode*> children;
			list<Stmt*>::iterator itr;
			for(itr=ifs.begin(); itr != ifs.end(); itr++){
				children.push_back((ASTNode*) *itr);
			}

			return children;
		}

		void accept(CounterVisitor &v){
			v.visit(this);
		}


	list<Stmt*> ifs;
};

class If: public Stmt{
	public:
		string getType(){
			return "If";
		}


		list<ASTNode*> getChildren(){
			list<ASTNode*> children;
			if(test){
				children.push_back(test);
			}

			if(compoundStmt){
				children.push_back((ASTNode*)compoundStmt);
			}else{
				list<ASTNode*>::iterator itr;
				for(itr=body.begin(); itr != body.end(); itr++){
					if(*itr){
						children.push_back(*itr);	
					}	

				}
			}

			list<Stmt*>::iterator itr;
			for(itr=orelses.begin(); itr != orelses.end(); itr++){
				children.push_back(*itr);	
			}
			return children;
		}

		void accept(CounterVisitor &v){
			list<ASTNode*> children = getChildren();

			list<ASTNode*>::iterator itr;
			for(itr=children.begin(); itr != children.end(); itr++){
				(*itr)->accept(v);	
				if(this->complexity <= (*itr)->complexity){
					this->complexity = (*itr)->complexity;
				}
			}

			v.visit(this);
		}




		Expr* test;
		CompoundStmt* compoundStmt;
		list<ASTNode*> body;
		list<Stmt*> orelses;	
};

class Bases: public Stmt{
	public:
		string getType(){
			return "Bases";
		}

		list<ASTNode*> getChildren(){
			list<ASTNode*> children;
			list<Identifier*>::iterator itr;
			for(itr=data.begin(); itr != data.end(); itr++){
				if(*itr){
					children.push_back(*itr);
				}
			}
			return children;
		}

		void accept(CounterVisitor &v){
			list<ASTNode*> children = getChildren();

			list<ASTNode*>::iterator itr;
			for(itr=children.begin(); itr != children.end(); itr++){
				(*itr)->accept(v);	
			}

			v.visit(this);
		}



		list<Identifier*> data;
};

class Switch: public Stmt{
	public:
		string getType(){
			return "Switch";	
		}


		list<ASTNode*> getChildren(){
			list<ASTNode*> children;
			if(cond){
				children.push_back(cond);
			}

			children.push_back((ASTNode*) compoundStmt);
			return children;
		}	

		void accept(CounterVisitor& v){
			list<ASTNode*> children = getChildren();

			list<ASTNode*>::iterator itr;
			for(itr=children.begin(); itr != children.end(); itr++){
				(*itr)->accept(v);	
				if(this->complexity <= (*itr)->complexity){
					this->complexity = (*itr)->complexity;
				}
			}

			v.visit(this);
		}


		Expr* cond;
		CompoundStmt* compoundStmt;
};


class Case: public Stmt{
	public:
		string getType(){
			return "Case";	
		}


		list<ASTNode*> getChildren(){
			list<ASTNode*> children;

			list<ASTNode*>::iterator itr2;
			for(itr2=body.begin(); itr2 != body.end(); itr2++){
				if(*itr2){
					children.push_back(*itr2);
				}	
			}
			return children;

		}	

		void accept(CounterVisitor& v){
			list<ASTNode*> children = getChildren();

			list<ASTNode*>::iterator itr;
			for(itr=children.begin(); itr != children.end(); itr++){
				(*itr)->accept(v);	

				if(this->complexity <= (*itr)->complexity){
					this->complexity = (*itr)->complexity;
				}
			}

			v.visit(this);
		}



		list<ASTNode*> body;

};

class ClassDef : public Stmt{
	public:
		string getType(){
			return "ClassDef";
		}

		list<ASTNode*> getChildren(){
			list<ASTNode*> children;
			children.push_back(name);

			if(bases){
				children.push_back(bases);
			}

			list<ASTNode*>::iterator itr2;
			for(itr2=body.begin(); itr2 != body.end(); itr2++){
				if(*itr2){
					children.push_back(*itr2);
				}	
			}
			return children;
		}	

		void accept(CounterVisitor& v){
			list<ASTNode*> children = getChildren();

			list<ASTNode*>::iterator itr;
			for(itr=children.begin(); itr != children.end(); itr++){
				(*itr)->accept(v);	
				if(this->complexity <= (*itr)->complexity){
					this->complexity = (*itr)->complexity;
				}

			}

			v.visit(this);
		}



		Identifier* name;
		Bases* bases;
		list<ASTNode*> body;
};



class Import: public Stmt{
	public: 
		string getType(){
			return "Import";
		}

		list<ASTNode*> getChildren(){
			list<ASTNode*> children;
			list<Identifier*>::iterator itr;
			for(itr=names.begin(); itr != names.end(); itr++){
				if(*itr){
					children.push_back(*itr);
				}
			}
			return children;
		}

		void accept(CounterVisitor &v){
			list<ASTNode*> children = getChildren();

			list<ASTNode*>::iterator itr;
			for(itr=children.begin(); itr != children.end(); itr++){
				(*itr)->accept(v);	
			}

			v.visit(this);
		}



		list<Identifier*> names;
};

class CompoundStmt : public Stmt{
	public:
		string getType(){
			return "CompoundStmt";
		}

		list<ASTNode*> getChildren(){
			list<ASTNode*> children;
			list<ASTNode*>::iterator itr;
			for(itr=body.begin(); itr != body.end(); itr++){
				if(*itr){
					children.push_back(*itr);
				}
			}
			return children;
		}

		void accept(CounterVisitor &v){
			list<ASTNode*> children = getChildren();

			list<ASTNode*>::iterator itr;
			for(itr=children.begin(); itr != children.end(); itr++){
				(*itr)->accept(v);	
				if(this->complexity <= (*itr)->complexity){
					this->complexity = (*itr)->complexity;
				}

			}

			v.visit(this);
		}



		list<ASTNode*> body;
};

class Return: public Stmt{
	public:
		string getType(){
			return "Return";
		}

		list<ASTNode*> getChildren(){
			list<ASTNode*> children;
			if(value){
				children.push_back(value);
			}
			return children;
		}

		void accept(CounterVisitor &v){
			list<ASTNode*> children = getChildren();

			list<ASTNode*>::iterator itr;
			for(itr=children.begin(); itr != children.end(); itr++){
				(*itr)->accept(v);	
				if(this->complexity <= (*itr)->complexity){
					this->complexity = (*itr)->complexity;
				}

			}

			v.visit(this);
		}


		Expr* value;
};


class Assign: public Stmt{
	public:
		string getType(){
			return "Assign";
		}

		list<ASTNode*> getChildren(){
			list<ASTNode*> children;
			list<Expr*>::iterator itr; 
			for(itr=targets.begin(); itr != targets.end(); itr++){
				if(*itr){
					children.push_back(*itr);
				}
			}
			if(value){
				children.push_back(value);
			}
			return children;
		}

		void accept(CounterVisitor &v){
			list<ASTNode*> children = getChildren();

			list<ASTNode*>::iterator itr;
			for(itr=children.begin(); itr != children.end(); itr++){
				(*itr)->accept(v);	
				if(this->complexity <= (*itr)->complexity){
					this->complexity = (*itr)->complexity;
				}

			}

			v.visit(this);
		}


		list<Expr*> targets;
		Expr* value;	
};

class AugAssign: public Stmt{
	public:
		string getType(){
			return "AugAssign";
		}

		list<ASTNode*> getChildren(){
			list<ASTNode*> children;
			if(target){
				children.push_back(target);
			}
			if(value){
				children.push_back(value);
			}
			return children;
		}

		void accept(CounterVisitor &v){
			list<ASTNode*> children = getChildren();

			list<ASTNode*>::iterator itr;
			for(itr=children.begin(); itr != children.end(); itr++){
				(*itr)->accept(v);	
				if(this->complexity <= (*itr)->complexity){
					this->complexity = (*itr)->complexity;
				}

			}

			v.visit(this);
		}




		Expr* target;
		Expr* value;
};

class Raise: public Stmt{
	string getType(){
		return "Raise";
	}

	list<ASTNode*> getChildren(){
		list<ASTNode*> children;
		return children;
	}

	void accept(CounterVisitor &v){
		v.visit(this);
	}


};

class Exec: public Stmt{
	public:
		string getType(){
			return "Exec";	
		}

		list<ASTNode*> getChildren(){
			list<ASTNode*> children;
			return children;
		}

		void accept(CounterVisitor &v){
			v.visit(this);
		}
};

class Try: public Stmt{
	public:
		string getType(){
			return "Try";
		}

		list<ASTNode*> getChildren(){
			list<ASTNode*> children;
			children.push_back(body);
			children.push_back((ASTNode*)except);
			return children;
		}

		void accept(CounterVisitor &v){
			list<ASTNode*> children = getChildren();

			list<ASTNode*>::iterator itr;
			for(itr=children.begin(); itr != children.end(); itr++){
				(*itr)->accept(v);	
				if(this->complexity <= (*itr)->complexity){
					this->complexity = (*itr)->complexity;
				}

			}

			v.visit(this);
		}


		CompoundStmt* body;
		Except* except;
};

class Except: public Stmt{
	public:
		string getType(){
			return "Except";
		}

		list<ASTNode*> getChildren(){
			list<ASTNode*> children;
			children.push_back(body);
			return children;	
		}

		void accept(CounterVisitor &v){
			list<ASTNode*> children = getChildren();

			list<ASTNode*>::iterator itr;
			for(itr=children.begin(); itr != children.end(); itr++){
				(*itr)->accept(v);	
			}

			v.visit(this);
		}


		CompoundStmt* body;
};

class BinOp: public Expr{
	public:
		string getType(){
			return "BinOp";
		}

		list<ASTNode*> getChildren(){
			list<ASTNode*> children;
			if(left){
				children.push_back(left);
			}
			if(right){
				children.push_back(right);
			}
			return children;
		}

		void accept(CounterVisitor &v){
			list<ASTNode*> children = getChildren();

			list<ASTNode*>::iterator itr;
			for(itr=children.begin(); itr != children.end(); itr++){
				(*itr)->accept(v);	
				if(this->complexity <= (*itr)->complexity){
					this->complexity = (*itr)->complexity;
				}

			}

			v.visit(this);
		}



		Expr* left;
		Expr* right;
};

class UnaryOp: public Expr{
	public:
		string getType(){
			return "UnaryOp";
		}


		list<ASTNode*> getChildren(){
			list<ASTNode*> children;
			if(operand){
				ASTNode* cast = (ASTNode*) operand;
				children.push_back(cast);
			}
			return children;
		}

		void accept(CounterVisitor &v){
			list<ASTNode*> children = getChildren();

			list<ASTNode*>::iterator itr;
			for(itr=children.begin(); itr != children.end(); itr++){
				(*itr)->accept(v);	
			}

			v.visit(this);
		}



		Expr* operand;		
};

class Comparison: public Expr{
	public:
		string getType(){
			return "Comparison";
		}

		list<ASTNode*> getChildren(){
			list<ASTNode*> children;
			if(left){
				children.push_back(left);		
			}

			list<Expr*>::iterator itr;
			for(itr=comparators.begin(); itr != comparators.end(); itr++){
				if(*itr){
					children.push_back(*itr);
				}
			}

			return children;
		}

		void accept(CounterVisitor &v){
			list<ASTNode*> children = getChildren();
			list<ASTNode*>::iterator itr;
			for(itr=children.begin(); itr != children.end(); itr++){
				(*itr)->accept(v);	
				if(this->complexity <= (*itr)->complexity){
					this->complexity = (*itr)->complexity;
				}
			}

			v.visit(this);
		}



		Expr* left;
		list<Expr*> comparators;
		int level;
};

#endif
