#include <iostream>
#include <vector>
#include <stdlib.h>


class Module;
class Identifier;
class FunctionDef;
class For;
class While;
class VariableDecl;
class If;
class Bases;
class ClassDef;
class Import;
class CompoundStmt;
class Return;
class Assign;
class AugAssign;
class Raise;
class Exec;
class Try;
class Except;
class Call;
class BinOp;
class UnaryOp;
class Comparison;
class Switch;
class Case;


using namespace std;

//create class to count
//in that class, there will be functions for visiting each type of node
class CounterVisitor{
	public:
		CounterVisitor(){};

		CounterVisitor(vector<string> nodesToCount){
			
			for(unsigned int i=0; i<nodesToCount.size(); i++){
				string node = nodesToCount[i];
				if(node == "Module"){

				}else if(node == "Identifier"){

				}else if(node == "FunctionDef"){

				}else if(node == "For"){

				}else if(node == "While"){

				}else if(node == "If"){
				
				}else if(node == "ClassDef"){

				}else if(node == "Import"){

				}else if(node == "CompoundStmt"){

				}else if(node == "Return"){

				}else if(node == "Assign"){

				}else if(node == "AugAssign"){

				}else if(node == "Raise"){

				}else if(node == "Exec"){

				}else if(node == "Try"){

				}else if(node == "Except"){

				}else if(node == "Call"){

				}else if(node == "BinOp"){

				}else if(node == "UnaryOp"){

				}else if(node == "Comparison"){

				}else{
					cerr << "ERROR: The input node: " << node;
					cerr << " does not fit the grammar. Exiting!" << endl;
					exit(1);
				}

			}	



			countModule = 0;
			countIdentifier = 0; 
			countFunctionDef = 0;
			countVariableDecl = 0;
			countFor = 0;
			countWhile = 0;
			countIf = 0;
			countClassDef = 0;
			countImport = 0;
			countCompoundStmt = 0; 
			countReturn = 0;
			countAssign = 0;
			countAugAssign = 0;
			countRaise = 0;
			countExec = 0;
			countTry = 0;
			countExcept = 0;
			countBases = 0;
			countCall = 0;
			countBinOp = 0;
			countUnaryOp = 0;
			countComparison = 0;
			countCase = 0;
			countSwitch = 0;	

		}

		//visit for each type of not abstract node
		void visit(For* f){
			countFor++;	
		}
	
		int getFor() const{
			return countFor;
		}

		int getModule(){
			return countModule;
		}


		void visit(Module* m){
			countModule += 1;
		}

		void visit(Identifier* i){
			countIdentifier += 1;
		}

		void visit(FunctionDef* f){
			countFunctionDef += 1;
		}

		void visit(VariableDecl* vd){
			countVariableDecl += 1;
		}

		void visit(While* w){
			countWhile += 1;
		}

		void visit(Switch* s){
			countSwitch += 1;
		}

		void visit(Case* c){
			countCase += 1;
		}

		void visit(If* i){
			countIf += 1;
		}

		void visit(ClassDef* cd){
			countClassDef += 1;
		}

		void visit(Import* i){
			countImport += 1;
		}


		void visit(Return* r){
			countReturn += 1;
		}

		void visit(CompoundStmt* cs){
			countCompoundStmt += 1;
		}

		void visit(Assign* a){
			countAssign += 1;
		}


		void visit(AugAssign* a){
			countAugAssign += 1;
		}


		void visit(Raise* r){
			countRaise += 1;
		}


		void visit(Exec* e){
			countExec += 1;
		}


		void visit(Try* t){
			countTry += 1;
		}


		void visit(Except* e){
			countExcept += 1;
		}

		void visit(Bases* b){
			countBases += 1;
		}

		void visit(Call* c){
			countCall += 1;
		}


		void visit(BinOp* bo){
			countBinOp += 1;
		}


		void visit(UnaryOp* uo){
			countUnaryOp += 1;
		}

		void visit(Comparison* c){
			countComparison += 1;
		}

	private:

		int countModule;
		int countIdentifier;
		int countFunctionDef;
		int countVariableDecl;
		int countFor;
		int countWhile;
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

		/*	
		bool countModule;
		bool countIdentifier;
		int countFunctionDef;
		int countVariableDecl;
		int countFor;
		int countWhile;
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
		int countCall;
		int countBinOp;
		int countUnaryOp;
		int countComparison;
		*/

		//member variables for each thing we might want to count in the AST example - int forCount, int funcCount


};

