#include "traversal.h"
#include "parser.h"

using namespace std;

bool debugComplexity = false;

string getIndentation(int level){
	string ret = "";
	for(int i=1; i<=level; i++){
		ret += "   "; //custom tab size to match Sam's spacing
		//ret += "\t";
	}
	return ret;
}

//create class to count
//in that class, there will be functions for CounterVisitor::visiting each type of node
//class CounterVisitor/*: public Visitor*/{
CounterVisitor::CounterVisitor(){};

CounterVisitor::CounterVisitor(map<string, vector<string> > nodesToCount){

	this->nodesToCount = nodesToCount;
	findingComplexity  =  nodesToCount.find("-Complexity") != nodesToCount.end();

	map<string, vector<string> >::iterator itr;
	for(itr = nodesToCount.begin(); itr != nodesToCount.end(); itr++){
		string node = itr->first; 
		if(node == "-Module"){

		}else if(node == "-Identifier"){

		}else if(node == "-FunctionDef"){

		}else if(node == "-For"){

		}else if(node == "-While"){

		}else if(node == "-If"){

		}else if(node == "-ClassDef"){
			//options for checking bases
		}else if(node == "-Import"){

		}else if(node == "-CompoundStmt"){

		}else if(node == "-Return"){

		}else if(node == "-Assign"){

		}else if(node == "-AugAssign"){

		}else if(node == "-Raise"){

		}else if(node == "-Exec"){

		}else if(node == "-Try"){

		}else if(node == "-RaiseExcept"){

		}else if(node == "-Except"){

		}else if(node == "-Call"){

		}else if(node == "-BinOp"){

		}else if(node == "-UnaryOp"){

		}else if(node == "-Comparison"){
		
		}else if(node == "-VariableDecl"){

		}else if(node == "-Switch"){

		}else if(node == "-Complexity"){

		}else{
			cerr << "ERROR: The input case: " << node;
			cerr << " does not fit the use cases. Exiting!" << endl;
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
	countList = 0;
	countSet = 0;
	countTuple = 0;
	countDict = 0;
	countArgs = 0;
	complexity = 0;
}

//CounterVisitor::visit for each type of not abstract node


int CounterVisitor::getFor() const{
	return countFor;
}

int CounterVisitor::getModule() const{
	return countModule;
}

int CounterVisitor::getIdentifier() const{
	return countIdentifier;
}

int CounterVisitor::getFunctionDef() const{
	return countFunctionDef;
}

int CounterVisitor::getVariableDecl() const{
	return countFunctionDef;
}

int CounterVisitor::getWhile() const{
	return countWhile;
}

int CounterVisitor::getSwitch() const{
	return countSwitch;
}

int CounterVisitor::getArgs() const{
	return countArgs;
}

int CounterVisitor::getCase() const{
	return countCase;
}

int CounterVisitor::getIf() const{
	return countIf;
}

int CounterVisitor::getClassDef() const{
	return countClassDef;
}

int CounterVisitor::getImport() const{
	return countImport;
}

int CounterVisitor::getReturn() const{
	return countReturn;
}

int CounterVisitor::getCompoundStmt() const{
	return countCompoundStmt;
} 

int CounterVisitor::getAssign() const {
	return countAssign;
}

int CounterVisitor::getRaise() const{
	return countRaise;
}

int CounterVisitor::getExec() const{
	return countExec;
}

int CounterVisitor::getTry() const{
	return countTry;
}

int CounterVisitor::getExcept() const{
	return countExcept;
}

int CounterVisitor::getBases() const{
	return countBases;
}

int CounterVisitor::getCall() const{
	return countCall;
}

int CounterVisitor::getBinOp() const{
	return countBinOp;
}

int CounterVisitor::getUnaryOp() const{
	return countUnaryOp;
}

int CounterVisitor::getComparison() const{
	return countComparison;
}

int CounterVisitor::getComplexity() const{
	return this->complexity; 
}

string CounterVisitor::getClassesAndBases() const{

}

void CounterVisitor::visit(For* f){
	countFor += 1; 	
	
	if(debugComplexity){
		cout << "current complexity: " << this->complexity << endl;
		cout << "For loop's complexity: " << f->complexity << endl;
	}

	//If we are counting complexity
	if(findingComplexity && nodesToCount["-Complexity"].size() == 0 
		&& f->complexity > this->complexity){

		if(debugComplexity){
			cout << "setting current complexity to: " << f->complexity << endl;
		}
		
		this->complexity = f->complexity;
	}
}

void CounterVisitor::visit(Expr* e){

}

void CounterVisitor::visit(Module* m){
	countModule += 1;
}

void CounterVisitor::visit(Identifier* i){
	countIdentifier += 1;
}

void CounterVisitor::visit(FunctionDef* f){

	bool countingFuncDefs = nodesToCount.find("FunctionDef") != nodesToCount.end();

	//If we are counting function defs  
	if(countingFuncDefs && nodesToCount["FunctionDef"].size() > 0){

		string arg1 = nodesToCount["FunctionDef"][0];
		if(f->name->name == arg1){
			countFunctionDef += 1;
		}

	}else if(countingFuncDefs){
		countFunctionDef += 1;
	}

	if(debugComplexity){
		cout << "function's complexity: " << f->complexity << endl;
		cout << "current complexity: " << this->complexity << endl;
	}

	//If we are counting complexity
	if(findingComplexity && nodesToCount["-Complexity"].size() > 0){

		string arg1 = nodesToCount["-Complexity"][0];
		if(f->name->name == arg1 && f->complexity > complexity){
			
			if(debugComplexity){
				cout << "setting complexity to funciton's complexity" << endl;
			}

			complexity = f->complexity;	
		}

	}else if(findingComplexity && f->complexity > complexity){
		if(debugComplexity){
			cout << "setting complexity to funciton's complexity" << endl;
		}

		complexity = f->complexity;
	}
}

void CounterVisitor::visit(VariableDecl* vd){
	countVariableDecl += 1;

	if(debugComplexity){
		cout << "current complexity: " << this->complexity << endl;
		cout << "variable declaration's complexity: " << vd->complexity << endl; 
	}

	//If we are counting complexity
	if(findingComplexity && nodesToCount["-Complexity"].size() == 0 
		&& vd->complexity > complexity){

		if(debugComplexity){
			cout << "setting complexity to variable declaration's complexity" << endl;
		}


		complexity = vd->complexity;
	}

}

void CounterVisitor::visit(While* w){
	countWhile += 1;

	if(debugComplexity){
		cout << "current complexity: " << this->complexity << endl;
		cout << "while's complexity: " << w->complexity << endl; 
	}


	//If we are counting complexity
	if(findingComplexity && nodesToCount["-Complexity"].size() == 0 
		&& w->complexity > complexity){

		if(debugComplexity){
			cout << "setting complexity to while's complexity" << endl;
		}


		complexity = w->complexity;
	}
}

void CounterVisitor::visit(DoWhile* dw){
	countDoWhile += 1;

	if(debugComplexity){
		cout << "current complexity: " << this->complexity << endl;
		cout << "do while's complexity: " << dw->complexity << endl; 
	}

	//If we are counting complexity
	if(findingComplexity && nodesToCount["-Complexity"].size() == 0 
		&& dw->complexity > complexity){

		if(debugComplexity){
			cout << "setting complexity to while's complexity" << endl;
		}


		complexity = dw->complexity;
	}
}

void CounterVisitor::visit(List* l){
	countList += 1;
}

void CounterVisitor::visit(Dict* d){
	countDict += 1;
}
		
void CounterVisitor::visit(Set* s){
	countSet += 1;
}
		
void CounterVisitor::visit(Tuple* t){
	countTuple +=1;
}


void CounterVisitor::visit(Switch* s){
	countSwitch += 1;
}

void CounterVisitor::visit(Args* a){
	countArgs += 1;
}

void CounterVisitor::visit(Case* c){
	countCase += 1;
}

void CounterVisitor::visit(If* i){
	countIf += 1;
}

void CounterVisitor::visit(ClassDef* cd){
	countClassDef += 1;
}

void CounterVisitor::visit(Import* i){
	countImport += 1;
}


void CounterVisitor::visit(Return* r){
	countReturn += 1;
}

void CounterVisitor::visit(CompoundStmt* cs){
	countCompoundStmt += 1;
}

void CounterVisitor::visit(Assign* a){
	countAssign += 1;
}


void CounterVisitor::visit(AugAssign* a){
	countAugAssign += 1;
}


void CounterVisitor::visit(Raise* r){
	countRaise += 1;
}


void CounterVisitor::visit(Exec* e){
	countExec += 1;
}


void CounterVisitor::visit(Try* t){
	countTry += 1;
}


void CounterVisitor::visit(Except* e){
	countExcept += 1;
}

void CounterVisitor::visit(Bases* b){
	countBases += 1;
}

void CounterVisitor::visit(Call* c){
	vector<string> vect = nodesToCount["-Call"];

	if(vect.size() == 2){
		string arg1 = vect[0];
		string arg2 = vect[1];
		if((c->obj == arg1) && (c->func == arg2)){
				countCall += 1;
		}
	}else if(vect.size() == 1){
		string arg1 = vect[0];
		if(c->func == arg1){
			countCall += 1;
		}
	}else{
		countCall += 1;
	}


}

void CounterVisitor::visit(BinOp* bo){
	countBinOp += 1;
}


void CounterVisitor::visit(UnaryOp* uo){
	countUnaryOp += 1;
}

void CounterVisitor::visit(Comparison* c){
	countComparison += 1;
}


