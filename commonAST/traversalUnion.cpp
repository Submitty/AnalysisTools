#include "traversalUnion.h"
#include "parserUnion.h"

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



	countASTNode = 0;
}

//CounterVisitor::visit for each type of not abstract node



int CounterVisitor::getASTNodes() const{
	return countASTNode;
}

void CounterVisitor::visit(ASTNode* n){
	countASTNode += 1;
}

