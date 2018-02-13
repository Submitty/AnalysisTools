#include "clang/ASTMatchers/ASTMatchers.h"
#include "clang/ASTMatchers/ASTMatchFinder.h"
#include "clang/AST/ASTConsumer.h"
// Declares clang::SyntaxOnlyAction.
#include "clang/Frontend/FrontendActions.h"
#include "clang/Tooling/CommonOptionsParser.h"
#include "clang/Tooling/Tooling.h"
// Declares llvm::cl::extrahelp.
#include "clang/Frontend/CompilerInstance.h"
#include "llvm/Support/CommandLine.h"
#include <iostream>
#include <fstream>
#include <sstream>
#include "clang/AST/RecursiveASTVisitor.h"
#include <vector>


using namespace clang;
using namespace clang::ast_matchers;
using namespace clang::tooling;
using namespace llvm;
using namespace std;

//prototypes
bool isFlowControl(const Stmt* S, ASTContext *Context);


//Global list of files to include in our AST traversal
vector<string> includeList;

//When true -> prints the name of every node we reach 
//(regardless of if we care about it for the use cases)
bool debugPrint = false;
bool allNodes = debugPrint || false;
bool callStackDebug = debugPrint || false;

stack<const Stmt*> callStack;

bool prevCondition = false;
bool previousRhsDecl = false;

int numClosingArgsNeeded = 0;
int numClosingVarsNeeded = 0;


void printCallStack(){
	stack<const Stmt*> tempStack = callStack;

	cout << endl;
	cout << "CALL STACK:" << endl;
	cout << "------------------------------" << endl;
	while(!tempStack.empty()){	
		cout << tempStack.top()->getStmtClassName() << endl;;
		tempStack.pop();
	}
	cout << "------------------------------" << endl;
	cout << endl;
}


/*
   Returns true if the node D is in a file in the include list
   Returns false if the node D is from a different file (example: an include)
 */
bool isInCurFile(ASTContext *Context, const Decl* D, string& filename){


	SourceManager &sm = Context->getSourceManager();
	SourceLocation loc = D->getLocation();
	StringRef filenameRef = sm.getFilename(loc);
	filename = filenameRef.str();

	vector<string>::iterator fileItr = find(includeList.begin(), includeList.end(), filename);
	bool ret = fileItr != includeList.end();
	return ret;
}

bool isInCurFile(ASTContext *Context, const Stmt* S, string& filename){

	SourceManager &sm = Context->getSourceManager();
	SourceLocation loc = S->getLocStart();
	StringRef filenameRef = sm.getFilename(loc);
	filename = filenameRef.str();

	vector<string>::iterator fileItr = find(includeList.begin(), includeList.end(), filename);
	bool ret = fileItr != includeList.end();
	if(debugPrint && S->getStmtClassName() == "CXXConstructExpr"){
		cerr << "context is is current file: "  << ret << endl;

	}
	return ret;
}

bool isFunctionDef(const Decl* D){
	string node = D->getDeclKindName();

	CXXDestructorDecl* CD = (CXXDestructorDecl*) D;
	return ((node == "CXXConstructor" || node == "CXXDestructor" || node == "CXXMethod") && !CD->isImplicit());
}

bool endsIn(const string& word, const string& ending){
	if(word.length() < ending.length()){
		return false;
	}

	return std::equal(ending.rbegin(), ending.rend(), word.rbegin());
} 

/*
   returns the first parent of input s that is of type Stmt
 */
const Stmt* getStmtParent(const Stmt *s, ASTContext *Context){
	const Stmt* ret = NULL;
	if(!s) {
		return ret;
	}
	const ASTContext::DynTypedNodeList parents = Context->getParents(*s);
	if(parents.size() > 0){
		ret = parents[0].get<Stmt>();
	}
	return ret;
}


/*
   returns the first parent of input d that is of type Stmt
 */
const Stmt* getStmtParent(const Decl *d, ASTContext *Context){
	const Stmt* ret = NULL;
	if(!d) {
		return ret;
	}
	const ASTContext::DynTypedNodeList parents = Context->getParents(*d);
	if(parents.size() > 0){
		ret = parents[0].get<Stmt>();
	}
	return ret;
}


/*
   returns the first parent of input d that is of type Decl
 */

const Decl* getDeclParent(const Decl* d,  ASTContext *Context){
	const Decl* ret = NULL;
	if(!d){
		return ret;
	}

	const ASTContext::DynTypedNodeList parents = Context->getParents(*d);
	if(parents.size() > 0){
		ret = parents[0].get<Decl>();
	}
	return ret;
}

/*
   returns the first parent of input s that is of type Decl
 */
const Decl* getDeclParent(const Stmt* s, ASTContext *Context){
	const Decl* ret = NULL;
	if(!s){
		return ret;
	}
	const ASTContext::DynTypedNodeList parents = Context->getParents(*s);
	if(parents.size() > 0){
		ret = parents[0].get<Decl>();
	}
	return ret;
}




/*
   Function used to check if a Decl is part of the (init; condition; or increment) of a for loop
 */


bool isFlowControl(const Decl* D, ASTContext *Context){
	if(D == NULL){
		return false;
	}

	const Decl* parent = getDeclParent(D, Context);
	const Stmt* stmtParent = getStmtParent(D, Context);
	if(parent == NULL && isFlowControl(stmtParent, Context)){
		return true;
	}

	return isFlowControl(parent, Context);
}

/*
   Function used to check if a Stmt is part of the (init; condition; or increment) of a for loop
 */
bool isFlowControl(const Stmt* S, ASTContext *Context){

	if(S == NULL){
		return false;
	}else if(strcmp(S->getStmtClassName(), "ForStmt") == 0){
		return true;
	}



	if(strcmp(S->getStmtClassName(), "CompoundStmt") == 0){
		return false;
	}


	const Stmt* parent = getStmtParent(S, Context);
	const Decl* declParent = getDeclParent(S, Context);
	if(parent == NULL && isFlowControl(declParent, Context)){
		return true;
	}

	return isFlowControl(parent, Context);
}

class ASTMatcherVisitor : public RecursiveASTVisitor<ASTMatcherVisitor> {
	public:
		explicit ASTMatcherVisitor(ASTContext *Context){
			this->Context = Context;
		}


		/*
		   this helper function is called when the traversal reaches a node of type Decl
		 */
		bool DeclHelper(Decl *D){

			const Stmt* parent = getStmtParent(D, Context);
			const Stmt* parentsParent = getStmtParent(parent, Context);

			//if it is part of the (init; condition; increment) of a for loop, we don't care about it
			if(isFlowControl(D, Context)){
				return false;
			}


			//supresses the catch stmt's arguments
			if(parent != NULL && strcmp(parent->getStmtClassName(), "CXXCatchStmt") == 0){
				return true;
			}


			string filename;
			if(!isInCurFile(Context, D, filename) && filename.size() != 0){
				return false;
			}else if(filename.size() == 0){
				return true;
			}



			string output = "";
			//get the name of the node type
			string node = D->getDeclKindName();
			//calculate the current level, nextLevel, and previousLevel
			int intLevel = getLevelDecl(D);int intNextLevel = intLevel+1;
			int intNextNextLevel = intLevel+2; int intPrevLevel = intLevel-1;
			//create string values for the levels to use as output
			string level; string nextLevel;
			string nextNextLevel; string prevLevel;
			stringstream ss; stringstream ss2; stringstream ss3; stringstream ss4;
			ss << intLevel;
			level = ss.str();
			ss2 << intNextLevel;
			nextLevel = ss2.str();
			ss3 << intPrevLevel;
			prevLevel = ss3.str();
			ss4 << intNextNextLevel;
			nextNextLevel = ss4.str();



			if(callStackDebug && !callStack.empty()){
				cerr << "decl: call stack top: " << callStack.top()->getStmtClassName() << endl;
			}

			//if top of stack is no longer a parent
			while(!callStack.empty() && numClosingArgsNeeded > 0
					&& !isParentDecl(D, callStack.top()->getStmtClassName())){

				if(debugPrint){
					cerr << "adding args" << endl;
				}
				numClosingArgsNeeded--;
				output += "</args,1>\n";

				callStack.pop();
				if(callStackDebug){
					cerr << "poping" << endl;
					printCallStack();
				}
			}


			//add new calls to stack
			if(isParentDeclInCurFile(D,"CXXConstructExpr") && isParentDecl(D, "CXXConstructExpr")){

				if(debugPrint){
					cerr << "setting previousConstructorCall to true" << endl;
				}


			}else if(isParentDeclInCurFile(D,"CXXTemporaryObjectExpr") && isParentDecl(D, "CXXTemporaryObjectExpr")){

				if(debugPrint){
					cerr << "setting previousTempConstructorCallArg" << endl;
				}


			}else if(isParentDecl(D, "CallExpr")){

				if(debugPrint){
					cerr << "setting previousCallArgs to true" << endl;
				}


			}else if(isParentDecl(D, "CXXMemberCallExpr")){

				if(debugPrint){
					cerr << "setting previousMemberCallArg to true" << endl;
				}

			}


			if(isParentDecl(getDeclParent(D, Context), "Var")){
				previousRhsDecl = true;
				if(debugPrint){
					cout << "setting prev var to true" << endl;
				}
			}else if(previousRhsDecl && numClosingVarsNeeded > 0){
				//if the current node is not a child of a variable declaration 
				//but the previous node was a child of a variable declation 
				//then we know to print a </decl>
				output +="</variableDecl,1>\n";
				numClosingVarsNeeded--;
				previousRhsDecl = false;
			}


			if(node == "Var"){
				output += "<variableDecl, " + prevLevel +  ">";
				numClosingVarsNeeded++;
				VarDecl* VD = (VarDecl*) D;
				if(!VD->hasInit()){
					output +="\n</variableDecl,1>\n";
					numClosingVarsNeeded--;
				}
			}else if(node == "Function"){
				FunctionDecl* FD = (FunctionDecl*) D; 
				output += "<functionDef," + level +">";
				//add function name to the output
				output += "\n<name: " + FD->getNameInfo().getAsString()
					+ "," + nextLevel + ">";

			}else if(node == "CXXRecord"){
				const Decl* parent = getDeclParent(D, Context);
				if(parent && strcmp(parent->getDeclKindName(), "CXXRecord") != 0){
					CXXRecordDecl* CD = (CXXRecordDecl*) D;
					output += "<classDef," + level + ">";
					output += "\n<name: " + CD->getNameAsString() + "," + nextLevel + ">";
					output += "\n<bases," + nextLevel + ">";

					//iterate over all bases and add them to the output
					CXXRecordDecl::base_class_iterator basesItr =  CD->bases_begin();
					while(basesItr != CD->bases_end()){
						QualType qt = basesItr->getType();
						output +=  "\n<base: " +  qt.getBaseTypeIdentifier()->getName().str();
						output +=  "," + nextNextLevel + ">";
						basesItr++;
					}

					//iterate over all of the virtual bases and add them to the output
					auto vBasesItr = CD->vbases_begin();
					while(vBasesItr != CD->vbases_end()){
						QualType qt = vBasesItr->getType();
						output +=  "\n<base: " +  qt.getBaseTypeIdentifier()->getName().str();
						output +=  "," + nextNextLevel + ">";
						vBasesItr++;
					}

				}
			}else if(node == "CXXDestructor"){
				CXXDestructorDecl* CD = (CXXDestructorDecl*) D;
				if(!CD->isImplicit()){
					output += "<functionDef," + level +">";
					//add function name to the output
					output += "\n<name: ~" + CD->getNameInfo().getAsString()
						+ "," + nextLevel + ">";
				}


			}else if(node == "CXXConstructor"){
				CXXConstructorDecl* CD = (CXXConstructorDecl*) D;
				if(!CD->isImplicit()){
					output += "<functionDef," + level +">";
					//add function name to the output
					output += "\n<name: " + CD->getNameInfo().getAsString()
						+ "," + nextLevel + ">";
				}
			}else if(node == "CXXMethod"){
				CXXMethodDecl* CM = (CXXMethodDecl*) D;
				if(!CM->isImplicit()){
					output += "<functionDef," + level +">";
					//add function name to the output
					output += "\n<name: " + CM->getNameInfo().getAsString()
						+ "," + nextLevel + ">";
				}
			}else{

				if(debugPrint){
					output += "<";
					output += node;
					output += ">";
				}
			}

			if(output.size() != 0){
				cout << output << endl;
			}

			return true;
		}



		bool TraverseDecl(Decl *D) {

			bool continueTraversing = DeclHelper(D);
			if(continueTraversing){
				RecursiveASTVisitor<ASTMatcherVisitor>::TraverseDecl(D); // Forward to base class
			}

			return true;
		}


		/*
		   this helper function is called when the traversal reaches a node of type Stmt
		 */
		void StmtHelper(Stmt *x){
			//variable used for <cond> </cond>
			bool condition = false;
			bool isElse = false;
			if(x != NULL){
				string output = "";
				//find current level and next level
				int intLevel = getLevelStmt(x); int intNextLevel = intLevel+1;
				//convert them both to strings to use for output
				string level; string nextLevel;
				stringstream ss;
				ss << intLevel;
				level = ss.str();
				stringstream ss2;
				ss2 << intNextLevel;
				nextLevel = ss2.str();

				const Stmt* parent = getStmtParent(x, Context);
				//PROBLEM
				if(x->getStmtClassName() != "ForStmt" && isFlowControl(x, Context)){
					return;
				}

				//if the parent is calling any type of funciton then this node should be enclosed in <args> </args>
				string filename;
				if(callStackDebug && !callStack.empty()){
					cerr << "stmt: call stack top: " << callStack.top()->getStmtClassName() << endl;
				}

				while(!callStack.empty() && numClosingArgsNeeded > 0
						&& !isParentStmt(parent, callStack.top()->getStmtClassName())){

					if(debugPrint){
						cerr << "adding args" << endl;
					}
					numClosingArgsNeeded--;
					output += "</args,1>\n";

					callStack.pop();

					if(callStackDebug){
						cerr << "popping" << endl;
						printCallStack();
					}
				}

				if(isParentStmtInCurFile(x,"CXXConstructExpr") && isParentStmt(x, "CXXConstructExpr")){

					if(debugPrint){
						cerr << "setting previousConstructorCall to true" << endl;
					}

				}else if(isParentStmtInCurFile(x,"CXXTemporaryObjectExpr") && isParentStmt(x, "CXXTemporaryObjectExpr")){

					if(debugPrint){
						cerr << "setting previousTempConstructorCallArg" << endl;
					}


				}else if(isParentStmt(x, "CallExpr")){

					if(debugPrint){
						cerr << "setting previousCallArgs to true" << endl;
					}


				}else if(isParentStmt(x, "CXXMemberCallExpr")){

					if(debugPrint){
						cerr << "setting previousMemberCallArgs to true" << endl;
					}

				}

				//if the parent is a variable declaration then this node should be encolsed in <decl> </decl>
				if(isParentStmt(x, "Var")){
					previousRhsDecl = true;
					if(debugPrint){
						cout << "setting prev var to true" << endl;
					}

				}else if(previousRhsDecl && numClosingVarsNeeded > 0){
					//if the current node is not a child of a variable declaration 
					//but the previous node was a child of a variable declation 
					//then we know to print a </decl>
					output +="</variableDecl,1>\n";
					numClosingVarsNeeded--;
					previousRhsDecl = false;
				}


				if(parent != NULL && strcmp(parent->getStmtClassName(), "IfStmt") == 0){
					if(debugPrint){
						cerr << "possibly an if statement" << endl;
					}
					//find the first child of the if statemt
					const Stmt* firstChild = NULL;
					auto children = parent->children();
					for(const Stmt* child : children){
						if(child != NULL){
							firstChild = child;
							break;
						}
					}

					//if the first child is the current node, then we know it is part of the condition
					if(firstChild != NULL  && x->getLocStart() == firstChild->getLocStart()){
						if(debugPrint){
							cerr << "part of the condition" << endl;
						}
						prevCondition = true;
					}else if(prevCondition){
						output +="</cond,1>\n";
						prevCondition = false;
					}


					//find if else
					const IfStmt* ifstmt = (IfStmt*) parent;
					const Stmt* elseStmt = ifstmt->getElse();
					if(x == elseStmt){
						isElse = true;
					}

				}

				string node = x->getStmtClassName();
				if(node == "ReturnStmt"){
					output += "<return";
				}else if(node == "ForStmt"){
					output += "<forLoop";
				}else if(node == "WhileStmt"){
					output += "<whileLoop";
				}else if(node == "DoStmt"){
					output += "<do";		
				}else if(node == "IfStmt"){
					output += "<ifStatement";
				}else if(node == "SwitchStmt"){
					output += "<switch";
				}else if(node == "CaseStmt"){
					output += "<case";
				}else if(node == "CXXMemberCallExpr"){
					CXXMemberCallExpr* ce = (CXXMemberCallExpr*) x;
					Expr* obj = ce->getImplicitObjectArgument();
					CallExpr* expr = (CallExpr*) x;
					output += "<object: ";
					QualType qt = obj->getType();
					output += qt.getBaseTypeIdentifier()->getName().str();
					output += "; calling func: ";
					output += expr->getDirectCallee()->getNameInfo().getAsString();
					output += ", " + level + ">\n";
					output += "<args";
					numClosingArgsNeeded++;
					callStack.push(x);

					if(callStackDebug){
						cerr << "pushing" << endl;
						printCallStack();								
					}

				}else if(node == "CallExpr"){
					CallExpr* expr = (CallExpr*) x;
					output += "<calling func: ";
					output += expr->getDirectCallee()->getNameInfo().getAsString();
					output += ", " + level + ">\n";
					output += "<args";
					numClosingArgsNeeded++;
					callStack.push(x);
					if(callStackDebug){
						cerr << "pushing" << endl;
						printCallStack();								
					}

				}else if(node == "CXXConstructExpr"){
					CXXConstructExpr* ce = (CXXConstructExpr*) x;
					Decl* CD = ce->getConstructor();

					string filename;
					if(isInCurFile(Context, CD, filename)){
						CXXMethodDecl* MD =  ce->getConstructor();
						output += "<calling func: ";
						output += MD->getNameInfo().getAsString();
						output += "," + level + ">\n";
						output += "<args";
						numClosingArgsNeeded++;
						callStack.push(x);
						if(callStackDebug){
							cerr << "pushing" << endl;
							printCallStack();								
						}

					}

				}else if(node == "BinaryOperator"){
					BinaryOperator* binaryOp = (BinaryOperator*) x;
					if(binaryOp->isAssignmentOp()){
						output += "<assignment";
					}else if(binaryOp->isComparisonOp()){
						output += "<comparison";
					}else{
						output += "<binaryOp";
					}
				}else if(node == "UnaryOperator"){
					UnaryOperator* uo = (UnaryOperator*) x;
					string op = uo->getOpcodeStr(uo->getOpcode()).str();
					if(op != "-"){
						output += "<unaryOp";
					}
				}else if(node == "CompoundAssignOperator"){
					output += "<augAssign";
				}else if(node == "CompoundStmt"){
					if(isElse){
						output += "<elseStatement";
					}else{
						output += "<compoundStmt";
					}
				}else if(node == "CXXThrowExpr"){
					output += "<raisingException";
				}else if(node == "CXXTryStmt"){
					output += "<try";
				}else if(node == "CXXCatchStmt"){
					output += "<except";
				}else if(node == "CXXOperatorCallExpr"){
					CXXOperatorCallExpr* ce = (CXXOperatorCallExpr*) x;
					if(ce->isAssignmentOp()){
						output += "<assignment";
					}
				}else if(node == "CXXTemporaryObjectExpr"){
					CXXTemporaryObjectExpr* ce = (CXXTemporaryObjectExpr*) x;
					Decl* CD = ce->getConstructor();



					string filename;
					if(isInCurFile(Context, CD, filename)){
						CXXMethodDecl* MD =  ce->getConstructor();
						output += "<calling func: ";
						output += MD->getNameInfo().getAsString();
						output += "," + level + ">\n";
						output += "<args";
						numClosingArgsNeeded++;
						callStack.push(x);
						if(callStackDebug){
							cerr << "pushing" << endl;
							printCallStack();								
						}


					}

				}else{
					if(allNodes){
						output += "<";
						output += node;
						output += ">";

					}



				}


				if(output.size() != 0 && !endsIn(output, "</cond,1>\n") && 
						!endsIn(output,"</variableDecl,1>\n") && !endsIn(output,"</args,1>\n") 
						&& !endsIn(output,">") && !endsIn(output, ">\n")){

					output += ", " + level + ">";
					cout << output << endl;
					output = "";
				}else if(output.size() != 0){
					cout << output << endl;
					output = "";
					if(debugPrint){
						cerr << "printing output" << endl;
					}
				}	


			}
		}

		bool TraverseStmt(Stmt *x) {
			StmtHelper(x);
			RecursiveASTVisitor<ASTMatcherVisitor>::TraverseStmt(x);
			return true;
		}

		/*
		   Function assumes that "nodeToFind" is a parent of S. 
		   Checks if nodeToFind is in the source code we are looking at or a seperate file
		 */
		bool isParentStmtInCurFile(const Stmt* S, const string& nodeToFind){
			if(debugPrint && nodeToFind == "CXXConstructExpr"){
				//cerr << "checking if parentStmtInCurFile" << endl;
			}

			if(S == NULL){
				return false;
			}



			const Stmt* parent = getStmtParent(S, Context);
			const Decl* declParent = getDeclParent(S, Context);

			//we found the node we're looking for
			if(strcmp(S->getStmtClassName(), nodeToFind.c_str()) == 0){

				//FIX THIS
				/*
				   if(declParent == NULL){
				   return false;					
				   }
				 */
				string filename;
				bool ret = isInCurFile(Context, S, filename);
				if(debugPrint){
					cerr << "returning "  << ret << endl;
				}
				return ret;	
			}


			//check is the first parent of type Decl is in the current file
			if(parent == NULL && isParentDeclInCurFile(declParent, nodeToFind)){
				return true;
			}

			//recurse
			return isParentStmtInCurFile(parent, nodeToFind);
		}


		/*
		   Function assumes that "nodeToFind" is a parent of S. 
		   Checks if nodeToFind is in the source code we are looking at or a seperate file
		 */

		bool isParentDeclInCurFile(const Decl *D, const string& nodeToFind){
			if(D == NULL){
				return false;
			}

			//we found the node we are looking for
			if(strcmp(D->getDeclKindName(), nodeToFind.c_str()) == 0){
				string filename;
				//check if it sin the current file
				bool ret = isInCurFile(Context, D, filename);
				if(debugPrint){
					cerr << "returning "  << ret << endl;
				}
				return ret;
			}

			const Decl* parent = getDeclParent(D, Context);
			const Stmt* stmtParent = getStmtParent(D, Context);
			//check is the first parent of type Stmt is in the current file
			if(parent == NULL && isParentStmtInCurFile(stmtParent, nodeToFind)){
				return false;
			}

			//recurse
			return isParentDeclInCurFile(parent, nodeToFind);
		}


		/*
		   Function to check is node of type "nodeToFind" is a parent of D
Note: this parent does not have to be a direct parent
It can be a grandparent, great grand parent etc
		 */
		bool isParentDecl(const Decl *D, const string& nodeToFind){
			//root node
			if(D == NULL){
				return false;
			}

			//we found the node we're looking for
			if(strcmp(D->getDeclKindName(), nodeToFind.c_str()) == 0){
				return true;
			}

			
			const Decl* parent = getDeclParent(D, Context);
			const Stmt* stmtParent = getStmtParent(D, Context);
			//if there are no more parents of type Decl, 
			//check the parents of type Stmt for nodeToFind
			if(parent == NULL && isParentStmt(stmtParent, nodeToFind)){
				return true;
			}

			//recurse
			return isParentDecl(parent, nodeToFind);

		}


		/*
		   Function to check is node of type "nodeToFind" is a parent of S
Note: this parent does not have to be a direct parent
It can be a grandparent, great grand parent etc
		 */
		bool isParentStmt(const Stmt *S, const string& nodeToFind){
			//root node
			if(S == NULL){
				return false;
			}

			//found the node we're looking for
			if(strcmp(S->getStmtClassName(), nodeToFind.c_str()) == 0){
				return true;
			}


			const Stmt* parent = getStmtParent(S, Context);
			const Decl* declParent = getDeclParent(S, Context);
			//if there are no more parents of type Stmt.
			//check the parents of type Decl for nodeToFind
			if(parent == NULL && isParentDecl(declParent, nodeToFind)){
				return true;
			}

			//recurse
			return isParentStmt(parent, nodeToFind);
		}


		/*
		   Function to find D's level in the AST
		 */
		int getLevelDecl(const Decl *D, int level=0){
			//root node
			if(D == NULL){
				return level;
			}

			const Decl* decl = getDeclParent(D, Context);
			//if there are no more parents of type Decl
			//continue upwards on the tree nodes of type Stmt
			if(decl == NULL){
				const Stmt* stmt = getStmtParent(D, Context); 
				level = getLevelStmt(stmt, level);

			}

			//recurse
			return getLevelDecl(decl, level+1);
		}


		/*
		   Function to find D's level in the AST
		 */
		int getLevelStmt(const Stmt *S, int level=0){
			//root node
			if(S == NULL){
				return level;
			}

			const Stmt* parent = getStmtParent(S, Context);
			//if there are no more parents of type Stmt
			//continue upwards on the tree nodes of type Decl
			if(parent == NULL){
				const Decl* decl = getDeclParent(S, Context);
				level = getLevelDecl(decl, level);
			}

			return getLevelStmt(parent, level+1);
		}


	private:
		ASTContext *Context;
};

//AST CONSUMER - an interface that provides actions on the AST
class astConsumer : public clang::ASTConsumer{
	public:
		explicit astConsumer(ASTContext *Context) : Visitor(Context) {}

		virtual void HandleTranslationUnit(clang::ASTContext &Context){
			// Traversing the translation unit decl via a RecursiveASTVisitor
			// will visit all nodes in the AST.
			Visitor.TraverseDecl(Context.getTranslationUnitDecl());
		}

	private:
		//Declare a RecursiveASTVisitor
		ASTMatcherVisitor Visitor;

};


//FRONTEND ACTION - allows excecution of actions as part of the compilation
class ASTMatcherAction : public clang::ASTFrontendAction {
	public:

		//create an astConsumer to perform actions on the AST
		virtual std::unique_ptr<clang::ASTConsumer> CreateASTConsumer(
				clang::CompilerInstance &Compiler, llvm::StringRef InFile) {
			return std::unique_ptr<clang::ASTConsumer>(
					new astConsumer(&Compiler.getASTContext()));
		}
};


int main(int argc, char** argv){

	callStack = stack<const Stmt*>();

	includeList.push_back("input.cc");

	for(int i=1; i<argc; i++){
		includeList.push_back(argv[i]);
	}


	if (argc > 1) {
		ifstream f(argv[1]);
		if(!f.good()){
			cerr << "can't open: " << argv[1] << endl;
		}
		stringstream buffer;
		buffer << f.rdbuf();
		cout << "<module,1>" << endl;
		cout << "<importing,2>" << endl;
		cout << "</importing,2>" << endl;
		clang::tooling::runToolOnCode(new ASTMatcherAction, buffer.str());
	}

}

