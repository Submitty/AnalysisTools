#include "parser.h"

using namespace std;

//prototypes
bool isExpr(string);
bool isStmt(string);
bool printDebug = false;


class Parser{
	public:

		Parser(const string& filename) : file(filename.c_str()) {};

		Parser(const string& filename, vector<string> nodesToCount): file(filename.c_str()) {
			visitor = CounterVisitor(nodesToCount);
		}


		Module* parseModule(){
			getToken();
			Module* module = new Module();
			while(getLookaheadToken()->value != "END"){
				if(isExpr(getLookaheadToken()->value)){
					module->body.push_back(parseExpr());
				}else if(isStmt(getLookaheadToken()->value)){
					module->body.push_back(parseStmt());
				}else{
					cerr << "PARSE ERROR: parsed token that is not an EXPR or a STMT" << endl;	
					exit(1);
				}
			}
			return module;
		}

		Stmt* parseStmt(){
			Token* t = getToken();
			string compVal("name:");
			if(printDebug){
				cout << "Parsing Stmt: " << t->value << endl;
			}
			if(t->value == "functionDef"){
				return (Stmt*) parseFunctionDef();
			}else if(t->value == "classDef"){
				return (Stmt*) parseClassDef(t->level);
			}else if(t->value == "compoundStmt"){
				return (Stmt*) parseCompoundStmt();
			}else if(t->value == "return"){
				return (Stmt*) parseReturn(t->level);
			}else if(t->value == "assignment"){
				return (Stmt*) parseAssign(t->level);
			}else if(t->value == "augAssign"){
				return (Stmt*) parseAugAssign(t->level);
			}else if(t->value == "switch"){
				return (Stmt*) parseSwitch();
			}else if(t->value == "case"){
				return (Stmt*) parseCase(t->level);
			}else if(t->value == "forLoop"){
				return (Stmt*) parseFor();
			}else if(t->value == "whileLoop"){
				return (Stmt*) parseWhile(t->level);
			}else if(t->value == "ifStatement"){
				return (Stmt*) parseIf(t->level);
			}else if(t->value == "importing"){
				return (Stmt*) parseImport(t->level);	
			}else if(t->value == "exec"){
				return (Stmt*) parseExec();
			}else if(t->value == "raisingException"){
				return (Stmt*) parseRaise();
			}else if(t->value == "try"){
				return (Stmt*) parseTry();
			}else if(t->value == "except"){
				return (Stmt*) parseExcept();
			}else if(t->value == "variableDecl"){
				return (Stmt*) parseVariableDecl();
			}else if(t->value == "END"){
				return NULL;
			}else if(t->value.compare(0, compVal.length(), compVal) == 0){
				return (Stmt*) parseIdentifier();
			}else{
				cerr << "parseStmt -> PARSE ERROR: did not expect token: " << t->value << endl;
				exit(1);
			}

			return NULL;
		}

		Expr* parseExpr(){
			Token* t = getToken();
			string compVal("calling func");
			string objCompVal("object:");
			if(printDebug){
				cout << "Parsing Expr: " << t->value << endl;
			}
			if(t->value == "binaryOp"){
				return (Expr*) parseBinOp(t->level);
			}else if(t->value == "unaryOp"){
				return (Expr*) parseUnaryOp(t->level);
			}else if(t->value == "comparison"){
				return (Expr*) parseComparison(t->level);
			}else if(t->value.compare(0, compVal.length(), compVal) == 0){
				return (Expr*) parseCallingFunc(t->value, t->level);
			}else if(t->value.compare(0, objCompVal.length(), objCompVal) == 0){
				return (Expr*) parseObjectCallingFunc(t->value);	
			}else if(t->value == "END"){
				return NULL;
			}else{
				cerr << "parseExpr -> PARSE ERROR: did not expect token: " << t->value << endl;
				exit(1);
			}

			return NULL;
		}

		FunctionDef* parseFunctionDef(){
			FunctionDef* fd = new FunctionDef();			
			fd->name = parseIdentifier();
			if(getLookaheadToken()->value == "compoundStmt"){
				fd->compoundStmt = parseCompoundStmt();
			}//else its a prototype
			return fd;
		}

		Bases* parseBases(){
			Bases* b = new Bases();
			Token* t = getToken();
			if(t->value != "bases"){
				cerr << "PARSE ERROR: expected \"bases\" but recieved token: " << t->value << endl;
				exit(1);
			}

			while(getLookaheadToken()->level > t->level){
				b->data.push_back(parseIdentifier());	
			}

			return b;
		}


		ClassDef* parseClassDef(int level){
			ClassDef* cd = new ClassDef();
			cd->name = parseIdentifier();
			cd->bases = parseBases();
			cd->body = parseBody(level);	
			return cd;
		}

		Identifier* parseIdentifier(){
			Identifier* i = new Identifier();
			Token* t = getToken();	
			string val = t->value;
			size_t pos = t->value.find(":");
			if(pos == string::npos){
				cerr << "ERROR: illformated <name: > xml: " << t->value << endl;
				exit(1);
			}

			i->name = t->value.substr(pos+1);
			return i;
		}

		Call* parseObjectCallingFunc(string val){
			Call* c = new Call();

			size_t pos = val.find(":");
			size_t semiPos = val.find(";");
			if(pos == string::npos || semiPos == string::npos){
				cerr << "ERROR: illformated <callingFunc: > xml" << endl;
				exit(1);
			}

			c->obj = val.substr(pos+2, (semiPos-(pos+2)));
			val = val.substr(pos+2);
			pos = val.find(":");
			if(pos == string::npos){
				cerr << "ERROR: illformated <callingFunc: > xml" << endl;
				exit(1);
			}


			c->func = val.substr(pos+2);
			return c;
		}


		Call* parseCallingFunc(string val, int level){
			Call* c = new Call();
			size_t pos = val.find(":");
			if(pos == string::npos){
				cerr << "ERROR: illformated <callingFunc: > xml" << endl;
				exit(1);
			}
			c->func = val.substr(pos+2);
			return c;
		}

		list<ASTNode*> parseBody(int level){
			list<ASTNode*> body;

			Token* lt = getLookaheadToken();
			while(lt->level > level){
				if(isExpr(lt->value)){
					body.push_back(parseExpr());
				}else if(isStmt(lt->value)){
					body.push_back(parseStmt());
				}else{
					cerr << "ERROR: Attempted to add value which is not an EXPR or STMT" << endl;
				}

				lt = getLookaheadToken();
			}
			return body;
		}

		CompoundStmt* parseCompoundStmt(){
			if(printDebug){
				cout << "parsing compoundStmt" << endl;
			}
			Token* t = getToken();
			if(t->value != "compoundStmt" && t->value != "elseStatement"){
				cerr << "PARSE ERROR: expected compoundStmt token, but parsed: " << t->value << endl;
				exit(1);
			}

			CompoundStmt* cs = new CompoundStmt();
			while(getLookaheadToken()->level > t->level){
				if(isExpr(getLookaheadToken()->value)){
					cs->body.push_back(parseExpr());	
				}else if(isStmt(getLookaheadToken()->value)){
					cs->body.push_back(parseStmt());	
				}else{
					cerr << "ERROR: Attempted to add value which is not an EXPR or STMT" << endl;	
				}
			}	
			return cs;
		}


		Return* parseReturn(int level){
			Return* ret= new Return();
			if(getLookaheadToken()->level > level){ 
				ret->value = parseExpr();	
			}else{
				ret->value = NULL;
			}
			return ret;
		}

		Switch* parseSwitch(){
			Switch* s = new Switch();
			if(getLookaheadToken()->value != "compoundStmt"){
				s->cond = parseExpr();
			}

			s->compoundStmt = parseCompoundStmt();

			return s;
		}

		Case* parseCase(int level){
			Case* c = new Case();
			c->body = parseBody(level);
			return c;
		}


		Assign* parseAssign(int level){
			Assign* assign = new Assign();
			while(getLookaheadToken()->level > level /*&& getLookaheadToken()->value != "END"*/) {
				assign->targets.push_back(parseExpr());
			}
			return assign;
		}

		AugAssign* parseAugAssign(int level){
			AugAssign* augAssign = new AugAssign();
			augAssign->target = NULL;
			augAssign->value = NULL;

			return augAssign;
		}


		For* parseFor(){
			For* f = new For();
			while(getLookaheadToken()->value != "compoundStmt"){
				if(isExpr(getLookaheadToken()->value)){
					f->stopCond.push_back(parseExpr());	
				}else if(isStmt(getLookaheadToken()->value)){
					f->stopCond.push_back(parseStmt());	
				}else{
					cerr << "ERROR: Attempted to add value which is not an EXPR or STMT" << endl;	
				}

			}

			f->compoundStmt = parseCompoundStmt();
			return f;
		}

		While* parseWhile(int level){
			While* w = new While();
			while(getLookaheadToken()->level > level && getLookaheadToken()->value != "compoundStmt"){
				w->test.push_back(parseExpr());
			}
			w->compoundStmt = parseCompoundStmt();
			return w;
		}


		If* parseIf(int level){
			If* i = new If();

			while(getLookaheadToken()->value != "/cond"){
				if(printDebug){
					cout << "parsing If's test: " << getLookaheadToken()->value << endl;
				}

				i->test = parseExpr();
			}

			if(printDebug){
				cout << "reached /cond" << endl;
			}
			getToken();


			if(printDebug){
				cout << "parsing If's compoundStmt: " << getLookaheadToken()->value << endl;
			}

			if(getLookaheadToken()->value == "compoundStmt"){
				i->compoundStmt = parseCompoundStmt();
			}else{
				i->body = parseBody(level);	
			}


			if(getLookaheadToken()->level > level && getLookaheadToken()->value == "elseStatement"){
				i->orelse = parseCompoundStmt();
			}else if(getLookaheadToken()->level > level && getLookaheadToken()->value == "ifSatement"){
				Token* t = getToken();
				i->orelse = parseIf(t->level);
			}else{
				i->orelse = NULL;
			}
			return i;
		}

		Exec* parseExec(){
			Exec* e = new Exec();
			return e;
		}

		Raise* parseRaise(){
			Raise* r = new Raise();
			return r;
		}

		Try* parseTry(){
			Try* t = new Try();
			t->body = parseCompoundStmt();
			t->except = parseExcept();		
			return t;
		}

		Except* parseExcept(){
			Token* t = getToken();
			if(t->value != "except"){
				cerr << "PARSE ERROR: expected token: \"except\" but recieved token: " << t->value << endl;
			}
			Except* e = new Except();
			e->body = parseCompoundStmt();
			return e;
		}

		VariableDecl* parseVariableDecl(){
			VariableDecl* vd = new VariableDecl();

			while(getLookaheadToken()->value != "/variableDecl" && getLookaheadToken()->value != "END"){
			   	vd->right.push_back(parseExpr());
			}

			//throwaway </variableDecl> token
			getToken();

			return vd;
		}

		Import* parseImport(int level){
			Import* i = new Import();
			while(getLookaheadToken()->level > level /*&& getLookaheadToken()->value != "END"*/){
				i->names.push_back(parseIdentifier());
			}

			return i;
		}


		BinOp* parseBinOp(int level){
			BinOp* bo = new BinOp();
			if(getLookaheadToken()->level > level /*&& getLookaheadToken()->value != "END"*/){
				bo->left = parseExpr();
			}else{
				bo->left = NULL;
			}

			if(getLookaheadToken()->level > level /*&& getLookaheadToken()->value != "END"*/){
				bo->right = parseExpr();
			}else{
				bo->right = NULL;
			}

			return bo;
		}

		Comparison* parseComparison(int level){
			Comparison* cp = new Comparison();

			cp->level = level;
			if(getLookaheadToken()->level > level){
				if(printDebug){
					cout << "parsing compare's left: " << getLookaheadToken()->value;
				}
				cp->left = parseExpr();
			}
			while(getLookaheadToken()->level > level){
				if(printDebug){
					cout << "parsing comparators " << getLookaheadToken()->value;
				}

				cp->comparators.push_back(parseExpr());
			}

			return cp;
		}

		UnaryOp* parseUnaryOp(int level){
			UnaryOp* uo = new UnaryOp();
			if(getLookaheadToken()->level > level /*&& getLookaheadToken()->value != "END"*/){
				uo->operand = parseExpr();
			}else{
				uo->operand = NULL;
			}
			return uo;
		}


		Token* getToken(){
			Token* token = new Token();
			file >> ws;
			string word;
			getline(file,word);
			if(word.size() == 0){
				token->value = "END";
				token->level = -1;
				return token;
			}

			token->value = getWord(word);
			token->level = getLevel(word);
			return token;
		}

		Token* getLookaheadToken(){
			Token* token = new Token();
			string word;
			int len = file.tellg();
			file >> ws;
			getline(file,word);
			if(word.size() == 0){
				token->value = "END";
				token->level = -1;
				return token;
			}

			token->value = getWord(word);

			token->level = getLevel(word);
			file.seekg(len ,std::ios_base::beg);	
			return token;

		}


		int getFor() const{
			return visitor.getFor();
		}


		int getModule() {
			return visitor.getModule();
		}

		void traverse(ASTNode* node){
			node->accept(visitor);
		}


	private:
		ifstream file;
		CounterVisitor visitor;

		string getWord(string word){
			size_t pos = word.find(",");	
			if(pos == string::npos){
				cout << "ERROR. Word: " << word << " not in format \",level\". Exiting" << endl;
				exit(1);
			}

			return word.substr(1,pos-1);
		}

		int getLevel(string word){

			size_t pos = word.find(",");
			if(pos ==  string::npos){
				cout << "ERROR. Word: " << word << " not in format \",level\". Exiting" << endl;

				exit(1);
			}

			string level = word.substr(pos+1, word.size()-1);
			return atoi(level.c_str());
		}
};


bool isExpr(string val){
	if(printDebug){
		cout << "checking if " << val << " is an expr" << endl;
	}

	string objCompVal("object:");
	string compVal("calling func");
	return val == "binaryOp" || val == "unaryOp" || val == "comparison"||
		(val.compare(0, compVal.length(), compVal) == 0) || val.compare(0, objCompVal.length(), objCompVal) == 0;
}

bool isStmt(string val){
	if(printDebug){
		cout << "checking if " << val << " is a stmt" << endl;
	}


	string compVal("name");
	return val == "functionDef" || val == "classDef" || val == "compoundStmt" || val == "return" || val == "assignment" || val == "augAssign" || val == "forLoop" || val == "whileLoop" || val == "ifStatement" || val == "importing" || val == "exec" || val == "variableDecl" || val == "try" || val == "except" || val == "raisingException" || val == "switch" || val == "case" || val.compare(0, compVal.length(), compVal) == 0;
}


void printAST(ASTNode* node, int level=0){

	list<ASTNode*> children = node->getChildren();
	node->printNode(level);

	list<ASTNode*>::iterator itr;
	for(itr=children.begin(); itr!= children.end(); itr++){
		printAST(*itr, level+1);

	}

}

int main(int argc, char** argv){

	if(argc < 2){
		cerr << "ERROR: no input file specified" << endl;
		exit(1);
	}

	string inputFile = argv[1];
	vector<string> nodesToCount;
	argv += 2;
	for(int i=2; i<argc; i++){
		nodesToCount.push_back(*argv);	
		argv++;
	}

	Parser parser(inputFile, nodesToCount);
	Module* m = parser.parseModule();
	printAST(m);

	parser.traverse(m);

	cout << parser.getFor() << endl;
}

