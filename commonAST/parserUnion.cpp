//#include "parser.h"
#include "assert.h"
#include "parserUnion.h"

using namespace std;

//prototypes
bool printDebug = false;

class Parser{
	public:
	
		Parser(const string& filename) : file(filename.c_str()) {};

		Parser(const string& filename, map<string, vector<string > > nodesToCount): file(filename.c_str()) {
			visitor = CounterVisitor(nodesToCount);
		}

		ASTNode* parseNode(){
			Token* t = getToken();
			if(t->value == "END"){
				return NULL;	
			}else if(t->value == "Function"){
				return parseFunction(t);
			}else if(t->value == "ifBlock"){
				return parseIfBlock(t->level);
			}else if(t->value == "ifStatement"){
				return parseIf(t->level);
			}else{
				return parseASTNode(t);	
			}
		}

		IfBlock* parseIfBlock(int level){
			IfBlock* ib = new IfBlock();
			ib->type = "IfBlock";
			while(getLookaheadToken()->level > level && getLookaheadToken()->value != "END" && (getLookaheadToken()->value == "ifStatement" || getLookaheadToken()->value == "compoundStmt")){
				ib->children.push_back(parseNode());
			}

			return ib;
		}

		If* parseIf(int level){
			If* i = new If();
			i->type = "If";
			while(getLookaheadToken()->level >= level && getLookaheadToken()->value != "END"){
				i->children.push_back(parseNode());
			}

			return i;
		}


		Function* parseFunction(Token* t){
			Function* func = new Function();	
			func->type = "Function";
			while(getLookaheadToken()->level < t->level && getLookaheadToken()->value != "END"){
				func->children.push_back(parseParameter());
			}

			while(getLookaheadToken()->level > t->level && getLookaheadToken()->value != "END"){
				func->children.push_back(parseNode());
			}

			return func;
		}

		ASTNode* parseParameter(){
			Token* t = getToken();
			ASTNode* p = new ASTNode();
			p->type = t->value;

			return p;
		}

		ASTNode* parseASTNode(Token* t){
			ASTNode* node = new ASTNode();
			node->type = t->value;
			while(getLookaheadToken()->level > t->level && getLookaheadToken()->value != "END"){
				node->children.push_back(parseNode());
			}

			return node;
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

		int getASTNodes() const{
			return visitor.getASTNodes();
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


void printASTasJSON(ASTNode* node, int level=0, bool addComma=false){

	list<ASTNode*> children = node->getChildren();

	cout << getIndentation(level);
	cout << "{" << endl;
	cout << getIndentation(level+1);
	cout << "\"children\": ["; 

	list<ASTNode*>::iterator itr;
	list<ASTNode*>::iterator temp;
	for(itr=children.begin(), temp=itr; itr!= children.end(); itr++){
		cout << endl;
		bool comma = (++temp) != children.end();
		printASTasJSON(*itr, level+2, comma);
	}

	if(children.size() > 0){
		cout << getIndentation(level+1);
	}
	cout << "]," << endl;

	node->printNodeAsJSON(level+1);
	cout << getIndentation(level);

	cout << "}";
	if(addComma){
		cout << ",";
	}else{
		cout << endl;
	}
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

	bool jsonOutput = false;
	
	string inputFile = argv[1];

	map<string, vector<string> > nodesToCount;


	//skips the name of the excecutable and skips the filename
	argv += 2;

	if(*argv == "json"){
		//hack to skip the for loop
		argv += argc;
	}

	for(int i=2; i<argc; i+=2){

		string itemToCount = *argv;
		argv++;
		if(!(*argv)){
			jsonOutput = (itemToCount == "json");
			break;
		}
		string argsString = *argv;

		vector<string> args;	
		//ADD SOME ERROR HANDLING. MAKE SURE THE USER HAS SOME ARGUMENTS SUCH AS VOID

		if(argsString.find(",") == string::npos){
			args.push_back(argsString);
		}


		while(argsString.find(",") != string::npos){
			int index = argsString.find(",");
			args.push_back(argsString.substr(0,index));
			argsString = argsString.substr(index+1);
		}

		if(nodesToCount.find(itemToCount) == nodesToCount.end()){
			nodesToCount[itemToCount] = args;
		}else{
			//do this later
		}

		argv++;
	}


	if(jsonOutput){
		Parser parser(inputFile);
		ASTNode* root = parser.parseNode();
		printASTasJSON(root);
		exit(0);
	}

	Parser parser(inputFile, nodesToCount);
	ASTNode* root = parser.parseNode();

	parser.traverse(root);


	map<string, vector<string> >::iterator itr;
	for(itr= nodesToCount.begin(); itr != nodesToCount.end(); itr++){
		cout << parser.getASTNodes() << endl;
	}
}

