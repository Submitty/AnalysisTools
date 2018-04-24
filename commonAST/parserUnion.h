#ifndef __PARSER_H__
#define __PARSER_H__

#include <iostream>
#include <list>
#include <fstream>
#include <stdlib.h>
#include <vector>
#include "traversalUnion.h"



string getIndentation(int level);


class Token{
	public:
		string value;
		int level;
};

class ASTNode{
	public:
		string getType(){
			return type;
		}

		list<ASTNode*> getChildren(){
			return children;
		}
		
		void printNode(int level){
			cout << getIndentation(level);
			cout << "-----------------" << endl;
			cout << getIndentation(level);
			cout << "- " << getType() << ": " << endl;
			cout << getIndentation(level);
			cout << "-----------------" << endl;
		}

		void printNodeAsJSON(int level){
			cout << getIndentation(level);
			cout << "\"type\": \"node\"," << endl;
			cout << getIndentation(level);
			cout << "\"tags\":[" << endl;
			cout << getIndentation(level+1);
			cout << "\"" << getType() << "\"\n";
			cout << getIndentation(level);
			cout << "]" << endl;
		}

		void accept(CounterVisitor& v){
			v.visit(this);	
		}

		string type;
		list<ASTNode*> children;
};


class IfBlock: public ASTNode{
	public: 
	
		string getType(){
			return "IfBlock";
		}

};

class If: public ASTNode{
	public:
		string getType(){
			return "If";
		}

};



class Function : public ASTNode{
	public:
		
		string getType(){
			return "Function";
		}

};

#endif
