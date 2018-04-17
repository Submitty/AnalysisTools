#ifndef __TRAVERSAL_H__
#define __TRAVERSAL_H__

#include <iostream>
#include <map>
#include <vector>
#include <stdlib.h>

using namespace std;


class ASTNode;

class CounterVisitor{
	public:
		CounterVisitor();
		CounterVisitor(map<string, vector<string> > nodesToCount);

		int getASTNodes() const;
		string getClassesAndBases() const;
		
		void visit(ASTNode* n);

private:

		int countASTNode;
	
		map<string, vector<string> > nodesToCount;


};

#endif
