#include "traversal.cpp"

int main(int argc, char** argv){
	//argv is a list of nodes that we can count
	vector<string> nodesToCount;
	argv++;
	for(int i=1; i<argc; i++){
		nodesToCount.push_back(*argv);	
		argv++;
	}

	CounterVisitor cv(nodesToCount);
	cout << cv.getFor() << endl;
}
