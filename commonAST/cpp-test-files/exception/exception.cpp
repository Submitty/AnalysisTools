#include <iostream>
using namespace std;

int div(int a, int b){
	if(b == 0){
		throw "division by zero";
	}
	return a/b;
}

int main(){

	try{
		cout <<div(10,5) << endl;
		div(2,0);
		int x = 1;
	}catch(const char* excep){
		int y = 1;
	}
}
