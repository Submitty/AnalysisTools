#include <iostream>


int main(){
	int a = 1;
	int p = ++a;
	for(int i=0; i<10; i++){
		p += 1;
		std::cout << p << std::endl;
	}
}
