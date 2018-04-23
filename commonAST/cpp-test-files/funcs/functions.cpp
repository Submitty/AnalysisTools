#include <iostream>
#include <math.h>

using namespace std;


void func(int a, int b, int c){
	cout << a << " " << b << " " << c << endl;
}

int addOne(int a){
	return a+1;
}

int subOne(int a){
	return a-1;
}


int main(){
	//TODO: IF I'M PRINTING, THIS CALL SHOULD BE IGNORED. HOW DO I IGNORE A PRINT?
	//cout << ceil(5.5) << endl;
	int p = 0;
	p = ceil(5.5);
	string r = "";
	int s = p+2;
	for(int i=0; i<5; i++){
		int k = 0;
		int j = addOne(ceil(i));
		while (j > 0){
			j = subOne(j);
			if(i == j){
				p = 1;
				r = "test";
				if(j < 1){
					p = 7;
				}
			}else{
				k += 1;
				if(k > 10){
					p = 2;
				}
			}
		}
	}

}
