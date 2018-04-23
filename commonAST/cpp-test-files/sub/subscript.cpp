#include<vector>

using namespace std;

int addOne(int a){
	return a+1;
}

int main(){
	vector<int> b;
	int a[5];
	a[0] = 5 + 5;
	a[1] = addOne(4);
	b.push_back(1);
	b[0] = 2;
}
