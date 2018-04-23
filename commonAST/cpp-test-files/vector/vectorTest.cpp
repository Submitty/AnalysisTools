#include <iostream>
#include <vector>

using namespace std;

int main(){
	int q = 1;
	int p = q+1;
	vector<int> v;
	v.push_back(1);
	v.clear();
	v.push_back(3);
	v.push_back(7);
	vector<int>::iterator itr = v.begin();
	v.erase(itr);
}
