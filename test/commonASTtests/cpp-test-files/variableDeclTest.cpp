#include <vector>
using namespace std;

int main(){
	int a = 1;
	int b = 2;
	int c = a+b;
	int d = a+b+c;
	int e = c;
	vector<int> v;
	v.push_back(1);
	vector<int>::iterator f = v.erase(v.begin());
}
