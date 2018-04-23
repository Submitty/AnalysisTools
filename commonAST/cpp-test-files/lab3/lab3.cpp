#include <iostream>

using namespace std;

void compute_squares(const unsigned int* a, unsigned int* b, unsigned int n)
{
	for(int i=0; i<n; i++)
	{
		*(b+i) = (*(a+i)) * (*(a+i));
	}
}

int main(){
	
	const unsigned int size = 3;
	unsigned int testOne[size];	
	unsigned int testTwo[size];

	for(int i=0; i<size; i++)
	{
		testOne[i] = i;
	}

	compute_squares(testOne, testTwo, size);
	
	for(int i=0; i<size; i++)
	{
		cout << "a: " << testOne[i] << " b: " << testTwo[i] << endl;
	}

	return 0;
}
