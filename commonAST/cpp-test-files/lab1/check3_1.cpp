#include <iostream>

using namespace std;

int main()
{	
	int numOfValues;
	cin >> numOfValues;

	float values[numOfValues];
	for(int i=0; i<numOfValues; i++)
	{
		cin >> values[i];
	}

	float total =0;
	for(int i=0; i<numOfValues; i++)
	{
		total += values[i];
	}

	float average = total/float(numOfValues);
	cout << "average: " <<  average << endl;

	for(int i=0; i<numOfValues; i++)
	{
		if(values[i] > average)
		{
			cout << values[i] << endl;
		}
	}


	return 0;
}