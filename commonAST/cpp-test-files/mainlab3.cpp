#include "Time.h"
#include <iostream>
#include <algorithm>
#include <vector>
using namespace std;

int main(){

	Time t1(5,30,59);
	Time t2;
	Time t3(11,2,6);

	t1.changeTime();
	t2.changeTime();

	cout << isEarlierThan(t2, t3) << endl;

	vector<Time> times;
	times.push_back(t1);
	times.push_back(t2);
	times.push_back(t3);

	sort(times.begin(), times.end(), isEarlierThan);

	for(int i=0; i<3; i++)
	{
		times[i].printAmPm();
	}

	return 0;
}
