#include <iostream>

using namespace std;

int numPaths(int x, int y)
{
    int num = 0;

    if((x == 1 and y == 0) or (x == 0 and y == 1))
    {
        num = 1;
    }
    else if(x == 0 and y == 0)
    {
        return 0; 
    }
    else
    {
        if(x > 0)
        {
            num += numPaths(x-1, y);
        }  
        if(y > 0)
        {
            num += numPaths(x, y-1);
        }
    }

    return num;
}

int main()
{
    cout << numPaths(1,1) << endl;
    cout << numPaths(1,2) << endl;
    cout << numPaths(2,1) << endl;
}
