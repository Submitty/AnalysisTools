#include <iostream>

using namespace std;

bool isSorted(int* b, int size)
{
    int* p;
    for(p=b; p<(b+size-1); p++)
    {
        if(*p > *(p+1))
        {
            return false;
        }
    }
    return true;
}

int main()
{
    const int n = 10;
    int a[10] = {1,2,3,4,5,6,7,8,9,1};
    int* p;
    for(p=a+n-1; p>=a; p--)
    {
        cout << *p << endl;
    } 

    for(p=a; p<(a+n); p++)
    {
        if((p-a)%2 == 0)
        {
            cout << *p << endl;
        } 
    }    

    cout << isSorted(a,n) << endl;

}
