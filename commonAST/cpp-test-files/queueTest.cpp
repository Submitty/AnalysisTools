#include <stack>
#include <iostream>

using namespace std;

void insert_in_order(double x, stack<double>& q)
{
    stack<double> temp;
    int s = q.size();
    cout << "SIZE: " << s << endl;
    bool entered = false;

    for(int i=0; i<s; i++)
    {
        double element = q.top();
        cout << "ELEMENT: " << element << endl;
        q.pop();

        if(element < x && !entered)
        {
            temp.push(x);
            entered = true;
        }

        temp.push(element);
    }

    q = temp;
    if(q.size() == 0)
    {
        q.push(x);
    }
}


int main()
{
    stack<double> a;
    a.push(1);
    a.push(3);
    a.push(5);
    cout << "BEFORE: " << a.size() << endl;
    insert_in_order(4,a);
    cout << "AFTER: " << a.size() << endl;
    
    int s = a.size();
    for(int i=0; i<s; i++)
    {
        cout << a.top() << endl;
        a.pop();
    }   
}
