#include <set>
#include <iostream>

using namespace std;

int unionCard(const set<int>& a, const set<int>& b)
{ 
    int aIntersectB = 0;

    set<int>::const_iterator itr = a.begin();
    for(itr; itr != a.end(); itr++)
    {
        if(b.find((*itr)) != b.end())
        {
            aIntersectB++;
        }
    }

    return a.size() + b.size() - aIntersectB; 
}


int main(){


    set<int> A;
    A.insert(0);
    A.insert(1);
    A.insert(2);
    A.insert(3);
    set<int> B;
    B.insert(69);
    B.insert(696);
    B.insert(1);

    cout << unionCard(A,B) << endl;

    return 0;
}
