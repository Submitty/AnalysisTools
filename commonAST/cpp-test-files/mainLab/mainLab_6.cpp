#include <iostream>
#include <vector>
#include <list>

using namespace std;

void reverse_list(list<int>& l)
{
    list<int>::reverse_iterator ritr;
    list<int>::iterator itr;
    itr = l.begin(); 
    int count = 0;
    for(ritr=l.rbegin(); count< (l.size()/2); ritr++)
    {
        swap((*itr), (*ritr));
        itr++; 
        count++;
    } 
}

    template <typename T>
void reverse_vector(vector<T>& v)
{
    for(int i=0; i<(v.size()/2); i++)
    {
        swap(v[i],v[v.size()-1-i]);
    } 
}

    template <typename T>
void printVector(const vector<T>& v)
{
    for(int i=0; i<v.size(); i++)
    {
        cout << v[i] << " ";
    }
    cout << endl;
}

void printList(const list<int>& l)
{
    list<int>::const_iterator itr;
    for(itr=l.begin(); itr!=l.end(); itr++)
    {
        cout << *itr << " ";
    }
    
    cout << endl;
}


int main()
{

    vector<int> v1;
    list<int> l1;
    for(int i=0; i<=10; i++)
    {
        v1.push_back(i);
        l1.push_back(i);
    }

    cout<< "Vector: ints 0-10" << endl;;
    cout << "BEFORE: " << endl;
    printVector(v1);
    reverse_vector(v1);
    cout << "AFTER: " << endl;
    printVector(v1);   
    cout<< endl;

    cout << "List: ints 0-10" << endl;;
    cout << "BEFORE: " << endl;
    printList(l1);
    reverse_list(l1);
    cout << "AFTER: " << endl;
    printList(l1);   
    cout << endl;

    list<int> oddL;
    vector<int> odd;
    for(int i=0; i<12; i++)
    {
        oddL.push_back(i);
        odd.push_back(i);
    }

    cout << "Vector: ints 0-11" << endl;
    cout << "BEFORE: " << endl;
    printVector(odd);
    reverse_vector(odd);
    cout << "AFTER: " << endl;
    printVector(odd);
    cout << endl;

    cout << "List: ints 0-11" << endl;
    cout << "BEFORE: " << endl;
    printList(oddL);
    reverse_list(oddL);
    cout << "AFTER: " << endl;
    printList(oddL);
    cout << endl;

    vector<int> v2;
    cout << "Vector: empty" << endl;
    cout << "BEFORE: " << endl;
    printVector(v2);
    reverse_vector(v2);
    cout << "AFTER: " << endl;
    printVector(v2);

    list<int> l2; 
    cout << "List: empty" << endl;
    cout << "BEFORE: " << endl; 
    printList(l2);
    reverse_list(l2);
    cout << "AFTER: " << endl;
    printList(l2);

    vector<int> v3;
    v3.push_back(2);
    cout << "Vector: one element" << endl;
    cout << "BEFORE: " << endl;
    printVector(v3);
    reverse_vector(v3);
    cout << "AFTER: " << endl;
    printVector(v3);

    list<int> l3;
    l3.push_back(2);
    cout << "List: one element" << endl;
    cout << "BEFORE: " << endl;
    printList(l3);
    reverse_list(l3);
    cout << "AFTER: " << endl;
    printList(l3);

    vector<int> v5;
    v5.push_back(2);
    v5.push_back(3);
    cout << "Vector: two elements" << endl;
    cout << "BEFORE: " << endl;
    printVector(v5);
    reverse_vector(v5);
    cout << "AFTER: " << endl;
    printVector(v5);
    
    list<int> l4;
    l4.push_back(2);
    l4.push_back(3);
    cout << "List: two elements" << endl;
    cout << "BEFORE: " << endl;
    printList(l4);
    reverse_list(l4);
    cout << "AFTER: " << endl;
    printList(l4);


    vector<string> v4;
    v4.push_back("a");
    v4.push_back("b");
    v4.push_back("c");
    v4.push_back("d");
    v4.push_back("e");
    v4.push_back("f");
    cout << "BEFORE: " << endl;
    printVector(v4);
    reverse_vector(v4);
    cout << "AFTER: " << endl;
    printVector(v4);

    return 0;
}
