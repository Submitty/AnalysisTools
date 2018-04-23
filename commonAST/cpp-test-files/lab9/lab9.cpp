#include <iostream>
#include <map>
#include <fstream>
#include <vector>

using namespace std;

int main(int argc, char* argv[])
{
    if(argc != 2)
    {
        cout << "enter a file name " << endl;
        return 1;
    }

    ifstream file(argv[1]); 

    if(!file.good())
    {
        cout << "file not found " << endl;
        return 1;
    }

    map<int, int> m;   
    
    int temp;
    while(file >> temp)
    {
        m[temp]++;
    }


    vector<int> modes; 
    map<int, int>::const_iterator itr = m.begin();
    int maxNumOf = itr->second;
    modes.push_back(itr->first);
    itr++;
    for(itr; itr != m.end(); itr++)
    {
        if(itr->second == maxNumOf)
        {
            modes.push_back(itr->first);
        } 
        else if(itr->second > maxNumOf)
        {
            maxNumOf = itr->second;
            modes.clear();
            modes.push_back(itr->first);
        }
    }

    for(int i=0; i<modes.size(); i++)
    {
        cout << modes[i] << " ";
    }    
    cout << endl;
 
    return 0;
}
