#include <vector>
#include <iostream>
#include <string>
#include <algorithm>

using namespace std;

bool byAlpha(const string& s1, const string& s2)
{
        return s1 < s2;
}

bool byLength(const string& s1, const string& s2)
{
        if(s1.size() < s2.size())
        {
            return true;
        }
        else if(s1.size() > s2.size())
        {
            return false;
        }
        else
        {
            return s1 > s2;
        }
}

int main()
{
    vector<string> words;
    words.push_back("dog");
    words.push_back("snake");
    words.push_back("cat");
    words.push_back("ferret");
    words.push_back("gorilla");
    words.push_back("bird");
    words.push_back("jaguar");

    sort(words.begin(), words.end(), byAlpha);
    for(int i=0; i<words.size(); i++)
    {
        cout << words[i] << " ";
    }
    cout << endl;
    
    sort(words.begin(), words.end(), byLength);
    for(int i=0; i<words.size(); i++)
    {
        cout << words[i] << " ";
    }


    return 0;
}
