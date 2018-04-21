#include <iostream>
#include <vector>

using namespace std;

vector<string> compoundWords(const vector<string>& words)
{ 
    vector<string> output;
    for(int i=0; i<words.size(); i++)
    {
        for(int j=0; j<words.size(); j++)
        {
            for(int k=0; k<words.size(); k++)
            {
                if(words[i] == words[j] + words[k] or words[i] == words[k] + words[j])
                {
                    output.push_back(words[i]);
                }
            }
        }
    }

    return output;
}


int main()
{
    //a back backlog backwoods backwoodsman cat catalog
    //less log man none nonetheless ship the woods woodsman
    vector<string> w;
    w.push_back("a");
    w.push_back("back");
    w.push_back("backlog");
    w.push_back("backwoods");
    w.push_back("backwoodsman");
    w.push_back("cat");
    w.push_back("catalog");
    w.push_back("less");
    w.push_back("log");
    w.push_back("man");
    w.push_back("none");
    w.push_back("nonetheless");
    w.push_back("ship");
    w.push_back("the");
    w.push_back("woods");
    w.push_back("woodsman");

    vector<string> out;
    out = compoundWords(w);

    for(int i=0; i<out.size(); i++)
    {
        cout << out[i] << endl;
    }        

    return 0;
}
