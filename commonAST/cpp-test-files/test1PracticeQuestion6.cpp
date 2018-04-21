#include <vector>
#include <iostream>
#include <fstream>
#include <algorithm>

using namespace std;

int getNumVowels(const string& word)
{
    int numVowels = 0;
    for(int i=0; i<word.size(); i++)
    {
        if(word[i] == 'a' or word[i] == 'e' or word[i] == 'i' or word[i] == 'o' or word[i] == 'u')
        {
            numVowels++;
        }
    }
    
    return numVowels;
}

bool sort_by_vowels(const string& w1, const string& w2)
{
    if(getNumVowels(w1) < getNumVowels(w2))
    {
        return true;
    }
    else if(getNumVowels(w1) > getNumVowels(w2))
    {
        return false;
    }
    else
    {
        return w1 < w2;
    }
}

int main()
{
    int a[3] = {0,1,2};    

    vector<string> words;
    ifstream file("input.txt");

    string word;
    while(file >> word)
    {
        words.push_back(word);
    }
        
    sort(words.begin(), words.end(), sort_by_vowels);
    for(int i=0; i<words.size(); i++)
    {
        cout << words[i] << " ";
    }
    return 0;
}
