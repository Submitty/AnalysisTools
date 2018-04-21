
#include <iostream>
#include <fstream>
#include <string>
#include <list>
#include <vector>
#include <map>
#include <cstdlib>
#include <cassert>
#include <unordered_set>
#include <unordered_map>

using namespace std;

typedef unordered_set<string, int> MY_MAP;

bool ReadNextWord(std::istream &istr, std::string &word) {
    char c;
    word.clear();
    while (istr) {
        // just "peek" at the next character in the stream
        c = istr.peek();
        if (isspace(c)) {
            // skip whitespace before a word starts
            istr.get(c);
            if (word != "") {
                // break words at whitespace
                return true;
            }
        } else if (c == '"') {
            // double quotes are a delimiter and a special "word"
            if (word == "") {
                istr.get(c);
                word.push_back(c);
            }
            return true;
        } else if (isalpha(c)) {
            // this a an alphabetic word character
            istr.get(c);
            word.push_back(tolower(c));
        } else {
            // ignore this character (probably punctuation)
            istr.get(c);
        }
    }
    return false;
}

// Loads the sample text from the file, storing it in the map data
// structure Window specifies the width of the context (>= 2) of the
// sequencing stored in the map.  parse_method is a placeholder for
// optional extra credit extensions that use punctuation.
//WINDOW SIZE 2
int  main() {
    MY_MAP data;
    // open the file stream
    string filename = "input.txt"; 

    string lastWord;
    string word;
    while (ReadNextWord(istr,word)) {
        // skip the quotation marks (not used for this part)
        if (word == "\"") continue;

        //if first word in file
        if(lastWord.empty())
        {
            data.insert(word);            
        }
        else
        {
            data.find(lastWord);
            
        } 

        lastWord = word;
    }
}


