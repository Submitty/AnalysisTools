// -----------------------------------------------------------------
// HOMEWORK 7 WORD FREQUENCY MAPS
//
// You may use all of, some of, or none of the provided code below.
// You may edit it as you like (provided you follow the homework
// instructions).
// -----------------------------------------------------------------

#include <iostream>
#include <fstream>
#include <string>
#include <list>
#include <vector>
#include <map>
#include <cstdlib>
#include <cassert>
#include "MersenneTwister.h"

using namespace std;

//typedef for window size of 2
typedef map< string, map<string, int> >  MY_MAP;
//typedef for window size of 3
typedef map< string, MY_MAP > MY_MAP2;

// Custom helper function that reads the input stream looking for
// double quotes (a special case delimiter needed below), and white
// space.  Contiguous blocks of alphabetic characters are lowercased &
// packed into the word.
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


// Custom helper function that reads the input stream looking a
// sequence of words inside a pair of double quotes.  The words are
// separated by white space, but the double quotes might not have
// space between them and the neighboring word.  Punctuation is
// ignored and words are lowercased.
std::vector<std::string> ReadQuotedWords(std::istream &istr) {
    // returns a vector of strings of the different words
    std::vector<std::string> answer;
    std::string word;
    bool open_quote = false;
    while (ReadNextWord(istr,word)) {
        if (word == "\"") {
            if (open_quote == false) { open_quote=true; }
            else { break; }
        } else {
            // add each word to the vector
            answer.push_back(word);
        }
    }
    return answer;
}

//Load sample text for window size 3
void LoadSampleText(MY_MAP2 &data, const std::string &filename, int window, const std::string &parse_method) {
    // open the file stream
    std::ifstream istr(filename.c_str());
    if (!istr) { 
        std::cerr << "ERROR cannot open file: " << filename << std::endl; 
        exit(1);
    } 
    // verify the window parameter is appropriate
    if (window < 2) {
        std::cerr << "ERROR window size must be >= 2:" << window << std::endl;
    }
    // verify that the parse method is appropriate
    bool ignore_punctuation = false;
    if (parse_method == "ignore_punctuation") {
        ignore_punctuation = true;
    } else {
        std::cerr << "ERROR unknown parse method: " << parse_method << std::endl;
        exit(1);
    }

    string word;
    string prevWordOne;
    string prevWordTwo; 

    while (ReadNextWord(istr,word)) {
        // skip the quotation marks (not used for this part)
        if (word == "\"") continue;

        //if first word in file
        if(prevWordOne.empty())
        {
            data[word];            
        }
        //if second word in file
        else if(prevWordTwo.empty())
        {
            data[prevWordOne][word];
        }
        else
        {
            data[prevWordTwo][prevWordOne][word]++;
        } 

        prevWordTwo = prevWordOne;
        prevWordOne = word;
    }
    
    //make sure two last words in file get incremented properly
    string temp;
    data[prevWordOne][temp][temp]++;
    data[prevWordOne][prevWordTwo][temp]++;
    data[prevWordTwo][temp][temp]++; 
}

// Loads the sample text from the file, storing it in the map data
// structure Window specifies the width of the context (>= 2) of the
// sequencing stored in the map.  parse_method is a placeholder for
// optional extra credit extensions that use punctuation.
//WINDOW SIZE 2
void LoadSampleText(MY_MAP &data, const std::string &filename, int window, const std::string &parse_method) {
    // open the file stream
    std::ifstream istr(filename.c_str());
    if (!istr) { 
        std::cerr << "ERROR cannot open file: " << filename << std::endl; 
        exit(1);
    } 
    // verify the window parameter is appropriate
    if (window < 2) {
        std::cerr << "ERROR window size must be >= 2:" << window << std::endl;
    }
    // verify that the parse method is appropriate
    bool ignore_punctuation = false;
    if (parse_method == "ignore_punctuation") {
        ignore_punctuation = true;
    } else {
        std::cerr << "ERROR unknown parse method: " << parse_method << std::endl;
        exit(1);
    }

    string lastWord;
    string word;
    while (ReadNextWord(istr,word)) {
        // skip the quotation marks (not used for this part)
        if (word == "\"") continue;

        //if first word in file
        if(lastWord.empty())
        {
            data[word];            
        }
        else
        {
            data[lastWord][word]++;
        } 

        lastWord = word;
    }
   
    //make sure last word in file gets incremented correctly 
    data[lastWord][""]++;
}

//function to print all words that appear after input word for 3 WINDOW
void print(const MY_MAP2& data, const string& word1, const string& word2)
{
    MY_MAP2::const_iterator itr = data.find(word1);

    //if word not in map 
    if(itr == data.end())
    {
        cerr << word1 << " not in file" << endl;
        cerr << endl;
        return;
    }

    //if window size is 3 but only one word entered
    if(word2 == "")
    {
        //iterate through outside map keys 
        MY_MAP::const_iterator insideItr = itr->second.begin();
        int total = 0;

        //find total number of occurances
        for(insideItr; insideItr != itr->second.end(); insideItr++)
        {
            map<string, int>::const_iterator sumItr = insideItr->second.begin();
            for(sumItr; sumItr != insideItr->second.end(); sumItr++)
            {
                total += sumItr->second;
            }
        }        

        cout << word1 << " (" << total << ")" << endl;

        //print all subsequent keys and number of times they appear
        for(insideItr = itr->second.begin(); insideItr != itr->second.end(); insideItr++)
        {
            map<string, map<string,int> >::const_iterator itr2 = itr->second.find(insideItr->first);
            if(itr2 == itr->second.end())
            {
                cerr << word1 << " " << word2 << " not in file" << endl;
                cerr << endl;
                return;
            }
   
            //find number of time word appears 
            int t = 0;
            map<string, int>::const_iterator sumItr = itr2->second.begin();
            for(sumItr; sumItr != itr2->second.end(); sumItr++)
            {
                t+= sumItr->second;
            }

            if(insideItr->first != "")
            {
                cout << word1 << " " << insideItr->first << " (" << t << ")" << endl;
            }
        }
    }
    else
    {
        map<string, map<string,int> >::const_iterator itr2 = itr->second.find(word2);

        //make sure the value is in the map 
        if(itr2 == itr->second.end())
        {
            cerr << word1 << " " << word2 << " not in file" << endl;
            cerr << endl;
            return;
        }

        //loop through and find totals 
        int total =0;

        map<string, int>::const_iterator itr3 = itr2->second.begin();
        for(itr3; itr3 != itr2->second.end(); itr3++)
        {
            total += itr3->second; 
        }

        cout << word1 << " " << word2 << " (" << total << ")" << endl;

        //iterate through and print each subsequent word and the number of times it appears
        for(itr3=itr2->second.begin(); itr3 != itr2->second.end(); itr3++)
        {
            if(itr3->first != "")
            {
                cout << word1 << " " << word2 << " " << itr3->first << " (" << itr3->second << ")" << endl;
            }
        } 
    }
    cout << endl;
}


//function to print all words that appear after input word FOR 2 WINDOWS
void print(const MY_MAP& data, const string& word)
{
    MY_MAP::const_iterator itr = data.find(word);
    
    //if word not in map
    if(itr == data.end())
    {
        cerr << word << " not in file" << endl;
        cerr << endl;
        return;
    }

    //iterate over corresponding word's next word map and print 
    map<string,int>::const_iterator itr2 = itr->second.begin();

    int total =0;
    for(itr2; itr2 != itr->second.end(); itr2++)
    {
        total += itr2->second;
    }

    cout << word << " (" << total << ")" << endl;

    for(itr2=itr->second.begin(); itr2 != itr->second.end(); itr2++)
    {
        if(itr2->first != "")
        {
            cout << word << " " << itr2->first << " (" << itr2->second << ")" << endl; 
        }
    }  

    cout << endl;
}

const string& getRandNextWord(MY_MAP::const_iterator outer, map<string,int>::const_iterator inner)
{
    //if random, generate a random number between 0 and 1 
    MTRand mtrand;
    float val = mtrand();
    float bottomRange = 0;
    float probability = 0;        
    float topRange = 0;

    //iterate over possible values and create "ranges" based on the word's probabilities
    //words with higher probabilities have larger ranges and more of a chance of being "chosen"

    for(inner; inner != outer->second.end(); inner++)
    {
        probability = float(inner->second) / float(outer->second.size());

        topRange += probability;
        if(val > bottomRange and val <= topRange)
        {
            return inner->first;
        }

        bottomRange = probability;
    }

}

//recursive function to generate next words for 3 WINDOW and two words
string generate(const MY_MAP2& m, const string& word1, const string& word2, int num, bool rand_flag)
{
    //base case - if we have generated input number words, we are done
    if(num == 0)
    {
        return word1;
    }

    //find the first word
    MY_MAP2::const_iterator itr = m.find(word1);
    if(itr == m.end())
    {
        cerr << "word: " << word1 << " not found" << endl;
        cerr << endl;
        exit(1);
    }

    //find the second word
    MY_MAP::const_iterator itr2 = itr->second.find(word2);
    if(itr2 == itr->second.end())
    {
        cerr << "word: " << word1 << " " << word2 << " not found" << endl;
        cerr << endl;
        exit(1);
    }

    //iterate over the words 
    map<string,int>::const_iterator itr3 = itr2->second.begin();

    int max = itr3->second;
    string nextWord = itr3->first;

    //if looking for most_common, look for the word that appears the max number of times
    if(!rand_flag)
    {
        for(itr3; itr3 != itr2->second.end(); itr3++)
        {
            if(itr3->second > max)
            {
                nextWord = itr3->first;
                max = itr3->second;
            }
        } 
    }
    else
    {
        //nextWord = getRandNextWord(itr2, itr3);
        //if random, generate a random number between 0 and 1 
        MTRand mtrand;
        float val = mtrand();
        float bottomRange = 0;
        float probability = 0;        
        float topRange = 0;

        //iterate over possible values and create "ranges" based on the word's probabilities
        //words with higher probabilities have larger ranges and more of a chance of being "chosen"

        for(itr3; itr3 != itr2->second.end(); itr3++)
        {
        probability = float(itr3->second) / float(itr2->second.size());
        topRange += probability;
        if(val > bottomRange and val <= topRange)
        {
        nextWord = itr3->first;
        break;
        }

        bottomRange = probability;
        }
    }

    //recurse with next word 
    return word1 + " " + generate(m, word2, nextWord, --num, rand_flag);
}

//recursive function to generate next words 3 WINDOW and 1 word
string stageGenerate(const MY_MAP2& m, const string& word, int num, bool rand_flag)
{
    //find the first word
    MY_MAP2::const_iterator itr = m.find(word);
    if(itr == m.end())
    {
        cerr << "word: " << word << " not found" << endl;
        exit(1);
    }

    //iterate over the words in the first word's second
    MY_MAP::const_iterator itr2 = itr->second.begin();

    int max = 0;
    string nextWord = itr2->first;

    //if looking for most_common, look for the word that appears the max number of times
    if(!rand_flag)
    {
        for(itr2; itr2 != itr->second.end(); itr2++)
        {
            map<string, int>::const_iterator itr3 = itr2->second.begin();

            int num =0;
            for(itr3; itr3 != itr2->second.end(); itr3++)
            {
                num += itr3->second;
            }

            if(num > max)
            {
                nextWord = itr2->first;
                max = num; 
            }
        }
    } 
    else
    {

        //if random, generate a random number between 0 and 1 
        MTRand mtrand;
        float val = mtrand();
        float bottomRange = 0;
        float probability = 0;        
        float topRange = 0;

        //iterate over possible values and create "ranges" based on the word's probabilities
        //words with higher probabilities have larger ranges and more of a chance of being "chosen"

        for(itr2; itr2 != itr->second.end(); itr2++)
        {
        map<string, int>::const_iterator itr3 = itr2->second.begin();

        probability = float(itr2->second.size()) / float(itr->second.size());
        topRange += probability;
        if(val > bottomRange and val <= topRange)
        {
        nextWord = itr2->first;
        break;
        }

        bottomRange = probability;
        }

    }

    return generate(m, word, nextWord, num, rand_flag);
}

//recursive function to generate next words 2 WINDOW
string generate(const MY_MAP& m, const string& word, int num, bool rand_flag)
{
    //base case - if we have generated input number words, we are done
    if(num == 0)
    {
        return word;
    }

    //find the first word
    MY_MAP::const_iterator itr = m.find(word);
    if(itr == m.end())
    {
        cerr << "word: " << word << " not found" << endl;
        exit(1);
    }

    //iterate over the words in the first word's second
    map<string,int>::const_iterator itr2 = itr->second.begin();

    int max = itr2->second;
    string nextWord = itr2->first;

    //if looking for most_common, look for the word that appears the max number of times
    if(!rand_flag)
    {
        for(itr2; itr2 != itr->second.end(); itr2++)
        {
            if(itr2->second > max)
            {
                nextWord = itr2->first;
                max = itr2->second;
            }
        } 
    }
    else
    {
        nextWord = getRandNextWord(itr, itr2);

     }

    //recurse with next word 
    return word + " " +  generate(m, nextWord, --num, rand_flag);

}

int main () {

    MY_MAP data;
    MY_MAP2 data2;
    int window;

    // Parse each command
    std::string command;    
    while (std::cin >> command) {

        // load the sample text file
        if (command == "load") {
            std::string filename;
            std::string parse_method;
            std::cin >> filename >> window >> parse_method;      

            if(window == 2)
            {            
                LoadSampleText(data, filename, window, parse_method); 
            }
            else if(window == 3)
            {
                LoadSampleText(data2, filename, window, parse_method); 
            }
            else
            {
                cerr << "window size: " << window << " not valid" << endl;
                exit(1);
            }

            cout << "Loaded " << filename << " with window = " << window << " and parse method = " << parse_method << endl;
            cout << endl;
        } 

        // print the portion of the map structure with the choices for the
        // next word given a particular sequence.
        else if (command == "print") {
            std::vector<std::string> sentence = ReadQuotedWords(std::cin);

            if(sentence.size() == 1)
            {
                if(window == 2)
                {
                    print(data, sentence[0]);
                }
                else if(window == 3)
                {
                    print(data2, sentence[0], "");
                }
            }
            else if(sentence.size() == 2)
            {
                print(data2, sentence[0], sentence[1]);
            }
            else
            {
                cerr << "window size: " << sentence.size() << " not valid" << endl;
            }
        }

        // generate the specified number of words 
        else if (command == "generate") {
            std::vector<std::string> sentence = ReadQuotedWords(std::cin);
            // how many additional words to generate
            int length;
            std::cin >> length;
            std::string selection_method;
            std::cin >> selection_method;
            bool random_flag;
            if (selection_method == "random") {
                random_flag = true;
            } else {
                assert (selection_method == "most_common");
                random_flag = false;
            }

            if(sentence.size() == 1) 
            {
                if(window == 2)
                {
                    cout << generate(data, sentence[0], length, random_flag) << endl; 
                }
                else if(window == 3)
                {
                    cout << stageGenerate(data2, sentence[0], length, random_flag) << endl;
                }
            }
            else if(sentence.size() == 2 and window == 3)
            {
                //call generate with one added to length to account for the second word passed in
                cout << generate(data2, sentence[0], sentence[1], length+1, random_flag) << endl;
            }
            else
            {
                cerr << "window size: " << sentence.size() << " not valid" << endl;
            }
            cout << endl;

        } else if (command == "quit") {
            break;
        } else {
            std::cout << "WARNING: Unknown command: " << command << std::endl;
        }
    }
}
