#ifndef __WORD__
#define __WORD__

#include <string>

using namespace std;

class word
{
    public:
        //CONSTRUCTORS
        word();
        word(const string& s) { name = s; numOccurances = 1; }

        //ACCESSORS
        int getNumOccurances() const { return numOccurances; }
        const string& getName() const { return name; }

        //MODIFIERS
        void incrementOccurances() { numOccurances++; } 

    private: 
        int numOccurances;
        string name;
};

bool operator< (const word& one, const word& two);
bool operator== (const word& one, const word& two);


#endif
