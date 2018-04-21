#ifndef __SUPERHERO__
#define __SUPERHERO__ 

#include <string>

using namespace std;

class Superhero{

public:
    friend class Team;
    Superhero(const char* n, const char* i, const char* p);
    Superhero();

    const string& getName() const;
    const string& getPower() const; 
    bool isGood() const;
    
    friend bool operator==(const Superhero& hero, const char* guess);
    friend bool operator!=(const Superhero& hero, const char* guess);
    friend ostream& operator<<(ostream& out, const Superhero& s);
    friend void operator-(Superhero& s); 
    friend bool operator>(const Superhero& a, const Superhero& b);
    
private:
    string name;
    string identity;
    string power;
    bool good;

    const string& getTrueIdentity() const;
};


#endif
