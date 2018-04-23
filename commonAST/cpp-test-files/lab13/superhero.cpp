#include "superhero.h"
#include <iostream>

using namespace std;

Superhero::Superhero(const char* n, const char* i, const char* p)
{
    name = string(n);
    identity = string(i);
    power = string(p);
    good = true;
}


Superhero::Superhero()
{
    name = "";
    identity = "";
    power = "";
    good = true;
}

const string& Superhero::getName() const
{
    return name;    
}

const string& Superhero::getPower() const
{
    return power;
} 

bool operator==(const Superhero& s, const char* guess) 
{
    return string(guess) == s.identity;
}

bool operator!=(const Superhero& s, const char* guess)
{
    return not(string(guess) == s.identity);
} 

ostream& operator<< (ostream& out, const Superhero& s)
{
    out << "Superhero " << s.name << " has power " << s.power << endl;
    return out;
}

bool Superhero::isGood() const
{
    return good;
}

void operator-(Superhero& s)
{
    s.good = !s.good;
} 

bool operator>(const Superhero& a, const Superhero& b)
{
    return true;    
}

const string& Superhero::getTrueIdentity() const
{
    return identity;
}


