#include "Ingredient.h"
#include <iostream>
using namespace std;

Ingredient::Ingredient(const string& _name)
{
    name = _name;
    numOf = 1;
}

Ingredient::Ingredient(const string& _name, int numToAdd)
{
    name = _name;
    numOf = 0;
    numOf += numToAdd;
}
bool sortAlphabetical(const Ingredient& i1, const Ingredient& i2)
{
    return i1.getName() < i2.getName();
}
bool sortQuantity(const Ingredient& i1, const Ingredient& i2)
{
    if(i1.getNumOf() < i2.getNumOf())
    {
        return true;
    }
    else if(i1.getNumOf() > i2.getNumOf())
    {
        return false;
    }
    else
    {
        return i1.getName() < i2.getName();
    }
}
ostream& operator<<(ostream& out, const Ingredient& i)
{
        
        out << i.numOf;
        if(i.numOf > 1)
        {
            out << " units of ";
        }
        else
        {
            out << " unit of ";
        }
        out << i.name << endl;
        return out;
}
