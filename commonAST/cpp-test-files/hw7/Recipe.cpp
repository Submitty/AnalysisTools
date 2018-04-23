#include <iostream>
#include "Recipe.h"
using namespace std;

Recipe::Recipe(const string& _name)
{
    name = _name;
}

void Recipe::addIngredient(const string& ingredientName, int numToAdd)
{
    Ingredient i(ingredientName, numToAdd);
    ingredients.push_back(i);
    ingredients.sort(sortAlphabetical);
}

ostream& operator<<(ostream& out, const Recipe& r)
{
    list<Ingredient>::const_iterator itr = r.ingredients.begin();
    for(itr; itr != r.ingredients.end(); itr++)
    {
        out << "  " << *itr;
    } 
    return out;
}

bool sortAlphabetically(const Recipe& r1, const Recipe& r2)
{
    return r1.getName() < r2.getName(); 
}  

bool operator==(const Recipe& r1, const Recipe& r2)
{
    return r1.getName() == r2.getName();
}
