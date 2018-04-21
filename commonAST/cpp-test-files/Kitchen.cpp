#include "/home/elizabethdinella/test-files/dataStructures/hw4/Ingredient.h"
#include "/home/elizabethdinella/test-files/dataStructures/hw4/Kitchen.h"
#include <iostream>
using namespace std;
    
void Kitchen::addIngredient(const string& name, int numToAdd)
{
    //check if ingredient already in kitchen
    bool alreadyIn = false;        
    list<Ingredient>::iterator itr = ingredients.begin();
    for(itr; itr != ingredients.end() and !alreadyIn; itr++)
    {
        if(itr->getName() == name)
        {
            alreadyIn = true;
            itr->addMore(numToAdd);
        }
    }

    if(!alreadyIn)
    {
        Ingredient newIngredient(name, numToAdd);
        ingredients.push_back(newIngredient);
        ingredients.sort(sortAlphabetical);
    }

}

void Kitchen::useIngredient(list<Ingredient>::iterator itrToIngredient, int numToUse)
{
    itrToIngredient->useIngredient(numToUse);
}

void Kitchen::printIngredients(ostream& out)
{
    ingredients.sort(sortQuantity);
    out  << "In the kitchen:" <<  endl;
    list<Ingredient>::iterator itr = ingredients.begin();
    for(itr; itr!= ingredients.end(); itr++)
    {
       if(itr->getNumOf() != 0)
       {
            out  << "  " << *itr; 
        }
    }
}
