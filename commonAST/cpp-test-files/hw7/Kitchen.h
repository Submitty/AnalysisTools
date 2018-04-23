#ifndef _kitchen_h
#define _kitchen_h

#include <list>
#include <string>
#include "Ingredient.h"

using namespace std;

class Kitchen{

public:
    //Note: using compiler created default constructor 

    //ACCESSORS
    const list<Ingredient>& getIngredients() const { return ingredients; };
    list<Ingredient>& getIngredientsReference() { return ingredients; }; 
   
    //OTHER MEMBER FUNCTIONS    
    void useIngredient(list<Ingredient>::iterator itrToIngredient, int numToUse);
    void addIngredient(const string& name, int numToAdd);
    void printIngredients(ostream& out);    

private:
    list<Ingredient> ingredients;

};

#endif
