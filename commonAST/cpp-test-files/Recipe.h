#ifndef _recipe_h
#define _recipe_h

#include <string>
#include <list>
#include "/home/elizabethdinella/test-files/dataStructures/hw4/Ingredient.h"

using namespace std;

class Recipe{

    public:
        //CONSTRUCTOR
        Recipe(const string& _name);

        //ACCESSORS
        const string& getName() const{ return name; };
        const list<Ingredient>& getIngredients() const{ return ingredients; };    

        //OTHER MEMBER FUNCTIONS
        void addIngredient(const string& ingredientName, int numToAdd);

        friend ostream& operator<<(ostream& out, const Recipe& r);
    
    private:
        list<Ingredient> ingredients;
        string name;

};

bool sortAlphabetically(const Recipe& r1, const Recipe& r2);    
bool operator==(const Recipe& r1, const Recipe& r2);

#endif
