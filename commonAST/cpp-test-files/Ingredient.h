#ifndef _ingredient_h
#define _ingredient_h

#include <string>

using namespace std;

class Ingredient{

public:
    //CONSTRUCTOR   
    Ingredient(const string& _name);
    Ingredient(const string& _name, int numToAdd);

    //ACCESSORS
    const string& getName() const { return name; };
    int getNumOf() const { return numOf; };

    //MODIFIERS 
    void addMore(int numberToAdd) { numOf += numberToAdd; }    
    void useIngredient(int numToUse){ numOf -= numToUse; }


    friend ostream& operator<<(ostream& out, const Ingredient& i);
private:
    string name;
    int numOf;
    

};
bool operator==(const Ingredient& i1, const Ingredient& i2);
bool sortAlphabetical(const Ingredient& i1, const Ingredient& i2);
bool sortQuantity(const Ingredient& i1, const Ingredient& i2);

#endif
