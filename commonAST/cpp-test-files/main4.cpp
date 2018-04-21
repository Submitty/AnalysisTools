//
// PROVIDED CODE FOR HOMEWORK 4: GROCERY LISTS
// 
// You may use none, a little, or all of this, as you choose, but we
// strongly urge you to examine it carefully.  You may modify this
// code as you wish to complete the assignment.
//

#include <algorithm>
#include <cassert>
#include <cstdlib>
#include <iostream>
#include <fstream>


#include "/home/elizabethdinella/test-files/dataStructures/hw4/Recipe.h"
#include "/home/elizabethdinella/test-files/dataStructures/hw4/Kitchen.h"

using namespace std;


// Helper functions
void readRecipe(istream &istr, ostream &ostr, list<Recipe> &recipes);
void addIngredients(istream &istr, ostream &ostr, Kitchen &kitchen);
void printRecipe(istream &istr, ostream &ostr, const list<Recipe> &recipes);
void makeRecipe(istream &istr, ostream &ostr, const list<Recipe> &recipes, Kitchen &kitchen);
list<Recipe> recipeSuggestions(ostream &ostr, const list<Recipe> &recipes, const Kitchen &kitchen, bool print=true);
void suggestDinner(ostream &ostr, const list<Recipe> &recipes, const Kitchen& kitchen);

// The main loop parses opens the files for I/O & parses the input
int main(int argc, char* argv[]) {

    // Check the number of arguments.
    if (argc != 3) {
        cerr << "Usage: " << argv[0] << " in-file out-file\n";
        return 1;
    }
    // Open and test the input file.
    ifstream istr(argv[1]);
    if (!istr) {
        cerr << "Could not open " << argv[1] << " to read\n";
        return 1;
    }
    // Open and test the output file.
    ofstream ostr(argv[2]);
    if (!ostr) {
        cerr << "Could not open " << argv[2] << " to write\n";
        return 1;
    }

    // the kitchen & recipe list
    Kitchen kitchen;
    list<Recipe> recipes;

    // some variables to help with parsing
    char c;
    while (istr >> c) {
        if (c == 'r') {
            // READ A NEW RECIPE
            readRecipe(istr,ostr,recipes);

        } else if (c == 'a') {
            // ADD INGREDIENTS TO THE KITCHEN
            addIngredients(istr,ostr,kitchen);

        } else if (c == 'p') {
            // PRINT A PARTICULAR RECIPE
            printRecipe(istr,ostr,recipes);

        } else if (c == 'm') {
            // MAKE SOME FOOD
            makeRecipe(istr,ostr,recipes,kitchen);

        } else if (c == 'k') {
            // PRINT THE CONTENTS OF THE KITCHEN
            kitchen.printIngredients(ostr);

        } else if (c == 's') {
            // SUGGEST ALL RECIPES THAT CAN BE MADE INDIVIDUALLY FROM THE
            // CURRENT CONTENTS OF THE KITCHEN
            recipeSuggestions(ostr,recipes,kitchen);

        }else if (c == 'd') {
            // EXTRA CREDIT: SUGGEST THE COLLECTION OF RECIPES THAT CAN BE
            // MADE TOGETHER THAT USE THE MAXIMUM NUMBER OF UNITS OF
            // INGREDIENTS
            suggestDinner(ostr,recipes,kitchen);
        } else {
            cerr << "unknown character: " << c << endl;
            exit(0);
        }

        //FOR TESTING
        //every time the program reads a line, print recipes, and kitchen ingredients
        /*
           cout << "RECIPES" << endl;
           cout << "_________________________________________________" << endl;
           list<Recipe>::iterator itr = recipes.begin();
           for(itr; itr!=recipes.end(); itr++)
           {
           cout << "RECIPE: " << itr->getName() << endl;
           cout << *itr << endl;
           }

           cout << "_________________________________________________" << endl;
           cout << "CURRENT KITCHEN" << endl;
           list<Ingredient>::const_iterator itr2 = kitchen.getIngredients().begin();
           for(itr2; itr2 != kitchen.getIngredients().end(); itr2++)
           {
           cout << *itr2;
           }
         */
    }  
}


void readRecipe(istream &istr, ostream &ostr, list<Recipe> &recipes) {
    int units;
    string name, name2;  
    istr >> name;
    // build the new recipe
    Recipe r(name);
    while (1) {
        istr >> units;
        if (units == 0)
        {
            break;
        }
        assert (units > 0);
        istr >> name2;
        r.addIngredient(name2,units);
    }

    bool alreadyIn = false;
    //check to make sure recipe is not already in list
    list<Recipe>::iterator itr = recipes.begin();
    for(itr; itr!= recipes.end() and !alreadyIn; itr++)
    {
        if(itr->getName() == name)
        {
            alreadyIn = true;
        }
    }

    if(alreadyIn)
    {
        ostr << "Recipe for " << name << " already exists" << endl;
    }
    else
    {
        // add it to the list
        recipes.push_back(r);
        ostr << "Recipe for " << name << " added" << endl;
    }

}


void addIngredients(istream &istr, ostream &ostr, Kitchen &kitchen) {
    int units;
    string name;
    int count = 0;
    while (1) {
        istr >> units;
        if (units == 0) break;
        assert (units > 0);
        istr >> name;
        // add the ingredients to the kitchen
        kitchen.addIngredient(name,units);
        count++;
    }
    ostr << count;
    if(count >1)
    {
        ostr  << " ingredients added to kitchen" << endl;
    }
    else
    {
        ostr << " ingredient added to kitchen" << endl;
    }
}


void printRecipe(istream &istr, ostream &ostr, const list<Recipe> &recipes) {
    string name;
    istr >> name;

    //flag to check if recipe is in list of recipes
    bool validRecipe = false;

    //for each recipe in recipes 
    list<Recipe>::const_iterator recipe = recipes.begin();
    for(recipe; recipe != recipes.end(); recipe++)
    {
        if(recipe->getName() == name)
        {
            validRecipe = true;
            ostr << "To make " << name << ", mix together:" << endl;
            ostr << *recipe;
        }
    }    

    if(!validRecipe)
    {
        ostr << "No recipe for " << name << endl;
    }

}


void makeRecipe(istream &istr, ostream &ostr, const list<Recipe> &recipes, Kitchen &kitchen) {
    string name;
    istr >> name;

    bool validRecipe = false;
    list<Recipe>::const_iterator recipe = recipes.begin();
    for(recipe; recipe != recipes.end(); recipe++)
    {
        if(recipe->getName() == name)
        {
            //flag to check if recipe is in recipe list
            validRecipe = true;
            //flag to print the heading only once
            bool firstIngredientNeeded = true;

            //creates a boolean array of size recipe ingredients 
            //will change to true if the kitchen has enough of that ingredient 
            list<bool> haveEnoughOf;  

            const list<Ingredient> ingredientsNeeded = recipe->getIngredients();
            //create iterator to loop through all ingredients needed in the recipe
            list<Ingredient>::const_iterator ingredientNeeded = recipe->getIngredients().begin(); 
            //create iterator to loop through all ingredients in the kitchen
            list<Ingredient>::iterator ingredientHave = kitchen.getIngredientsReference().begin();
            for(ingredientNeeded; ingredientNeeded != recipe->getIngredients().end(); ingredientNeeded++)
            {
                //flag to make sure the ingredient is in the kitchen
                bool haveIngredient = false;
                ingredientHave=kitchen.getIngredientsReference().begin();
                for(ingredientHave; ingredientHave != kitchen.getIngredientsReference().end(); ingredientHave++)
                {
                    //if the ingredient needed is = ingredient have
                    if(ingredientNeeded->getName() == ingredientHave->getName())
                    {
                        haveIngredient = true;
                        if(ingredientNeeded->getNumOf() <= ingredientHave->getNumOf())
                        {
                            haveEnoughOf.push_back(true);
                            //have enough
                        }
                        else
                        {
                            haveEnoughOf.push_back(false);
                            //dont have enough

                        }
                    }
                } 
                if(!haveIngredient)
                {
                    haveEnoughOf.push_back(false);
                }              
            }


            ingredientNeeded = recipe->getIngredients().begin(); 
            bool allTrue = true;
            for(list<bool>::iterator itr = haveEnoughOf.begin(); itr!=haveEnoughOf.end(); itr++)
            {

                if(*itr == false) 
                {

                    allTrue = false;
                    if(firstIngredientNeeded)
                    {
                        ostr << "Cannot make " << name << ", need to buy:" << endl;   
                        firstIngredientNeeded = false;
                    }

                    int need = ingredientNeeded->getNumOf();
                    bool haveIngredient = false;
                    ingredientHave = kitchen.getIngredientsReference().begin(); 
                    for(ingredientHave; ingredientHave != kitchen.getIngredientsReference().end(); ingredientHave++)
                    {
                        if(ingredientNeeded->getName() == ingredientHave->getName())
                        {
                            haveIngredient = true;
                            int have = ingredientHave->getNumOf();
                            int diff = need-have; 
                            ostr << "  " << diff;
                            if(diff > 1)
                            { 
                                ostr << " units of ";
                            }
                            else
                            {
                                ostr << " unit of ";
                            }

                            ostr << ingredientNeeded->getName() << endl; 
                        }
                    }
                    if(!haveIngredient)
                    {
                        ostr << "  " << need;
                        if(need > 1)
                        {
                            ostr << " units of ";
                        }
                        else
                        {
                            ostr << " unit of ";
                        }

                        ostr << ingredientNeeded->getName() << endl;
                    }
                }
                ingredientNeeded++;
            }
            if(allTrue)
            {
                ostr << "Made " << name << endl;
                ingredientNeeded = ingredientsNeeded.begin();
                for(ingredientNeeded; ingredientNeeded != ingredientsNeeded.end(); ingredientNeeded++)
                {
                    ingredientHave = kitchen.getIngredientsReference().begin(); 
                    for(ingredientHave; ingredientHave!=kitchen.getIngredientsReference().end(); ingredientHave++)
                    {
                        if(ingredientNeeded->getName() == ingredientHave->getName())
                        {
                            kitchen.useIngredient(ingredientHave, (ingredientNeeded->getNumOf()));
                        }
                    }
                }
            }  
        }
    }

    if(!validRecipe)
    {
        ostr << "Don't know how to make " << name << endl; 
    }
}

list<Recipe> recipeSuggestions(ostream &ostr, const list<Recipe> &recipes, const Kitchen &kitchen, bool print) {

    list<Recipe> recipesThatCanBePrepared; 

    //for each recipe in recipes: 
    list<Recipe>::const_iterator recipe = recipes.begin();
    for(recipe; recipe != recipes.end(); recipe++)
    {
        //creates a boolean array of size recipe ingredients 
        //will change to true if the kitchen has enough of that ingredient 
        list<bool> haveEnoughOf;  

        const list<Ingredient> ingredientsNeeded = recipe->getIngredients();
        //create iterator to loop through all ingredients needed in the recipe
        list<Ingredient>::const_iterator ingredientNeeded = recipe->getIngredients().begin(); 
        //create iterator to loop through all ingredients in the kitchen
        list<Ingredient>::const_iterator ingredientHave; 
        for(ingredientNeeded; ingredientNeeded != recipe->getIngredients().end(); ingredientNeeded++)
        {
            bool haveIngredient = false;
            ingredientHave = kitchen.getIngredients().begin();
            for(ingredientHave; ingredientHave != kitchen.getIngredients().end(); ingredientHave++)
            {
                //if the ingredient needed is = ingredient have
                if(ingredientNeeded->getName() == ingredientHave->getName())
                {
                    haveIngredient = true;
                    if(ingredientNeeded->getNumOf() <= ingredientHave->getNumOf())
                    {
                        haveEnoughOf.push_back(true);
                        //have enough
                    }
                    else
                    {
                        haveEnoughOf.push_back(false);
                        //dont have enough
                    }
                }
            }

            if(!haveIngredient)
            {
                haveEnoughOf.push_back(false);
            }               
        }

        bool allTrue = true;
        for(list<bool>::iterator itr = haveEnoughOf.begin(); itr!=haveEnoughOf.end() and allTrue; itr++)
        {
            if(*itr == false)
            {
                allTrue = false;

            }

            ingredientNeeded++;
            ingredientHave++;
        }

        if(allTrue)
        {
            recipesThatCanBePrepared.push_back(*recipe); 
        }

    }

    if(recipesThatCanBePrepared.size() > 0 and print) 
    {
        recipesThatCanBePrepared.sort(sortAlphabetically);
        if(recipesThatCanBePrepared.size() > 1)
        {
            ostr << "Recipes that can be prepared:" << endl;
        }
        else
        {
            ostr << "Recipe that can be prepared:" << endl;
        }
        list<Recipe>::iterator itr = recipesThatCanBePrepared.begin(); 
        for(itr; itr != recipesThatCanBePrepared.end(); itr++)
        {
            ostr << "  " <<  itr->getName() << endl;
        }        
    }    
    else if(print)
    {
        ostr << "No recipes can be prepared" << endl;
    }   

    return recipesThatCanBePrepared;
}

list<Recipe> subset(list<Recipe>::iterator i1, list<Recipe>::iterator i2)
{
    list<Recipe>::iterator one;
    list<Recipe>::iterator two = i2;
    list<Recipe> subset;
    for(one=i1; one!=two; one++)
    {
            subset.push_back(*one);
    }
    return subset;
}

list< list<Recipe> > getAllPermutations(list<Recipe> recipes)
{
    list< list<Recipe> > powerSet;

    if(recipes.size() == 0)
    {
        return powerSet;
    }

    list<Recipe>::iterator itr; 
    list<Recipe>::iterator itr2; 
    
    //add each subset to permutations
    for(itr= recipes.begin(); itr != recipes.end(); itr++)
    {
        itr2 = recipes.end();
        for(itr2; itr2 != itr; itr2--)
        {
            if(itr!= itr2)
            {
                powerSet.push_back(subset(itr,itr2));
            }
        }
    } 

    list< list<Recipe> > intermediateSets;

    list< list<Recipe> >::iterator itr3;
    for(itr3 = powerSet.begin(); itr3!= powerSet.end(); itr3++)
    { 
        list<Recipe>::iterator mid;
        for(mid= itr3->begin(); mid != itr3->end(); mid++)
        { 
            list<Recipe> temp;
            for(itr2= itr3->begin(); itr2!= itr3->end(); itr2++)
            {
                if((itr2->getName()) != (mid->getName()))
                {
                     temp.push_back(*itr2);
                }
            }
            if(temp.size() != 0) 
            {
                intermediateSets.push_back(temp);
            }
        }

    }

    list<Recipe> tempList;
    tempList.push_back(*(recipes.begin()));
    tempList.push_back(*(--recipes.end()));
    if(recipes.size() > 1) 
    {
        intermediateSets.push_back(tempList);
    }

    list< list<Recipe> >::iterator itr4;
    for(itr4 = intermediateSets.begin(); itr4 != intermediateSets.end(); itr4++)
    {

        if(find(powerSet.begin(), powerSet.end(), *itr4) == powerSet.end()) 
        {
            powerSet.push_back(*itr4);
        }
    }

    return powerSet;
}

//returns an integer with the "score" or number of ingredients used to create the given recipe combination
//returns a -1 if the dinner is not possible given the ingredients in the kitchen
int scoreDinner(const list<Recipe> &dinner, const list<Ingredient>& tKitchen)
{
    int score = 0;
    list<Recipe>::const_iterator recipe = dinner.begin();
    //for each reicpe in the dinner
    for(recipe; recipe != dinner.end(); recipe++)
    {
        list<Ingredient>::const_iterator ingredient = recipe->getIngredients().begin();
        //for each ingredient in the recipe
        for(ingredient; ingredient != recipe->getIngredients().end(); ingredient++)
        {
            list<Ingredient>::const_iterator ingredientHave = tKitchen.begin();
            for(ingredientHave; ingredientHave != tKitchen.end(); ingredientHave++)
            {
                //if the kitchen has enough supplies add the amount of ingredients used to the score
                if(ingredientHave->getName() == ingredient->getName() and ingredientHave->getNumOf() >= 0)
                {
                    score+= ingredient->getNumOf();
                } 
                //if the kitchen does not have enough supplies return -1
                else if(ingredientHave->getName() == ingredient->getName())
                {
                    return -1;
                }
            }
        }
    }

    return score;
}

void use(const  list<Recipe> & possibleDinner, list<Ingredient>& tKitchen)
{

    //itr to go through each recipe in dinner
    list<Recipe>::const_iterator recipe;
    //itr to go through each ingredient in each recipe
    list<Ingredient>::const_iterator ingredientNeeded;
    //itr to go through ingredients stored in kitchen
    list<Ingredient>::iterator ingredientHave; 


        recipe = possibleDinner.begin();
        //for each recipe in possible recipes
        for(recipe; recipe != possibleDinner.end(); recipe++)
        {
            //for each ingredient in the recipe and ingredient in the kitchen
            list<Ingredient> rs = recipe->getIngredients();
            ingredientNeeded = rs.begin();
            for(ingredientNeeded; ingredientNeeded!= rs.end(); ingredientNeeded++)
            {  
                bool haveIngredient = false; 
                ingredientHave = tKitchen.begin();
                for(ingredientHave; ingredientHave!= tKitchen.end() and !haveIngredient; ingredientHave++)   
                {
                    if(ingredientNeeded->getName() == ingredientHave->getName())
                    {
                        haveIngredient=true;
                        ingredientHave->useIngredient(ingredientNeeded->getNumOf());
                    }
                } 
                if(!haveIngredient)
                {
                   Ingredient temp(ingredientNeeded->getName(), -1);
                   tKitchen.push_back(temp); 
                }
            }
        }
}

void suggestDinner(ostream &ostr, const list<Recipe> &recipes, const Kitchen &kitchen) {

    //create a new copy of tempKitchen to modify later
    list<Ingredient> tempKitchen = kitchen.getIngredients();
    //find all recipes that can be made (individually)    
    list<Recipe> possibleRecipes = recipeSuggestions(ostr,recipes,kitchen,false);
    //find all possible combinations of recipes
    list< list<Recipe> > dinners = getAllPermutations(possibleRecipes);
  /*  
    //code to test list of recipes that can be made (individually)
    for(list<Recipe>::iterator i = possibleRecipes.begin(); i!= possibleRecipes.end(); i++)
    {
        cout << i->getName() << "  ,  ";
    }
    cout << endl;
*/
/*    
    // test permutations code
    //for each recipe in dinner in dinners, print the recipe name
    for(list< list<Recipe> >::iterator d=dinners.begin(); d != dinners.end(); d++)
    {
    for(list<Recipe>::iterator r=d->begin(); r != d->end(); r++)
    {
    cout << r->getName() << " ";
    }
    cout << endl;
    }
*/

    //"use" ingredients in tempKitchen
     
    /*
  //code to test using ingredients 
    for(list<Ingredient>::iterator ingr = tempKitchen.begin(); ingr != tempKitchen.end(); ingr++)
    {
        cout << *ingr << endl;
    }
*/
    list<Ingredient>::iterator ingredientHave = tempKitchen.begin();
    list<int> dinnerScores;
    list< list<Recipe> >::iterator dinnerCombination = dinners.begin(); 
    //score each dinner combination
    for(dinnerCombination; dinnerCombination != dinners.end(); dinnerCombination++)
    { 
        use(*dinnerCombination, tempKitchen);
        dinnerScores.push_back(scoreDinner((*dinnerCombination), tempKitchen));
        tempKitchen = kitchen.getIngredients();
    }

    //find the maxScore and the dinner that uses the most ingredients
    int maxScore = -1;
    list<int>::iterator itr = dinnerScores.begin();
    dinnerCombination = dinners.begin();
    list<Recipe> maxIngredientDinner = *dinnerCombination;
    for(itr; itr != dinnerScores.end(); itr++)
    {
        if(*itr > maxScore)
        {
            maxScore = *itr;
            maxIngredientDinner = *dinnerCombination;
        } 
        dinnerCombination++;
    } 

    /*    
    //testing maxIngredientDinner
    cout << "OPTIMAL DINNER" << endl;
    for(list<Recipe>::iterator itr2= maxIngredientDinner.begin(); itr2!= maxIngredientDinner.end(); itr2++)
    {
    cout << itr2->getName() << endl;
    }

    cout << endl;
    cout << endl;
    */     

    if(maxIngredientDinner.size() > 0)
    {
        ostr << "Menu suggestion for dinner:" << endl; 
        list<Recipe>::iterator outputItr = maxIngredientDinner.begin(); 
        ostr << "  ";
        //temp that is one "ahead" of outputItr
        //used to stop at second to last element in list
        //this is needed to print in format: "A, B, C, and D"
        list<Recipe>::iterator temp = outputItr;
        temp++;
        for(outputItr; temp != maxIngredientDinner.end(); outputItr++)
        {
            ostr << outputItr->getName() << ", "; 
            temp++;
        }

        if(maxIngredientDinner.size() > 1)
        {
            ostr << "and ";
        }
        
        ostr << outputItr->getName() << endl;
    }
    else
    {
        ostr << "Nothing can be made for dinner" << endl;
    }
}
