// =======================================================================
//
/// IMPORTANT NOTE: You should edit this file
//
// =======================================================================

#include <iostream>
#include <iomanip>
#include <string>
#include <cassert>
#include <cmath>
#include <cstdlib>
#include <vector>
#include "/home/elizabethdinella/test-files/dataStructures/hw5/traincar.h"

using namespace std;

// =======================================================================
// =======================================================================
//
//  HERE'S WHERE YOU SHOULD IMPLEMENT THE FUNCTIONS YOU PROTOTYPED IN
//  THE "traincar_prototypes.h" FILE.
//
void PushBack(TrainCar*& head, TrainCar* carToAdd)
{
    if(head == NULL)
    {
        //empty chain   
        head = carToAdd;
    }
    else
    {
        TrainCar* temp = head; 
        //start at head and walk through to the end
        while(temp->next != NULL)
        {
            temp = temp->next;
        }

        temp->next = carToAdd;
        carToAdd->prev = temp; 

    }
} 

void DeleteAllCars(TrainCar* head)
{
    if(head == NULL)
    {
        return;
    }
    else if(head->next == NULL)
    {
        //only node in chain
        delete head;
        head = NULL;
    }
    else
    {
        TrainCar* temp = head;
        TrainCar* previous;
        while(temp->next != NULL)
        {
            previous = temp;
            temp = temp->next;
            delete previous;
            previous = NULL;
        }

        delete temp;
        temp = NULL;
    }

}

float CalculateSpeed(TrainCar* head)
{
    return speed(getNumEngines(head), getTotalWeight(head));
}

float speed(int numEngines, int totalWeight)
{
    double totalHorsepower = 3000 * numEngines;
    double numerator = totalHorsepower*19800;
    double denomenator = 2112*totalWeight;

    return numerator/denomenator;
}

int getSize(TrainCar* head)
{
    int numCars = 0;
    if(head == NULL)
    {
        return 0;
    }
    else
    {
        TrainCar* temp = head;
        while(temp->next != NULL)
        {
            numCars+=1;
            temp = temp->next;
        }
    }
    return numCars;
}

int getTotalWeight(TrainCar* freightHead)
{
    int totalWeight = 0;
    TrainCar* temp = freightHead;

    if(getSize(freightHead) == 0)
    {
        return temp->getWeight();
    }


    for(int i=0; i<=getSize(freightHead); i++)
    {
        totalWeight += temp->getWeight();
        temp = temp->next;
    }
    return totalWeight;
}

TrainCar* popFront(TrainCar*& head)
{
    if(head != NULL)
    {
        TrainCar* temp = head; 

        temp = temp->next;
        if(temp != NULL)
        {
            temp->prev = NULL;   
        }

        TrainCar* carToReturn = head; 
        head = temp;

        carToReturn->prev = NULL;
        carToReturn->next = NULL;
        return carToReturn; 
    }
    else
    {
        //head is null
        return NULL;    
    }
}

TrainCar* remove(TrainCar*& head, int position)
{
    if(position > getSize(head)+1)
    {
        return NULL;
    }

    TrainCar* before = head;
    for(int i=0; i<position-1; i++)
    {
        before = before->next; 
    }

    TrainCar* atPosition = before->next;
    
    //if the carToRemove is not the last element in the list
    if(atPosition->next != NULL)
    {
        TrainCar* after = atPosition->next;
        after->prev = before;
        before->next = after;
    }  
    else
    {
        
        before->next = NULL;
    }

    atPosition->prev = NULL;
    atPosition->next = NULL;

    return atPosition;
}

//puts traincar in position. Goes before the car originally in position
void insert(TrainCar*& head, TrainCar* carToInsert, int position)
{    
    //move to position in chain
    TrainCar* atPosition = head;
    for(int i=0; i<position; i++)
    {
        atPosition = atPosition->next;
    } 

    TrainCar* beforePosition = NULL;
    if(atPosition != NULL)
    {
        beforePosition = atPosition->prev;
    }

    if(beforePosition != NULL)
    {
        beforePosition->next = carToInsert;
    }

    if(atPosition != NULL)
    {
        atPosition->prev = carToInsert;
    }

    if(carToInsert != NULL)
    {
        carToInsert->next = atPosition;
        carToInsert->prev = beforePosition;
    }

    if(position == 0)
    {
        head = carToInsert;
    }

    head->prev = NULL;
}

vector<TrainCar*> ShipFreight(TrainCar*& enginesHead, TrainCar*& freightHead, int minSpeed, int maxCarsPerTrain) 
{

    vector<TrainCar*> trains;
    getFreightTrain(enginesHead, freightHead, minSpeed, maxCarsPerTrain,  trains); 
    return trains;
}

//recursive function to help ship freight
void getFreightTrain(TrainCar*& eHead, TrainCar*& fHead, int minSpeed, int maxCars, vector<TrainCar*>& v)
{
    //base case- if there are no more engines or freight cars left
    if(eHead == NULL or fHead == NULL)
    {
        return;
    }

    TrainCar* head = popFront(eHead);

    while((eHead != NULL and fHead !=NULL) and (getSize(head) < maxCars -1))
    {   
        //if the current configuration can handle another freight car
        if(speed(getNumEngines(head), getTotalWeight(head) + fHead->getWeight()) >= minSpeed)
        {
            PushBack(head, popFront(fHead));
        }
        else
        {
            insert(head, popFront(eHead), 1);
        }
    }

    v.push_back(head);
    if(eHead !=  NULL and fHead != NULL)
    {
        //recurse
        getFreightTrain(eHead, fHead, minSpeed, maxCars, v);
    }
}


int getNumEngines(TrainCar* head)
{
    int numEngines = 0;
    TrainCar* temp = head;
    for(int i=0; i<=getSize(head); i++)
    {
        if(temp->isEngine())
        {
            numEngines++;
        } 
        temp = temp->next;
    }
    return numEngines;
}

int getNumFreight(TrainCar* head)
{
    int numFreight = 0;
    TrainCar* temp = head;
    for(int i=0; i<=getSize(head); i++)
    {
        if(temp->isFreightCar())
        {
            numFreight++;
        }
        temp = temp->next; 
    }
    return numFreight;
}

int getNumPassenger(TrainCar* head)
{
    int numPassenger = 0;
    TrainCar* temp = head;
    for(int i=0; i<=getSize(head); i++)
    {
        if(temp->isPassengerCar())
        {
            numPassenger++;
        }
        temp = temp->next; 
    }
    return numPassenger;
}

int getNumDining(TrainCar* head)
{
    int numDining = 0;
    TrainCar* temp = head;
    for(int i=0; i<=getSize(head); i++)
    {
        if(temp->isDiningCar())
        {
            numDining++;
        }
        temp = temp->next; 
    }
    return numDining;
}

int getNumSleeping(TrainCar* head)
{
    int numSleeping = 0;
    TrainCar* temp = head;
    for(int i=0; i<=getSize(head); i++)
    {
        if(temp->isSleepingCar())
        {
            numSleeping++;
        } 
        temp = temp->next;
    }
    return numSleeping;
}

void TotalWeightAndCountCars(TrainCar* head, int& totalWeight, int& numEngines, int& numFreight , int& numPassenger , int& numDining , int& numSleeping)
{
    totalWeight = getTotalWeight(head);
    numEngines = getNumEngines(head);
    numFreight = getNumFreight(head);  
    numPassenger = getNumPassenger(head);
    numDining = getNumDining(head);
    numSleeping = getNumSleeping(head);     
}

float AverageDistanceToDiningCar(TrainCar* head)
{
    TrainCar* temp = head;
    int sum = 0;
    int numPassenger = getNumPassenger(head);
    for(int i=0; i<=getSize(head); i++)
    {
        if(temp->isPassengerCar())
        {

            TrainCar* original = temp;
            bool foundDining = false; 
            int lDistance = 0;
            int rDistance = 0;
            //check for dining to the left
            while(temp->prev != NULL and not temp->prev->isEngine() and not temp->prev->isFreightCar())
            {
                lDistance++;
                temp = temp->prev; 
                if(temp->isDiningCar())
                {
                    foundDining = true;
                    break;
                }
            }

            if(!foundDining)
            {
                lDistance = 0;
            }

            foundDining = false;
            temp = original;
            //check for dining to the right 
            while(temp->next != NULL and not temp->next->isEngine() and not temp->next->isFreightCar())
            {
                rDistance++;
                temp = temp->next; 
                if(temp->isDiningCar())
                {
                    foundDining = true;
                    break;
                }
            }

            if(!foundDining)
            {
                rDistance = 0;
            }


            if(lDistance ==0 and rDistance ==0)
            {
                return -1;
            }   
            if(lDistance == 0 and rDistance !=0)
            {
                sum += rDistance;
            }
            else if(rDistance == 0 and lDistance !=0)
            {
                sum += lDistance;
            }
            else if(lDistance < rDistance and lDistance != 0)
            {
                sum +=  lDistance;
            }
            else if(rDistance < lDistance and rDistance !=0)
            {
                sum +=  rDistance;
            }        


            temp = original;
        }

        temp = temp->next;
    }

    return float(sum)/float(numPassenger); 
}

int ClosestEngineToSleeperCar(TrainCar* head)
{
    int closestDistance = getSize(head);
    TrainCar* temp = head;
    for(int i=0; i<getSize(head); i++)
    {
        if(temp->isSleepingCar())
        {
            TrainCar* original = temp;
            bool foundEngine = false; 
            int lDistance = 0;
            int rDistance = 0;
            //check for engine to the left
            while(temp->prev != NULL) 
            {
                lDistance++;
                temp = temp->prev; 
                if(temp->isEngine())
                {
                    foundEngine= true;
                    break;
                }
            }

            if(!foundEngine)
            {
                lDistance = 0;
            }

            foundEngine = false;
            temp = original;
            //check for engine to the right 
            while(temp->next != NULL) 
            {
                rDistance++;
                temp = temp->next; 
                if(temp->isEngine())
                {
                    foundEngine = true;
                    break;
                }
            }

            if(!foundEngine)
            {
                rDistance = 0;
            }

            int closestDistanceLocal = getSize(head);
            if(rDistance != 0 and rDistance < closestDistanceLocal)
            {
                closestDistanceLocal = rDistance;
            } 
            if(lDistance != 0 and lDistance < closestDistanceLocal)
            {
                closestDistanceLocal = lDistance;
            }

            if(closestDistanceLocal < closestDistance)
            {
                closestDistance = closestDistanceLocal;
            }
            temp = original;
        }

        temp = temp->next;
    }

    return closestDistance; 
}

void Separate(TrainCar*& original, TrainCar*& new1, TrainCar*& new2)
{
    TrainCar* temp = original;
    //split the train in half
    for(int i=0; i<getSize(original)/2; i++)
    {
        temp = temp->next;
    }
    
    new2 = temp->next;
    new2->prev = NULL;
    
    new1 = original;
    temp->next = NULL;
    
    //tail pointers
    TrainCar* new1End = temp;
    TrainCar* new2End = new2;
    while(new2End->next != NULL)
    {
        new2End = new2End->next;
    }
    
    original = NULL;

    if(getNumEngines(new1) == 0 or getNumEngines(new2) == 0)
    {
        //if new1 has no engines and new2 has more than one engine
        if(getNumEngines(new1) == 0 and getNumEngines(new2) > 1)
        {
            int count = 0;
               
            TrainCar* findEngine = new2;
            //find the first engines on new2
            while(!findEngine->isEngine())
            {
                findEngine = findEngine->next;
                count++;
            } 
        
            //remove it from new2 and put it on the back of new1
            findEngine = remove(new2, count); 
            PushBack(new1, findEngine);
        }
        //if new2 has no engines and new1 has more than one engine
        else if(getNumEngines(new2) == 0 and getNumEngines(new1) > 1)
        {
            int count = 0;
                
            TrainCar* findEngine = new1End;
            //find the first engine from the back of new1
            while(!findEngine->isEngine()) 
            {
                findEngine = findEngine->prev;
                count++;
            }

            count = getSize(new1) - count ;
       
            //remove it from new1 and put in on the front of new2
            findEngine = remove(new1, count); 
            insert(new2, findEngine, 0);
        }
    }

   //if there is a significant difference between the speeds of the two cars
   if(CalculateSpeed(new1) + 15 < CalculateSpeed(new2) or CalculateSpeed(new2) + 15 < CalculateSpeed(new1))
    {
        int new1CarWeight = getTotalWeight(new1) - (150 * getNumEngines(new1));
        int new2CarWeight = getTotalWeight(new2) - (150 * getNumEngines(new2));

        if(new1CarWeight < new2CarWeight) 
        {
            
            //take a car from train2 and put it on train1
            int count = 0;

             //from front
            TrainCar* carToRemove = new2;
            while(carToRemove->isEngine() and carToRemove != NULL)
            {
                carToRemove = carToRemove->next;
                count++;
            }
          
            carToRemove = remove(new2, count);
            PushBack(new1, carToRemove);
        }  
        else if( new2CarWeight < new1CarWeight) 
        {   
            
            //take a car from train1 and put it on train2
            int count = 0;
            
            //from back 
            TrainCar* carToRemove = new1End;
            while(carToRemove->isEngine() and carToRemove != NULL)
            {
                carToRemove = carToRemove->prev;
                count++;
            }

            count = getSize(new1) - count - 1;
            carToRemove = remove(new1, count);
            insert(new2, carToRemove, 0);
        }  
    }
}

