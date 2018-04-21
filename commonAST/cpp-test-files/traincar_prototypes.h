// =======================================================================
//
// IMPORTANT NOTE: You should edit this file
//                 This file is #include'd from traincar.h and main.cpp
//
// =======================================================================
//
// There are a number of additional functions used in main.cpp that
// you need to declare and implement.  Study main.cpp and the provided
// output to determine the arguments, return type, and overall
// behavior.
//
// Add all required additional function prototypes here
// (you may also add your own helper function prototypes here too)
//
// Implement these functions in "traincar.cpp"
//

#ifndef __traincar__prototypes__
#define __traincar__prototypes_

#include <vector>

using namespace std;

void PushBack(TrainCar*& head, TrainCar* carToAdd); 
void DeleteAllCars(TrainCar* head);
float speed(int numEngines, int totalWeight);
vector<TrainCar*> ShipFreight(TrainCar*& enginesHead, TrainCar*& frieghtHead, int minSpeed, int maxCarsPerTrain);
int getTotalWeight(TrainCar* freightHead);
TrainCar* popFront(TrainCar*& head);
void insert(TrainCar*& headForInsert, TrainCar* carToInsert, int position);
void TotalWeightAndCountCars(TrainCar* head, int& totalWeight, int& numEngines, int& numFreight , int& numPassenger , int& numDining , int& numSleeping);
int getNumEngines(TrainCar* head);
float CalculateSpeed(TrainCar* head);
float AverageDistanceToDiningCar(TrainCar* head);
int ClosestEngineToSleeperCar(TrainCar* head);
void Separate(TrainCar*& original, TrainCar*& new1, TrainCar*& new2);
void getFreightTrain(TrainCar*& eHead, TrainCar*& fHead, int minSpeed, int maxCars, vector<TrainCar*>& v);

#endif

