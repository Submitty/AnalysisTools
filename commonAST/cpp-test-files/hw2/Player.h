#ifndef _player_h
#define _player_h

#include <string>

using namespace std;

class Player{

public:
	Player(string & _name);

	//ACCESSORS
	string getName() const { return fullName; }
	string getFirstName() const { return firstName; }
	string getLastName() const { return lastName; } 
	int getMatchWins() const { return matchWins; }
	int getMatchLosses() const { return matchLosses; }
	float getMatchPercentage() const { return matchPercentage; } 
	float getGamePercentage() const { return gamePercentage; }
	int getGameWins() const { return gameWins; }
	int getGameLosses() const { return gameLosses; }
	int getNumTies() const { return numOfTies; }
	int getNotAveragePoints() const { return notAveragePoints; }
			
	//MODIFIERS
	void incrementMatchWins() { matchWins++; }
	void incrementMatchLosses() { matchLosses++; }
	void incrementTies() { numOfTies++; }
	void addGameWins(int numToAdd);
	void addGameLosses(int numToAdd);
	void addNotAveragePoints(int numToAdd);

	void calculateGamePercentage();
	void calculateMatchPercentage(); 
			
private:
	int numOfTies;
	int matchWins;
	int matchLosses;
	int gameWins;
	int gameLosses;
	int notAveragePoints; //number calculated from percentage of game wins,  percentage of match wins, and number of ties (see README)
	string fullName;
	string firstName;
	string lastName;
	float matchPercentage;
	float gamePercentage;
	
};

/*bool operator<(const string& first, const string& second){
		return first < second;
};*/

bool operator==(const Player& first, const Player& second);
bool greater_matchPercentage(const Player& first, const Player& second);
bool greater_gamePercentage(const Player& first, const Player& second);
bool greater_numTies(const Player& first, const Player& second);
bool less_numNotAveragePoints(const Player& first, const Player& second);

#endif
