#include <string>
#include "/home/elizabethdinella/test-files/dataStructures/hw2/Player.h"

using namespace std;

Player::Player(string & _name)
{
	fullName = _name;
	int spaceIndex = fullName.find(" ");
	lastName = fullName.substr(spaceIndex+1);	
	firstName = fullName.substr(0, spaceIndex);
	matchWins = 0;
	matchLosses = 0;
	gameWins = 0;
	gameLosses = 0;
	matchPercentage = 0;
	gamePercentage = 0;
	notAveragePoints = 0;
	numOfTies = 0;
}

void Player::addGameWins(int numToAdd)
{
	gameWins += numToAdd;
}

void Player::addGameLosses(int numToadd)
{
	gameLosses += numToadd;
}

bool operator==(const Player& first, const Player& second)
{
	return first.getName() == second.getName();
}

void Player::addNotAveragePoints(int numToadd)
{
	notAveragePoints += numToadd;
}

void Player::calculateMatchPercentage() 
{
	matchPercentage = float(matchWins)/float((matchLosses + matchWins));
}

bool greater_matchPercentage(const Player& first, const Player& second)
{	
	if(first.getMatchPercentage() > second.getMatchPercentage())
	{
		return true; 
	}
	else if(first.getMatchPercentage() < second.getMatchPercentage())
	{
		return false;
	}
	else
	{
		if(first.getLastName() < second.getLastName())
		{
			return true; 
		}
		else if(second.getLastName() < first.getLastName())
		{
			return false;
		}
		else
		{
			return first.getFirstName() < second.getFirstName();
		}	 
	}
}

void Player::calculateGamePercentage() 
{
	gamePercentage = float(gameWins)/float((gameLosses + gameWins));
}

bool greater_gamePercentage(const Player& first, const Player& second)
{	
	if(first.getGamePercentage() > second.getGamePercentage())
	{
		return true;
	}
	else if(first.getGamePercentage() < second.getGamePercentage())
	{
		return false;
	}
	else
	{
		if(first.getLastName() < second.getLastName())
		{
			return true; 
		}
		else if(second.getLastName() < first.getLastName())
		{
			return false;
		}
		else
		{
			return first.getFirstName() < second.getFirstName();
		}	 
	}
}
bool greater_numTies(const Player& first, const Player& second)
{
	if(first.getNumTies() > second.getNumTies())
	{
		return true; 
	}
	else if(first.getNumTies() < second.getNumTies())
	{
		return false;
	}
	else
	{
		if(first.getLastName() < second.getLastName())
		{
			return true; 
		}
		else if(second.getLastName() < first.getLastName())
		{
			return false;
		}
		else
		{
			return first.getFirstName() < second.getFirstName();
		}	 
	}
}

bool less_numNotAveragePoints(const Player& first, const Player& second)
{
	if(first.getNotAveragePoints() < second.getNotAveragePoints())
	{
		return true; 
	}
	else if(first.getNotAveragePoints() > second.getNotAveragePoints())
	{
		return false;
	}
	else
	{
		if(first.getLastName() < second.getLastName())
		{
			return true; 
		}
		else if(second.getLastName() < first.getLastName())
		{
			return false;
		}
		else
		{
			return first.getFirstName() < second.getFirstName();
		}	 
	}
}
