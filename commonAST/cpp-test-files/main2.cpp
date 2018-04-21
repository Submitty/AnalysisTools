#include <stdlib.h>
#include <iostream>
#include <vector>
#include <fstream>
#include <iomanip>
#include <algorithm>
#include "/home/elizabethdinella/test-files/dataStructures/hw2/Player.h"

using namespace std;

//takes an input file, parses text and returns a vector of Players
vector<Player> parse(string& inFilename)
{
	vector<Player> _players;

	ifstream inputFile(inFilename.c_str());

	if(!inputFile.good()){
	
		cerr << "Could not open " << inFilename << endl;
		exit(1);
	}	

	string line;
	while(getline(inputFile, line))
	{
		//find the index of the second space (the space after the last name)
		//cut the string to include only the name
		int indexFirst = line.find(" ");
		if(indexFirst > line.size())
		{
			cerr << "input file does not have data in correct format" << endl;
			exit(1);		
		}	
		int index = line.find(" ", indexFirst+1);	
		string winner = line.substr(0,index);	
		
		//checks if player already has a class
		bool alreadyExists = false;
		for(int i=0; i<_players.size(); i++)
		{
			if(_players[i] == winner)
			{
				_players[i].incrementMatchWins();		
			      	alreadyExists = true;
			}
		}
		
		//creates and adds the player to the vector if it does not already have a class
		if(!alreadyExists)
		{	
			Player p(winner);
			p.incrementMatchWins();
			_players.push_back(p);
		} 
		
		//cuts the string to remove the "winner d. "
		index = line.find("d.") ;
		index+= 3;
			
		line = line.substr(index);

		//find the index of the second space (the space after the last name)
		//cut the string to include only the name
		indexFirst = line.find(" ");		
		index = line.find(" ", indexFirst+1);	
		string loser = line.substr(0,index);	

		//fix because sample_scores has two spaces after d but the other files only have one
		if(line[1] ==  ' ')
		{
			line = line.substr(1);
		}
		line = line.substr(index+1);
		
		//checks if player already has a class
		alreadyExists = false;
		for(int i=0; i<_players.size(); i++)
		{
			if(_players[i] == loser)
			{
				_players[i].incrementMatchLosses();		
			      	alreadyExists = true;
			}
		}
		
		//creates and adds the player to the vector if it does not already have a class
		if(!alreadyExists)
		{	
			Player p(loser);
			p.incrementMatchLosses();
			_players.push_back(p);
		}
		
		//add a space to the end of the line so the newline character does not break the program
		line = line + " ";	
		while(line.find("-") < line.size())
		{
			bool tie = false;
			int i = line.find("-");
			int games_won = atoi(line.substr(0,i).c_str());
			int end = line.find(" ");
		
			//check so the last statistic without the space and the end does not break the program
			if(! end < line.size())
			{
				end = line.size();
			}
			int games_lost = atoi(line.substr(i+1, end).c_str());
			int total = games_lost + games_won;
			if(total > 12)
			{
				tie = true;	
			}
			
			for(int i=0; i<_players.size(); i++)
			{
				if(_players[i] == winner)
				{
					_players[i].addGameWins(games_won);
					_players[i].addGameLosses(games_lost);
					if(tie)
					{
						_players[i].incrementTies();
					}
				}
				else if(_players[i] == loser)
				{
					_players[i].addGameWins(games_lost);
					_players[i].addGameLosses(games_won);
					if(tie)
					{
						_players[i].incrementTies();
					}
				}
			}
		
			//cut the processed statistics off the beginning of the line
			line = line.substr(i+3);	
		}
	}	
	return _players;
}

void writeMatchStatistics(const vector<Player> &_players, string & outFilename)
{
	ofstream outputFile(outFilename.c_str());
	
	if(!outputFile.good()){
		
		cerr << "Could not open " << outFilename << endl;
	}

	int longestNameLength = 0;
	for(int i=0; i<_players.size(); i++)
  	{
		if((_players[i].getName()).size() > longestNameLength)
		{
			longestNameLength = (_players[i].getName()).size();
		}
		
	}	

	outputFile << "MATCH STATISTICS" << endl;
	outputFile << left << setw(longestNameLength) << "Player";
	outputFile << right << setw(5) << "W" << setw(5) << "L" << right << setw(13) << "percentage" << endl;

	for(int i=0; i<_players.size(); i++)
	{
		outputFile << left << setw(longestNameLength) << _players[i].getName();
		outputFile << right << setw(5) <<  _players[i].getMatchWins() << setw(5) <<  _players[i].getMatchLosses();
		outputFile << right << setw(13) << setprecision(3) << fixed  << _players[i].getMatchPercentage() << endl;

	}	
	outputFile  << endl;
}

void writeGameStatistics(const vector<Player> &_players, string & outFilename)
{
	ofstream outputFile(outFilename.c_str(), ios::app);
	
	if(!outputFile.good()){
		
		cerr << "Could not open " << outFilename << endl;
	}

	int longestNameLength = 0;
	for(int i=0; i<_players.size(); i++)
  	{
		if((_players[i].getName()).size() > longestNameLength)
		{
			longestNameLength = (_players[i].getName()).size();
		}
		
	}	

	outputFile << "GAME STATISTICS" << endl; 
	outputFile << left << setw(longestNameLength) << "Player";
	outputFile << right << setw(5) << "W" << setw(5) << "L" << right << setw(13) << "percentage" << endl;

	for(int i=0; i<_players.size(); i++)
	{
		outputFile << left << setw(longestNameLength) << _players[i].getName();
		outputFile << right << setw(5) <<  _players[i].getGameWins() << setw(5) << _players[i].getGameLosses();
		outputFile << right << setw(13) << setprecision(3) << fixed  << _players[i].getGamePercentage() << endl;

	}	
	outputFile  << endl;
}

void writeAveragenessStatistics(const vector<Player> &_players, string & outFilename)
{
	ofstream outputFile(outFilename.c_str(), ios::app);
	
	if(!outputFile.good()){
		
		cerr << "Could not open " << outFilename << endl;
	}

	int longestNameLength = 0;
	for(int i=0; i<_players.size(); i++)
  	{
		if((_players[i].getName()).size() > longestNameLength)
		{
			longestNameLength = (_players[i].getName()).size();
		}
		
	}	

	outputFile << "AVERAGENESS STATISTICS" << endl; 
	outputFile << left << setw(longestNameLength) << "Player";
	outputFile << right << setw(20) << "match percentage" << setw(20) << "game percentage" << right << setw(5) << "ties" << endl;

	for(int i=0; i<_players.size(); i++)
	{
		outputFile << left << setw(longestNameLength) << _players[i].getName();
		outputFile << right << setw(20) << setprecision(3) << fixed <<  _players[i].getMatchPercentage();
		outputFile << right << setw(20) << setprecision(3) << fixed <<  _players[i].getGamePercentage();
		outputFile << right << setw(5) << _players[i].getNumTies() << endl;
	}	
	outputFile  << endl;
}

int main(int argc, char* argv[]){

	if(argc !=  3){
		cerr << "Usage of " << argv[0] << ": " << "inputFile outputFile" << endl;
		return 1;
	}
	
	string inputFilename = argv[1];
	string outputFilename = argv[2];

	vector<Player> players;
	players = parse(inputFilename);

	//loops through players and calculate game percentage
	//this must be done because of the iterative nature of the numMatchWins and numMatchLosses variables
	for(int i=0; i<players.size(); i++)
	{
		players[i].calculateMatchPercentage();
		players[i].calculateGamePercentage();
	}
	
	sort(players.begin(), players.end(), greater_matchPercentage);
	writeMatchStatistics(players, outputFilename);	
	
	//adds not average points for how far the player is away from the median (see README)	
	int median = players.size()/2;
	for(int i=0; i<median; i++)
	{
		players[i].addNotAveragePoints(median - i);	
		players[(players.size()-1-i)].addNotAveragePoints(median-i);
	}

	sort(players.begin(), players.end(), greater_gamePercentage); 
	writeGameStatistics(players, outputFilename);

	//adds not average points for how far the player is away from the median (see README)
	for(int i=0; i<median; i++)
	{
		players[i].addNotAveragePoints(median - i);	
		players[(players.size()-1-i)].addNotAveragePoints(median-i);
	}

	sort(players.begin(), players.end(), greater_numTies);
	
	//adds not average points for how far the player is away from the meadian (see README)
	for(int i=0; i<median; i++)
	{
		players[i].addNotAveragePoints(median - i);	
		players[(players.size()-1-i)].addNotAveragePoints(median-i);
	}

	sort(players.begin(), players.end(), less_numNotAveragePoints); 
	writeAveragenessStatistics(players, outputFilename);
	
	return 0;
}
