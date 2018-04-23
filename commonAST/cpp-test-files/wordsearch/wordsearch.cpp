// Program: word_search
// Author:  Chuck Stewart
//
// Purpose: A program to solve the word search problem where the
// letters in the word do not need to appear along a straight
// line. Instead, they can twist and turn. The only requirements are
// two-fold: the consecutive letters must be "8-connected" to each
// other (meaning that the locations must touch along an edge or at a
// corner), and no location may be used more than once.
//
// The real issue is how to search for the letters and then record
// locations as we search. This is most easily done with a recursive
// function. This function will be written during lecture.
//
// The input is from an input file. The grid is a series of strings,
// ended by a string that begins with '-'. Each subsequent string in
// the file is used to search the input.

#include <algorithm>
#include <fstream>
#include <iostream>
#include <string>
#include <vector>
#include <cassert>
using namespace std;

// Simple class to record the grid location. 
class loc {
public:
  loc(int r=0, int c=0) : row(r), col(c) {}
  int row, col;
};

bool operator== (const loc& lhs, const loc& rhs) { 
  return lhs.row == rhs.row && lhs.col == rhs.col; 
}

//  Prototype for the main search function
bool search_from_loc(loc position, const vector<string>& board, const string& word, vector<loc>& path);


// Read in the letter grid, the words to search and print the results
int main(int argc, char* argv[]) {
  if (argc != 2) {
    cerr << "Usage: " << argv[0] << " grid-file\n";
    return 1;
  }
  ifstream istr(argv[1]);
  if (!istr) {
    cerr << "Couldn't open " << argv[1] << '\n';
    return 1;
  }

  vector<string> board;   
  string word;
  vector<loc> path;           //  The sequence of locations...
  string line;
  
  //  Input of grid from a file.  Stops when character '-' is reached.
  while ((istr  >> line) && line[0] != '-')
    board.push_back(line);
  
  while (istr >> word) {
    bool found = false;
    vector<loc> path;  //  Path of locations in finding the word
    
    //  Check all grid locations.  For any that have the first
    //  letter of the word, call the function search_from_loc
    //  to check if the rest of the word is there.
    
    for (unsigned int r=0; r<board.size() && !found; ++r)
      for (unsigned int c=0; c<board[r].size() && !found; ++c) {
        if (board[r][c] == word[0] && 
            search_from_loc(loc(r,c), board, word, path))
          found = true;
      }

    //  Output results
    cout << "\n** " << word << " ** ";
    if (found) {
      cout << "was found.  The path is \n";
      for(unsigned int i=0; i<path.size(); ++i)
        cout << "  " << word[i] << ":  (" << path[i].row << "," << path[i].col << ")\n";
    } else {
      cout << " was not found\n";
    }
  }
  return 0;
}

// helper function to check if a position has already been used for this word
bool on_path(loc position, vector<loc> const& path) {
  for (unsigned int i=0; i<path.size(); ++i)
    if (position == path[i]) return true;
  return false;
}

// This recursive function will search for the remainder of the word
// starting from the current location.  Returns true if the remainder
// of the word is found, returns false if we reach a dead end.  If the
// word is found, the sequence of locations that trace out the word
// are stored in path.
bool search_from_loc(loc position, //  current position
                     const vector<string>& board,  
                     const string& word,  
                     vector<loc>& path)  //  path up to the current pos           
{

  // DOUBLE CHECKING OUR LOGIC: the letter at the current board
  // position should equal the next letter in the word
  assert (board[position.row][position.col] == word[path.size()]);

  // start by adding this location to the path
  path.push_back(position);

  // BASE CASE: if the path length matches the word length, we're done!
  if (path.size() == word.size()) return true;

  //  search all the places you can get to in one step
  for (int i = position.row-1; i <= position.row+1; i++) {
    for (int j = position.col-1; j <= position.col+1; j++) {
      
      // don't walk off the board though!
      if (i < 0 || i >= board.size()) continue;
      if (j < 0 || j >= board[0].size()) continue;
      // don't consider locations already on our path
      if (on_path(loc(i,j),path)) continue;

      // if this letter matches, recurse!
      if (word[path.size()] == board[i][j]) {
        // if we find the remaining substring, we're done!
        if (search_from_loc (loc(i,j),board,word,path))
          return true;
      }
    }
  }

  // At this point we know we have failed to find a path from the
  // current loc.  Remove the current loc from the path and end this
  // recursive call with a failure so that the search can move in a
  // different direction.
  path.pop_back();
  return false;
}

