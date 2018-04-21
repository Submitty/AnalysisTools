#ifndef __BOARD_H__
#define __BOARD_H__

#include <vector>
#include "/mnt/c/Users/Elizabeth/Documents/spring2017/commonASTs/cpp-test-files/dataStructures/hw6/tile.h"


// This class stores a grid of Tile pointers, which are NULL if the
// grid location does not (yet) contain a tile

class Board {
public:

  // CONSTRUCTOR
  // takes in the dimensions (height & width) of the board
  Board(int i, int j);

  // ACCESSORS
  int numRows() const { return board.size(); }
  int numColumns() const { return board[0].size(); }
  Tile* getTile(int i, int j) const;
  const std::vector<std::vector<Tile*> > getBoard() const{ return board; };  

  // MODIFIERS
  void setTile(int i, int j, Tile* t);
  void clearBoard();  

  // FOR PRINTING
  void Print() const;

  //OTHER
  bool isEmpty() const;

private:

  // REPRESENTATION
  std::vector<std::vector<Tile*> > board;
};

bool operator==(const Board& b1, const Board& b2);
bool operator!=(const Board& b1, const Board& b2);


#endif
