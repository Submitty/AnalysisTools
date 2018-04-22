#include <cassert>
#include <cstdlib>
#include <cstdio>

#include "board.h"


// this global variable is set in main.cpp and is adjustable from the command line
// (you are not allowed to make your own global variables)
extern int GLOBAL_TILE_SIZE;


// ==========================================================================
// CONSTRUCTOR
Board::Board(int i, int j) {
    board = std::vector<std::vector<Tile*> >
        ( (unsigned int)i,
          std::vector<Tile*>((unsigned int)j,NULL) );
}


// ==========================================================================
// ACCESSORS
Tile* Board::getTile(int i, int j) const {
    assert (i >= 0 && i < numRows());
    assert (j >= 0 && j < numColumns());
    return board[i][j];
}


// ==========================================================================
// MODIFIERS
void Board::setTile(int i, int j, Tile* t) {
    assert (i >= 0 && i < numRows());
    assert (j >= 0 && j < numColumns());
    assert (t != NULL);
    assert (board[i][j] == NULL);
    board[i][j] = t;
}

void Board::clearBoard()
{
    for(int i=0; i<board.size(); i++)
    {
        for(int j=0; j<board[i].size(); j++)
        {
            board[i][j] = NULL;
        }
    }
}

bool Board::isEmpty() const
{
    for(int i=0; i<board.size(); i++)
    {
        for(int j=0; j<board[i].size(); j++)
        {
            if(board[i][j] != NULL)
            {
                return false;
            }
        }
    }
    return true;
} 


// ==========================================================================
// PRINTING
void Board::Print() const {
    for (int b = 0; b < numRows(); b++) {
        for (int i = 0; i < GLOBAL_TILE_SIZE; i++) {
            for (int j = 0; j < numColumns(); j++) {
                if (board[b][j] != NULL) {
                    board[b][j]->printRow(std::cout,i);
                } else {
                    std::cout << std::string(GLOBAL_TILE_SIZE,' ');
                }
            }
            std::cout << "\n";
        }
    }
    fflush(stdout);
}

// ==========================================================================
bool operator==(const Board& b1, const Board& b2)
{
    for(int i=0; i<b1.numRows(); i++)
    {
        for(int j=0; j<b1.numColumns(); j++)
        {
            if(b1.getTile(i,j) != NULL and b2.getTile(i,j) != NULL and *(b1.getTile(i,j)) != *(b2.getTile(i,j)))
            {
                return false;
            }
            else if(b1.getTile(i,j) != NULL and b2.getTile(i,j) == NULL)
            {
                return false;
            }
            else if(b1.getTile(i,j) == NULL and b2.getTile(i,j) != NULL)
            {   
                return false;
            }       
        }
    }
    
    return true;
}
bool operator!=(const Board& b1, const Board& b2)
{
    return not operator==(b1,b2);
}
