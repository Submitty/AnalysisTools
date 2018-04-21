#include <iostream>
#include <fstream>
#include <iomanip>
#include <cstdlib>
#include <string>
#include <vector>
#include <cassert>
#include <cmath>


#include "/home/elizabethdinella/test-files/dataStructures/hw6/MersenneTwister.h"



#include "/home/elizabethdinella/test-files/dataStructures/hw6/Node.h"
#include "/home/elizabethdinella/test-files/dataStructures/hw6/tile.h"
#include "/home/elizabethdinella/test-files/dataStructures/hw6/location.h"

#include "/home/elizabethdinella/test-files/dataStructures/hw6/board.h"

using namespace std;

// this global variable is set in main.cpp and is adjustable from the command line
// (you are not allowed to make your own global variables)
int GLOBAL_TILE_SIZE = 11;

// ==========================================================================
// Helper function that is called when an error in the command line
// arguments is detected.
void usage(int argc, char *argv[]) {
    std::cerr << "USAGE: " << std::endl;
    std::cerr << "  " << argv[0] << " <filename>  -board_dimensions <h> <w>" << std::endl;
    std::cerr << "  " << argv[0] << " <filename>  -board_dimensions <h> <w>  -all_solutions" << std::endl;
    std::cerr << "  " << argv[0] << " <filename>  -board_dimensions <h> <w>  -allow_rotations" << std::endl;
    std::cerr << "  " << argv[0] << " <filename>  -all_solutions  -allow_rotations" << std::endl;
    std::cerr << "  " << argv[0] << " <filename>  -tile_size <odd # >= 11>" << std::endl;
    exit(1);
}

// ==========================================================================
void HandleCommandLineArguments(int argc, char *argv[], std::string &filename, 
        int &rows, int &columns, bool &all_solutions, bool &allow_rotations) {

    // must at least put the filename on the command line
    if (argc < 2) {
        usage(argc,argv);
    }
    filename = argv[1];

    // parse the optional arguments
    for (int i = 2; i < argc; i++) {
        if (argv[i] == std::string("-tile_size")) {
            i++;
            assert (i < argc);
            GLOBAL_TILE_SIZE = atoi(argv[i]);
            if (GLOBAL_TILE_SIZE < 11 || GLOBAL_TILE_SIZE % 2 == 0) {
                std::cerr << "ERROR: bad tile_size" << std::endl;
                usage(argc,argv);
            }
        } else if (argv[i] == std::string("-all_solutions")) {
            all_solutions = true;
        } else if (argv[i] == std::string("-board_dimensions")) {
            i++;
            assert (i < argc);
            rows = atoi(argv[i]);
            i++;
            assert (i < argc);
            columns = atoi(argv[i]);
            if (rows < 1 || columns < 1) {
                usage(argc,argv);
            }
        } else if (argv[i] == std::string("-allow_rotations")) {
            allow_rotations = true;
        } else {
            std::cerr << "ERROR: unknown argument '" << argv[i] << "'" << std::endl;
            usage(argc,argv);
        }
    }
}

// ==========================================================================
void ParseInputFile(int argc, char *argv[], const std::string &filename, std::vector<Tile*> &tiles) {

    // open the file
    std::ifstream istr(filename.c_str());
    if (!istr) {
        std::cerr << "ERROR: cannot open file '" << filename << "'" << std::endl;
        usage(argc,argv);
    }
    assert (istr);

    // read each line of the file
    std::string token, north, east, south, west;
    while (istr >> token >> north >> east >> south >> west) {
        assert (token == "tile");
        Tile *t = new Tile(north,east,south,west);
        tiles.push_back(t);
    }
}


//returns a new tile rotated the amount specified
Tile* rotateTile(Tile* const tile, int rotationValue)
{
    if(rotationValue == 0)
    {
        return new Tile(tile->getNorth(), tile->getEast(), tile->getSouth(), tile->getWest());
    }
    else if(rotationValue == 90)
    {
        return new Tile(tile->getWest(), tile->getNorth(), tile->getEast(), tile->getSouth());
    }
    else if(rotationValue == 180)
    {
        return new Tile(tile->getSouth(), tile->getWest(), tile->getNorth(), tile->getEast());
    }
    else if(rotationValue == 270)
    {
        return new Tile(tile->getEast(), tile->getSouth(),tile->getWest(), tile->getNorth());
    }
} 


//returns vector of all valid locations the tile can be placed on the board
//valid here means - sides of tiles must match but is not necessarily a solution
vector<Location> getValidLocations(const Board& board, Tile* const tile, bool allowRotations)
{
    vector<Location> validLocations;

    for(int rot=0; (rot<360 and allowRotations) or (rot==0); rot+=90) 
    {
        Tile* rotatedTile = rotateTile(tile, rot); 
        for(int row=0; row<board.numRows(); row++)
        {
            for(int col=0; col<board.numColumns(); col++)
            {
                //if the board is empty, the first piece can be placed anywhere
                if(not board.isEmpty())
                {
                    //check to make sure that spot isn't already taken
                    if(board.getTile(row,col) != NULL)
                    {
                        continue;
                    }
                    //check to make sure spot is touching another tile
                    else if(not(((row-1 >= 0) and board.getTile(row-1,col) != NULL) or
                                ((row+1 < board.numRows()) and board.getTile(row+1,col) != NULL) or 
                                ((col-1 >= 0) and  board.getTile(row,col-1) != NULL) or
                                ((col+1 < board.numColumns()) and board.getTile(row,col+1) != NULL)))
                    {
                        continue;
                    }
                    else
                    {
                        //check that sides match
                        if((row-1 >= 0) and board.getTile(row-1,col)!= NULL and 
                                board.getTile(row-1,col)->getSouth() != rotatedTile->getNorth())
                        {
                            continue;
                        }   
                        if((row+1 < board.numRows()) and board.getTile(row+1,col)!= NULL and 
                                board.getTile(row+1,col)->getNorth() != rotatedTile->getSouth())
                        {
                            continue;
                        }
                        if((col-1 >= 0) and board.getTile(row,col-1)!= NULL and 
                                board.getTile(row,col-1)->getEast() != rotatedTile->getWest())
                        {
                            continue;
                        }
                        if((col+1 < board.numColumns()) and board.getTile(row,col+1)!= NULL and
                                board.getTile(row,col+1)->getWest() != rotatedTile->getEast())
                        {
                            continue;
                        }

                    }
                }

                validLocations.push_back(Location(row,col,rot));
            }
        }
        if(rot != 0)
        {
            delete rotatedTile;
        }
    }
    return validLocations;
}

//recursively builds a tree of nodes that have a level corresponding to each tile. Each node is a possible valid tile location and its children are all the places the next tile can be placed
vector<Node*> buildPlacementTree(Board board, const vector<Tile*>& tiles, const Location& loc, bool allowRotations, int level = -1) 
{
    vector<Node*> possiblePositions;

    if(level+1 < tiles.size())
    {
        if(level >= 0)
        {

            board.setTile(loc.row, loc.column, rotateTile(tiles[level], loc.rotation));
            //early solution checking to speed up solution - paths die out if the permiter of the board is not a pasture
            for(int i=0; i<board.numColumns(); i++)
            {
                if((board.getTile(0, i) !=  NULL and board.getTile(0,i)->getNorth() != "pasture")
                        or (board.getTile(board.numRows()-1, i) != NULL and board.getTile(board.numRows()-1, i)->getSouth() != "pasture"))
                {
                    return possiblePositions;
                }
            }
            for(int i=0; i<board.numRows(); i++)
            {
                if((board.getTile(i,0) != NULL and board.getTile(i,0)->getWest() != "pasture")
                        or (board.getTile(i,board.numColumns()-1) != NULL and board.getTile(i,board.numColumns()-1)->getEast() != "pasture"))

                {
                    return possiblePositions;
                }
            }

        }

        vector<Location> locations;

        if(level == -1)
        {
            //first tile is not rotated so the entire board doesn't get rotated
            locations = getValidLocations(board, tiles[level+1],false);
        }
        else
        {
            locations = getValidLocations(board, tiles[level+1], allowRotations);
        }

        for(int i=0; i<locations.size(); i++)
        {
            Node* m = new Node(locations[i], buildPlacementTree(board, tiles, locations[i], allowRotations, level+1), level+1);
            possiblePositions.push_back(m);
        }
    }
    else
    {
        board.setTile(loc.row, loc.column, rotateTile(tiles[level], loc.rotation));
    } 

    return possiblePositions; 
}

//checks perimiter of placement tiles for edges that are not pastures
bool isASolution(const vector<Location>& placement, const vector<Tile*>& tiles)
{       
    bool hasANeighborNorth = false;
    bool hasANeighborSouth = false;
    bool hasANeighborEast = false;
    bool hasANeighborWest = false;

    for(int i=0; i<placement.size(); i++)
    {
        hasANeighborNorth = false;
        hasANeighborSouth = false;
        hasANeighborEast = false;
        hasANeighborWest = false;

        for(int j=0; j<placement.size(); j++)
        {
            if(i != j)
            {
                if((placement[i].column == placement[j].column) and (placement[i].row - placement[j].row) == 1)
                {
                    hasANeighborNorth = true; 
                }
                else if((placement[i].column == placement[j].column) and (placement[i].row - placement[j].row) == -1)
                {
                    hasANeighborSouth = true;
                }
                else if((placement[i].row == placement[j].row) and (placement[i].column - placement[j].column) == 1)
                {
                    hasANeighborWest = true;
                }
                else if((placement[i].row == placement[j].row) and (placement[i].column - placement[j].column) == -1)
                {
                    hasANeighborEast = true;
                }
            }
        }

        if((not hasANeighborNorth) and rotateTile(tiles[i],placement[i].rotation)->getNorth() != "pasture")
        {
            return false;
        }
        if((not hasANeighborSouth) and rotateTile(tiles[i],placement[i].rotation)->getSouth() != "pasture")
        {
            return false;
        }
        if((not hasANeighborEast) and rotateTile(tiles[i],placement[i].rotation)->getEast() != "pasture")
        {
            return false;
        }
        if((not hasANeighborWest) and rotateTile(tiles[i],placement[i].rotation)->getWest() != "pasture")
        {
            return false;
        }
    } 

    return true;
}

//flattens tree into a vector of vectors of locations
void flattenTree(vector< vector<Location> >& allLocs, vector<Location> locs, Node* node, int height)
{
    locs.push_back(node->getLocation());

    vector<Node*> temp = node->getNextTileLocations();
    for(int i=0; i< temp.size(); i++)
    {
        flattenTree(allLocs, locs, temp[i], height); 
    }

    if(locs.size() == height)
    { 
        allLocs.push_back(locs);
    } 
}

//removes solutions that are not unique
vector< vector<Location> > removeDuplicateSolutions(const vector< vector<Location> >& solutions, const vector<Tile*>& tiles, int row, int col)
{
    //if there is only one solution, it must be unique
    if(solutions.size() < 2)
    {
        return solutions;
    }

    vector< vector<Location> > uniqueSolutions;
    vector< vector<Location> > reducedSolutions = solutions;
    vector< vector<Location> > reducedUniqueSolutions;

    //push all solutions to top left corner of board so translations are not counted twice
    for(int i=0; i<reducedSolutions.size(); i++)
    {
        int minCol = reducedSolutions[i][0].column;
        int minRow = reducedSolutions[i][0].row;
        for(int j=1; j<reducedSolutions[i].size(); j++)
        {
            if(reducedSolutions[i][j].column < minCol)
            {
                minCol = reducedSolutions[i][j].column;
            } 
            if(reducedSolutions[i][j].row < minRow)
            {
                minRow = reducedSolutions[i][j].row;
            }
        }

        int rowOffset = minRow;
        int colOffset = minCol; 
        for(int j=0; j<reducedSolutions[i].size(); j++)
        {
            reducedSolutions[i][j].row = reducedSolutions[i][j].row - rowOffset;
            reducedSolutions[i][j].column = reducedSolutions[i][j].column - colOffset;
        }
    }

    bool unique = true;
    uniqueSolutions.push_back(solutions[0]);
    reducedUniqueSolutions.push_back(reducedSolutions[0]);
    for(int i=1; i<reducedSolutions.size(); i++)
    {
        Board b(row, col);

        for(int k=0; k<reducedSolutions[i].size(); k++)
        {
            Tile* rotatedTile = rotateTile(tiles[k], reducedSolutions[i][k].rotation); 
            b.setTile(reducedSolutions[i][k].row, reducedSolutions[i][k].column, rotatedTile);

        }

        for(int j=0; j<reducedUniqueSolutions.size() and unique; j++) 
        {
            //translation without rotations check
            if(reducedSolutions[i] == reducedUniqueSolutions[j])
            {
                unique = false;
            }


            //if still unique, check all pieces on the board to make sure they are not the same
            if(unique)
            {
                Board b2(row, col);
                for(int k=0; k<uniqueSolutions[j].size(); k++)
                {
                    Tile* rotated_tile = rotateTile(tiles[k], reducedUniqueSolutions[j][k].rotation);
                    b2.setTile(reducedUniqueSolutions[j][k].row, reducedUniqueSolutions[j][k].column, rotated_tile);
                }

                unique = (b2 != b);

            }
        } 

        if(unique)
        {
            uniqueSolutions.push_back(solutions[i]);
            reducedUniqueSolutions.push_back(reducedSolutions[i]);
        }

        unique = true; 
    }

    return uniqueSolutions;
}

// ==========================================================================
int main(int argc, char *argv[]) 
{
    std::string filename;
    int rows = -1;
    int columns = -1;
    bool all_solutions = false;
    bool allow_rotations = false;
    HandleCommandLineArguments(argc, argv, filename, rows, columns, all_solutions, allow_rotations);

    // load in the tiles
    std::vector<Tile*> tiles;
    ParseInputFile(argc,argv,filename,tiles);


    if(rows < 1 or columns < 1)
    {
        rows = tiles.size();
        columns = tiles.size();
    }


    // confirm the specified board is large enough
    if (rows < 1  ||  columns < 1  ||  rows * columns < tiles.size()) {
        std::cerr << "ERROR: specified board is not large enough";
        cerr  << rows << "X" << columns << "=" << rows*columns << " " << tiles.size() << std::endl;
        usage(argc,argv);
    }

    Board board(rows, columns);
    vector< vector<Location> > allLocations;

    vector<Location> locations;

    Location* l = new Location();
    vector<Node*> allPaths = buildPlacementTree(board,tiles, *l, allow_rotations);


    vector< vector<Location> > solutions;

    for(int i=0; i< allPaths.size(); i++)
    {
        flattenTree(allLocations, locations, allPaths[i], tiles.size());       
    }

    bool foundOne = false;
    for(int i=0; i<allLocations.size(); i++)    
    {
        if(isASolution(allLocations[i], tiles))
        {
            foundOne = true;            
            solutions.push_back(allLocations[i]);
        }

        if(not all_solutions and foundOne)
        {
            break;
        }
    }


    if(not all_solutions)
    {
        if(solutions.size() == 0)
        {
            cout << "No Solution." << endl;
        }
        else
        {
            cout << "Solution: " ;
            for(int i=0; i<solutions[0].size(); i++)
            {
                cout << solutions[0][i];
                Tile* _t = rotateTile(tiles[i], solutions[0][i].rotation);
                board.setTile(solutions[0][i].row, solutions[0][i].column, _t); 
            }
            cout << endl;
            board.Print();
            board.clearBoard();
        }
    }
    
    else
    {
        vector< vector<Location> > uniqueSolutions = removeDuplicateSolutions(solutions, tiles, rows, columns); 

        for(int i=0; i<uniqueSolutions.size(); i++)
        {
            cout << "Solution: ";
            for(int j=0; j<uniqueSolutions[i].size(); j++)
            {
                cout << uniqueSolutions[i][j];
                Tile* tempTile = rotateTile(tiles[j], uniqueSolutions[i][j].rotation);
                board.setTile(uniqueSolutions[i][j].row, uniqueSolutions[i][j].column, tempTile);
            }

            cout << endl;
            board.Print();
            board.clearBoard();
        }


        cout << "Found " << uniqueSolutions.size() << " Solution(s)." << endl;

        // delete the tiles
        for (int t = 0; t < tiles.size(); t++) {
            delete tiles[t];
        }
    }
}
// ==========================================================================
