#ifndef __NODE__
#define __NODE__


#include <vector>
#include "location.h"

using namespace std;

class Node{

    public:
        //CONSTRUCTOR
        Node(const Location& l, vector<Node*> nodes, int lev);
 
        //ACCESSORS
        int getLevel() const{ return level; };
        const vector<Node*>& getNextTileLocations() const { return nextTileLocations; }
        const Location& getLocation() const { return loc; };
    
        //MODIFIERS
        void setNextLocations(const vector<Node*>& v) { nextTileLocations = v; };
        
    private:
        int level;
        Location loc;
        vector<Node*> nextTileLocations;    
};

#endif
