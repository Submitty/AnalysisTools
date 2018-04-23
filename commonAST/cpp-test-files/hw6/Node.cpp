#include "Node.h"

Node::Node(const Location& l, vector<Node*> nodes, int lev)
{
    loc = l;
    level = lev;
    nextTileLocations = nodes;
}
