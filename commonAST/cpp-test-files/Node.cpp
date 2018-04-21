#include "/home/elizabethdinella/test-files/dataStructures/hw6/Node.h"

Node::Node(const Location& l, vector<Node*> nodes, int lev)
{
    loc = l;
    level = lev;
    nextTileLocations = nodes;
}
