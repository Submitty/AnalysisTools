#include <cassert>
#include "/home/elizabethdinella/test-files/dataStructures/hw6/location.h"


Location::Location (int r, int c, int rot) : row(r), column(c), rotation(rot) { 
  assert (rotation == 0 || 
          rotation == 90 || 
          rotation == 180 || 
          rotation == 270); 
}

Location::Location()
{
    row= 0;
    column = 0;
    rotation = 0;
}

bool operator==(const Location &loc1, const Location &loc2) {
  return (loc1.row      == loc2.row      && 
          loc1.column   == loc2.column   && 
          loc1.rotation == loc2.rotation );
}

std::ostream& operator<<(std::ostream &ostr, const Location &loc) {
  ostr << "(" << loc.row << "," << loc.column << "," << loc.rotation << ")";
  return ostr;
}
/*
bool operator<(const Location& loc1, const Location& loc2)
{
    if(loc1.row < loc2.row)
    {
        return true;
    }
    else if(loc1.row > loc2.row)
    {
        return false;
    }
    else
    {
        if(loc1.column < loc2.column)
        {
            return true;
        }
        else
        {
            return false;
        }
    }
}
*/
