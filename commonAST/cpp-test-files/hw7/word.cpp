#include "word.h"

bool operator< (const word& one, const word& two)
{
    return one.getName() < two.getName();
}


bool operator== (const word& one, const word& two)
{
    return one.getName() == two.getName();
}

