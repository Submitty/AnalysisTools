#include "/mnt/c/Users/Elizabeth/Documents/spring2017/commonASTs/cpp-test-files/dataStructures/hw7/word.h"

bool operator< (const word& one, const word& two)
{
    return one.getName() < two.getName();
}


bool operator== (const word& one, const word& two)
{
    return one.getName() == two.getName();
}

