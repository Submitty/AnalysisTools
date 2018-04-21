#include <cassert>
#include "superhero.h"
#include "team.h"

bool isvowel(char c) {
    if (!isalpha(c)) return false;
    char c2 = tolower(c);
    return (c2 == 'a' || c2 == 'e' || c2 == 'i' || c2 == 'o' || c2 == 'u');
}

bool isconsonant(char c) {
    return (isalpha(c) && !isvowel(c));
}

std::string Team::getName() const {
    if (superheroes.size() == 0) 
        return "";
    std::string answer;
    std::list<Superhero>::const_iterator itr;
    for (itr = superheroes.begin(); itr != superheroes.end(); itr++) {
        char first_consonant = ' ';
        char first_vowel = ' ';
        std::string true_identity = itr->getTrueIdentity();
        for (int j = 0; j < true_identity.size(); j++) {
            if (first_consonant == ' ' && isconsonant(true_identity[j]))
                first_consonant = tolower(true_identity[j]);
            if (first_vowel == ' ' && isvowel(true_identity[j]))
                first_vowel = tolower(true_identity[j]);
        }
        answer.push_back(first_consonant);
        answer.push_back(first_vowel);
    }

    answer[0] = toupper(answer[0]);
    return answer;
}

void Team::operator+=(const Superhero &a)
{
    superheroes.push_back(a);
}

void Team::operator-=(const Superhero &a)
{
    for(list<Superhero>::iterator itr = superheroes.begin(); itr != superheroes.end(); itr++)
    {
        if(*itr == (a.identity).c_str())
        {
            superheroes.erase(itr);
        }
    }
}
Team operator+(const Team &a, const Team &b)
{
    Team c;
    c.superheroes = a.superheroes;
    for(list<Superhero>::const_iterator itr = b.superheroes.begin(); itr != b.superheroes.end(); itr++)
    {
       c.superheroes.push_back(*itr); 
    }
    return c;
}
Team operator+(const Team &a, const Superhero &b)
{
    Team c;
    c.superheroes = a.superheroes;
    c.superheroes.push_back(b);
    return c;
}
Team operator+(const Superhero& a, const Superhero& b)
{
    Team c;
    list<Superhero> l;
    l.push_back(a);
    l.push_back(b);
    c.superheroes = l;
    return c;
}
