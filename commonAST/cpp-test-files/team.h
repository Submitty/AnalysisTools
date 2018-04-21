#ifndef _TEAM_H_
#define _TEAM_H_

#include <string>
#include <iostream>
#include <list>

class Superhero;

class Team {

public:
    
  friend class Superhero;
  Team(){};
  // accessor function
  std::string getName() const;

  // adding and removing a single superhero
  void operator+=(const Superhero &a);
  void operator-=(const Superhero &a);

  // adding two teams together or a team and a superhero together
  // makes a team.  these functions must be member functions to allow
  // access to the list of superheroes.
  friend Team operator+(const Team &a, const Team &b);
  friend Team operator+(const Team &a, const Superhero &b);
  friend Team operator+(const Superhero& a, const Superhero& b);
   
private:
  // REPRESENATION
  std::list<Superhero> superheroes;
};

// non-member function because it doesn't need access to any Team variables
Team operator+(const Superhero &a, const Superhero &b);

#endif
