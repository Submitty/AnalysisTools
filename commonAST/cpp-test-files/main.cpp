#include <string>
#include <iostream>
#include <sstream>
#include <cassert>

#include "superhero.h"
#include "team.h"

int main() {

  // -------------------------------------------------------
  // IMPORTANT: DO NOT MODIFY ANY OF THE PROVIDED TEST CASES
  // -------------------------------------------------------

  // CHECKPOINT 1

  // create a bunch of superheroes
  Superhero a("Elastigirl", "Zoe", "Flexible");
  Superhero b("Falcon", "Roland", "Flying");
  Superhero c("Shadow", "Violet", "Invisible");
  Superhero d("Gazerbeam", "Pierce", "Laser");
  Superhero e("Flame", "Fiona", "Fire");
  Superhero f("Lumberman", "Jack", "Wood");
  Superhero g("Aquawoman", "Marie", "Water");
  Superhero h("Dash", "Aaron", "Speed");
  
  // access information about the superheroes
  assert (a.getName() == "Elastigirl");
  assert (b.getPower() == "Flying");
  assert (c.getName() == "Shadow");
  assert (d.getPower() == "Laser");
  
  // test superhero output
  // should print:
  // "Superhero Elastigirl has power Flexible"
  // "Superhero Falcon has power Flying"
  std::cout << a << b;

  // there is no public accessor for a superhero's true identity
  // but we can try to guess a superheroe's true identity
  assert (a == "Zoe");
  assert (!(b == "Bob"));
  assert (c != "Lilly");
  assert (!(d != "Pierce"));

  // -------------------------------------------------------
  
  // CHECKPOINT 2 (uncomment when you're ready)


  // superheroes start out good, but can be corrupted (negated)
  -b;
  -c;
  -c;
  assert (a.isGood());
  assert (!b.isGood());
  assert (c.isGood());

  // test superhero output
  // should print:
  // "Supervillain Falcon has power Flying"
  // "Superhero Shadow has power Invisible"
  std::cout << b << c;

  // superhero powers can be compared.  note that this property is *not* transitive!
  assert (e > f);  // fire beats wood
  assert (f > g);  // wood beats water
  assert (g > e);  // water beats fire

  //
  // add your own comparison tests here!
  //


  // -------------------------------------------------------

  // CHECKPOINT 3 (uncomment when you're ready)


  // superheroes can form teams
  Team team1;
  team1 += a;
  team1 += b;
  Team team2 = c + d;

  // team names are a combination of the member's true identities
  assert (team1.getName() == "Zoro");
  assert (team2.getName() == "Vipi");
  
  // Teams can merge
  Team team3 = team1 + team2;
  assert (team3.getName() == "Zorovipi");
  // note that this is the same as:
  Team team4 = (a + b) + (c + d);
  assert (team4.getName() == "Zorovipi");

  // and this:
  Team team5 = e + f + g + h;
  assert (team5.getName() == "Fijamara");
  // is the same as:
  Team team6 = ((e + f) + g) + h;  
  assert (team6.getName() == "Fijamara");

  // Superheroes can leave teams
  team3 -= a;
  assert (team3.getName() == "Rovipi");
  team4 -= b;
  assert (team4.getName() == "Zovipi");
  // the most recently added member is always at the end of the name
  team4 += b;
  assert (team4.getName() == "Zovipiro");


  std::cout << "Passed all test cases." << std::endl;
  return 0;
}
