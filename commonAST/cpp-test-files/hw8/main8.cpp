// ===================================================================
//
// IMPORTANT: Do not make any changes to this file, except to
//            uncomment the test cases as you work, and to write your
//            own test cases in student_tests
//
// ===================================================================

#include <cassert>
#include <vector>
#include <cstdlib>
#include <ctime>  // for randomness

#include "quad_tree.h"

// ==============================================================
// ==============================================================

// function prototypes
void simple_test();
void random_test(int num_points,int extra_space);
void more_tests();
void extra_credit_test();
void student_tests();

// helper function for random shuffling
int myrandom (int i) { return std::rand()%i;}

// ==============================================================
// ==============================================================

int main() {


  // illustrative example from .pdf
  simple_test();


  // seed the random number generator with the current time
  // (do this just once within the program!)
  std::srand ( unsigned ( std::time(0) ) );
  // for debugging, it may be helpful to instead use a fixed seed 
  // (not connected to the current time)
  //std::srand ( 42 );
  
  // randomly generated input tests
  random_test(10,2);
  random_test(10,2);
  random_test(20,2);
  random_test(20,2);
  random_test(52,1);
  random_test(52,1);
  random_test(100,1);
  


  
  // more tests for robustness and conventions of the overall data structure
  more_tests();
  


  // reordering the input for a more balanced resulting tree
  extra_credit_test();


  // your test cases!
//  student_tests();
}
 

// ==============================================================
// ==============================================================

void simple_test() {

  std::cout << "Beginning simple_test()..." << std::endl;


  // --------------------------------------------------------
  // a collection of 21 points that make a nice sample tree
  std::vector< std::pair<Point<int>,char> > simple_points;
  simple_points.push_back(std::make_pair(Point<int>(20,10), 'A'));
  simple_points.push_back(std::make_pair(Point<int>(10,5), 'B'));
  simple_points.push_back(std::make_pair(Point<int>(30,4), 'C'));
  simple_points.push_back(std::make_pair(Point<int>(11,15), 'D'));
  simple_points.push_back(std::make_pair(Point<int>(31,16), 'E'));
  simple_points.push_back(std::make_pair(Point<int>(5,3), 'F'));
  simple_points.push_back(std::make_pair(Point<int>(15,2), 'G'));
  simple_points.push_back(std::make_pair(Point<int>(4,7), 'H'));
  simple_points.push_back(std::make_pair(Point<int>(14,8), 'I'));
  simple_points.push_back(std::make_pair(Point<int>(25,1), 'J'));
  simple_points.push_back(std::make_pair(Point<int>(35,2), 'K'));
  simple_points.push_back(std::make_pair(Point<int>(26,7), 'L'));
  simple_points.push_back(std::make_pair(Point<int>(36,6), 'M'));
  simple_points.push_back(std::make_pair(Point<int>(3,13), 'N'));
  simple_points.push_back(std::make_pair(Point<int>(16,12), 'O'));
  simple_points.push_back(std::make_pair(Point<int>(4,17), 'P'));
  simple_points.push_back(std::make_pair(Point<int>(15,18), 'Q'));
  simple_points.push_back(std::make_pair(Point<int>(25,13), 'R'));
  simple_points.push_back(std::make_pair(Point<int>(37,14), 'S'));
  simple_points.push_back(std::make_pair(Point<int>(24,19), 'T'));
  simple_points.push_back(std::make_pair(Point<int>(36,18), 'U'));


  // --------------------------------------------------------
  // the quad tree data structure starts out empty
  QuadTree<int,char> simple;
  assert (simple.size() == 0);
  // an empty tree has height == -1
  assert (simple.height() == -1); 
  // plot the structure with with these dimensions (width=40,height=20)
  std::cout << "\nan empty tree:" << std::endl;
  simple.plot(40,20);



  // --------------------------------------------------------
  for (int i = 0; i < simple_points.size(); i++) {

    // add each point from the collection
    simple.insert(simple_points[i].first,simple_points[i].second);
    // verify the size (total # of points in the tree)
    assert (simple.size() == i+1);

    // a few some specific checks along the way
    if (i == 0) { 
      std::cout << "\nafter inserting first data point:" << std::endl;
      simple.plot(40,20);
      // a tree with 1 node has height == 0
      assert (simple.height() == 0); 
      // check that the newly inserted element can be found
      QuadTree<int,char>::iterator itr = simple.find(20,10);
      assert (itr != simple.end());
      // read the label & coordinates from the iterator
      assert (itr.getLabel() == 'A');
      // dereference the iterator to get the point
      const Point<int> &pt = *itr;
      assert (pt.x == 20);
      assert (pt.y == 10);
    } else if (i <= 4) {
      std::cout << "\nafter inserting " << i+1 << " data points:" << std::endl;
      simple.plot(40,20);
      // the next 4 additions for this simple all happen at the
      // second level, tree has height = 1
      assert (simple.height() == 1);
    } else if (i == 8) {
      std::cout << "\nafter inserting " << i+1 << " data points:" << std::endl;
      simple.plot(40,20);
      assert (simple.height() == 2);
      // check for an element that exists
      QuadTree<int,char>::iterator itr = simple.find(4,7);
      assert (itr != simple.end());
      assert (itr.getLabel() == 'H');
      assert ((*itr).x == 4);
      assert ((*itr).y == 7);
      // check for a couple elements that aren't in the tree
      itr = simple.find(14,14);
      assert (itr == simple.end());
      itr = simple.find(15,18);
      assert (itr == simple.end());
      // another visualization of the tree structure
      // note: this is a pre-order traversal of the data (print the node, then recurse on each child)
      std::cout << "\na 'sideways' printing of the tree structure with 9 nodes:" << std::endl;
      simple.print_sideways();
    }
  }

  // --------------------------------------------------------
  // a few more checks
  std::cout << "\nafter inserting all 21 data points:" << std::endl;
  simple.plot(40,20);
  assert (simple.size() == 21);
  assert (simple.height() == 2);
  QuadTree<int,char>::iterator itr = simple.find(15,18);
  assert (itr != simple.end());
  assert (itr.getLabel() == 'Q');
  assert ((*itr).x == 15);
  assert ((*itr).y == 18);

  // plot the data without the lines
  std::cout << "\na plot of the point data without the lines:" << std::endl;
  simple.plot(40,20,false);


  // --------------------------------------------------------
  // another visualization of the tree structure
  // note: this is a pre-order traversal of the data (print the node, then recurse on each child)
  std::cout << "\na 'sideways' printing of the finished tree structure:" << std::endl;
  simple.print_sideways();


  // --------------------------------------------------------
  // use the primary (depth-first) iterator to traverse the tree structure
  // note: this is a pre-order traversal, the same order as the 'sideways' tree above!!
  std::cout << "\nA depth-first traversal of the simple tree (should match sideways output!):" << std::endl;
  QuadTree<int,char>::iterator df_itr = simple.begin();
  char expected_depth_first_order[21] = 
    { 'A','B','F','G','H','I','C','J','K','L','M','D','N','O','P','Q','E','R','S','T','U' };
  for (int i = 0; i < 21; i++) {
    assert (df_itr != simple.end());
    // get the depth/level of this element in the tree (distance from root node!)
    int depth = df_itr.getDepth();
    // use the depth to indent the output (& match the sideways tree output above)
    std::cout << std::string(depth*2,' ') << df_itr.getLabel() << " " << *df_itr << std::endl;
    // check that the output is in the correct order!
    assert (df_itr.getLabel() == expected_depth_first_order[i]);
    // test the pre-increment operator++ 
    ++df_itr;  
  }
  // after 21 increments, we better be at the end!
  assert (df_itr == simple.end());


  // --------------------------------------------------------
  // using the breadth-first iterator to traverse the data by level
  std::cout << "\nA breadth first traversal of the simple tree:";
  QuadTree<int,char>::bf_iterator bf_itr = simple.bf_begin();
  char expected_breadth_first_order[21] = 
    { 'A','B','C','D','E','F','G','H','I','J','K','L','M','N','O','P','Q','R','S','T','U' };
  int level = -1;
  for (int i = 0; i < 21; i++) {
    assert (bf_itr != simple.bf_end());
    // get the depth/level of this element in the tree (distance from root node!)
    int depth = bf_itr.getDepth();
    if (level != depth) {
      level = depth;
      // starting a new level!
      std::cout << std::endl << "   level " << level << ":";
    }
    // print out this data point
    std::cout << " " << bf_itr.getLabel() << *bf_itr;
    // check that the output is in the correct order!
    assert (bf_itr.getLabel() == expected_breadth_first_order[i]);
    // test the pre-increment operator++
    ++bf_itr;
  }
  // after 21 increments, we better be at the end!
  assert (bf_itr == simple.bf_end());
  std::cout << std::endl;

  // --------------------------------------------------------
  std::cout << "\nFinished with simple_test().\n" << std::endl;

  // Note: the destructor for the QuadTree object 'simple' is
  // automatically called when we leave this function and the variable
  // goes out of scope!
}

// ==============================================================
// ==============================================================

void random_test(int num_points, int extra_space) {

  std::cout << "Beginning random_test()..." << std::endl;


  
  // --------------------------------------------------------
  // generate some random points
  // no two points may have the same x-coordinate or same y-coordinate
  std::vector<int> x_coordinates;
  std::vector<int> y_coordinates;
  assert (extra_space >= 1);
  for (int i = 0; i < num_points; i++) {
    // scale ASCII output horizontally since each character is taller than wide
    x_coordinates.push_back(2*i*extra_space+2*extra_space); 
    y_coordinates.push_back(  i*extra_space+extra_space);
  }
  // shuffle the ordering of these coordinates
  std::random_shuffle ( x_coordinates.begin(), x_coordinates.end(), myrandom );
  std::random_shuffle ( y_coordinates.begin(), y_coordinates.end(), myrandom );


  // --------------------------------------------------------
  // create the tree and add all of the points
  QuadTree<int,char> random_tree;
  for (int i = 0; i < num_points; i++) {
    char label;
    if (i < 26) {
      // uppercase letters for the first 26 points
      label = char('A'+i);
    } else if (i < 52) {
      // lowercase letters for the first 26 points
      label = char('a'+i-26);
    } else {
      // just use '?' for all remaining points
      label = '?';
    }
    random_tree.insert( Point<int>(x_coordinates[i],y_coordinates[i]),  label);
  }


  // --------------------------------------------------------
  // plot this data, print the tree format, and iterate through the elements
  std::cout << "\nafter inserting all data points:" << std::endl;
  random_tree.plot(num_points*2*extra_space+extra_space*4-2,num_points*extra_space+extra_space*2-1);
  std::cout << "\na 'sideways' printing of the finished tree structure:" << std::endl;
  random_tree.print_sideways();
  std::cout << "\ndepth-first:   ";
  QuadTree<int,char>::iterator df_itr = random_tree.begin();
  while (df_itr != random_tree.end()) {
    std::cout << " " << df_itr.getLabel();
    ++df_itr; 
  }
  std::cout << "\nbreadth-first: ";
  QuadTree<int,char>::bf_iterator bf_itr = random_tree.bf_begin();
  while (bf_itr != random_tree.bf_end()) {
    std::cout << " " << bf_itr.getLabel();
    ++bf_itr; 
  }
  std::cout << std::endl;

  std::cout << "\nFinished with random_test().\n" << std::endl;
  

  // Note: the destructor for the QuadTree object 'random_tree' is
  // automatically called when we leave this function and the variable
  // goes out of scope!
}

// ==============================================================
// ==============================================================

void more_tests() {
  std::cout << "Beginning more_tests()..." << std::endl;


  // --------------------------------------------------------
  // testing a dynamically allocated tree
  QuadTree<int,char>* my_tree = new QuadTree<int,char>;
  my_tree->insert( Point<int>(10,5),  'A');
  my_tree->insert( Point<int>(5,2),  'B');
  my_tree->insert( Point<int>(15,3),  'C');
  my_tree->insert( Point<int>(6,7),  'D');

  // test return value of insert
  std::pair<QuadTree<int,char>::iterator,bool> ret_val;
  ret_val = my_tree->insert( Point<int>(14,8),  'E');
  assert (ret_val.second == true);
  assert ((*(ret_val.first)).x == 14);
  assert ((*(ret_val.first)).y == 8);
  assert (ret_val.first.getLabel() == 'E');
  assert (my_tree->size() == 5);

  // try inserting a duplicate point with a different label
  ret_val = my_tree->insert( Point<int>(5,2),  'F');
  assert (ret_val.second == false);
  assert ((*(ret_val.first)).x == 5);
  assert ((*(ret_val.first)).y == 2);
  assert (ret_val.first.getLabel() == 'B');
  assert (my_tree->size() == 5);

  std::cout << "\nmy_tree:" << std::endl;
  my_tree->plot(20,10);


  // --------------------------------------------------------
  // make a copy of my_tree using the copy constructor
  QuadTree<int,char> other_tree (*my_tree);
  std::cout << "\nother_tree:" << std::endl;
  other_tree.plot(20,10);
  assert (my_tree->size() == 5);
  assert (other_tree.size() == 5);
  // edits should only affect the new tree
  other_tree.insert( Point<int>(2,9),  'F');
  std::cout << "\nafter adding other point to other_tree:" << std::endl;
  other_tree.plot(20,10);
  assert (my_tree->size() == 5);
  assert (other_tree.size() == 6);
  // need to explicitly de-allocate the original tree (since it was allocated with new)
  delete my_tree;
  my_tree = NULL;


  // --------------------------------------------------------
  // test the assignment operator
  QuadTree<int,char> yet_another_tree;
  yet_another_tree.insert( Point<int>(16,6),  'Q');
  yet_another_tree.insert( Point<int>(9,2),  'R');
  yet_another_tree.insert( Point<int>(6,8),  'S');

  std::cout << "\nyet_another_tree:" << std::endl;
  yet_another_tree.plot(20,10);

  assert (other_tree.size() == 6);
  other_tree = yet_another_tree;
  std::cout << "\nother_tree after using the assignment operator" << std::endl;
  other_tree.plot(20,10);
  assert (yet_another_tree.size() == 3);
  assert (other_tree.size() == 3);

  // edits should not affect the new tree
  yet_another_tree.insert( Point<int>(3,4),  'T');
  std::cout << "\nyet_another_tree, after adding a point:" << std::endl;
  yet_another_tree.plot(20,10);
  assert (yet_another_tree.size() == 4);
  assert (other_tree.size() == 3);


  // --------------------------------------------------------
  // testing the post-increment operator++ 
  // (only tested pre-increment in simple_test and random_test)
  std::cout << "\nsideways tree output:   " << std::endl;
  yet_another_tree.print_sideways();
  std::cout << "\ndepth-first:   ";
  QuadTree<int,char>::iterator df_itr = yet_another_tree.begin();
  while (df_itr != yet_another_tree.end()) {
    std::cout << " " << df_itr.getLabel();
    QuadTree<int,char>::iterator before = df_itr;
    QuadTree<int,char>::iterator before2 = df_itr++; 
    assert (before == before2);
    assert (before2 != df_itr);
  }
  std::cout << "\nbreadth-first: ";
  QuadTree<int,char>::bf_iterator bf_itr = yet_another_tree.bf_begin();
  while (bf_itr != yet_another_tree.bf_end()) {
    std::cout << " " << bf_itr.getLabel();
    QuadTree<int,char>::bf_iterator before = bf_itr;
    QuadTree<int,char>::bf_iterator before2 = bf_itr++; 
    assert (before == before2);
    assert (before2 != bf_itr);
  }
  std::cout << std::endl;

  std::cout << "\nFinished with more_tests().\n" << std::endl;

  // Note: the destructor for the QuadTree objects 'other_tree' and
  // 'yet_another_tree' is automatically called when we leave this
  // function and the variable goes out of scope!
}

// ==============================================================
// ==============================================================


void extra_credit_test() {

  std::cout << "Beginning extra_credit_test()..." << std::endl;

  // --------------------------------------------------------
  // start with a collection of points sorted by y coordinate
  std::vector< std::pair<Point<int>,char> > points;

  points.push_back(std::make_pair(Point<int>(24, 1), 'A'));
  points.push_back(std::make_pair(Point<int>(34, 2), 'B'));
  points.push_back(std::make_pair(Point<int>(16, 3), 'C'));
  points.push_back(std::make_pair(Point<int>( 5, 4), 'D'));
  points.push_back(std::make_pair(Point<int>(30, 5), 'E'));
  points.push_back(std::make_pair(Point<int>(10, 6), 'F'));
  points.push_back(std::make_pair(Point<int>(26, 7), 'G'));
  points.push_back(std::make_pair(Point<int>( 3, 8), 'H'));
  points.push_back(std::make_pair(Point<int>(14, 9), 'I'));
  points.push_back(std::make_pair(Point<int>(35,10), 'J'));
  points.push_back(std::make_pair(Point<int>(20,11), 'K'));
  points.push_back(std::make_pair(Point<int>(17,12), 'L'));
  points.push_back(std::make_pair(Point<int>(25,13), 'M'));
  points.push_back(std::make_pair(Point<int>( 2,14), 'N'));
  points.push_back(std::make_pair(Point<int>(37,15), 'O'));
  points.push_back(std::make_pair(Point<int>(31,16), 'P'));
  points.push_back(std::make_pair(Point<int>(11,17), 'Q'));
  points.push_back(std::make_pair(Point<int>(36,18), 'R'));
  points.push_back(std::make_pair(Point<int>( 4,19), 'S'));
  points.push_back(std::make_pair(Point<int>(15,20), 'T'));
  points.push_back(std::make_pair(Point<int>(23,21), 'U'));


  // --------------------------------------------------------
  QuadTree<int,char> sorted_tree;
  for (int i = 0; i < points.size(); i++) {
    sorted_tree.insert(points[i].first,points[i].second);
  }
  std::cout << "\npoint collection:" << std::endl;
  sorted_tree.plot(40,22,false);

  std::cout << "\ninserted sorted by y coordinate:" << std::endl;
  sorted_tree.plot(40,22);
  std::cout << "before balancing this tree has height = " << sorted_tree.height() << std::endl;
  sorted_tree.print_sideways();

  // --------------------------------------------------------
  // Calculate a new order for these points
  BalanceTree(points);

  QuadTree<int,char> balanced_tree;
  std::cout << "\nnew insertion order to improve tree quality:" << std::endl;
  for (int i = 0; i < points.size(); i++) {
    balanced_tree.insert(points[i].first,points[i].second);
    std::cout << points[i].second << " " << points[i].first << std::endl;
  }
  std::cout << "\nresulting tree:" << std::endl;
  balanced_tree.plot(40,22);
  std::cout << "after balancing this tree has height = " << balanced_tree.height() << std::endl;
  balanced_tree.print_sideways();


  // --------------------------------------------------------
  std::cout << "\nFinished with extra_credit_test().\n" << std::endl;

}


// ==============================================================
// ==============================================================


void student_tests() {
  std::cout << "Beginning student_tests()..." << std::endl;

    //double char tree
    QuadTree<double, char> t;
    t.insert(Point<double>(7.2,7.2), 'A');
    t.insert(Point<double>(1.5,0), 'B');
    t.insert(Point<double>(8.4,0), 'C');
    t.insert(Point<double>(0,9.99999), 'D');
    t.insert(Point<double>(67.4839,92.3482),'E'); 

    t.plot(70, 95); 
    t.print_sideways();

    //float char tree
    QuadTree<float, char> t2;
    t2.insert(Point<float>(8.3,8.3), 'A');
    t2.insert(Point<float>(.5,.5), 'B');
    t2.insert(Point<float>(9.12121, 0.01), 'C');
    t2.insert(Point<float>(.5, 23.2), 'D');
    t2.insert(Point<float>(24.1,22.1), 'E');

    t2.plot(25,25);
    t2.print_sideways(); 

    //unsigned int tree
    QuadTree<unsigned int, char> t3;
    t3.insert(Point<unsigned int>(7.5,7.5), 'A');
    t3.insert(Point<unsigned int>(1,1.002), 'B');
    t3.insert(Point<unsigned int>(9,0.4), 'C');
    t3.insert(Point<unsigned int>(.913, 15), 'D');
    t3.insert(Point<unsigned int>(17,17), 'E');

    t3.plot(20,20);
    t3.print_sideways();

    //int label tree (ascii numbers)
    QuadTree<int, int> t4;
    t4.insert(Point<int>(7,7), 1);
    t4.insert(Point<int>(1,1), 2);
    t4.insert(Point<int>(8,1), 3);
    t4.insert(Point<int>(0,8), 4);
    t4.insert(Point<int>(8,8), 5);

    t4.plot(9,9);
    t4.print_sideways();

    //string label tree 
    QuadTree<int, string> t6;
    t6.insert(Point<int>(7,7), "APPLE");
    t6.insert(Point<int>(1,1), "BANANA");
    t6.insert(Point<int>(8,1), "CARROT");
    t6.insert(Point<int>(0,8), "DATE");
    t6.insert(Point<int>(8,8), "EGGPLANT");
    t6.insert(Point<int>(0,0), "BEAN");
    t6.insert(Point<int>(5,9), "DANISH");    

    t6.print_sideways();
    

    //testing insert all in one line
    QuadTree<int, char> t5;
    t5.insert(Point<int>(7,7), 'A'); 
    t5.insert(Point<int>(6,6), 'B'); 
    t5.insert(Point<int>(5,5), 'C'); 
    t5.insert(Point<int>(4,4), 'D'); 
    t5.insert(Point<int>(3,3), 'E'); 
    t5.insert(Point<int>(2,2), 'F'); 
    t5.insert(Point<int>(1,1), 'G'); 

    t5.plot(10,10);
    t5.print_sideways();


    //testing insert on negative points
    pair<QuadTree<int,char>::iterator,bool> p = t5.insert(Point<int>(-1,-1), 'A'); 
    //make sure at end
    assert(p.first == t5.end());
    //and was not inserted
    assert(p.second == false);


    //testing assignment operator
    QuadTree<int, char> test;
    test.insert(Point<int>(10,10), 'T');
    test.plot(20,20);
    test = t5;
    test.plot(20,20);

    //copy constructor on an empty tree
    QuadTree<int, char> empty;
    QuadTree<int, char> empty2(empty);
    //assignment operator on an empty tree
    QuadTree<int, char> empty3 = empty;
    //testing plot on empty
    empty.plot(10,10);
    empty2.plot(10,10);  
    empty3.plot(10,10);
    
    //testing destructor on empty
    empty.~QuadTree<int,char>(); 
    empty2.~QuadTree<int,char>();
    empty3.~QuadTree<int,char>();

  //
  // Add your own test cases here
  //
  // Be sure to test:
  //   * "corner" cases for insert, plotting, iteration, etc. 
  //   * additional tests of the copy constructor, assignment operator, & destructor
  //


  std::cout << "\nFinished with student_tests().\n" << std::endl;
}


// ==============================================================
// ==============================================================
