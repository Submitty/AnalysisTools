#include <ctime>
#include <cassert>
#include <iostream>
#include <fstream>
#include <string>
#include <vector>
#include <list>
#include <set>
#include <map>
#include <queue>
#include <unordered_set>
#include <unordered_map>
#include <algorithm>


// ================================================================= 
// ================================================================= 




// Info on command line arguments
void usage() {
    std::cerr << "Usage: <executable_name> <data structure> <operation> <input file> <output file>" << std::endl;
    std::cerr << "Usage: <executable_name> <data structure> <operation> random <input count> <string length> <output file>" << std::endl;
    std::cerr << "Usage:    <data structure> = vector, list, bst, priority_queue, hash_table" << std::endl;
    std::cerr << "Usage:    <operation>      = sort, remove_dups, mode" << std::endl;
    exit(1);
}


// prototypes of the functions that perform the operations
void vector_test(const std::string* input_data, int input_count, const std::string &operation, std::string *output_data, int &output_count);
void list_test(const std::string* input_data, int input_count, const std::string &operation, std::string *output_data, int &output_count);
void bst_test(const std::string* input_data, int input_count, const std::string &operation, std::string *output_data, int &output_count);
void priority_queue_test(const std::string* input_data, int input_count, const std::string &operation, std::string *output_data, int &output_count);
void hash_table_test(const std::string* input_data, int input_count, const std::string &operation, std::string *output_data, int &output_count);


// =================================================================
// =================================================================

// Create a random string of the specified length for testing
std::string random_string(int length) {
  std::string s = "";
  for (int i = 0; i < length; i++) {
    char c = 'a' + rand()%26;
    s += c;
  }
  return s;
}


// Load the data either from a file, or generate random data
void load_data(const std::string &input, std::string*& input_data, int& input_count, int string_length) {

  if (input == "random") {
    // seed the random number generator
    // using a fixed seed for repeatable tests for debugging
    srand(37);
    // using the time as a seed
    //srand(time(0));
    input_data = new std::string[input_count];
    for (int i = 0; i < input_count; i++) 
      input_data[i] = random_string(string_length);

  } else {
    // load the file once to get the count
    std::ifstream istr(input.c_str());
    if (!istr) {
      std::cerr << "Error: Can't open input file: " << input << std::endl;
      exit(0);
    }
    std::string s;
    input_count = 0;
    while (istr >> s) {
      input_count++;
    }
    // make an array exactly the right size
    input_data = new std::string[input_count];
    // close & reopen & reread the file to store the data
    istr.close();
    istr.open(input.c_str());
    for (int i = 0; i < input_count; i++) {
      istr >> s;
      input_data[i] = s;
    }
  }
}

// =================================================================
// =================================================================

int main(int argc, char* argv[]) {

  // timing mechanism
  clock_t start, before_operation, after_operation, end;
  start = clock();

  // parse the command line arguments
  if (argc != 5 && argc != 7) {
    std::cerr << "Error: wrong number of arguments." << std::endl;
    usage();
  }
  std::string data_structure = argv[1];
  std::string operation = argv[2];
  std::string input = argv[3];
  int input_count = -1;
  int string_length = -1;
  std::string output_file;
  if (input == "random") {
    assert (argc == 7);
    input_count = atoi(argv[4]);
    string_length = atoi(argv[5]);
    output_file = argv[6];
  } else {
    assert (argc == 5);
    output_file = argv[4];
  } 

  // load the data into a heap-allocated, C-style array
  std::string *input_data;
  load_data(input,input_data,input_count,string_length);

  // prepare space for the answer (at most the same size as the input!)
  std::string *output_data = new std::string[input_count];
  int output_count;

  // mark the time before we start
  before_operation = clock();

  // perform the operation
  if (data_structure == "vector") 
    vector_test(input_data,input_count,operation,output_data,output_count);
  else if (data_structure == "list")
    list_test(input_data,input_count,operation,output_data,output_count);
  else if (data_structure == "bst")  // STL set or map
    bst_test(input_data,input_count,operation,output_data,output_count);
  else if (data_structure == "priority_queue")
    priority_queue_test(input_data,input_count,operation,output_data,output_count);
  else if (data_structure == "hash_table")  // STL unordered_set or unordered_map
    hash_table_test(input_data,input_count,operation,output_data,output_count);
  else {
    std::cerr << "Error: unknown data structure: " << data_structure << std::endl;
    usage();
    exit(0);
  }

  // mark the time once we are done
  after_operation = clock();

  // output the data
  std::ofstream ostr(output_file.c_str());
  for (int i = 0; i < output_count; i++) {
    ostr << output_data[i] << std::endl;
  }
  // cleanup
  delete [] input_data;
  delete [] output_data;

  // print statistics
  end = clock();
  double load_time = double(before_operation-start)/CLOCKS_PER_SEC;
  double operation_time = double(after_operation-before_operation)/CLOCKS_PER_SEC;
  double output_time = double(end-after_operation)/CLOCKS_PER_SEC;
  std::cout << "load time:          " << load_time << std::endl;
  std::cout << "operation time:     " << operation_time << std::endl;
  std::cout << "output time:        " << output_time << std::endl;
  std::cout << "input/output ratio: " << input_count << ":" << output_count << std::endl;
  return 0;
}

// =================================================================

void vector_test(const std::string* input_data, int input_count, const std::string &operation, std::string *output_data, int &output_count) {

  // for all cases, simply put the data into a vector
  std::vector<std::string> vec;
  for (int i = 0; i < input_count; i++) 
    vec.push_back(input_data[i]);
  
  if (operation == "sort") {
    // use the vector sort algorithm
    sort(vec.begin(),vec.end());
    output_count = input_count;
    for (int i = 0; i < output_count; i++)
      output_data[i] = vec[i];

  } else if (operation == "remove_dups") {
    // don't reorder the elements, just do all pairwise comparisons
    output_count = 0;
    for (int i = 0; i < input_count; i++) {
      bool dup = false;
      for (int j = 0; j < output_count; j++) {
	if (vec[i] == output_data[j]) {
	  dup = true;
	  break;
	}
      }
      // if it has not already been added to the output list
      if (!dup) {
	output_data[output_count] = input_data[i];
	output_count++;
      }
    }

  } else if (operation == "mode") {
    // use the vector sort algorithm
    sort(vec.begin(),vec.end());
    int current_count = 1;
    std::string mode;
    int mode_count = 0;
    // keep track of two pointers into the structure
    std::vector<std::string>::iterator current = vec.begin();
    ++current;
    std::vector<std::string>::iterator previous = vec.begin();
    for (; current != vec.end(); ++current, ++previous) {
      if (*current == *previous) {
	// if they are the same increment the count
        current_count++;
      } else if (current_count >= mode_count) {
        // found a new mode!
        mode = *previous;
        mode_count = current_count;
        current_count = 1;
      } else {
        current_count = 1;
      }
    }
    if (current_count >= mode_count) {
      // last entry is a new mode!
      mode = *previous;
      mode_count = current_count;
    }
    // save the mode to the output vector
    output_count = 1;
    output_data[0] = mode;
  } else {
    std::cerr << "Error: Unknown operation: " << operation << std::endl;
    usage();
    exit(0);
  }
}

// =================================================================

void list_test(const std::string* input_data, int input_count, const std::string &operation, std::string *output_data, int &output_count) {

  // HINT: declare your list like this:
  std::list<std::string> lst;

  std::cerr << "Error: DATA STRUCTURE NOT IMPLEMENTED" << std::endl;
  exit(0);

  if (operation == "sort") {
  } else if (operation == "remove_dups") {
  } else if (operation == "mode") {
  } else {
    std::cerr << "Error: Unknown operation: " << operation << std::endl;
    usage();
  }
}

// =================================================================

void bst_test(const std::string* input_data, int input_count, const std::string &operation, std::string *output_data, int &output_count) {

  // HINT: declare your binary search tree (BST) like this:
  // std::set<std::string> st;
  // OR 
  // std::map<std::string,int> mp;

  std::cerr << "Error: DATA STRUCTURE NOT IMPLEMENTED" << std::endl;
  exit(0);

  if (operation == "sort") {
  } else if (operation == "remove_dups") {
  } else if (operation == "mode") {
  } else {
    std::cerr << "Error: Unknown operation: " << operation << std::endl;
    usage();
  }
}

// =================================================================

void priority_queue_test(const std::string* input_data, int input_count, const std::string &operation, std::string *output_data, int &output_count) {

  // HINT: declare your priority_queue like this:
  // std::priority_queue<std::string,std::vector<std::string> > pq(input_data,input_data+input_count);
  
  std::cerr << "Error: DATA STRUCTURE NOT IMPLEMENTED" << std::endl;
  exit(0);

  if (operation == "sort") {
  } else if (operation == "remove_dups") {
  } else if (operation == "mode") {
  } else {
    std::cerr << "Error: Unknown operation: " << operation << std::endl;
    usage();
  }
}

// =================================================================

void hash_table_test(const std::string* input_data, int input_count, const std::string &operation, std::string *output_data, int &output_count) {

  // HINT: declare your hash table like this:
  // std::unordered_set<std::string> ht(input_count);
  // OR 
  // std::unordered_map<std::string,int> ht(input_count);

  std::cerr << "Error: DATA STRUCTURE NOT IMPLEMENTED" << std::endl;
  exit(0);

  if (operation == "sort") {
  } else if (operation == "remove_dups") {
  } else if (operation == "mode") {
  } else {
    std::cerr << "Error: Unknown operation: " << operation << std::endl;
    usage();
  }
}

// =================================================================
