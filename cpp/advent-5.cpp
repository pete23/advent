

#include <fstream>
#include <iostream>
#include <vector>

// c++ advent-5.cpp && ./a.out
int main(int argc, char** argv) {
  std::ifstream input("advent-5.input");
  std::vector<int> arena;
  int next;
  while (input >> next) {
    arena.push_back(next);
  }
  std::cerr << arena.size() << std::endl;

  int index = 0;
  int n = 0;
  while (index >= 0 && index < arena.size()) {
    // advent 5-1: index += arena[index]++;
    // advent 5-2:
    int offset = arena[index];
    if (offset > 2) {
      arena[index]--;
    } else {
      arena[index]++;
    }
    index += offset;
    
    n++;
  }

  std::cerr << n << std::endl;

  return 0;
}
