

#include <fstream>
#include <iostream>
#include <vector>

// c++ advent-10.cpp && ./a.out
int main(int argc, char** argv) {
  std::ifstream input("advent-10.input");
  std::vector<uint8_t> lengths;
  int next;
  while (input >> next) {
    lengths.push_back(next); // silent conversion to smaller type!
    input.ignore(); // , separator
  }
  std::cerr << lengths.size() << std::endl;

  uint8_t knot[256];
  for (size_t i = 0; i < 256; i++) {
    knot[i] = i;
  }

  const uint8_t MASK = 0xff;
  uint8_t position = 0;
  uint8_t skip = 0;

  for (const auto length : lengths) {
    // int casts because uint8_t is treated as a char:-(
    std::cerr << (int)position << " " << (int)skip << " " << (int)length << std::endl;
    for (uint16_t from = position, to = (position + length - 1);
         from < to;
         from++, to--) {
      uint8_t tmp = knot[to & MASK];
      knot[to & MASK] = knot[from & MASK];
      knot[from & MASK] = tmp;
    }
    position += length;
    position += skip;
    skip++;
  }
  
  std::cerr << (knot[0] * knot[1]) << std::endl;

  return 0;
}
