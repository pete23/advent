

#include <fstream>
#include <iostream>
#include <iomanip>
#include <vector>

// TODO: refactor

// c++ advent-10.cpp && ./a.out
int main(int argc, char** argv) {
  std::ifstream input("advent-10.input");

  std::vector<uint8_t> lengths;

  // for part two we want the actual bytes
  uint8_t next;
  while (input >> next) {
    lengths.push_back(next);
  }
  std::cerr << lengths.size() << std::endl;

  // add these constants to every message...
  std::vector<uint8_t> constants = { 17, 31, 73, 47, 23 };
  for (const auto constant : constants) {
    lengths.push_back(constant);
  }

  // initialise the knot
  uint8_t knot[256];
  for (size_t i = 0; i < 256; i++) {
    knot[i] = i;
  }

  const uint8_t MASK = 0xff;
  uint8_t position = 0;
  uint8_t skip = 0;

  for (int i = 0; i < 64; i++) {
    for (const auto length : lengths) {
      // int casts because uint8_t is treated as a char:-(
      // std::cerr << (int)position << " " << (int)skip << " " << (int)length << std::endl;
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
  }

  uint8_t dense_hash[16];
  int hash_position = 0;
  for (int i = 0; i < 16; i++) {
    dense_hash[i] = 0;
    for (int j = 0; j < 16; j++) {
      dense_hash[i] ^= knot[hash_position++];
    }
    std::cerr << std::hex << std::setfill('0') << std::setw(2) << (int)dense_hash[i];
  }
  
  std::cerr << std::endl;

  return 0;
}
