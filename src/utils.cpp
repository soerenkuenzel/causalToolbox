#include <vector>
#include <string>
#include <iostream>

void print_vector(
  std::vector<size_t> v
){
  for (auto i = v.begin(); i != v.end(); ++i)
    std::cout << *i << ' ';
  std::cout << std::endl;
}
