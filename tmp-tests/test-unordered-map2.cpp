#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::plugins(cpp11)]]

// http://www.cplusplus.com/reference/unordered_map/unordered_map/reserve/
// unordered_map::reserve
#include <iostream>
#include <string>
#include <unordered_map>

// [[Rcpp::export]]
int test_unordered_map(CharacterVector strings) {

  int n = strings.size();
  std::unordered_map<int, std::string> mymap(n);
  // mymap.reserve(n);
  Rcout << mymap.size() << std::endl;

  for (int i = 0; i < n; i++) {
    mymap[i] = strings[i];
  }

  for (auto& x: mymap) {
    Rcout << x.first << ": " << x.second << std::endl;
  }

  for (int i = 0; i < n; i++) {
    Rcout << i << ": " << mymap[i] << std::endl;
  }

  return 0;
}


/*** R
test_unordered_map(paste(LETTERS, letters, sep = "_"))
*/
