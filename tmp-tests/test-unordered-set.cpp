#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::plugins(cpp11)]]

#include <iostream>
#include <string>
#include <unordered_set>

// [[Rcpp::export]]
int test_unordered_set(CharacterVector strings) {

  int n = strings.size();
  std::unordered_set<int, std::string> myset(n);
  // myset.reserve(n);
  Rcout << myset.size() << std::endl;

  for (int i = 0; i < n; i++) {
    myset[i] = strings[i];
  }

  for (const std::string& x: myset) {
    Rcout << x << std::endl;
  }

  for (int i = 0; i < n; i++) {
    Rcout << i << ": " << myset[i] << std::endl;
  }

  return 0;
}


/*** R
test_unordered_set(paste(LETTERS, letters, sep = "_"))
*/
