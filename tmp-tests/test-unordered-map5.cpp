// [[Rcpp::depends(BH)]]
#include <boost/unordered_map.hpp>
#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
void test_unordered_map(NumericVector vec) {

  boost::unordered_map<double, int> mymap;
  int n = vec.size();
  for (int i = 0; i < n; i++) {
    mymap.insert(std::make_pair(vec[i], i));
  }

  boost::unordered_map<double, int>::iterator it = mymap.begin(), end = mymap.end();
  while (it != end) {
    Rcout << it->first << "\t";
    it++;
  }
  Rcout << std::endl;
}

/*** R
x <- c(sample(10, 100, TRUE), rep(NA, 5), NaN)
test_unordered_map(x)
*/
