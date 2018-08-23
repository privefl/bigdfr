// [[Rcpp::depends(BH)]]
#include <boost/unordered_set.hpp>
#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector unique2(NumericVector x) {
  return unique(x);
}

// [[Rcpp::export]]
NumericVector unique3(NumericVector x) {

  boost::unordered_set<double> myset;
  myset.insert(x.begin(), x.end());

  NumericVector uniq(myset.size());
  std::copy(myset.begin(), myset.end(), uniq.begin());

  return uniq;
}

// [[Rcpp::export]]
NumericVector unique4(NumericVector x) {

  std::set<double> myset;
  myset.insert(x.begin(), x.end());

  NumericVector uniq(myset.size());
  std::copy(myset.begin(), myset.end(), uniq.begin());

  return uniq;
}

// [[Rcpp::export]]
IntegerVector unique2_int(IntegerVector x) {
  return unique(x);
}

// [[Rcpp::export]]
IntegerVector unique3_int(IntegerVector x) {

  boost::unordered_set<int> myset;
  myset.insert(x.begin(), x.end());

  IntegerVector uniq(myset.size());
  std::copy(myset.begin(), myset.end(), uniq.begin());

  return uniq;
}


/*** R
x <- sample(c(runif(10), NA), 1e5, TRUE)
bench::mark(
  unique(x),
  unique2(x),
  unique3(x),
  unique4(x),
  check = FALSE
)
unique3(x)
unique4(x)

x <- sample(c(seq_len(10), NA), 1e5, TRUE)
bench::mark(
  unique(x),
  unique2_int(x),
  unique3_int(x),
  check = FALSE
)
unique3_int(x)
*/
