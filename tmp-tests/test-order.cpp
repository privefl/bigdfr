#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
IntegerVector order_dbl(NumericVector x) {
  NumericVector sorted = clone(x).sort();
  return match(sorted, x);
}

// [[Rcpp::export]]
IntegerVector order_int(IntegerVector x) {
  IntegerVector sorted = clone(x).sort();
  return match(sorted, x);
}

// [[Rcpp::export]]
IntegerVector order_chr(CharacterVector x) {
  CharacterVector sorted = clone(x).sort();
  return match(sorted, x);
}

/*** R
order_dbl(rnorm(10))

iris <- datasets::iris[rep(1:150, 10000), ]
x <- iris$Species
x2 <- as.character(x)

system.time(order(x))
system.time(order(x2))
system.time(order_int(x))
system.time(order_chr(x2))
*/
