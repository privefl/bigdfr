#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
IntegerVector NA_to_0(IntegerVector x) {

  size_t n = x.size();
  for (size_t i = 0; i < n; i++)
    if (x[i] == NA_INTEGER) x[i] = 0;

  return x;
}

/*** R
x <- c(1:3, NA)
y <- NA_to_0(x)
stopifnot(sum(is.na(x)) == 0)
*/
