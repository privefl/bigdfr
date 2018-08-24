#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
IntegerVector match_int(IntegerVector x, IntegerVector table) {
  return match(x, table);
}

/*** R
iris <- datasets::iris
x <- sample(1e5)
y <- sample(1e5)
microbenchmark::microbenchmark(
  m1 <- match(x, y),
  m2 <- match_int(x, y)
)
stopifnot(identical(m2, m1))
match_int(c(1:3, NA, 5), 1:4)
*/
