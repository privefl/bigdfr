#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
std::vector< std::vector<int> > split_indices(IntegerVector x, IntegerVector ints, int n) {

  std::vector<std::vector<int> > ids(n);
  IntegerVector counts(n);

  int i, k, nx = x.size();

  // for (i = 0; i < nx; i++) {
  //   k = ints[x[i]];
  //   counts[k]++;
  // }

  for (k = 0; k < n; k++) {
    ids[k].reserve(nx * 1.2 / n);
  }

  for (i = 0; i < nx; i++) {
    k = ints[x[i]];
    ids[k].push_back(i);
  }

  return ids;
}


/*** R
x <- sample(c(3, 3, 3:5, 500), 1e7, TRUE)
x2 <- as.factor(x)
ints <- integer(501); ints[c(3:5, 500) + 1L] <- 0:3
str(res <- split_indices(x, ints, 4))
str(res2 <- split(seq_along(x2), x2))
microbenchmark::microbenchmark(
  split_indices(x, ints, 4),
  split(seq_along(x2), x2)
)
bench::mark(
  split_indices(x, ints, 4),
  split(seq_along(x2), x2),
  check = FALSE
)
*/
