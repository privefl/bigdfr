#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
CharacterVector unique_chr(CharacterVector x) {
  return sort_unique(x);
}

// [[Rcpp::export]]
IntegerVector match_chr(CharacterVector x, CharacterVector uniq) {
  return match(x, uniq);
}

/*** R
iris <- datasets::iris
species <- as.character(iris$Species)
species <- c(species[rep(1:150, 1000)], NA)
microbenchmark::microbenchmark(
  u1 <- unique(species),
  u2 <- unique_chr(species)
)
stopifnot(identical(u2, u1))

microbenchmark::microbenchmark(
  m1 <- match(species, u2),
  m2 <- match_chr(species, u2)
)
stopifnot(identical(m2, m1))
*/
