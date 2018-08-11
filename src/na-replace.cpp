#include <Rcpp.h>
using namespace Rcpp;

bool isna(double x_i) {
  return R_IsNA(x_i);
}
bool isna(int x_i) {
  return(x_i == NA_INTEGER);
}

template<int RTYPE>
Vector<RTYPE> na_replace(Vector<RTYPE> x, Vector<RTYPE> val) {

  size_t n = x.size();
  for (size_t i = 0; i < n; i++)
    if (isna(x[i])) x[i] = val[0];

  return x;
}

// [[Rcpp::export]]
SEXP NA_replace(SEXP x, SEXP by = 0) {   // x is modified directly (no copy)

  switch(TYPEOF(x)) {
  case INTSXP:  return na_replace<INTSXP> (x, by);
  case REALSXP: return na_replace<REALSXP>(x, by);
  default: stop("This type is not yet implemented.");
  }
}

/*** R
x <- c(1, NA, 2, 3)
x2 <- as.integer(x)
y <- NA_replace(x)
stopifnot(sum(is.na(x)) == 0)
stopifnot(is.integer(NA_replace(x2)))
stopifnot(sum(is.na(x2)) == 0)
*/
