#include <bigdfr/utils.h>
using namespace Rcpp;

// [[Rcpp::export]]
IntegerVector NA_to_0(SEXP x_) {

  myassert(TYPEOF(x_) == INTSXP, ERROR_REPORT);
  IntegerVector x(x_);

  size_t n = x.size();
  for (size_t i = 0; i < n; i++)
    if (x[i] == NA_INTEGER) x[i] = 0;

  return x;
}
