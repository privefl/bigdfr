#include <bigdfr/FDF.h>
using namespace Rcpp;

// [[Rcpp::export]]
void fill_double(SEXP xptr, size_t j, NumericVector vec) {

  XPtr<FDF> xpDF(xptr);
  ColAcc<double> col(xpDF, j - 1);

  size_t n = col.nrow();
  myassert(n == size_t(vec.size()), ERROR_DIM);
  Rcout << n << std::endl;
  for (size_t i = 0; i < n; i++) {
    col[i] = vec[i];
  }
}

/*** R
test <- FDF(datasets::iris)
fill_double(test$address, 1, rep(1, 150))
readBin(test$backingfile, what = 0, n = 300)
fill_double(test$address, 2, rep(1:150))
readBin(test$backingfile, what = 0, n = 300)
*/
