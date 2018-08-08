#include <bigdfr/FDF.h>
#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
void fill_double(SEXP xptr, size_t j) {

  XPtr<FDF> xpDF(xptr);
  ColAcc<double> col(xpDF, j - 1);

  size_t n = col.nrow();
  Rcout << n << std::endl;
  for (size_t i = 0; i < n; i++) {
    col[i] = 1;
  }
}

/*** R
test <- FDF(datasets::iris)
fill_double(test$address, 1)
readBin(test$backingfile, what = 0, n = 300)
*/
