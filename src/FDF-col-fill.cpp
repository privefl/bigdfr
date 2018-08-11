/******************************************************************************/

#include <bigdfr/FDF.h>
using namespace Rcpp;

/******************************************************************************/

template<typename T, int RTYPE>
void fill(SEXP xptr, size_t j, Vector<RTYPE> vec) {

  XPtr<FDF> xpDF(xptr);
  ColAcc<T> col(xpDF, j - 1);

  size_t n = col.nrow();
  myassert(n == size_t(vec.size()), ERROR_DIM);

  for (size_t i = 0; i < n; i++) {
    col[i] = vec[i];
  }
}

/******************************************************************************/

// [[Rcpp::export]]
void fill_double(SEXP xptr, size_t j, SEXP vec) {
  fill<double, REALSXP>(xptr, j, vec);
}

// [[Rcpp::export]]
void fill_int(SEXP xptr, size_t j, SEXP vec) {
  fill<int, INTSXP>(xptr, j, vec);
}

// [[Rcpp::export]]
void fill_ushort(SEXP xptr, size_t j, SEXP vec) {
  fill<unsigned short, INTSXP>(xptr, j, vec);
}

/******************************************************************************/
