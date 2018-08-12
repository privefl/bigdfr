/******************************************************************************/

#include <bigdfr/FDF.h>
using namespace Rcpp;

/******************************************************************************/

template<typename T, int RTYPE>
Vector<RTYPE> extract(SEXP xptr, size_t j) {

  XPtr<FDF> xpDF(xptr);
  ColAcc<T> col(xpDF, j - 1);

  size_t n = col.nrow();
  Vector<RTYPE> vec(n);

  for (size_t i = 0; i < n; i++) {
    vec[i] = col[i];
  }

  return vec;
}

/******************************************************************************/

// [[Rcpp::export]]
SEXP extract_dbl(SEXP xptr, size_t j) {
  return extract<double, REALSXP>(xptr, j);
}

// [[Rcpp::export]]
SEXP extract_int(SEXP xptr, size_t j) {
  return extract<int, INTSXP>(xptr, j);
}

// [[Rcpp::export]]
SEXP extract_lgl(SEXP xptr, size_t j) {
  return extract<int, LGLSXP>(xptr, j);
}

// [[Rcpp::export]]
SEXP extract_ushort(SEXP xptr, size_t j) {
  return extract<unsigned short, INTSXP>(xptr, j);
}

/******************************************************************************/

// [[Rcpp::export]]
CharacterVector extract_string(SEXP xptr, size_t j, CharacterVector strings) {

  XPtr<FDF> xpDF(xptr);
  ColAcc<unsigned short> col(xpDF, j - 1);

  myassert(strings.size() == 65536, "'strings' should be of size 65536.");

  size_t n = col.nrow();
  CharacterVector vec(n);

  for (size_t i = 0; i < n; i++) {
    vec[i] = strings[col[i]];
  }

  return vec;
}

/******************************************************************************/
