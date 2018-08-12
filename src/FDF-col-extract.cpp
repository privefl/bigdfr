/******************************************************************************/

#include <bigdfr/FDF.h>
using namespace Rcpp;

/******************************************************************************/

template<typename T, int RTYPE>
Vector<RTYPE> extract(SEXP xptr, size_t j, const IntegerVector& ind_row) {

  XPtr<FDF> xpDF(xptr);
  ColAcc<T> col(xpDF, j - 1);

  int n = ind_row.size();
  Vector<RTYPE> vec(n);

  for (int i = 0; i < n; i++) {
    vec[i] = col[ind_row[i]];
  }

  return vec;
}

/******************************************************************************/

// [[Rcpp::export]]
SEXP extract_dbl(Environment X, size_t j) {
  return extract<double, REALSXP>(X["address"], j, X["ind_row"]);
}

// [[Rcpp::export]]
SEXP extract_int(Environment X, size_t j) {
  return extract<int, INTSXP>(X["address"], j, X["ind_row"]);
}

// [[Rcpp::export]]
SEXP extract_lgl(Environment X, size_t j) {
  return extract<int, LGLSXP>(X["address"], j, X["ind_row"]);
}

// [[Rcpp::export]]
SEXP extract_ushort(Environment X, size_t j) {
  return extract<unsigned short, INTSXP>(X["address"], j, X["ind_row"]);
}

/******************************************************************************/

// [[Rcpp::export]]
CharacterVector extract_string(Environment X, size_t j, CharacterVector strings) {

  XPtr<FDF> xpDF = X["address"];
  ColAcc<unsigned short> col(xpDF, j - 1);

  myassert(strings.size() == 65536, "'strings' should be of size 65536.");

  IntegerVector ind_row = X["ind_row"];
  int n = ind_row.size();
  CharacterVector vec(n);

  for (int i = 0; i < n; i++) {
    vec[i] = strings[col[ind_row[i]]];
  }

  return vec;
}

/******************************************************************************/
