/******************************************************************************/

#include <bigdfr/FDF.h>
using namespace Rcpp;

/******************************************************************************/

template<typename T, int RTYPE>
void fill(SEXP xptr, size_t j, Vector<RTYPE> vec,
          const IntegerVector& ind_row) {

  XPtr<FDF> xpDF(xptr);
  ColAcc<T> col(xpDF, j - 1);

  int n = ind_row.size();
  myassert(n == vec.size(), ERROR_DIM);

  for (int i = 0; i < n; i++) {
    col[ind_row[i]] = vec[i];
  }
}

/******************************************************************************/

// [[Rcpp::export]]
void fill_dbl(Environment X, size_t j, SEXP vec) {
  fill<double, REALSXP>(X["address"], j, vec, X["ind_row"]);
}

// [[Rcpp::export]]
void fill_int(Environment X, size_t j, SEXP vec) {
  fill<int, INTSXP>(X["address"], j, vec, X["ind_row"]);
}

// [[Rcpp::export]]
void fill_lgl(Environment X, size_t j, SEXP vec) {
  fill<int, LGLSXP>(X["address"], j, vec, X["ind_row"]);
}

/******************************************************************************/

// [[Rcpp::export]]
void fill_chr(Environment X, size_t j, IntegerVector vec) {

  XPtr<FDF> xpDF = X["address"];
  ColAcc<unsigned short> col(xpDF, j - 1);

  IntegerVector ind_row = X["ind_row"];
  int n = ind_row.size();
  myassert(n == vec.size(), ERROR_DIM);

  for (int i = 0; i < n; i++) {
    col[ind_row[i]] = vec[i] - 1;
  }
}

/******************************************************************************/

// [[Rcpp::export]]
void fill_fct(Environment X, size_t j, IntegerVector vec, IntegerVector match) {

  XPtr<FDF> xpDF = X["address"];
  ColAcc<unsigned short> col(xpDF, j - 1);

  IntegerVector ind_row = X["ind_row"];
  int n = ind_row.size();
  myassert(n == vec.size(), ERROR_DIM);

  for (int i = 0; i < n; i++) {
    col[ind_row[i]] = (vec[i] == NA_INTEGER) ? 0 : match[vec[i] - 1];
  }
}

/******************************************************************************/
