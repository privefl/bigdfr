/******************************************************************************/

#include <bigdfr/FDF.h>
using namespace Rcpp;

/******************************************************************************/

template<typename T, int RTYPE>
List extract(SEXP xptr, size_t j, ListOf<IntegerVector> list_ind_row) {

  XPtr<FDF> xpDF(xptr);
  ColAcc<T> col(xpDF, j - 1);

  int K = list_ind_row.size();
  List res(K);

  for (int k = 0; k < K; k++) {

    IntegerVector ind_row = list_ind_row[k];
    int n = ind_row.size();
    Vector<RTYPE> vec(n);

    for (int i = 0; i < n; i++) {
      vec[i] = col[ind_row[i]];
    }

    res[k] = vec;
  }

  return res;
}

/******************************************************************************/

// [[Rcpp::export]]
SEXP extract_dbl(SEXP xptr, size_t j, ListOf<IntegerVector> list_ind_row) {
  return extract<double, REALSXP>(xptr, j, list_ind_row);
}

// [[Rcpp::export]]
SEXP extract_int(SEXP xptr, size_t j, ListOf<IntegerVector> list_ind_row) {
  return extract<int, INTSXP>(xptr, j, list_ind_row);
}

// [[Rcpp::export]]
SEXP extract_lgl(SEXP xptr, size_t j, ListOf<IntegerVector> list_ind_row) {
  return extract<int, LGLSXP>(xptr, j, list_ind_row);
}

// [[Rcpp::export]]
SEXP extract_ushort(SEXP xptr, size_t j, ListOf<IntegerVector> list_ind_row) {
  return extract<unsigned short, INTSXP>(xptr, j, list_ind_row);
}

/******************************************************************************/

// [[Rcpp::export]]
List extract_string(SEXP xptr, size_t j,
                    ListOf<IntegerVector> list_ind_row,
                    CharacterVector strings) {

  XPtr<FDF> xpDF(xptr);
  ColAcc<unsigned short> col(xpDF, j - 1);

  myassert(strings.size() == 65536, "'strings' should be of size 65536.");

  int K = list_ind_row.size();
  List res(K);

  for (int k = 0; k < K; k++) {

    IntegerVector ind_row = list_ind_row[k];
    int n = ind_row.size();
    CharacterVector vec(n);

    for (int i = 0; i < n; i++) {
      vec[i] = strings[col[ind_row[i]]];
    }

    res[k] = vec;
  }

  return res;
}

/******************************************************************************/
