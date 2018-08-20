/******************************************************************************/

#include <Rcpp.h>
#include <bigdfr/FDF.h>
#include <Rcpp.h>
using namespace Rcpp;

/******************************************************************************/

// http://gallery.rcpp.org/articles/fast-factor-generation/
template<int RTYPE>
List fast_factor_template(Vector<RTYPE> x) {
  Vector<RTYPE> levs = sort_unique(x);
  IntegerVector out = match(x, levs);
  return List::create(levs, out);
}

/******************************************************************************/

template<typename T, int RTYPE>
List extract_fct(SEXP xptr, size_t j, const IntegerVector& ind_row) {

  XPtr<FDF> xpDF(xptr);
  ColAcc<T> col(xpDF, j - 1);

  int n = ind_row.size();
  Vector<RTYPE> vec(n);

  for (int i = 0; i < n; i++) {
    vec[i] = col[ind_row[i]];
  }

  return fast_factor_template(vec);
}

/******************************************************************************/

// [[Rcpp::export]]
List extract_fct_dbl(SEXP xptr, size_t j, IntegerVector ind_row) {
  return extract_fct<double, REALSXP>(xptr, j, ind_row);
}

// [[Rcpp::export]]
List extract_fct_int(SEXP xptr, size_t j, IntegerVector ind_row) {
  return extract_fct<int, INTSXP>(xptr, j, ind_row);
}

// [[Rcpp::export]]
List extract_fct_ushort(SEXP xptr, size_t j, IntegerVector ind_row) {
  return extract_fct<unsigned short, INTSXP>(xptr, j, ind_row);
}

/******************************************************************************/
