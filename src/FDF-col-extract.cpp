/******************************************************************************/

#include <bigdfr/FDF.h>
using namespace Rcpp;

/******************************************************************************/

template<typename T, int RTYPE>
Vector<RTYPE> extract(XPtr<FDF> xpDF, size_t j) {

  ColAcc<T> col(xpDF, j - 1);

  size_t n = col.nrow();
  Vector<RTYPE> vec(n);

  for (size_t i = 0; i < n; i++) {
    vec[i] = col[i];
  }

  return vec;
}

// [[Rcpp::export]]
SEXP extract_numeric(SEXP xptr, size_t j) {

  XPtr<FDF> xpDF(xptr);
  switch(xpDF->column_type(j - 1)) {
  case 8: return extract<double, REALSXP>(xpDF, j);
  case 4: return extract<int, INTSXP>(xpDF, j);
  case 2: return extract<unsigned short, INTSXP>(xpDF, j);
  default: stop(ERROR_REPORT);
  }
}

/******************************************************************************/

// [[Rcpp::export]]
CharacterVector extract_string(SEXP xptr, size_t j, CharacterVector strings) {

  XPtr<FDF> xpDF(xptr);
  ColAcc<unsigned short> col(xpDF, j - 1);

  myassert(col.type() == 2, ERROR_REPORT);
  myassert(strings.size() == 65536, "'strings' should be of size 65536.");

  size_t n = col.nrow();
  CharacterVector vec(n);

  for (size_t i = 0; i < n; i++) {
    vec[i] = strings[col[i]];
  }

  return vec;
}

/******************************************************************************/
