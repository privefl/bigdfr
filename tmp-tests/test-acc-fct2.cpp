#include <Rcpp.h>
using namespace Rcpp;

template <int RTYPE>
IntegerVector fast_factor_template( const Vector<RTYPE>& x ) {
  Vector<RTYPE> levs = sort_unique(x);
  IntegerVector out = match(x, levs);
  out.attr("levels") = as<CharacterVector>(levs);
  out.attr("class") = "factor";
  return out;
}

// [[Rcpp::export]]
SEXP fast_factor( SEXP x ) {
  switch( TYPEOF(x) ) {
  case INTSXP: return fast_factor_template<INTSXP>(x);
  case REALSXP: return fast_factor_template<REALSXP>(x);
  case STRSXP: return fast_factor_template<STRSXP>(x);
  }
  return R_NilValue;
}


/*** R
library(microbenchmark)
stopifnot(all.equal( factor( 1:10 ), fast_factor( 1:10 )))
stopifnot(all.equal( factor( letters ), fast_factor( letters )))
lets <- sample( letters, 1E5, replace=TRUE )
lets.int <- match(lets, letters)
microbenchmark( factor(lets),
                fast_factor(lets),
                factor(lets.int),
                fast_factor(lets.int))

xNA <- c(1:3, NA)
factor(xNA, exclude = NULL)
fast_factor(xNA)
fast_factor(c(runif(10), NA))
*/
