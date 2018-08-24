/******************************************************************************/

#include <Rcpp.h>
using namespace Rcpp;

/******************************************************************************/

// [[Rcpp::export]]
IntegerVector match_int(IntegerVector x, IntegerVector table) {
  return Rcpp::match(x, table);
}

/******************************************************************************/

// [[Rcpp::export]]
CharacterVector unique_chr(CharacterVector x) {
  return Rcpp::sort_unique(x);
}

// [[Rcpp::export]]
IntegerVector match_chr(CharacterVector x, CharacterVector uniq) {
  return Rcpp::match(x, uniq);
}

/******************************************************************************/
