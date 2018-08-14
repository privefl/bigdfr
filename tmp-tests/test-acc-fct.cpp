#include <bigdfr/FDF.h>
using namespace Rcpp;

// [[Rcpp::export]]
IntegerVector extract_fct(Environment X, size_t j, IntegerVector values) {

  XPtr<FDF> xpDF = X["address"];
  ColAcc<unsigned short> col(xpDF, j - 1);

  // myassert(strings.size() == 65536, "'strings' should be of size 65536.");

  IntegerVector ind_row = X["ind_row"];
  int n = ind_row.size();
  IntegerVector vec(n);

  for (int i = 0; i < n; i++) {
    vec[i] = values[col[ind_row[i]]];
  }

  return vec;
}

/*** R
library(bigdfr)
test <- FDF(iris)
(lvl <- sample(levels(iris$Species)))
values <- integer(); values[match(lvl, test$strings)] <- seq_along(lvl)
structure(extract_fct(test, 5, values), levels = lvl, class = "factor")
*/
