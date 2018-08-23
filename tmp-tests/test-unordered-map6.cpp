// [[Rcpp::depends(BH)]]
#include <boost/unordered_map.hpp>
#include <bigdfr/FDF.h>
using namespace Rcpp;

// [[Rcpp::export]]
List test_unordered_map(SEXP xptr, size_t j, ListOf<IntegerVector> list_ind_row) {

  XPtr<FDF> xpDF(xptr);
  ColAcc<double> col(xpDF, j - 1);

  int K = list_ind_row.size();
  List res(K);
  boost::unordered_map<double, int> mymap;
  boost::unordered_map<double, int>::iterator got, it, end;

  for (int k = 0; k < K; k++) {

    IntegerVector ind_row = list_ind_row[k];
    int n = ind_row.size();
    mymap.clear();

    for (int i = 0; i < n; i++) {
      std::size_t i2 = ind_row[i];
      got = mymap.find(col[i2]);
      if (got == mymap.end()) {
        if (!ISNAN(col[i2])) mymap.insert(std::make_pair(col[i2], 1));
      } else {
        (got->second)++;
      }
    }

    int nk = mymap.size();
    NumericVector keys(nk + 2); keys[0] = NA_REAL; keys[1] = R_NaN;
    std::vector< std::vector<int> > ids(nk + 2);

    it = mymap.begin();
    end = mymap.end();
    int l = 2;
    while (it != end) {
      keys[l] = it->first;
      ids[l].reserve(it->second);
      it->second = l;
      it++; l++;
    }

    for (int i = 0; i < n; i++) {
      std::size_t i2 = ind_row[i];
      got = mymap.find(col[i2]);
      if (got != mymap.end()) {
        ids[got->second].push_back(i2);
      } else { // NA or NaN
        if (R_IsNA(col[i2])) {
          ids[0].push_back(i2);
        } else {
          ids[1].push_back(i2);
        }
      }
    }

    res[k] = List::create(_["keys"] = keys, _["ids"] = ids);
  }

  return res;
}


/*** R
library(bigdfr)
test <- FDF(datasets::iris)
str(test_unordered_map(test$address, 1, list(0:49, 50:99, 100:149)))
*/
