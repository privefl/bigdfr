#include <bigdfr/FDF.h>
using namespace Rcpp;

#include <iostream>
#include <map>
using namespace std;

// [[Rcpp::export]]
void test_multimap(SEXP xptr, size_t j, IntegerVector ind_row) {

  XPtr<FDF> xpDF(xptr);
  ColAcc<double> col(xpDF, j - 1);

  std::multimap<double, int> mymm;

  int i, i2, n = ind_row.size();

  for (i = 0; i < n; i++) {
    i2 = ind_row[i];
    mymm.insert(std::make_pair(col[i2], i2));
  }

  std::multimap<double, int>::iterator it = mymm.begin(), end = mymm.end();
  for(; it != end; it++) {
    Rcout << it->first << '\t' << it->second << std::endl;
  }
}

typedef std::unordered_multimap<std::string,std::string> stringmap;

// [[Rcpp::export]]
int test_umm() {
  stringmap myumm = {
    {"orange","FL"},
    {"strawberry","LA"},
    {"strawberry","OK"},
    {"pumpkin","NH"}
  };

  Rcout << "Size: " << myumm.size() << std::endl;

  auto it = myumm.begin();
  // while (it != end)
  Rcout << it->first << "  //  " << it->second << std::endl;
  Rcout << "Size of range: " << myumm.count(it->first) << std::endl;

  Rcout << "Entries with strawberry: ";
  auto range = myumm.equal_range("strawberry");
  for_each (
      range.first,
      range.second,
      [](stringmap::value_type& x){Rcout << " " << x.second;}
  );
  Rcout << std::endl;

  auto it2 = range.second;
  Rcout << it2->first << "  //  " << it2->second << std::endl;

  return 0;
}

// [[Rcpp::plugins(cpp11)]]
// [[Rcpp::export]]
List test_multimap2(SEXP xptr, size_t j, IntegerVector ind_row) {

  XPtr<FDF> xpDF(xptr);
  ColAcc<double> col(xpDF, j - 1);

  std::unordered_multimap<double, int> myumm;

  int i, i2, n = ind_row.size();

  for (i = 0; i < n; i++) {
    i2 = ind_row[i];
    myumm.insert(std::make_pair(col[i2], i2));
  }

  vector<int> sizes; sizes.reserve(65536);

  auto it = myumm.begin(), end = myumm.end();
  while (it != end) {
    int size = myumm.count(it->first);
    sizes.push_back(size);
    for (int i = 0; i < size; i++, it++);
  }

  int K = sizes.size();
  NumericVector keys(K);
  List indices(K);

  double first;
  it = myumm.begin();
  for (int k = 0; k < K; k++) {
    first = it->first;
    keys[k] = first;
    int size = sizes[k];
    IntegerVector res_i(size);
    Rcout << "Size of range for " << first << " : "  << size << std::endl;
    Rcout << first << ": ";
    for (int i = 0; i < size; i++, it++) {
      res_i[i] = it->second;
    }
    Rcout << res_i << std::endl;
    indices[k] = res_i;
  }

  return List::create(_["key"] = keys, _["indices"] = indices);
}

/*** R
library(bigdfr)
test <- FDF(iris)
# test_multimap(test$address, 1, 0:149)
# test_umm()
test2 <- test_multimap2(test$address, 1, 0:149)
arrange(as_tibble(test2), key)
*/
