#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::plugins(cpp11)]]
// [[Rcpp::depends(BH)]]
#include <boost/unordered_map.hpp>
#include <boost/unordered_set.hpp>

// [[Rcpp::export]]
List test_unordered_map(NumericVector vec) {

  boost::unordered_map<double, int> mymap;
  // mymap.reserve(n);
  // Rcout << mymap.size() << std::endl;

  boost::unordered_map<double, int>::iterator got;
  int n = vec.size();
  for (int i = 0; i < n; i++) {
    got = mymap.find(vec[i]);
    if (got == mymap.end()) {
      // Rcout << "Insert new elements." << std::endl;
      if (!ISNAN(vec[i])) mymap.insert(std::make_pair(vec[i], 1));
    } else {
      (got->second)++;
    }
  }
  // Rcout << mymap.size() << std::endl;

  int nk = mymap.size();
  NumericVector keys(nk + 2); keys[0] = NA_REAL; keys[1] = R_NaN;
  std::vector< std::vector<int> > ids(nk + 2);

  boost::unordered_map<double, int>::iterator it = mymap.begin(), end = mymap.end();
  int k = 2;
  while (it != end) {
    keys[k] = it->first;
    ids[k].reserve(it->second);
    it->second = k;
    it++; k++;
  }

  for (int i = 0; i < n; i++) {
    got = mymap.find(vec[i]);
    if (got != mymap.end()) {
      // Rcout << got->second << " ";
      ids[got->second].push_back(i);
    } else { // NA or NaN
      if (R_IsNA(vec[i])) {
        ids[0].push_back(i);
      } else {
        ids[1].push_back(i);
      }
    }
  }
  // Rcout << keys << std::endl;

  return List::create(_["keys"] = keys, _["ids"] = ids);
}

// [[Rcpp::export]]
boost::unordered_set<double> test_unordered_set(NumericVector vec) {

  boost::unordered_set<double> myset;

  int n = vec.size();
  for (int i = 0; i < n; i++) {
    myset.insert(vec[i]);
  }

  return myset;
}

/*** R
x <- c(sample(10, 100, TRUE), NA, NA, NaN)
tmp <- test_unordered_map(x)
tmp2 <- test_unordered_set(x)

split(0:102, x)

iris <- datasets::iris[rep(1:150, 1000), ]
microbenchmark::microbenchmark(
  dplyr::group_by(iris, Petal.Length),
  test_unordered_map(iris$Petal.Length),
  test_unordered_set(iris$Petal.Length),
  unique(iris$Petal.Length)
)
*/
