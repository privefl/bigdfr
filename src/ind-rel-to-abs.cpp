#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
void rel_to_abs(ListOf<IntegerVector> list_ind_row,
                ListOf<IntegerVector> list_ind,
                IntegerVector sizes) {

  int i, j, k, l, K, L, N = sizes.size();
  for (i = 0, j = 0; i < N; i++) {
    IntegerVector ind_row = list_ind_row[i];
    K = sizes[i];
    for (k = 0; k < K; k++, j++) {
      IntegerVector ind = list_ind[j];
      L = ind.size();
      for (l = 0; l < L; l++) {
        ind[l] = ind_row[ind[l]];
      }
    }
  }
}
