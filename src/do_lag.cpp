#include "covid19model.h"
#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
IntegerVector do_lag_int(IntegerVector s, int nThread = 1) {
  int n = s.length();

  IntegerVector lag_s = no_init(n);

#pragma omp parallel for num_threads(nThread)
  for (int i = 1; i < (n - 1); ++i) {
    int si = s[i];
    lag_s[i + 1] = si;
  }
  lag_s[0] = NA_INTEGER;
  lag_s[1] = s[0];
  return lag_s;
}
