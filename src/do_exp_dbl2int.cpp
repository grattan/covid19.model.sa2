#include "covid19model.h"
#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export(rng = false)]]
IntegerVector do_exp_dbl2int(DoubleVector x, int nThread = 1) {
  int n = x.length();
  IntegerVector out = no_init(n);
#pragma omp parallel for num_threads(nThread)
  for (int i = 0; i < n; ++i) {
    out[i] = (int)(exp(x[i]));
  }
  return out;
}
