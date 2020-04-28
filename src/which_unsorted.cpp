#include "covid19model.h"
#include <Rcpp.h>
using namespace Rcpp;

int which_unsorted_int(IntegerVector x) {
  // Expect to be sorted so branches not an issue
  int n = x.length();
  for (int i = 1; i < n; ++i) {
    if (x[i - 1] > x[i]) {
      return i;
    }
  }
  return 0;
}


// [[Rcpp::export(rng = false)]]
bool do_is_unsorted_pint(IntegerVector x) {
  bool o = false;
  int n = x.size();
#pragma omp parallel for reduction(|| : o)
  for (int i = 1; i < n; ++i) {
    o = o || x[i - 1] > x[i];
  }
  return o;
}

