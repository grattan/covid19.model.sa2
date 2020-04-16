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


