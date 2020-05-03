#include "covid19model.h"
#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
IntegerVector postcode_to_sa2_sorted(IntegerVector postcode,
                                     IntegerVector POSTCODE,
                                     IntegerVector SA2_MAINCODE) {
  int N = postcode.length();
  int tn = POSTCODE.length();
  if (tn != SA2_MAINCODE.length()) {
    stop("tn != SA2_MAINCODE.length()");
  }
  IntegerVector out = no_init(N);

  int t = 0;
  int i = 0;
  while (++i < N && t < tn) {
    if (postcode[i] < POSTCODE[0] || postcode[i] > POSTCODE[tn - 1]) {
      out[i] = NA_INTEGER;
      continue;
    }
    if (postcode[i] == POSTCODE[t]) {
      out[i] = SA2_MAINCODE[t];
      continue;
    }
    ++t;
    --i;
  }
  if (i < N) {
    stop("Internal error.");
  }

  return out;
}

