#include "covid19model.h"
#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
IntegerVector distr2status(int N,
                           int dead,
                           int healed,
                           int active,
                           int critical) {
  IntegerVector Status = no_init(N);
  for (int i = 0; i < N; ++i) {
    if (dead) {
      Status[i] = STATUS_KILLED;
      --dead;
      continue;
    }
    if (healed) {
      Status[i] = STATUS_HEALED;
      --healed;
      continue;
    }
    if (critical) {
      Status[i] = STATUS_CRITIC;
      --critical;
      continue;
    }
    if (active) {
      Status[i] = STATUS_INSYMP;
      --active;
      continue;
    }
    Status[i] = 0;
  }
  return Status;
}


