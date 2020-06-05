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
      Status[i] = -2;
      --dead;
      continue;
    }
    if (healed) {
      Status[i] = -1;
      --healed;
      continue;
    }
    if (critical) {
      Status[i] = 2;
      --critical;
      continue;
    }
    if (active) {
      Status[i] = 1;
      --active;
      continue;
    }
    Status[i] = 0;
  }
  return Status;
}


