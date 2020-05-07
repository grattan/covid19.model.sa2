#include "covid19model.h"
#include <Rcpp.h>
using namespace Rcpp;


// [[Rcpp::export]]
NumericVector testOpenmp(NumericVector x, int nThread = 1) {
  int myArray[6] = {};

  if (x[0] == 4) {
    x[0] = 3;
    return x;
  }
  int nn = x.length();

#if _OPENMP >= 201511
#pragma omp parallel for num_threads(nThread) reduction(+:myArray[:6])
#endif
  for (int i=0; i<50; ++i)
  {
    double a = x[i % nn]; // Or something non-trivial justifying the parallelism...
    for (int n = 0; n<6; ++n)
    {
      myArray[n] += a;
    }
  }
  // Print the array elements to see them summed
  for (int n = 0; n<6; ++n)
  {
    Rcout << myArray[n] << " " <<  "\n";
  }
  return x;
}

//' @title count by
//' @name count_by
//' @description count by test
//' @param Group1,Group2,nThread  Input.
//'
//'
//' @export count_by_sa2_age_status

// [[Rcpp::export]]
IntegerVector count_by_sa2_age_status(IntegerVector Group1,
                                      IntegerVector Group2,
                                      IntegerVector Group3,
                                      int nThread = 1) {
  int gn = Group1.length();
  if (gn != Group2.length() ||
      gn != Group3.length()) {
    stop("Wrong length.");
  }
  IntegerVector MinMax1 = do_minmax_par(Group1, nThread);
  IntegerVector MinMax2 = do_minmax_par(Group2, nThread);
  IntegerVector MinMax3 = do_minmax_par(Group3, nThread);
  if (MinMax1[0] != 0 || MinMax1[1] != 2309 ||
      MinMax2[0] != 0 || MinMax2[1] != 100 ||
      MinMax3[0] != 0 || MinMax3[1] != 11) {
    stop("Min must be zero, max must be 2310, 100, 11.");
  }

  std::vector<int> out(2799720, 0);
  std::fill(out.begin(), out.end(), 0);

  for (int g = 0; g < gn; ++g) {
    int i = (Group1[g] * (101 * 12)) + Group2[g] * 12 + Group3[g];
    out[i] += 1;
  }
  IntegerVector out_r = no_init(2799720);

#pragma omp parallel for num_threads(nThread)
  for (int i = 0; i < 2799720; ++i) {
    out_r[i] = out[i];
  }

  return out_r;
}
