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


