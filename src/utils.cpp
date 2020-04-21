#include "covid19model.h"
#include <Rcpp.h>
using namespace Rcpp;

int maxii(int a, int b) {
  return (a < b) ? b : a;
}

int max0(int x) {
  return (x > 0) ? x : 0;
}

int max0(double x) {
  return (x > 0) ? x : 0;
}
