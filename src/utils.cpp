#include "covid19model.h"
#include <Rcpp.h>
using namespace Rcpp;

int maxii(int a, int b) {
  return (a < b) ? b : a;
}
