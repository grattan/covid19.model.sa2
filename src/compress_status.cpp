#include "covid19model.h"
#include <Rcpp.h>
using namespace Rcpp;

int incubation_of(int statusi) {
  int incubation = statusi >> 7;
  incubation &= 255;
  return incubation;
}

int illness_of(int statusi) {
  int illness = statusi >> 15;
  illness &= 255;
  return illness;
}


