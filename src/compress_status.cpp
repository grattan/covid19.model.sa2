#include "covid19model.h"
#include <Rcpp.h>
using namespace Rcpp;

bool is_isolated(IntegerVector Status, int i) {
  return Status[i] >> 5;
}

void isolate_status(IntegerVector Status, int i) {
  if (!is_isolated(Status, i) && Status[i] >= 0) {
    Status[i] += 32;
  }
}

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


