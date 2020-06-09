#include "covid19model.h"
#include <Rcpp.h>
using namespace Rcpp;

void assert_is_weekday(const int & day) {
  if (day == 0) {
    if (yday2weekday(138) || !yday2weekday(139)) {
      stop("yday2weekday returning incorrect results for yday = 138 or 139.");
    }
  }
}

void assert_is_monday(const int & day) {
  if (day == 0 && !yday2monday(139)) {
    stop("yday2monday(139) not true.");
  }
}

// [[Rcpp::export]]
CharacterVector InstallTime() {
  CharacterVector out(1);
  std::string tempo1 = __TIME__;
  out[0] = tempo1;
  return out;
}

