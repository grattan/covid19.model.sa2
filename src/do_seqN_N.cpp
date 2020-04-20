#include "covid19model.h"
#include <Rcpp.h>
using namespace Rcpp;


// [[Rcpp::export]]
List do_seqN_N(IntegerVector hid, IntegerVector pid, bool check_hid_sorted = true) {
  int N = hid.length();
  IntegerVector HouseholdSize = no_init(N);
  IntegerVector seqN = no_init(N);
  if (check_hid_sorted && which_unsorted_int(hid)) {
    stop("Internal error: hid was not sorted.");
  }

  seqN[0] = 1;

  // going down - +1 each time the hid stays the same
  // hid  1  1  1  2  2  3  4  4  4  4
  // 1st  1  2  3  1  2  1  1  2  3  4
  for (int i = 1; i < N; ++i) {
    seqN[i] = seqN[i - 1] * (hid[i] == hid[i - 1]) + 1;
  }
  HouseholdSize[N - 1] = seqN[N - 1];
  // so that the final
  for (int i = N - 1; i >= 0; --i) {
    // index j is just i for the last person (which records the household)
    // size; otherwise it is the 'next' person in the household (which
    // by induction must have recorded the household size too)

    // hid  1  1  1  2  2  3  4  4  4  4
    // 1st  1  2  3  1  2  1  1  2  3  4
    // 2nd                          4<-4  (first iter)
    //                           4<-4  4
    int j = i + (hid[i] == hid[i + 1]);
    HouseholdSize[i] = seqN[j];

  }
  return List::create(Named("seqN") = seqN, Named("N") = HouseholdSize);
}


