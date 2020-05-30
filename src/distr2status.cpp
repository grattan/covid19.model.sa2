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


// [[Rcpp::export]]
List fifo_status(IntegerVector CumCases,
                 IntegerVector Healed,
                 IntegerVector Killed,
                 int first_day = 25) {
  int ncc = CumCases.size();
  int nh = Healed.size();
  int nk = Killed.size();
  if (nk != nh || ncc != nk) {
    // Rcerr << nk << " " << nh << " " << nh << "\n";
    stop("Lengths differ.");
  }
  int Pop = CumCases[ncc - 1];
  int first_yday= Healed[0];
  int final_yday = Healed[nh - 1];
  IntegerVector Active = no_init(nh);
  IntegerVector InfectedOn = no_init(Pop);
  IntegerVector ConcludedOn = no_init(Pop);
  IntegerVector NewCases = no_init(ncc);
  IntegerVector ConcludedCases = no_init(ncc);
  NewCases[0] = CumCases[0];
  ConcludedCases[0] = 0;

  // make monotonic
  for (int i = 1; i < ncc; ++i) {
    if (CumCases[i] < CumCases[i - 1]) {
      CumCases[i] = CumCases[i - 1];
    }
    if (Healed[i] < Healed[i - 1]) {
      Healed[i] = Healed[i - 1];
    }
    if (Killed[i] < Killed[i - 1]) {
      Killed[i] = Killed[i - 1];
    }
  }



  for (int i = 1; i < ncc; ++i) {
    NewCases[i] = CumCases[i] - CumCases[i - 1];
    ConcludedCases[i] = (Killed[i] - Killed[i - 1]) + (Healed[i] - Healed[i - 1]);
  }

  int n_active = CumCases[0];
  int n_healed = Healed[0];
  int n_killed = Killed[0];

  int i_InfectedOn = 0;
  int i_ConcludedOn = 0;

  for (int day = 0; day < ncc; ++day) {

    int concluded_today = ConcludedCases[day];
    int active_today = NewCases[day];
    while (concluded_today > 0) {
      if (i_ConcludedOn >= Pop) {
        Rcerr << day << " " << i_ConcludedOn << " " << Pop << "\n";
        stop("i_ConcludedOn >= Pop");
      }
      ConcludedOn[i_ConcludedOn] = first_day + day;
      --concluded_today;
      ++i_ConcludedOn;
    }
    while (active_today > 0) {
      if (i_InfectedOn >= Pop) {
        Rcerr << day << " " << i_InfectedOn << " " << Pop << "\n";
        stop("i_InfectedOn >= Pop");
      }
      InfectedOn[i_InfectedOn] = first_day + day;
      --active_today;
      ++i_InfectedOn;
    }
  }
  while (i_ConcludedOn < Pop) {
    ConcludedOn[i_ConcludedOn] = NA_INTEGER;
    ++i_ConcludedOn;
  }



  return List::create(Named("InfectedOn") = InfectedOn,
                      Named("ConcludedOn") = ConcludedOn);
}


