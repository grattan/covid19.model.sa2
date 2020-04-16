#include "covid19model.h"
#include <Rcpp.h>
using namespace Rcpp;

#define N_SUPERMARKETS 7487


// Just a way to get quasi-cauchy distributed nonnegative integer vector
IntegerVector rcauchy_int(int N, double l, double s) {
  NumericVector o = Rcpp::rcauchy(N, l, s);
  IntegerVector out = no_init(N);
  for (int i = 0; i < N; ++i) {
    double oi = o[i];
    out[i] = 1;
    if (oi < -1) {
      out[i] = 0;
    } else if (oi > 1000) {
      out[i] = 1000;
    } else {
      // won't be NaN and is now definitely in range.
      out[i] = (int)(oi);
    }
  }
  return out;
}

//' @name do_1day_supermarket
//' @description Simulate the experience of everyone's interactions
//' at supermarkets in a single day
//' @param Status \code{integer(N)}: Whether each individual is infected or not etc.
//' @param SA2 The 2016 SA2 code of each individual. Must be sorted.
//' @param Age The age of every individual.
//' @param Employment Information about employment of each individual.
//' @param SupermarketTarget The supermarket that the person visits,
//' equal to zero for each individual who doesn't visit a supermarket.
//' @param Resistance For each individual, inherent resistance to being infected.
//' @param CauchyM A pool of random numbers integers cauchy distributed,
//' the contagiousness of the individual if infected and an epheremal
//' element of resistance if not.
//' @param N \code{int} Population of Australia.
//' @param check_sa2_key \code{bool} Whether to check SA2 is sorted, defaults to \code{true}.
//' @noRd

IntegerVector do_1day_supermarket(IntegerVector Status,
                                  IntegerVector SA2,
                                  IntegerVector Age,
                                  IntegerVector Employment,
                                  IntegerVector SupermarketTarget,
                                  IntegerVector Resistance,
                                  IntegerVector CauchyM,
                                  int N = 25e6,
                                  bool check_sa2_key = true) {
  // Idea: First, count number of people and infected people
  // who visited each supermarket.  Then, if these numbers
  // are large enough, create new infections among the
  // susceptible.

  // Provide the supermarket each individual went to
  if (N != Status.length() ||
      N != SA2.length() ||
      N != Age.length() ||
      N != Employment.length() ||
      N != SupermarketTarget.length() ||
      N != Resistance.length()) {
    stop("Internal error: lengths differ.");
  }

  // Provide Cauchy if not already supplied.
  IntegerVector thisCauchyM = (N == CauchyM.length()) ? CauchyM : (rcauchy_int(N, 2, 0.01));


  // assume that SA2 is keyed
  if (check_sa2_key && which_unsorted_int(SA2)) {
    stop("Internal error: SA2 not sorted.");
  }


  IntegerVector nInfectedVisitorsBySupermarket(N_SUPERMARKETS);
  IntegerVector nVisitorsBySupermarket(N_SUPERMARKETS);

  int supermarket_j = 0;
  int infected_visitors = 0;
  for (int i = 0; i < N; ++i) {
    int supermarket_target = SupermarketTarget[i];
    if (supermarket_target > 0) {
      // SupermarketTarget is 1-indexed.
      nVisitorsBySupermarket[supermarket_target - 1] += 1;
      nInfectedVisitorsBySupermarket[supermarket_target - 1] += (Status[i] > 0) * thisCauchyM[i];
    }
  }

  // reinfect
  for (int i = 0; i < N; ++i) {
    if (Status[i]) {
      continue; // already infected
    }

    int supermarket_target = SupermarketTarget[i];

    // if the person visited a supermarket they become infected
    // probablistically
    if (supermarket_target > 0) {
      int n_visitors = nVisitorsBySupermarket[supermarket_target - 1];
      int n_infected = nInfectedVisitorsBySupermarket[supermarket_target - 1];

      // infected (s = 1) if resistance lower than n_infected
      // critical (s = 2) if resistance much lower than n_infected;
      int p = Resistance[i] + thisCauchyM[i];
      Status[i] = (p < n_infected) + (p * p < n_infected);
    }
  }
  return Status;
}


// [[Rcpp::export]]
int do_au_simulate(IntegerVector Status,
                   IntegerVector SA2,
                   IntegerVector Age,
                   IntegerVector PlaceTypeBySA2,
                   IntegerVector Employment,
                   List FreqsByDestType,
                   int yday_start,
                   int days_to_sim,
                   int N = 25e6) {

  if (FreqsByDestType.length() <= 98) {
    stop("Internal error: FreqsByDestType.length < 98");
  }

  IntegerVector SupermarketFreq = FreqsByDestType[98]; // Type_by_TypeInt.fst

  if (N != SupermarketFreq.length()) {
    stop("Internal error: SupermarketFreq.length mismatch");
  }

  for (int day = 0; day < days_to_sim; ++day) {

    // For example, SupermarketFreq[i] = 365  => every day
    // SupermarketFreq[i] = 1 every year.  So we create a vector
    // of 1:366 and compare that to the individual's tendency to
    // visit. So if TodaysHz[i] = 366 they will not visit anything
    // regardless; if TodaysHz[i] = 1 they will visit everything.
    IntegerVector TodaysHz = Rcpp::sample(365, N, true);


    //
    IntegerVector SupermarketTarget = no_init(N);
    for (int i = 0; i < N; ++i) {
      // did they go outside
      bool goes_outside = false;
      bool contagious = false;

      if (goes_outside) {

        bool moves_sa2 = false;
        if (moves_sa2) {
          int new_sa2 = 0;
          // don't get infected
          SA2[i] = new_sa2;
        } else {
          bool commutes = false; // goes to work
          bool is_pupil = false;
          int destination_type = 0; // 1-106

          if (SupermarketFreq[i] > TodaysHz[i]) {
            // they will visit a supermarket

          }

        }

        // based on where they went
        bool gets_infected = false;
      }
    }
  }
  return 0;
}






