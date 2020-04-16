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

// [[Rcpp::export(rng = false)]]
IntegerVector do_1day_supermarket(IntegerVector Status,
                                  IntegerVector SA2,
                                  IntegerVector Age,
                                  IntegerVector Employment,
                                  IntegerVector SupermarketTarget,
                                  IntegerVector Resistance,
                                  IntegerVector CauchyM,
                                  int N = 25e6,
                                  bool check_sa2_key = true) {


  const int PERSONS_PER_SUPERMARKET = N / N_SUPERMARKETS;

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
  IntegerVector thisCauchyM = (N == CauchyM.length()) ? CauchyM : (rcauchy_int(N, 2, 0.001));


  // assume that SA2 is keyed
  if (check_sa2_key && which_unsorted_int(SA2)) {
    stop("Internal error: SA2 not sorted.");
  }


  IntegerVector nInfectedVisitorsBySupermarket(N_SUPERMARKETS);
  IntegerVector nVisitorsBySupermarket(N_SUPERMARKETS);

  // supermarket_j is the shift from
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
      int64_t p = Resistance[i] + thisCauchyM[i];
      p *= PERSONS_PER_SUPERMARKET;
      Status[i] = (p < n_infected) + (p * p < n_infected);
    }
  }
  return Status;
}


// [[Rcpp::export]]
List do_au_simulate(IntegerVector Status,
                    IntegerVector SA2,
                    IntegerVector Age,
                    IntegerVector PlaceTypeBySA2,
                    IntegerVector Employment,
                    IntegerVector Resistance,
                    IntegerVector CauchyM,
                    List nPlacesByDestType,
                    List FreqsByDestType,
                    int yday_start,
                    int days_to_sim,
                    int N = 25e6) {

  if (FreqsByDestType.length() <= 98 ||
      nPlacesByDestType.length() <= 98) {
    stop("Internal error: FreqsByDestType.length < 98");
  }

  IntegerVector nSupermarketsBySA2 = nPlacesByDestType[97];
  IntegerVector SupermarketFreq = FreqsByDestType[97]; // Type_by_TypeInt.fst

  if (N != SupermarketFreq.length()) {
    stop("Internal error: SupermarketFreq.length mismatch");
  }
  if (NSA2 != nSupermarketsBySA2.length()) {
    stop("Internal error: nSupermarketsBySA2.length() != NSA2.");
  }

  if (PlaceTypeBySA2.length() > 1) {
    stop("Internal error: PlaceTypeBySA2 not implemented yet.");
  }


  IntegerVector nInfected = no_init(days_to_sim);
  IntegerVector FinalSupermarketTarget(N);
  for (int day = 0; day < days_to_sim; ++day) {



    // For example, SupermarketFreq[i] = 365  => every day
    // SupermarketFreq[i] = 1 every year.  So we create a vector
    // of 1:366 and compare that to the individual's tendency to
    // visit. So if TodaysHz[i] = 366 they will not visit anything
    // regardless; if TodaysHz[i] = 1 they will visit everything.
    IntegerVector TodaysHz = Rcpp::sample(365, N, true);


    // Need to initalize with zeroes as it will get too unwiedly
    // to write a case for each branch.
    IntegerVector SupermarketTarget(N);


    // ------------------------------------------------
    // SA2
    // |==============|========|========================|
    //                <--nsu-->                         // n_supermarkets_avbl
    // cum_j ----------------->
    // (Vertical pipes indicate the first person in a SA2)


    int supermarket_cumj = 0;
    int n_supermarkets_avbl = 0;
    for (int i = 0; i < N; ++i) {
      // did they go outside
      bool goes_outside = true;
      bool contagious = Status[i] != 0;

      int ssa2 = short_sa2(SA2[i]);
      bool sa2_change = ((i > 0) && SA2[i] != SA2[i - 1]);
      if (sa2_change) {
        // on the sa2 change we incremenent supermarket_cumj
        // by the previous SA2's number of supermarket
        // This moves along the array of supermarkets
        // so that rand % n_supermarkets_avbl moves within
        // the SA2's supermarket index for SupermarketTarget.
        supermarket_cumj += n_supermarkets_avbl;
        n_supermarkets_avbl = nSupermarketsBySA2[ssa2];
      }
      /*
       * Alternate array method of counting visits
       int s_visits_to_supermarket[n_supermarkets_avbl];
       int i_visits_to_supermarket[n_supermarkets_avbl];
       memset(s_visits_to_supermarket, 0, sizeof s_visits_to_supermarket);
       memset(i_visits_to_supermarket, 0, sizeof i_visits_to_supermarket);
       */



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

          // TODO: n_supermarkets_avbl needs to be loosened
          if (n_supermarkets_avbl && SupermarketFreq[i] > TodaysHz[i]) {
            // they will visit a supermarket
            // std::rand isn't uniform but who cares?
            int supermarket_visited = std::rand() % n_supermarkets_avbl;
            SupermarketTarget[i] = supermarket_visited + supermarket_cumj;
          }

        }

      }
      if (day + 1 == days_to_sim) {
        FinalSupermarketTarget[i] = SupermarketTarget[i];
      }
    }

    Status = do_1day_supermarket(Status,
                                 SA2,
                                 Age,
                                 Employment,
                                 SupermarketTarget,
                                 Resistance,
                                 CauchyM,
                                 N,
                                 /* check_sa2_key = */ day == 0);


    int n_infected_today = 0;
    for (int i = 0; i < N; ++i) {
      n_infected_today += (Status[i] == 1);
    }
    nInfected[day] = n_infected_today;


  }
  return Rcpp::List::create(Named("nInfected") = nInfected,
                            Named("SupermarketTarget") = FinalSupermarketTarget);
}






