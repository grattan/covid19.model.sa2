#include "covid19model.h"
#include <Rcpp.h>
#include <Rcpp/Benchmark/Timer.h>
using namespace Rcpp;

#define N_SUPERMARKETS 7487

// [[Rcpp::export(rng = false)]]
int status_killed() {
  return STATUS_KILLED;
}
// [[Rcpp::export(rng = false)]]
int status_healed() {
  return STATUS_HEALED;
}
// [[Rcpp::export(rng = false)]]
int status_suscep() {
  return STATUS_SUSCEP;
}
// [[Rcpp::export(rng = false)]]
int status_nosymp() {
  return STATUS_NOSYMP;
}
// [[Rcpp::export(rng = false)]]
int status_insymp() {
  return STATUS_INSYMP;
}
// [[Rcpp::export(rng = false)]]
int status_critic() {
  return STATUS_CRITIC;
}
// [[Rcpp::export(rng = false)]]
int supermarket_weekday_hrs() {
  return SUPERMARKET_WEEKDAY_HRS;
}
// [[Rcpp::export(rng = false)]]
int supermarket_weekend_hrs() {
  return SUPERMARKET_WEEKEND_HRS;
}


// [[Rcpp::export(rng = false)]]
int do_max_par_int(IntegerVector x, int nThread = 1) {
  int N = x.length();
  int out = x[0];
#pragma omp parallel for num_threads(nThread) reduction(max:out)
  for (int i = 0; i < N; ++i) {
    out = out < x[i] ? x[i] : out;
  }
  return out;
}

// Just a way to get quasi-cauchy distributed nonnegative integer vector
int rcauchy_int(double l, double s) {
  double out = cauchyRand(l, s);
  out = (out < -1 || out > 1000) ? 0 : out;
  return out;
}

int rpois_int(int l) {
  return Rcpp::rpois(1, l)[0];
}

int incubRand(double m, double s, int d) {
  // 1: Poisson  2: lognormal  3: dirac
  if (d == 1)
    return poisRand(m);
  if (d == 2)
    return lnormRand(m2mu(m, s), s);
  if (d == 3)
    return m;

  return m;
}

int illRand(double m, double s, int d) {
  if (d == 1)
    return poisRand(m);
  if (d == 2)
    return lnormRand(m2mu(m, s), s);
  if (d == 3)
    return m;
  if (d == 4)
    return cauchyRand0(m, s);

  return m;
}

int r_Rand(double m, double s, int d, bool dper, int e, int per, int yday) {
  if (dper)
    return (yday % per) ? 0 : e;
  if (d == 1)
    return poisRand(m);
  if (d == 2)
    return lnormRand(m2mu(m, s), s);
  if (d == 3)
    return m;
  if (d == 4)
    return cauchyRand0(m, s);

  return m;
}

int array3k(int x, int y, int z, int ny, int nz) {
  return x * (ny * nz) + y * (nz) + z;
}

int array4k(int w, int x, int y, int z, int nx, int ny, int nz) {
  return w * (nx * ny * nz) + x * (ny * nz) + y * nz + z;
}

// [[Rcpp::export]]
IntegerVector test_array4k(IntegerVector w, IntegerVector x, IntegerVector y, IntegerVector z,
                           int nw, int nx, int ny, int nz) {
  // purely to test array4k
  R_xlen_t n = nw * nx * ny * nz;
  if (n >= INT_MAX) {
    stop("test_array4k only available for integer-length.");
  }

  if (n != w.length() ||
      n != x.length() ||
      n != y.length() ||
      n != z.length()) {
    stop("Internal error: lengths differ.");
  }
  IntegerVector out = no_init(nw * nx * ny * nz);
  for (R_xlen_t i = 0; i < n; ++i) {
    int wi = w[i];
    int xi = x[i];
    int yi = y[i];
    int zi = z[i];
    if (wi >= nw || xi >= nx || yi >= ny || zi >= nz) {
      stop("Internal error: wi >= nw || xi >= nx || yi >= ny || zi >= nz");
    }
    out[i] = array4k(w[i], x[i], y[i], z[i], nx, ny, nz);
  }
  return out;
}



void contact_tracing(IntegerVector Status,
                     IntegerVector InfectedOn,
                     IntegerVector Incubation,
                     int day,
                     int yday,
                     IntegerVector seqN,
                     IntegerVector HouseholdSize,
                     IntegerVector TestsAvbl,
                     bool by_state,
                     IntegerVector State,
                     IntegerVector hid,
                     IntegerVector HouseholdInfectedToday,
                     int N,
                     IntegerVector School,
                     IntegerVector PlaceId,
                     int ptest_per_mille_sympto,
                     int ptest_per_mille_asympto,
                     IntegerVector TodaysK,
                     int days_before_test,
                     int days_to_notify,
                     int nThread,
                     IntegerVector TestedOn) {
#ifdef _OPENMP
  if (nThread < 1 || nThread > omp_get_num_procs()) {
    // lots of large ints nearby!
    stop("Internal error: nThread out of range");
  }
#endif

  // isTested -> isIdentified -> responseReq -> LockdownTomorrw

  // loop through each person who has been infected
  // does the person get a test?
  // is the test positive?
  // is the person part of a household
  // are any tests left?
  // test each person in the household

  // TestedOn = Day at which the person is tested and notified
  // IntegerVector TestedOn = no_init(N);
  const int test_array[3] = {0, ptest_per_mille_asympto, ptest_per_mille_sympto};
  const int yday_tested = yday + days_before_test;
  const int yday_result = yday + days_to_notify + days_to_notify;
  const int NSTATES1 = NSTATES + 1;

  int tests_avbl[NSTATES1] = {0};

  for (int s = 0; s < NSTATES1; ++s) {
    if (TestsAvbl[s] < 0) {
      stop("TestsAvbl[s] < 0 was negative.");
    }
    tests_avbl[s] = TestsAvbl[s];
  }

  // tests_performed[0] = all of australia
  int tests_performed[NSTATES1] = {0};
  if (TestsAvbl.length() != NSTATES1) {
    stop("Internal error: TestsAvbl.length() != NSTATES + 1.");
  }
  if (NSTATES1 != 10) {
    stop("Internal error. NSTATES1 != 10");
  }

  // until OpenMP 4.5
  int t_perf0 = 0;
  int t_perf1 = 0;
  int t_perf2 = 0;
  int t_perf3 = 0;
  int t_perf4 = 0;
  int t_perf5 = 0;
  int t_perf6 = 0;
  int t_perf7 = 0;
  int t_perf8 = 0;
  int t_perf9 = 0;




#pragma omp parallel for num_threads(nThread) reduction(+:t_perf0,t_perf1,t_perf2,t_perf3,t_perf4,t_perf5,t_perf6,t_perf7,t_perf8,t_perf9)
  for (int i = 0; i < N; ++i) {

    int test_k = ((Status[i] == STATUS_NOSYMP) + (Status[i] == STATUS_INSYMP));
    int ptest_per_mille = test_array[test_k];
    if (!ptest_per_mille) {
      continue;
    }

    int todayi = (i + day*13) % NTODAY;

    int prev_test_on = TestedOn[i];

    // if the person gets a (false) negative result
    // they reset with 1/32 probability
    if (prev_test_on < 0) {
      if (TodaysK[todayi] < 32) {
        TestedOn[i] = 0;
      }
      continue;
    }

    // Don't test everyday -- need time to isolate and
    // tests scheduled shouldn't be overwritten
    if (yday <= prev_test_on + 1) {
      continue;
    }
    //
    if (InfectedOn[i] + Incubation[i] != yday_tested) {
      continue;
    }


    // test_outcome = -1 (negative), 0 (no test), 1 (positive)
    if (TodaysK[todayi] < ptest_per_mille) {
      t_perf0 += 1;
      int statei = State[i];
      if (by_state) {
        if (statei == 1) t_perf1 += 1;
        if (statei == 2) t_perf2 += 1;
        if (statei == 3) t_perf3 += 1;
        if (statei == 4) t_perf4 += 1;
        if (statei == 5) t_perf5 += 1;
        if (statei == 6) t_perf6 += 1;
        if (statei == 7) t_perf7 += 1;
        if (statei == 8) t_perf8 += 1;
        if (statei == 9) t_perf9 += 1;
      }
      if (todayi == NTODAY - 1) {
        todayi = 0;
      } else {
        ++todayi;
      }

      // tests_performed is private so can't be used in an expression like
      // if (tests_performed < tests_avbl)
      bool false_negative = TodaysK[todayi] > SENSITIVITY;

      // false negatives
      int test_outcome = (false_negative) ? -1 : 1;
      // encode negative and time of test in one variable
      TestedOn[i] = yday_result * test_outcome;
    }
  }

  // At this point we know how many tests have been performed
  // and who has been identified.

  // If the tests_avbl has been exceeded (for a state) and the person
  // was tested today, we unset their test probabilistically, based
  // on how many tests were exceeded

  tests_performed[0] = t_perf0;
  tests_performed[1] = t_perf1;
  tests_performed[2] = t_perf2;
  tests_performed[3] = t_perf3;
  tests_performed[4] = t_perf4;
  tests_performed[5] = t_perf5;
  tests_performed[6] = t_perf6;
  tests_performed[7] = t_perf7;
  tests_performed[8] = t_perf8;
  tests_performed[9] = t_perf9;

#pragma omp parallel for num_threads(nThread)
  for (int i = 0; i < N; ++i) {
    int statei = by_state ? State[i] : 0;

    // if no tests were ever available we can quickly discard tested
    if (tests_avbl[statei] == 0 &&
        TestedOn[i] &&
        (TestedOn[i] == yday_result || TestedOn[i] == -yday_result)) {
      TestedOn[i] = 0;
      continue;
    }
    bool tests_exceeded = tests_performed[statei] > tests_avbl[statei];
    if (tests_exceeded &&
        TestedOn[i] &&
        (TestedOn[i] == yday_result || TestedOn[i] == -yday_result)) {
      int trace_per_mille = (tests_avbl[statei] * 1000) / tests_performed[statei];
      int todayi = (i + day + 13) % NTODAY;
      if (trace_per_mille < TodaysK[todayi]) {
        TestedOn[i] = 0;
      }
    }
  }



  // Isolate everyone who is in the same household
  // as a person who is 'TestedOn' today
#pragma omp parallel for num_threads(nThread)
  for (int i = 0; i < N; ++i) {

    // threadsafety -- go through the head of household
    if (seqN[i] != 1) {
      continue;
    }

    if (Status[i] < 0 || Status[i] >= ISOLATED_PLUS) {
      continue;
    }

    bool notified_positive_today = TestedOn[i] == yday;


    if (HouseholdSize[i] == 1) {
      if (notified_positive_today) {
        if (Status[i] >= 0 && Status[i] < ISOLATED_PLUS) {
          Status[i] += ISOLATED_PLUS;
        }
      }
      // no household tracing for single person
      continue;
    }



    // at the head of househodl seqN = 1,
    // and at least one of the household is infected

    int nh = HouseholdSize[i];
    for (int j = 1; j < nh; ++j) {
      if (notified_positive_today) {
        break;
      }
      notified_positive_today = TestedOn[i + j] == yday;
    }
    // if any household member gets notified then
    // they are all put in isolation
    if (notified_positive_today) {
      for (int j = 0; j < nh; ++j) {
        int statusij = Status[i + j];
        if (statusij >= 0 && statusij < ISOLATED_PLUS) {
          Status[i + j] += ISOLATED_PLUS;
        }
      }
    }
  }

  // void
}



//' @name do_1day_supermarket
//' @description Simulate the experience of everyone's interactions
//' at supermarkets in a single day
//' @param Status \code{integer(N)}: Whether each individual is infected or not etc.
//' @param InfectedOn The day on which the individual was infected.
//' @param SA2 The 2016 SA2 code of each individual. Must be sorted.
//' @param Age The age of every individual.
//' @param Employment Information about employment of each individual.
//' @param SupermarketTarget The supermarket that the person visits,
//' equal to zero for each individual who doesn't visit a supermarket.
//' @param Resistance For each individual, inherent resistance to being infected.
//' @param N \code{int} Population of Australia.
//' @param check_sa2_key \code{bool} Whether to check SA2 is sorted, defaults to \code{true}.
//' @param returner Used to return other elements. By default 0, i.e. just return Status.
//' @param r0_supermarket The reproduction rate of infection in each supermarket, without
//' including resistance.
//' @param resistance1 Resistance parameter 1: the threshold below which
//' an infection takes place.
//' @param resistance2 Reistance parameter 2: used with Age to increase
//' the likelihood of both critical and active cases among the elderly.
//' @noRd

void infect_supermarkets(IntegerVector Status,
                         IntegerVector InfectedOn,
                         IntegerVector shortSA2,
                         int nThread,
                         IntegerVector Age,
                         IntegerVector Employment,
                         IntegerVector nSupermarketsBySA2,
                         IntegerVector nSupermarketsAvbl,
                         IntegerVector Resistance,
                         int day,
                         int wday,
                         int yday,
                         int N,
                         IntegerVector SupermarketTypical,
                         double r_location,
                         double r_scale,
                         int r_d,
                         bool do_dirac_every, int dirac_num, int dirac_per,
                         IntegerVector SupermarketFreq,
                         IntegerVector TodaysHz,
                         int resistance1,
                         int max_persons_per_supermarket,
                         int resistance2 = 3,
                         double r_div = 36, // divide r_ by this // TODO: model is highly sensitive to this par!
                         bool verbose = false) {
  if (day < 0) {
    stop("Internal error(infect_supermarkets): day < 0");
  }
  if (day == 0) {
    // poor man's assert()
    if (SupermarketTypical.length() != N) {
      stop("Internal error(infect_supermarkets): SupermarketTypical.length() != N");
    }
    int maxSupermarketBySA2 = SupermarketTypical[0];
#pragma omp parallel for num_threads(nThread) reduction(max : maxSupermarketBySA2)
    for (int i = 0; i < N; ++i) {
      if (SupermarketTypical[i] > maxSupermarketBySA2) {
        maxSupermarketBySA2 = SupermarketTypical[i];
      }
    }

    // note maxSupermarketBySA2 is the index and MAXSUPER.. is the array size
    // so strict inequality is correct assertion
    if (maxSupermarketBySA2 > MAXSUPERMARKETSBYSA2) {
      stop("Internal error(infect_supermarkets): maxSupermarketBySA2 > MAXSUPERMARKETSBYSA2");
    }

    if (SUPERMARKET_WEEKDAY_HRS < SUPERMARKET_WEEKEND_HRS) {
      stop("Internal error(infect_supermarkets): SUPERMARKET_WEEKDAY_HRS < SUPERMARKET_WEEKEND_HRS");
    }
    if (resistance1 < 0 || resistance1 > 1000) {
      stop("Internal error(infect_supermarkets): resistance1 was not in [0, 1]");
    }
  }

  // array of supermarkets: SA2 x Supermarkets each SA2 x hour
  // (some sa2s have 17 supermarkets; others will just record 0 there)

  // int i_supermarkets[NSA2][maxSupermarketsBySA2][hrs_open];



  const int hrs_open = (wday < 6) ? SUPERMARKET_WEEKDAY_HRS : SUPERMARKET_WEEKEND_HRS;

  int i_supermarkets[NSA2][MAXSUPERMARKETSBYSA2][SUPERMARKET_WEEKDAY_HRS] = {};

  bool check_max_persons = max_persons_per_supermarket < 10e3;

#if defined _OPENMP && _OPENMP >= 201511
#pragma omp parallel for num_threads(nThread) reduction(+:i_supermarkets[:NSA2][:MAXSUPERMARKETSBYSA2][:SUPERMARKET_WEEKDAY_HRS])
#endif
  for (int i = 0; i < N; ++i) {
    if (Status[i] != STATUS_NOSYMP || TodaysHz[(i * 11 + yday) % NTODAY] > SupermarketFreq[i]) {
      continue;
    }
    int sa2i = shortSA2[i];
    int supermarketi = SupermarketTypical[i];
    int hri = i % hrs_open;
    i_supermarkets[sa2i][supermarketi][hri] +=
      r_Rand(r_location, r_scale, r_d, do_dirac_every, dirac_num, dirac_per, yday);
  }



#pragma omp parallel for num_threads(nThread)
  for (int hr = 0; hr < hrs_open; ++hr) {
    int s_supermarkets[NSA2][MAXSUPERMARKETSBYSA2] = {};
    for (int i = hr; i < N; i += hrs_open) {
      int sa2i = shortSA2[i];
      int supermarketi = SupermarketTypical[i];
      if (i_supermarkets[sa2i][supermarketi][hr] <= 0) {
        continue;
      }
      if (Status[i] || !nSupermarketsAvbl[i] || TodaysHz[(i * 11 + yday) % NTODAY] > SupermarketFreq[i]) {
        continue;
      }


      // technically by hour but loop guarantees independence
      s_supermarkets[sa2i][supermarketi] += 1;
      int s_supermarketi = s_supermarkets[sa2i][supermarketi];

      if (check_max_persons && s_supermarketi > max_persons_per_supermarket) {
        continue;
      }

      if (i_supermarkets[sa2i][supermarketi][hr] > 0) {
        i_supermarkets[sa2i][supermarketi][hr] -= 1;
        if (Resistance[i] < resistance1) {
          // i_supermarkets[sa2i][supermarketi][hr] -= 1;
          Status[i] = STATUS_NOSYMP;
          InfectedOn[i] = yday;
        }
      }
    }
  }
}

void infect_place(int place_id,
                  IntegerVector Status,
                  IntegerVector InfectedOn,
                  IntegerVector shortSA2,
                  IntegerVector SA2_firsts,
                  IntegerVector SA2_finals,
                  int nThread,
                  List minPlaceID_nPlacesByDestType,
                  int day,
                  int wday,
                  int yday,
                  int N,
                  IntegerVector Resistance,
                  int resistance1,
                  IntegerVector PlaceFreq,
                  IntegerVector TodaysHz,
                  double r_location,
                  double r_scale,
                  int r_d,
                  bool do_dirac_every, int dirac_num, int dirac_per,
                  int max_persons_per_place,
                  IntegerVector TodaysK) {

  if (place_id == PLACEID_ESTABLISHMENT ||
      place_id == PLACEID_POINT_OF_INTEREST ||
      place_id == PLACEID_STORE) {
    // don't support these
    // void function so just return early
    return;
  }

  List minPlaceID_nPlaces = minPlaceID_nPlacesByDestType[place_id];

  IntegerVector nPlacesBySA2 = minPlaceID_nPlaces["nPlaces"];
  IntegerVector minPlaceIdBySA2 = minPlaceID_nPlaces["minPlaceId"];

  if (day == 0) {
    // poor man's static assertions

    // infected implies positive status
    if (STATUS_KILLED > 0 || STATUS_HEALED > 0 || STATUS_SUSCEP > 0) {
      stop("Unexpected error(infect_place): 'infected implies positive status' violated");
    }

    if (SA2_firsts.length() != NSA2 || SA2_finals.length() != NSA2) {
      stop("Internal error(infect_place): SA2_firsts.length() != NSA2 || SA2_finals.length() != NSA2");
    }

    // make sure there is an entry for every SA2, not just those with places
    if (nPlacesBySA2.length() != NSA2) {
      stop("Internal error(infect_place): nPlacesBySA2.length() != NSA2");
    }
    if (minPlaceIdBySA2.length() != NSA2) {
      stop("Internal error(infect_place): minPlaceIdBySA2.length() != NSA2");
    }

    int n_places = 0;
    for (int sa2i = 0; sa2i < NSA2; ++sa2i) {
      n_places += nPlacesBySA2[sa2i];
    }

    if (n_places > MAX_N_PLACES_TOTAL) {
      stop("Internal error(infect_place): n_places > MAX_N_PLACES_TOTAL");
    }

    // check argument positions(!) what is even a struct? shutup
    if (N != Resistance.length()) {
      stop("Internal error(infect_place): N != Resistance.length().");
    }
    if (N != PlaceFreq.length()) {
      stop("Internal error(infect_place): N != PlaceFreq.length().");
    }

    if (NTODAY != TodaysHz.length()) {
      stop("Internal error(infect_place): NTODAY != TodaysHz.length().");
    }
    if (NTODAY != TodaysK.length()) {
      stop("Internal error(infect_place): NTODAY != TodaysK.length().");
    }


  }

  std::mt19937 mt_rand(time(0));
  unsigned int urandd = mt_rand();
  int irandd = urandd % N;


  // # Infect
  // ## Find all the places infected people visited

  // n_places is the total number of places across all places
  // place_id identifies the place and has the same order as sa2
  // if place_id_x < place_id_y  then  sa2(place_id_x) <= sa2(place_id_y)
  // sa2

  // Since gnu annoyingly follows the standards (/s), we can't do
  // int i_places[n_places][hrs_open]
  // because 'C++ forbids variable size array'
  int i_places[MAX_N_PLACES_TOTAL][PLACES_HRS_OPEN] = {};
  int n_infections_per_sa2[NSA2] = {};

#if defined _OPENMP && _OPENMP >= 201511
#pragma omp parallel for num_threads(nThread) reduction(+ : i_places[:MAX_N_PLACES_TOTAL][:PLACES_HRS_OPEN])
#endif
  for (int sa2i = 0; sa2i < NSA2; ++sa2i) {
    int n_places_this_sa2 = nPlacesBySA2[sa2i];
    if (n_places_this_sa2 <= 0) {
      continue;
    }

    int min_place_id = minPlaceIdBySA2[sa2i]; // 0-indexed
    int start_sa2i = SA2_firsts[sa2i];
    int end_sa2i   = SA2_finals[sa2i];
    if (start_sa2i == end_sa2i) {
      continue;
    }

    for (int i = start_sa2i; i < end_sa2i; ++i) {

      if (Status[i] != STATUS_INSYMP && Status[i] != STATUS_NOSYMP) {
        continue;
      }
      if (TodaysHz[(i + irandd) % NTODAY] > PlaceFreq[i]) {
        continue;
      }

      // both these should be deterministic
      int placei = min_place_id + (i % n_places_this_sa2);
      int hri = i % PLACES_HRS_OPEN;
      int randi = TodaysK[(i + irandd) % NTODAY];
      // occasionally (1/8 times) go at a different time
      if (randi < 128) {
        hri = randi % PLACES_HRS_OPEN;
      }


      int n_new_infections = r_Rand(r_location, r_scale, r_d, do_dirac_every, dirac_num, dirac_per, yday);
      i_places[placei][hri] += n_new_infections;

      int excess_persons = i_places[placei][hri] - max_persons_per_place;
      if (excess_persons >= 0) {
        i_places[placei][hri] += -1 - excess_persons;
        n_infections_per_sa2[sa2i] -= excess_persons;
      } else {
        // threadsafety ok as sa2i allocated indivi thread
        n_infections_per_sa2[sa2i] += n_new_infections;
      }
    }
  }



#pragma omp parallel for num_threads(nThread)
  for (int sa2i = 0; sa2i < NSA2; ++sa2i) {
    int n_places_this_sa2 = nPlacesBySA2[sa2i];
    if (n_places_this_sa2 <= 0) {
      continue;
    }
    int n_infections_this_sa2 = n_infections_per_sa2[sa2i];
    if (n_infections_this_sa2 <= 0) {
      continue;
    }
    int start_sa2i = SA2_firsts[sa2i];
    int end_sa2i   = SA2_finals[sa2i];
    if (start_sa2i == end_sa2i) {
      continue;
    }

    int min_place_id = minPlaceIdBySA2[sa2i]; // 0-indexed

    for (int i = start_sa2i; i < end_sa2i; ++i) {
      // sa2 is ordered
      // if we are below continue; if above skip
      if (shortSA2[i] != sa2i) {
        if (shortSA2[i] < sa2i) {
          continue;
        } else {
          break;
        }
      }

      // healed
      if (!Status[i]) {
        continue;
      }

      if (TodaysHz[(i + irandd) % NTODAY] > PlaceFreq[i]) {
        continue;
      }
      // both these should be deterministic
      int placei = min_place_id + (i % n_places_this_sa2);
      int hri = i % PLACES_HRS_OPEN;
      int randi = TodaysK[(i + irandd) % NTODAY];
      // occasionally (1/8 times) go at a different time
      if (randi < 128) {
        hri = randi % PLACES_HRS_OPEN;
      }
      if (i_places[placei][hri] > 0) {
        i_places[placei][hri] -= 1;
        if (Resistance[i] < resistance1) {
          Status[i] = STATUS_NOSYMP;
          InfectedOn[i] = yday;
        }
      }
    }
  }
  // void infect places
}



// [[Rcpp::export]]
IntegerVector do_rep(IntegerVector r, int nThread = 1) {
  int n = 0, nr = r.length();
#pragma omp parallel for num_threads(nThread) reduction(+:n)
  for (int i = 0; i < nr; ++i) {
    n += r[i];
  }
  IntegerVector out = no_init(n);
  for (int i = 0, k = 0, j = 0; i < nr; ++i) {
    int ri = r[i];
    while (k < ri) {
      out[j] = ri;
      ++j;
      ++k;
    }
    k = 0;
  }
  return out;
}




void infect_dzn(IntegerVector Status,
                IntegerVector InfectedOn,
                IntegerVector DZN,
                IntegerVector wid,
                const std::vector<int> &widIndex,
                IntegerVector LabourForceStatus,
                IntegerVector nColleagues,
                int day,
                int wday,
                int yday,
                int N,
                int resistance1,
                const std::vector<char> &Wrand,
                int workplaces_open,
                int workplace_size_max,
                double r_location,
                double r_scale,
                int r_d,
                bool do_dirac_every, int dirac_num, int dirac_per,
                IntegerVector TodaysK,
                IntegerVector Resistance,
                int zero,
                int optionz,
                int nThread = 1) {
  if (zero != 0) {
    stop("zero != 0");
  }

  if (day == 0) {
    if (optionz) {
      Rcout << Status.length() << " ";
      Rcout << InfectedOn.length() << " ";
      Rcout << DZN.length() << " ";
      Rcout << wid.length() << " ";
      Rcout << LabourForceStatus.length() << " ";
      Rcout << nColleagues.length() << " ";
      Rcout << TodaysK.length() << " ";
      Rcout << Resistance.length() << " ";

      Rcout << N << " ";
      Rcout << resistance1 << " ";
      Rcout << workplaces_open << " ";
      Rcout << workplace_size_max << " ";
      Rcout << r_location << " ";
      Rcout << r_scale << " ";
      Rcout << r_d << " ";
      Rcout << "\n";
    }

    // check that DZN is short and ranges from 1 to (NDZN - 1)
    // check that wid_supremum0
    int max_dzn = 0;
    int max_wid = 0;
    for (int i = 0; i < N; ++i) {
      if (DZN[i] > max_dzn) {
        max_dzn = DZN[i];
      }
      if (wid[i] > max_wid) {
        max_wid = wid[i];
      }
    }
    if (max_dzn >= NDZN) {
      stop("Internal error(infect_dzn): wmax_dzn >= NDZN");
    }

    if (max_wid >= WID_SUPREMUM) {
      stop("Internal error(infect_dzn): wid_supremeum0 > WID_SUPREMUM");
    }
    if (TodaysK.length() != NTODAY) {
      stop("TodaysK.length() != NTODAY");
    }
    if (nColleagues.length() != N) {
      stop("Internal error(infect_dzn): nColleagues.length() != N");
    }
  }

  // can't use array as overflow risk is real
  std::vector<int> InfectionsByWorkplace(WID_SUPREMUM, 0);
  if (optionz && day == 0) {
    Rcout << InfectionsByWorkplace[0] << "\n";
    Rcout << InfectionsByWorkplace[1] << "\n";
  }

  // for (int w = 0; w < WID_SUPREMUM; ++w) {
  //   InfectionsByWorkplace[w] = 0;
  // }

  unsigned int widIndexSize = widIndex.size();


  for (unsigned int k = 0; k < widIndexSize; ++k) {
    int i = widIndex[k];
    if (Status[i] != STATUS_INSYMP) {
      continue;
    }

    int widi = wid[i]; // if wid[i] is NA widi[i] - 1 is UBD
    if (day == 0 && widi <= 0) {
      stop("Internal error (day == 0 && widi <= 0)).");
    }
    int widi0 = widi - 1;

    if (workplaces_open < 1000 && ((widi0 % 1000) > workplaces_open)) {
      continue;
    }
    InfectionsByWorkplace[widi0] += 1;// r_Rand(r_location, r_scale, r_d);
  }

  // reinfection
  for (unsigned int k = 0; k < widIndexSize; ++k) {
    int i = widIndex[k];
    int widi0 = wid[i] - 1;
    if (!Wrand[widi0]) {
      continue;
    }
    if (nColleagues[i] <= 1) {
      continue;
    }
    if (InfectionsByWorkplace[widi0] <= 0) {
      continue;
    }

    int excess_workers = nColleagues[i] - workplace_size_max;
    bool no_further_infections = false; // due to the infectious not being allowed in (back of the queue)
    if (excess_workers > 0) {
      if (excess_workers > InfectionsByWorkplace[widi0]) {
        // assume infections are strictly less likely to attend work
        InfectionsByWorkplace[widi0] = 0;
        no_further_infections = true;
      } else {
        InfectionsByWorkplace[widi0] -= excess_workers;
      }
    }

    if (Status[i] || no_further_infections) {
      continue;
    }

    if (Resistance[i] < resistance1) {
      Status[i] = STATUS_NOSYMP;
      InfectedOn[i] = yday;
      InfectionsByWorkplace[widi0] -= 1;
    }

  }
}

bool during_school_term(const int & state, const int & yday) {
  bool o = true;
  if (state == 1) {
    o =
      (yday <= NSW_TERM2_FINAL_YDAY && yday >= NSW_TERM2_START_YDAY) ||
      (yday <= NSW_TERM3_FINAL_YDAY && yday >= NSW_TERM3_START_YDAY);
  }
  if (state == 2) {
    o =
      (yday <= VIC_TERM2_FINAL_YDAY && yday >= VIC_TERM2_START_YDAY) ||
      (yday <= VIC_TERM3_FINAL_YDAY && yday >= VIC_TERM3_START_YDAY);
  }
  if (state == 3) {
    o =
      (yday <= QLD_TERM2_FINAL_YDAY && yday >= QLD_TERM2_START_YDAY) ||
      (yday <= QLD_TERM3_FINAL_YDAY && yday >= QLD_TERM3_START_YDAY);
  }
  if (state == 4) {
    o =
      (yday <=  SA_TERM2_FINAL_YDAY && yday >=  SA_TERM2_START_YDAY) ||
      (yday <=  SA_TERM3_FINAL_YDAY && yday >=  SA_TERM3_START_YDAY);
  }
  if (state == 5) {
    o =
      (yday <=  WA_TERM2_FINAL_YDAY && yday >=  WA_TERM2_START_YDAY) ||
      (yday <=  WA_TERM3_FINAL_YDAY && yday >=  WA_TERM3_START_YDAY);
  }
  if (state == 6) {
    o =
      (yday <= TAS_TERM2_FINAL_YDAY && yday >= TAS_TERM2_START_YDAY) ||
      (yday <= TAS_TERM3_FINAL_YDAY && yday >= TAS_TERM3_START_YDAY);
  }
  if (state == 7) {
    o =
      (yday <=  NT_TERM2_FINAL_YDAY && yday >=  NT_TERM2_START_YDAY) ||
      (yday <=  NT_TERM3_FINAL_YDAY && yday >=  NT_TERM3_START_YDAY);
  }
  if (state >= 8) {
    o =
      (yday <= ACT_TERM2_FINAL_YDAY && yday >= ACT_TERM2_START_YDAY) ||
      (yday <= ACT_TERM3_FINAL_YDAY && yday >= ACT_TERM3_START_YDAY);
  }
  return o;
}


void infect_school(IntegerVector Status,
                   IntegerVector InfectedOn,
                   IntegerVector School,
                   IntegerVector Age,
                   IntegerVector AttendsWday,
                   const int &day,
                   const int &wday,
                   const int &yday,
                   const int &N,
                   IntegerVector State,
                   const unsigned char state_by_school[],
                   const std::array<bool, NSTATES1> &areSchoolsLockedDown,
                   unsigned char state_trigger_pulled[],
                   int lockdown_trigger_schools_with_infections,
                   int lockdown_trigger_schools_with_infections_geq,
                   int lockdown_trigger_schools_with_infections_duration_of_lockdown,
                   int lockdown_trigger_schools_any_critical,
                   int lockdown_trigger_schools_any_critical_duration_of_lockdown,
                   IntegerVector shortSA2,
                   const std::vector<int> &schoolIndices,
                   const std::vector<char> &Erand,
                   double r_location,
                   double r_scale,
                   const int &r_d,
                   bool do_dirac_every, int dirac_num, int dirac_per,
                   IntegerVector Srand,
                   IntegerVector Seed,
                   double q_school_dbl,
                   bool only_Year12,
                   List school_days_per_wk,
                   const int &nThread = 1,
                   int optionz = 0,
                   int zero = 0) {
  if (zero != -99) {
    stop("zero expected.");
  }

  // infect people within a school

  // Cube: number of visits by School x Age
  // First array index is the total, following indices are the age-based infections
  // Teachers are all aged '20'.



  // first school day may not be day 0
  if (day == 0 || (day < 3 && wday == 1)) {
    if (nThread <= 0) {
      stop("nThread <= 0");
    }
    if (!school_days_per_wk.containsElementNamed("week15combns")) {
      stop("Internal error: school_days_per_wk did not contain 'week15combns'.");
    }
    if (school_days_per_wk.length() < NSTATES1) {
      Rcout << "school_days_per_wk.length() = " << school_days_per_wk.length();
      stop("school_days_per_wk had wrong length");
    }
    if (AttendsWday.length() != (NPUPILS * 5)) {
      stop("Internal error: AttendsWday.length() != (NPUPILS * 5)");
    }
    bool max_too_large = false;
    bool min_too_small = false;
#pragma omp parallel for num_threads(nThread) reduction(|| : max_too_large, min_too_small)
    for (int i = 0; i < N; ++i) {
      int schooli = School[i]; // School[i] may be INT_MIN since we are cycling over everyone
      if (schooli == NA_INTEGER) {
        continue;
      }
      max_too_large = max_too_large || (schooli > NSCHOOLS);
      min_too_small = min_too_small || (schooli < 0);
    }

    if (min_too_small) {
      stop("Internal error(infect_schools): min_too_small.");
    }
    if (max_too_large) {
      stop("Internal error(infect_schools): max_too_large");
    }

    if (shortSA2.length() != N) {
      stop("Internal error(infect_schools):shortSA2.length() != N");
    }

  }

  bool all_states_in_school = true;
  bool any_states_in_school = false;
  for (int state = 1; state <= 8; ++state) {
    all_states_in_school = all_states_in_school && during_school_term(state, yday);
    any_states_in_school = any_states_in_school || during_school_term(state, yday);
  }

  if (!any_states_in_school) {
    return;
  }
  // 0 -> AUS i.e. all schools are locked down
  if (areSchoolsLockedDown[0]) {
    return;
  }
  const bool anySchoolsLockedDown =
    areSchoolsLockedDown[1] ||
    areSchoolsLockedDown[2] ||
    areSchoolsLockedDown[3] ||
    areSchoolsLockedDown[4] ||
    areSchoolsLockedDown[5] ||
    areSchoolsLockedDown[6] ||
    areSchoolsLockedDown[7] ||
    areSchoolsLockedDown[8] ||
    areSchoolsLockedDown[9];


  int all_full_time =
    school_days_per_wk.containsElementNamed("all_full_time") &&
    school_days_per_wk["all_full_time"];


  int combn2[10][2] =
    {{1, 2},
    {1, 3},
    {1, 4},
    {1, 5},
    {2, 3},
    {2, 4},
    {2, 5},
    {3, 4},
    {3, 5},
    {4, 5}};

  int combn3[10][3] =
    {{1, 2, 3},
    {1, 2, 4},
    {1, 2, 5},
    {1, 3, 4},
    {1, 3, 5},
    {1, 4, 5},
    {2, 3, 4},
    {2, 3, 5},
    {2, 4, 5},
    {3, 4, 5}};


  int DaysPerWk[NSTATES1][21] = {};
  for (int s = 0; s < NSTATES1; ++s) {
    IntegerVector sDaysPerWk = school_days_per_wk[s];
    if (sDaysPerWk.length() != 21) {
      stop("Internal error: sDaysPerWk.length() != 21.");
    }
    for (int a = 0; a < 21; ++a) {
      int da = sDaysPerWk[a];
      DaysPerWk[s][a] = da;
    }
  }

  const int wday0 = wday - 1;
  if (wday0 < 0 || wday0 >= 5) {
    stop("'wday' out of range (1:5).");
  }


  if (day < 7 && wday < 6) {
#pragma omp parallel for num_threads(nThread)
    for (int k = 0; k < NPUPILS; ++k) {
      int k5 = wday0 + (5 * k);
      int i = schoolIndices[k];

      int Agei = (Age[i] > 20) ? 20 : Age[i];
      if (only_Year12 && Agei < 17) {
        AttendsWday[k5] = 0;
        continue;
      }
      if (all_full_time) {
        AttendsWday[k5] = 1;
        continue;
      }

      // schools_days_per_wk
      int statei = State[i];
      int daysPerWk_Agei = DaysPerWk[statei][Agei];
      if (daysPerWk_Agei <= 0 || daysPerWk_Agei > 5) {
        // only daysPerWk_Agei == 0 should be valid but we presume
        // any other values are zero
        AttendsWday[k5] = 0;
        continue;
      }


      // if daysPerWk_Agei == 5 then they go to school every day
      // otherwise we need to work out whether they will stay at
      // home (continue;)
      bool attends_today = daysPerWk_Agei == 5;

      if (!attends_today) {
        // this is ok because school[i] is only in indices of pupils
        int schooli = School[i] - 1;
        if (daysPerWk_Agei == 1) {
          // this state-age combination is only set to go to school
          // once per week
          // randomly choose a weekday for this school age combination
          //
          // Verify good uniform distribution by just adding agei + schooli
          // library(data.table)
          // DT <- CJ(schools = seq_len(9501), ages = 0:20)
          // DT[, .N, keyby = .(M = (schools + ages) %% 5L)]
          // 39904 each

          // for both 1 and 4 days a week
          //    ((Agei + schooli) % 5) + 1
          // is a constant weekday for each person
          // for one day a week, the person *attends* on that day
          // for four days a week, the person doesn't attend


          attends_today = ((Agei + schooli) % 5) + 1 == wday;
        } else if (daysPerWk_Agei == 4) {
          attends_today = ((Agei + schooli) % 5) + 1 != wday;
        } else {
          // 2, 3 days per week. We need to access the
          // list of combinations
          int coli = (Agei + schooli) % 10;
          if (daysPerWk_Agei == 2) {
            attends_today = combn2[coli][0] == wday || combn2[coli][1] == wday;
          } else {
            attends_today = combn3[coli][0] == wday || combn3[coli][1] == wday || combn3[coli][2] == wday;
          }
        }
      }
      AttendsWday[k5] = attends_today ? 1 : 0;
    }
  }

  int i_visits[NSCHOOLS] = {};
  bool lockdownTriggeredByCritic[NSTATES1] = {};


#if defined _OPENMP && _OPENMP >= 201511
#pragma omp parallel for num_threads(nThread) \
  reduction(+:i_visits[:NSCHOOLS])            \
  reduction(||:lockdownTriggeredByCritic)
#endif
  for (int k = 0; k < NPUPILS; ++k) {
    int i = schoolIndices[k];
    if (anySchoolsLockedDown && areSchoolsLockedDown[State[i]]) {
      continue;
    }

    int statusi = Status[i];
    if (statusi <= 0) {
      continue;
    }
    int k5 = wday0 + (5 * k);
    if (!AttendsWday[k5]) {
      continue;
    }
    int statei = State[i];
    if (!all_states_in_school && !during_school_term(statei, yday)) {
      continue;
    }

    int schooli = School[i] - 1;
    if (!Erand[schooli]) {
      continue;
    }
    // rcauchy relates to the single day
    if (statusi == STATUS_NOSYMP) {
      i_visits[schooli] += 1;
    } else if ((statusi % ISOLATED_PLUS) == STATUS_CRITIC) {
      lockdownTriggeredByCritic[statei] = true;
    }
  }

  int newInfectionsBySchool[NSCHOOLS] = {};

  std::vector<char> Prand = do_lemire_char_par(NPUPILS, q_school_dbl, Seed, nThread);

#if defined _OPENMP && _OPENMP >= 201511
#pragma omp parallel for num_threads(nThread) reduction(+ :newInfectionsBySchool[:NSCHOOLS])
#endif
  for (int k = 0; k < NPUPILS; ++k) {
    int k5 = wday0 + (5 * k);
    if (!AttendsWday[k5]) {
      continue;
    }
    int i = schoolIndices[k];
    int statei = State[i];
    if (anySchoolsLockedDown && areSchoolsLockedDown[statei]) {
      continue;
    }
    if (!all_states_in_school && !during_school_term(statei, yday)) {
      continue;
    }

    if (Status[i]) {
      continue;
    }

    int schooli = School[i] - 1;
    if (!Erand[schooli]) {
      continue;
    }

    // N.B. This logic means the 'first' people in the table get infected
    // first.  We could randomize this, but I don't think it matters.

    // TODO: make students of the same age more likely/first to be infected
    if (i_visits[schooli] && Prand[k]) {
      Status[i] = STATUS_NOSYMP;
      InfectedOn[i] = yday;
      newInfectionsBySchool[schooli] += 1;
    }
  }

  int schoolsWithGeqInfections[NSTATES1] = {};
  const int b = lockdown_trigger_schools_with_infections_geq;
  for (int schooli = 0; schooli < NSCHOOLS; ++schooli) {
    if (newInfectionsBySchool[schooli] >= b) {
      if (optionz) Rcout << "schooli = " << schooli << "\t" << "newInfectionsBySchool = " << newInfectionsBySchool[schooli] << "\n";
      int statei = static_cast<int>(state_by_school[schooli]);
      schoolsWithGeqInfections[statei] += 1;
    }
  }



  for (int s = 0; s < NSTATES1; ++s) {
    if (lockdownTriggeredByCritic[s]) {
      state_trigger_pulled[s] = 2;
    } else if (schoolsWithGeqInfections[s] >= lockdown_trigger_schools_with_infections) {
      state_trigger_pulled[s] = 1;
    }
  }



  // void
}


void infect_household(IntegerVector Status,
                      IntegerVector InfectedOn,
                      IntegerVector shortSA2,
                      IntegerVector hid,
                      IntegerVector HouseholdSize,
                      const std::vector<int> &hhIndex,
                      IntegerVector Resistance,
                      IntegerVector Age,
                      int n_households,
                      const std::vector<char> &Hrand,
                      IntegerVector Srand,
                      int yday,
                      int N,
                      IntegerVector HouseholdInfectedToday,
                      int resistance1,
                      int q_household,
                      int a_household,
                      int nThread = 1) {
  // resistance_penalty = penalty against Resistance[i]
  // that makes infection more likely. Higher penalties
  // make infection more likely among otherwise resistant
  // individuals


#pragma omp parallel for num_threads(nThread)
  for (int k = 0; k < n_households; ++k) {
    // if the household is not infectible
    // skip immediately
    if (!Hrand[k]) {
      continue;
    }
    int i = hhIndex[k];

    if (HouseholdSize[i] == 1) {
      // no transmission in single-person household
      continue;
    }







    if (HouseholdSize[i] == 2) {
      // in this case, we only need to check adjacent
      bool household_infected = Status[i] > 0 || Status[i + 1] > 0;
      // Prima facie we have a race condition on Status, but in fact
      // we have skipped anyone who has Status[i] != 0 so the only way
      // the following assignment can occur is if the other thread is
      // skipping
      if (household_infected && Srand[i] < q_household) {
        int r = resistance1;
        if (Status[i] == 0 && Resistance[i] < r) {
          Status[i] = STATUS_NOSYMP;
          InfectedOn[i] = yday + 1;
        }

        if (Status[i + 1] == 0 && Resistance[i + 1] < r) {
          Status[i + 1] = STATUS_NOSYMP;
          InfectedOn[i + 1] = yday + 1;
        }
      }
      continue;
    }

    // For households larger than 2, we do the entire household
    // when we first see the house. On subsequent i in the same
    // household, skip -- we've already calculated it


    bool household_infected = Status[i] > 0;

    int nh = HouseholdSize[i]; // just for convenience
    // loop through the household, stop once an infection detected
    for (int j = 1; j < nh; ++j) {
      if (Status[i + j] > 0) {
        household_infected = true;
      }
    }

    if (household_infected) {
      // return to first person and infect as appropriate
      // (don't infect already infected)
      for (int j = 0; j < nh; ++j) {
        if (Srand[i] >= q_household) {
          continue;
        }
        int r = resistance1;
        int ij = i + j;
        if (Status[ij] == 0 && Resistance[ij] < r) {
          Status[ij] = STATUS_NOSYMP;
          InfectedOn[ij] = yday + 1;
        }
      }
      HouseholdInfectedToday[i] = 1;
    }
  }
  // void
}



// [[Rcpp::export]]
List do_au_simulate(IntegerVector Status,
                    IntegerVector InfectedOn,
                    IntegerVector State,
                    IntegerVector SA2,
                    IntegerVector hid,
                    IntegerVector seqN,
                    IntegerVector HouseholdSize,
                    IntegerVector Age,
                    IntegerVector School,
                    IntegerVector DZN,
                    IntegerVector wid,
                    IntegerVector nColleagues,
                    IntegerVector PlaceTypeBySA2,
                    IntegerVector LabourForceStatus,
                    IntegerVector Resistance,
                    IntegerVector Seed,
                    List Policy,
                    List nPlacesByDestType,
                    List FreqsByDestType,
                    List Epi, /* Epidemiological parameters */
                    IntegerVector nSupermarketsAvbl,
                    IntegerVector SupermarketTypical,
                    List minPlaceID_nPlacesByDestType,
                    int yday_start,
                    int days_to_sim,
                    int N = 25e6,
                    bool display_progress = true,
                    bool on_terminal = false,
                    bool by_state = true,
                    int returner = 0,
                    int console_width = 80,
                    int optionz = 0,
                    int nThread = 1) {


  Progress p(days_to_sim, display_progress && console_width <= 1);



  if (FreqsByDestType.length() <= 98 ||
      nPlacesByDestType.length() <= 98) {
    stop("Internal error: FreqsByDestType.length < 98");
  }

  IntegerVector nSupermarketsBySA2 = nPlacesByDestType[97];
  IntegerVector SupermarketFreq = FreqsByDestType[97]; // Type_by_TypeInt.fst

  if (N != SupermarketFreq.length() || N != nSupermarketsAvbl.length()) {
    stop("Internal error: SupermarketFreq.length mismatch");
  }
  if (NSA2 != nSupermarketsBySA2.length()) {
    stop("Internal error: nSupermarketsBySA2.length() != NSA2.");
  }

  if (PlaceTypeBySA2.length() > 1) {
    stop("Internal error: PlaceTypeBySA2 not implemented yet.");
  }

#ifdef _OPENMP
  if (nThread < 1 || nThread > omp_get_num_procs()) {
    stop("Internal error: nThread out of range");
  }
#endif



  // attach policy changes

  bool supermarkets_open = true;
  if (Policy.length() && Policy.containsElementNamed("supermarkets_open")) {
    supermarkets_open = Policy["supermarkets_open"];
  }
  bool use_mpps = Policy.length() && Policy.containsElementNamed("max_persons_per_supermarket");
  const int max_persons_per_supermarket =
    use_mpps ? Policy["max_persons_per_supermarket"] : 2e9;


  bool cafes_open = true;
  if (Policy.length() && Policy.containsElementNamed("cafes_open")) {
    cafes_open = Policy["cafes_open"];
  }
  const int max_persons_per_cafe = 10; // TODO: all this needs to be IntegerVectors!
  // school policies
  bool schools_open = false;
  bool only_Year12  = false;

  if (Policy.length() && Policy.containsElementNamed("schools_open")) {
    schools_open = Policy["schools_open"];
  }
  if (Policy.length() && Policy.containsElementNamed("only_Year12")) {
    only_Year12 = Policy["only_Year12"];
  }
  if (!Policy.containsElementNamed("school_days_per_wk")) {
    stop("Internal error: 'Policy' did not contain element 'school_days_per_wk'.");
  }
  List school_days_per_wk = Policy["school_days_per_wk"];

  // Dynamic modelling relating to schools
  int lockdown_trigger_schools_with_infections = 4;
  int lockdown_trigger_schools_with_infections_geq = 3;
  int lockdown_trigger_schools_with_infections_duration_of_lockdown = 28;
  int lockdown_trigger_schools_with_any_critical = 1;
  int lockdown_trigger_schools_with_any_critical_duration_of_lockdown = 91;
  if (Policy.containsElementNamed("lockdown_triggers__schools")) {
    List lockdown_triggers__schools = Policy["lockdown_triggers__schools"];
    List lockdown_triggers__schools_AUS = lockdown_triggers__schools[0];
    lockdown_trigger_schools_with_infections = lockdown_triggers__schools_AUS["default_schools_with_infections"];
    lockdown_trigger_schools_with_infections_geq = lockdown_triggers__schools_AUS["default_schools_with_infections_geq"];
    lockdown_trigger_schools_with_infections_duration_of_lockdown = lockdown_triggers__schools_AUS["default_schools_with_infections_duration_of_lockdown"];
    lockdown_trigger_schools_with_any_critical = lockdown_triggers__schools_AUS["default_schools_with_any_critical"];
    lockdown_trigger_schools_with_any_critical_duration_of_lockdown = lockdown_triggers__schools_AUS["default_schools_with_any_critical_duration_of_lockdown"];
  }



  bool do_contact_tracing = true;
  if (Policy.length() && Policy.containsElementNamed("do_contact_tracing")) {
    do_contact_tracing = Policy["do_contact_tracing"];
  }
  IntegerVector TestsAvbl(NSTATES1);
  // 2020-04-24 numbers
  TestsAvbl[0] = 482370 - 466659;
  if (Policy.length() && Policy.containsElementNamed("tests_by_state")) {
    IntegerVector tests_by_state = Policy["tests_by_state"];
    if (tests_by_state.length() != NSTATES1) {
      warning("tests_by_state.length() != NSTATES + 1 and will be ignored.");
    } else {
      for (int s = 0; s < NSTATES1; ++s) {
        TestsAvbl[s] = tests_by_state[s];
      }
    }
  }
  int ct_days_before_test = Policy["contact_tracing_days_before_test"];
  int ct_days_until_result = Policy["contact_tracing_days_until_result"];

  int workplaces_open = 0;
  int workplace_size_max = 10;
  double a_workplace_rate = 1;
  if (Policy.containsElementNamed("workplaces_open")) {
    workplaces_open = Policy["workplaces_open"];
    if (Policy.containsElementNamed("workplace_size_max")) {
      workplace_size_max = Policy["workplace_size_max"];
    }
    if (Epi.containsElementNamed("a_workplace_rate")) {
      a_workplace_rate = Epi["a_workplace_rate"];
    }
  }

  double a_household_rate = 1;
  if (Epi.containsElementNamed("a_household_rate")) {
    a_household_rate = Epi["a_household_rate"];
  }
  double a_schools_rate = 1;
  // int a_household_rate = 1000;
  // int a_schools_rate = 1000;



  IntegerVector TestedOn = no_init(N);

  // TODO: make user-avbl
  int ptest_per_mille_sympto = 1000; // 100%
  int ptest_per_mille_asympto = 10; // 1%

  // Age-based lockdown
  bool age_based_lockdown = false;
  IntegerVector AgesLockdown(100);
  if (Policy.containsElementNamed("age_based_lockdown")) {
    AgesLockdown = Policy["age_based_lockdown"];
    if (AgesLockdown.length() == 100) {
      for (int i = 0; i < 100; ++i) {
        if (AgesLockdown[i] != 0) {
          age_based_lockdown = true;
          break;
        }
      }
    }
  }






  // attach epipars
  double incubation_m = Epi["incubation_mean"];
  double incubation_s = Epi["incubation_sigma"];
  double illness_m = Epi["illness_mean"];
  double illness_s = Epi["illness_sigma"];
  double r_location = Epi["r_location"];
  double r_schools_location = Epi["r_schools_location"];
  double r_supermarket_location = Epi["r_supermarket_location"];
  double r_work_location = Epi["r_work_location"];
  double r_scale = Epi["r_scale"];
  int resistance_threshold = Epi["resistance_threshold"];
  int p_asympto = Epi["p_asympto"];
  int p_critical = Epi["p_critical"];
  int p_death = Epi["p_death"];

  int incubation_d = Epi["incubation_distribution"];
  int illness_d = Epi["illness_distribution"];
  int r_d = Epi["r_distribution"];

  bool do_dirac_every = Epi.containsElementNamed("dirac_num");
  int dirac_num = do_dirac_every ? Epi["dirac_num"] : 0;
  int dirac_per = do_dirac_every ? Epi["dirac_per"] : 0;

  int q_household = Epi["q_household"];
  int q_school = Epi["q_school"];
  double q_school_dbl = ((double)(q_school - INT_MIN) / 2) / (double)INT_MAX;

  IntegerVector Srand = do_lemire_rand_par(N, Seed, nThread);


  // household infections
  int n_households = 0;
#pragma omp parallel for num_threads(nThread) reduction(+:n_households)
  for (int i = 0; i < N; ++i) {
    n_households += seqN[i] == 1;
  }
  std::vector<int> hhIndex; // head of household index
  hhIndex.reserve(n_households);
  {
    int i = 0;
    while (i < N) {
      if (i == 0 || hid[i - 1] != hid[i]) {
        hhIndex.push_back(i);
        i += HouseholdSize[i];
      } else {
        stop("Internal error: hhIndex/HouseholdSize mismatch.");
      }
    }
  }


  // Hrand is the probability of infecting 100% of your household
  std::vector<char> Hrand = do_lemire_char_par(n_households, a_household_rate, Seed, nThread);



  int n_pupils = 0;
  unsigned char state_by_school[NSCHOOLS] = {};
  unsigned char state_trigger_pulled[NSTATES1] = {};
  std::vector<int> schoolsIndex;
  schoolsIndex.reserve(NPUPILS);
  for (int i = 0; i < N; ++i) {
    if (School[i] > 0) {
      int schooli = School[i] - 1;
      ++n_pupils;
      schoolsIndex.push_back(i);
      if (!state_by_school[schooli]) {
        state_by_school[schooli] = State[i];
      }
    }
  }
  std::vector<char> Erand = do_lemire_char_par(NSCHOOLS, a_schools_rate, Seed, nThread);


  if (n_pupils != NPUPILS) {
    Rcout << NPUPILS << "\n";
    Rcout << n_pupils << "\n";
    stop("n_pupils much larger than expected: likely an overestimate of schools.");
  }

  // 5 days (at most) in a school week
  IntegerVector AttendsWday = no_init(NPUPILS * 5);
  // memset(AttendsWday, 0, sizeof AttendsWday);

  std::array<int, NSTATES1> schools_lockdown_until = {};
  std::array<bool, NSTATES1> areSchoolsLockedDown = {};


  std::vector<int> widIndex;
  widIndex.reserve(WID_SUPREMUM);
  for (int i = 0; i < N; ++i) {
    if (wid[i] > 0) {
      widIndex.push_back(i);
    }
  }
  std::vector<char> Wrand = do_lemire_char_par(WID_SUPREMUM, a_workplace_rate, Seed, nThread);

  // variables which will be updated on day = 0
  // int n_schools = -1;

  IntegerVector nInfected = no_init(days_to_sim);
  IntegerVector Incubation = no_init(N);
  IntegerVector Illness = no_init(N);
  IntegerVector SupermarketTarget = no_init(N);

  // These could potentially be smaller vectors
  IntegerVector HouseholdInfectedToday = no_init(N); // was the household infected today?

#pragma omp parallel for num_threads(nThread)
  for (int i = 0; i < N; ++i) {
    Incubation[i] = 0;
    Illness[i] = 0;
    SupermarketTarget[i] = 0;
    HouseholdInfectedToday[i] = 0;
    TestedOn[i] = 0;
  }

  if (which_unsorted_int(SA2)) {
    stop("SA2 was unsorted.");
  }
  IntegerVector shortSA2 = shorten_sa2s_ordered(SA2);

  IntegerVector PlaceId = clone(SupermarketTarget);

  // For example, SupermarketFreq[i] = 365  => every day
  // SupermarketFreq[i] = 1 every year.  So we create a vector
  // of 1:366 and compare that to the individual's tendency to
  // visit. So if TodaysHz[i] = 366 they will not visit anything
  // regardless; if TodaysHz[i] = 1 they will visit everything.

  // Note these are < N
  IntegerVector TodaysK = dqsample_int2(1000, NTODAY);
  IntegerVector TodayHz = dqsample_int2(365, NTODAY);


  if (minPlaceID_nPlacesByDestType.length() != 106) {
    stop("minPlaceID_nPlacesByDestType.length() != 106.");
  }

  IntegerVector minmaxState = do_minmax_par(State, nThread);
  if (minmaxState[0] != 1) {
    stop("Internal error: minmaxState[0] != 1");
  }
  if (minmaxState[1] != NSTATES) {
    stop("Internal error: minmaxState[0] != NSTATES");
  }
  List FirstFinalsState = sa2_firsts_finals(State, NSTATES);
  IntegerVector State_firsts = FirstFinalsState[0];
  IntegerVector State_finals = FirstFinalsState[1];

  List FirstFinalsSA2 = sa2_firsts_finals(shortSA2, NSA2);
  IntegerVector SA2_firsts = FirstFinalsSA2[0];
  IntegerVector SA2_finals = FirstFinalsSA2[1];

  NumericVector notUsed = {1,2};
  // returner 0: Data frame of statuses
  DataFrame Statuses = DataFrame::create(Named("InitialStatus") = clone(Status));

  // returner 1: days by state by age by status
  //
  // const int out1_len = (days_to_sim * out1d_len);
  // IntegerVector out1 = no_init(out1_len);
  // int out1i = 0;
  IntegerVector out1 = no_init(days_to_sim * 7); // 7 statuses for each
  IntegerVector out2(days_to_sim * NSTATES * 7); // preallocate




  for (int day = 0; day < days_to_sim; ++day) {

    // Start with basic calendar information, needed for
    // opening hours, especially of schools
    int yday = yday_start + day;

    assert_is_weekday(day);
    assert_is_monday(day);
    // i.e yday 1 was a Wednesday, make the week start on Monday
    int wday = wday_2020[((yday - 1) % 7)];
    bool is_weekday = yday2weekday(yday);
    bool is_monday = yday2monday(yday);



    // Aggregate the information to return
    // Doing it the head rather than the tail of this loop means
    // we can (a) exit early if there are no infections and (b) potentially
    // supply these aggregates to other functions to provide early returns.
    int n_infected_today = 0;
    if (returner == 0) {
#if defined _OPENMP && _OPENMP >= 201511
#pragma omp parallel for num_threads(nThread) reduction(+:n_infected_today)
#endif
      for (int i = 0; i < N; ++i) {
        int statusi = Status[i];
        if (statusi > 0) {
          n_infected_today += ((statusi - ISOLATED_PLUS) != 0);
        }
      }
      nInfected[day] = n_infected_today;
    }
    // prepare returner 1
    if (returner == 1) {
      int n_killed = 0;
      int n_healed = 0;
      int n_suscep = 0;
      int n_nosymp = 0;
      int n_insymp = 0;
      int n_critic = 0;
      int n_isolat = 0;
#if defined _OPENMP && _OPENMP >= 201511
#pragma omp parallel for num_threads(nThread) reduction(+ : n_killed,n_healed,n_suscep,n_nosymp,n_insymp,n_critic,n_isolat)
#endif
      for (int i = 0; i < N; ++i) {
        if (Status[i] == 0) {
          n_suscep += 1;
          continue;
        }
        int statusi = Status[i];
        n_killed += statusi == STATUS_KILLED;
        n_healed += statusi == STATUS_HEALED;
        n_nosymp += statusi == STATUS_NOSYMP;
        n_insymp += statusi == STATUS_INSYMP;
        n_critic += statusi == STATUS_CRITIC;
        n_isolat += statusi >= ISOLATED_PLUS;
      }


      out1[7 * day + 0] = n_killed;
      out1[7 * day + 1] = n_healed;
      out1[7 * day + 2] = n_suscep;
      out1[7 * day + 3] = n_nosymp;
      out1[7 * day + 4] = n_insymp;
      out1[7 * day + 5] = n_critic;
      out1[7 * day + 6] = n_isolat;

      n_infected_today = n_nosymp + n_insymp + n_critic + n_isolat;
    }

    if (returner == 2) {
      if (NSTATES != 9) {
        stop("Internal error: NSTATES != 9");
      }
      // int out2d[63] = {};
      // by state
#pragma omp parallel for num_threads(nThread) reduction(+:n_infected_today)
      for (int state = 0; state < NSTATES; ++state) {
        int first = State_firsts[state];
        int final = State_finals[state];
        for (int i = first; i < final; ++i) {
          if (Status[i] > 0) {
            n_infected_today += 1;
          }
          int statusi = Status[i] + 2;

          if (statusi > 5) {
            statusi = 6;
          }
          int oi2 =  7 * day * NSTATES + state * (7) + statusi;
          out2[oi2] += 1;
        }
      }
    }








    if (display_progress) {
      if (console_width <= 1) {
        p.increment();
      } else {

        int pbar_w = console_width - 20 - 8;

        // daily increment
        double di = ((double)pbar_w)  / ((double)(days_to_sim));
        double b_d = ((double)day + 1) * di;

        double w_d = 0;
        double r_d = 0; // remainder

        Rcout << "| ";
        int w = 2;

        // w < 1024 in case of very large console width
        while (w < pbar_w && w < 1024) {
          Rcpp::checkUserInterrupt();
          int w_yday = w_d + yday_start + 1 + 0.5;
          int w_wday = wday_2020[((w_yday - 1) % 7)];
          w_d += di;
          r_d += di;
          int max_reds = (on_terminal) ? 0 : 2;
          while (r_d > 1 && w < 1024) {
            ++w;
            r_d -= 1;
            if ((w_d + 0.5) < b_d) {
              if (w_wday < 6 || max_reds <= 0) {
                if (max_reds && !during_school_term(1, w_yday)) {
                  Rcout << "\033[34m" << "=" << "\033[39m";
                } else {
                  Rcout << "=";
                }
              } else {
                --max_reds;
                Rcout << "\033[31m" << "=" << "\033[39m";
              }
            } else {
              // Rcout << w_yday << " ";
              Rcout << "_";
            }
          }
        }
        Rcout << " | ";

        // int last_yday = (days_to_sim + yday_start);

        Rcout << "day = " << day + 1 << "/" << days_to_sim << " ";

        // asking for log(0) = -Inf number of console outputs will do exactly
        // what is asked

        // number of digits in n_infected_today (-1)
        int ndig_nit = (n_infected_today > 9) ? floor(log10(n_infected_today)) : 0;
        for (int w = ndig_nit; w < 8; ++w) {
          Rcout << " ";
        }


        Rcout << n_infected_today << "\r";
        if (day == days_to_sim - 1) {
          Rcout << "\n";
        }
      }
    }



    // no more infections?
    if (n_infected_today == 0) {
      continue;
    }
    if (optionz != 2 || returner == 0) {
      Statuses.push_back(clone(Status));
    }
    if (optionz == 3) {
      continue;
    }

    // Exit point.
    // We have aggregated "today's" information above so if we are on the final
    // day but don't break here we will model the result but never save it.
    if (day + 1 == days_to_sim) {
      break;
    }


    // at this point we know there are infections
    int first_infected_i = 0;
    while (Status[first_infected_i] <= 0) {
      ++first_infected_i;
    }
    int final_infected_i = N;
    while (Status[final_infected_i - 1] <= 0) {
      --final_infected_i;
    }

    if (age_based_lockdown) {
#pragma omp parallel for num_threads(nThread)
      for (int i = 0; i < N; ++i) {
        int agei = Age[i];
        if (AgesLockdown[agei]) {
          Status[i] += ISOLATED_PLUS;
        }
      }
    }



#if defined _OPENMP && _OPENMP >= 201511
#pragma omp parallel for num_threads(nThread)
#endif
    for (int i = first_infected_i; i < final_infected_i; ++i) {

      // First, examine all individuals infected last night
      // and move them accordingly.
      if (Status[i] > 0) {
        int statusi = Status[i];
        // So the person is either
        //   1  Incubation period
        //   2  Illness period
        //   3  Critical

        //  Each position has three possibilities
        //   1  -->  -1   2 -->  -1   3 -->  -2
        //   1  -->   1   2 -->   2   3 -->  -1
        //   1  -->   2   2 -->   3   3 -->   3
        //



        // If the incubation period is zero, it has not been evaluated
        // for this individual. Draw from the distribution (once)
        if (Incubation[i] == 0) {
          Incubation[i] = incubRand(incubation_m, incubation_s, incubation_d);
        }
        int incubation = Incubation[i];


        if (yday <= InfectedOn[i] + incubation) {
          // Today is before the incubation period is over
          // nothing to do -- keep whatever the status is
        } else {
          // This is reevaluated each day, but because they are uniform
          // this is okay. Nonetheless we don't second-guess the original
          // statusi
          bool becomes_symptomatic = statusi > STATUS_NOSYMP || (unifRand(0, 1000) > p_asympto);
          bool becomes_critical = statusi == STATUS_CRITIC || (becomes_symptomatic && unifRand(0, 1000) < p_critical);
          bool dies = becomes_critical && unifRand(0, 1000) < p_death;
          bool in_isolation =
            (statusi == (STATUS_SUSCEP + ISOLATED_PLUS)) || // most common first
            (statusi == (STATUS_HEALED + ISOLATED_PLUS)) ||
            (statusi == (STATUS_KILLED + ISOLATED_PLUS)) ||
            (statusi == (STATUS_NOSYMP + ISOLATED_PLUS)) ||
            (statusi == (STATUS_INSYMP + ISOLATED_PLUS)) ||
            (statusi == (STATUS_CRITIC + ISOLATED_PLUS));

          // As before with incubation
          if (Illness[i] == 0) {
            Illness[i] = illRand(illness_m, illness_s, illness_d);
          }
          int illness = Illness[i];
          // Today is after the incubation, during the illness.
          // Assumption: if the person becomes critical, it happens immediately.
          if (yday <= InfectedOn[i] + incubation + illness) {
            if (becomes_symptomatic) {
              Status[i] = STATUS_INSYMP + becomes_critical * CRITIC_MINUS_INSYMP;
              Status[i] += in_isolation * ISOLATED_PLUS;
            } else {
              // nothing to do: they're still ill, but at Status 1.
            }
          } else {
            // Today is after the illness has run its course
            Status[i] = STATUS_HEALED - dies * HEALED_MINUS_KILLED;
            // int sa2i = shortSA2[i];
            // interactions no longer relevant
            continue;
          }
        }
      }
    }

    // This function actually performs the interactions and infections


    if (supermarkets_open) {
      infect_supermarkets(Status,
                          InfectedOn,
                          shortSA2,
                          nThread,
                          Age,
                          LabourForceStatus,
                          nSupermarketsBySA2,
                          nSupermarketsAvbl,
                          Resistance,
                          day,
                          wday,
                          yday,
                          N,
                          SupermarketTypical,
                          r_supermarket_location, r_scale, r_d,
                          do_dirac_every, dirac_num, dirac_per,
                          SupermarketFreq,
                          TodayHz,
                          resistance_threshold,
                          max_persons_per_supermarket);
    }

    // infect cafes
    if (cafes_open) {
      IntegerVector nCafes = FreqsByDestType[15 - 1];

      infect_place(15 - 1, // 15 is place id for cafe -1 for 0-index
                   Status,
                   InfectedOn,
                   shortSA2,
                   SA2_firsts,
                   SA2_finals,
                   nThread,
                   minPlaceID_nPlacesByDestType,
                   day,
                   wday,
                   yday,
                   N,
                   Resistance,
                   resistance_threshold,
                   nCafes,
                   TodayHz,
                   r_location,
                   r_scale,
                   r_d,
                   do_dirac_every, dirac_num, dirac_per,
                   max_persons_per_cafe,
                   TodaysK);
    }



    if (is_weekday && schools_open) {
      // Check whether any states are in lockdown
      // We only check every 'Monday' (i.e. just before Monday)
      // to reflect decision timing.


        for (int s = 0; s < NSTATES; ++s) {
          // Each Monday morning we check whether there has been an
          // event to trigger a lockdown (and what sort of trigger it was)
          // If there was one we set the lockdown period then reset the
          // trigger. A lockdown so triggered takes place immediately (i.e.
          // before 9am)

          // Otherwise, we continue any extant lockdown

          if (is_monday && state_trigger_pulled[s] != 0) {
              areSchoolsLockedDown[s] = true;
              int d_1 = lockdown_trigger_schools_with_infections_duration_of_lockdown;
              int d_2 = lockdown_trigger_schools_with_any_critical_duration_of_lockdown;
              schools_lockdown_until[s] = yday + ((state_trigger_pulled[s] == 1) ? d_1 : d_2);
              state_trigger_pulled[s] = 0;  // can only be reset on Mondays
          } else if (schools_lockdown_until[s]) {
            schools_lockdown_until[s] -= 1;
            areSchoolsLockedDown[s] = schools_lockdown_until[s] > yday;
          }
        }

      infect_school(Status, InfectedOn, School, Age,
                    AttendsWday,
                    day, wday, yday,
                    N,
                    State,
                    state_by_school,
                    areSchoolsLockedDown,
                    state_trigger_pulled,
                    lockdown_trigger_schools_with_infections,
                    lockdown_trigger_schools_with_infections_geq,
                    lockdown_trigger_schools_with_infections_duration_of_lockdown,
                    lockdown_trigger_schools_with_any_critical,
                    lockdown_trigger_schools_with_any_critical_duration_of_lockdown,
                    shortSA2,
                    schoolsIndex,
                    Erand,
                    r_schools_location, r_scale, r_d,
                    do_dirac_every, dirac_num, dirac_per,
                    Srand,
                    Seed,
                    q_school_dbl,
                    only_Year12,
                    school_days_per_wk,
                    nThread,
                    optionz,
                    -99);
    }
    if (workplaces_open) {
      infect_dzn(Status, InfectedOn,
                 DZN, wid,
                 widIndex,
                 LabourForceStatus, nColleagues,
                 day, wday, yday, N,
                 resistance_threshold,
                 Wrand,
                 workplaces_open,
                 workplace_size_max,
                 r_work_location, r_scale, r_d,
                 do_dirac_every, dirac_num, dirac_per,
                 TodaysK, Resistance, 0, optionz, nThread);
      if (day < 2 && optionz) {
        Rcout << "infected_dzn = " << day << "\n";
      }
    }



    // finally

    infect_household(Status, InfectedOn,
                     shortSA2,
                     hid, HouseholdSize,
                     hhIndex,
                     Resistance, Age,
                     n_households,
                     Hrand, Srand,
                     yday, N, HouseholdInfectedToday,
                     resistance_threshold,
                     q_household,
                     nThread);
    if (day < 2 && optionz) {
      Rcout << "infected_household " << "\n";
    }

    if (do_contact_tracing) {
      contact_tracing(Status,
                      InfectedOn,
                      Incubation,
                      day,
                      yday,
                      seqN,
                      HouseholdSize,
                      TestsAvbl,
                      by_state,
                      State,
                      hid,
                      HouseholdInfectedToday,
                      N,
                      School,
                      PlaceId,
                      ptest_per_mille_asympto,
                      ptest_per_mille_sympto,
                      TodaysK,
                      ct_days_before_test,
                      ct_days_until_result,
                      nThread,
                      TestedOn);
    }


  }

  if (returner == 0) {
    return Rcpp::List::create(Named("nInfected") = nInfected,
                              Named("Statuses") = Statuses,
                              Named("TestedOn") = TestedOn);
  }
  if (returner == 1) {
    return List::create(Named("Status7") = out1);
  }
  if (returner == 2) {
    return List::create(Named("Status7") = out2);
  }

  return Statuses;
}



