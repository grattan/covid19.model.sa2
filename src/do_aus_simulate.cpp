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

constexpr int MIN_ISOL = STATUS_KILLED + ISOLATED_PLUS;

inline bool isolatable(const int & statusi) {
  // Equivalent to  0 <= status < MIN_ISOL
  // from optimizing_cpp.pdf
  return (unsigned int)statusi < (unsigned int)MIN_ISOL;
}

inline bool is_isolated(const int & statusi) {
  return statusi >= MIN_ISOL;
}

inline bool is_infected(const int & statusi) {
  return statusi > 0 && statusi < MIN_ISOL;
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
                     IntegerVector Age,
                     const int day,
                     const int yday,
                     const std::vector<unsigned char> &seqN,
                     const std::vector<unsigned char> &HouseholdSize,
                     IntegerVector TestsAvbl,
                     bool by_state,
                     IntegerVector SA2,
                     IntegerVector hid,
                     IntegerVector HouseholdInfectedToday,
                     const int N,
                     IntegerVector School,
                     const std::vector<int> &schoolsIndex,
                     const int ptest_per_mille_sympto,
                     const int ptest_per_mille_asympto,
                     const double ct_success,
                     IntegerVector TodaysK,
                     const int days_before_test,
                     const int days_to_notify,
                     const int nThread,
                     IntegerVector TestedOn) {

#ifdef _OPENMP
  if (day == 0 && (nThread < 1 || nThread > omp_get_num_procs())) {
    // lots of large ints nearby!
    stop("Internal error: nThread out of range");
  }
#endif

  if (day == 0) {
    int n_pupils = schoolsIndex.size();
    if (n_pupils != NPUPILS) {
      stop("Internal error(contact tracing): n_pupils != NPUPILS");
    }
    if (ct_success < 0 || ct_success > 1) {
      stop("Internal error(contact tracing): ct_success < 0 || ct_success");
    }
  }

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

  // school and grade infection
  bool school_infected[NSCHOOLS][16];


#if defined _OPENMP && _OPENMP >= 201511
#pragma omp parallel for num_threads(nThread) reduction(+:t_perf0,t_perf1,t_perf2,t_perf3,t_perf4,t_perf5,t_perf6,t_perf7,t_perf8,t_perf9) reduction(|| : school_infected[:NSCHOOLS][:16])
#endif
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
    // Test gap is relative to end of incubation period -- i.e. when symptoms erupt
    if (InfectedOn[i] + Incubation[i] != yday_tested) {
      continue;
    }


    // test_outcome = -1 (negative), 0 (no test), 1 (positive)
    if (TodaysK[todayi] < ptest_per_mille) {
      t_perf0 += 1;
      int statei = sa2_to_state(SA2[i]);
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
      if (test_outcome > 0 && School[i] > 0) {
        int schooli = School[i] - 1;
        int agei = Age[i] & 15;
        school_infected[schooli][agei] = true;
      }
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
    int statei = by_state ? sa2_to_state(SA2[i]) : 0;

    // if no tests were ever available we can quickly discard tested
    if (tests_avbl[statei] == 0 &&
        TestedOn[i] &&
        (TestedOn[i] == yday_result || TestedOn[i] == -yday_result)) {
      TestedOn[i] = 0;
      continue;
    }
    // otherwise we test whether the tests have been exceeded and
    // then reset an excess number of people tested tested
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

  int t_perf0_binary_ceil = 1 << ((int)(1 + floor(log2(t_perf0))));
  int t_perf0_binary_ceilm1 = t_perf0_binary_ceil - 1;
  // for a power of two sized vector we can efficiently assign i to a cell

  // successfully contacts
  std::vector<unsigned char> CRand = do_lemire_char_par(t_perf0_binary_ceil, ct_success, nThread, false);



  // Isolate everyone who is in the same household
  // as a person who is 'TestedOn' today
#pragma omp parallel for num_threads(nThread)
  for (int i = 0; i < N; ++i) {

    // threadsafety -- go through the head of household
    if (seqN[i] != 1) {
      continue;
    }

    int statusi = Status[i];

    // same as below
    // if (Status[i] < 0 || Status[i] >= ISOLATED_PLUS) {
    //   continue;
    // }
    if (!isolatable(statusi)) {
      continue;
    }

    bool notified_positive_today = TestedOn[i] == yday;
    // if (!Crand)


    if (HouseholdSize[i] == 1) {
      if (notified_positive_today && CRand[i & t_perf0_binary_ceilm1]) {
        Status[i] += ISOLATED_PLUS;
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
    if (notified_positive_today && CRand[i & t_perf0_binary_ceilm1]) {
      for (int j = 0; j < nh; ++j) {
        int statusij = Status[i + j];
        if (isolatable(statusij)) {
          Status[i + j] += ISOLATED_PLUS;
        }
      }
    }
  }
  // household id // school id //


#pragma omp parallel num_threads(nThread)
  for (int k = 0; k < NPUPILS; ++k) {
    int i = schoolsIndex[k];
    int agei = Age[i] & 15;
    int schooli = School[i] - 1;
    if (school_infected[schooli][agei]) {
      int statusi = Status[i];
      if (isolatable(statusi)) {
        Status[i] = statusi + ISOLATED_PLUS;
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
                         IntegerVector SA2_starts,
                         IntegerVector SA2_finals,
                         const int nThread,
                         IntegerVector Age,
                         IntegerVector Employment,
                         IntegerVector nSupermarketsBySA2,
                         IntegerVector nSupermarketsAvbl,
                         const std::vector<unsigned char> &Resistance,
                         const int day,
                         const int wday,
                         const int yday,
                         const int N,
                         IntegerVector SupermarketTypical,
                         const double q_supermarket,
                         IntegerVector TodaysHz,
                         const unsigned char resistance1,
                         const int max_persons_per_supermarket,
                         const bool verbose = false) {
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

  }

  // array of supermarkets: SA2 x Supermarkets each SA2 x hour
  // (some sa2s have 17 supermarkets; others will just record 0 there)

  // int i_supermarkets[NSA2][maxSupermarketsBySA2][hrs_open];



  const int hrs_open = (wday < 6) ? SUPERMARKET_WEEKDAY_HRS : SUPERMARKET_WEEKEND_HRS;

  // beta distribution (can't use R::rbeta inside omp)
  // todo: accept user-supplied
  const int nFreqs = 1024;
  std::vector<int> SupermarketFreq;
  SupermarketFreq.reserve(nFreqs);
  for (int i = 0; i < nFreqs; ++i) {
    double o = R::rbeta(3, 1);
    o *= 360;
    int oi = (int)(o);
    SupermarketFreq.push_back(oi);
  }




  bool i_supermarkets[NSA2][MAXSUPERMARKETSBYSA2][SUPERMARKET_WEEKDAY_HRS] = {};



#if defined _OPENMP && _OPENMP >= 201511
#pragma omp parallel for num_threads(nThread) reduction(|| : i_supermarkets[:NSA2][:MAXSUPERMARKETSBYSA2][:SUPERMARKET_WEEKDAY_HRS])
#endif
  for (int i = 0; i < N; ++i) {
    if (Status[i] != STATUS_NOSYMP || TodaysHz[(i * 11 + yday) % NTODAY] > SupermarketFreq[i % nFreqs]) {
      continue;
    }
    int sa2i = shortSA2[i];
    int supermarketi = SupermarketTypical[i];
    int hri = i % hrs_open;

    i_supermarkets[sa2i][supermarketi][hri] = true;
  }

  bool check_max_persons = max_persons_per_supermarket < 255;
  std::vector<unsigned char> SuperRand = do_lemire_char_par(N, q_supermarket, nThread, false);

#pragma omp parallel for num_threads(nThread)
  for (int sa2i = 0; sa2i < NSA2; ++sa2i) {
    int start = SA2_starts[sa2i];
    int final = SA2_finals[sa2i];
    unsigned char s_supermarket[MAXSUPERMARKETSBYSA2][SUPERMARKET_WEEKDAY_HRS] = {};
    for (int i = start; i < final; ++i) {
      int hr = i % SUPERMARKET_WEEKDAY_HRS;
      int supermarketi = SupermarketTypical[i];
      if (!i_supermarkets[sa2i][supermarketi][hr]) {
        continue;
      }
      if (Status[i] || !nSupermarketsAvbl[i] || TodaysHz[(i * 11 + yday) % NTODAY] > SupermarketFreq[i % nFreqs]) {
        continue;
      }

      // not transmitted
      if (!SuperRand[i]) {
        continue;
      }



      if (check_max_persons) {
        if (s_supermarket[supermarketi][hr] >= max_persons_per_supermarket) {
          continue;
        }
        s_supermarket[supermarketi][hr] += 1;
      }

      if (Resistance[i] < resistance1) {
        // i_supermarkets[sa2i][supermarketi][hr] -= 1;
        Status[i] = STATUS_NOSYMP;
        InfectedOn[i] = yday;
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
                  IntegerVector Age,
                  const int nThread,
                  List minPlaceID_nPlacesByDestType,
                  const int day,
                  const int wday,
                  const int yday,
                  const int N,
                  const std::vector<unsigned char> &Resistance,
                  const unsigned char resistance1,
                  IntegerVector TodaysHz,
                  const int max_persons_per_place,
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

    if (NTODAY != TodaysHz.length()) {
      stop("Internal error(infect_place): NTODAY != TodaysHz.length().");
    }
    if (NTODAY != TodaysK.length()) {
      stop("Internal error(infect_place): NTODAY != TodaysK.length().");
    }

    if (Age.length() != N) {
      stop("Internal error(infect_place): Age.length() != N.");
    }

    int observed_min_age = 100;
    int observed_max_age = 0;
#pragma omp parallel for num_threads(nThread) reduction(min : observed_min_age) reduction(max : observed_max_age)
    for (int i = 0; i < N; ++i) {
      observed_min_age = (Age[i] < observed_min_age) ? Age[i] : observed_min_age;
      observed_max_age = (Age[i] > observed_max_age) ? Age[i] : observed_max_age;
    }
    if (observed_min_age != 0 || observed_max_age != 100) {
      stop("Internal error(infect_place): Age was not in 0:100.");
    }


  }

  int irandd = (int)(R::runif(0, 2147483646 - N)); // irandd + i must always be < INT_MAX

  // int FreqCafe[8] = {0, 1, 2, 3, 4, 5, 6, 7}; // just use mod 8


  // # Infect
  // ## Find all the places infected people visited

  // n_places is the total number of places across all places
  // place_id identifies the place and has the same order as sa2
  // if place_id_x < place_id_y  then  sa2(place_id_x) <= sa2(place_id_y)
  // sa2

  // Since gnu annoyingly follows the standards (/s), we can't do
  // int i_places[n_places][hrs_open]
  // because 'C++ forbids variable size array'
  if (max_persons_per_place >= 255) {
    stop("Internal error: max_persons_per_place > 255, exceeding unsigned char limit.");
  }
  unsigned char max_persons = max_persons_per_place & 255;


  unsigned char i_places[MAX_N_PLACES_TOTAL][PLACES_HRS_OPEN] = {};
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
      int placefreqi = 1;
      if (place_id == 14) {
        if (Age[i] < 12) {
          continue;
        }
        placefreqi = i % 8;
      }
      placefreqi *= 52;
      int randi = TodaysHz[(i + irandd) % NTODAY];
      if (randi > placefreqi) {
        continue;
      }

      // both these should be deterministic
      int placei = min_place_id + (i % n_places_this_sa2);
      int hri = i % PLACES_HRS_OPEN;
      // occasionally (1/8 times) go at a different time
      if (irandd < 268435456) {
        hri = irandd % PLACES_HRS_OPEN;
      }

      unsigned char infected_visits = i_places[placei][hri];

      if (infected_visits < max_persons) {
        i_places[placei][hri] += 1;
        // threadsafety ok as sa2i allocated indivi thread
        n_infections_per_sa2[sa2i] += 1;
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
      int placefreqi = 1;
      if (place_id == 14) {
        if (Age[i] < CAFES_MIN_AGE) {
          continue;
        }
        placefreqi = i % 8;
      }
      placefreqi *= 52;

      int randi = TodaysK[(i + irandd) % NTODAY];
      if (randi > placefreqi) {
        continue;
      }
      // both these should be deterministic
      int placei = min_place_id + (i % n_places_this_sa2);
      int hri = i % PLACES_HRS_OPEN;

      // occasionally (1/8 times) go at a different time
      if (randi < 128) {
        hri = randi % PLACES_HRS_OPEN;
      }

      int n_infections_here = (int)(i_places[placei][hri]);

      if (n_infections_here) {
        // increase resistance asymptotically for more and more infections
        unsigned char resistance_ = 255 - (255 - resistance1) / n_infections_here;
        if (Resistance[i] < resistance_) {
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
                const int n_workplaces,
                const std::vector<int> &widIndex,
                IntegerVector LabourForceStatus,
                IntegerVector nColleagues,
                const int day,
                const int wday,
                const int yday,
                const int N,
                const unsigned char resistance1,
                const std::vector<unsigned char> &Wrand,
                const int workplaces_open,
                const int workplace_size_max,
                IntegerVector TodaysK,
                const std::vector<unsigned char> &Resistance,
                const int zero,
                const int optionz,
                const int nThread) {
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

      Rcout << N << " ";
      Rcout << resistance1 << " ";
      Rcout << workplaces_open << " ";
      Rcout << workplace_size_max << " ";
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

    if (max_wid >= n_workplaces) {
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
  std::vector<unsigned char> InfectionsByWorkplace(n_workplaces, 0);
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
    InfectionsByWorkplace[widi0] += 1;
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
    if (InfectionsByWorkplace[widi0] == 0) {
      continue;
    }
    int infected_workers = (int)(InfectionsByWorkplace[widi0]);
    int excess_workers = nColleagues[i] - workplace_size_max;
    bool no_further_infections = false; // due to the infectious not being allowed in (back of the queue)
    if (excess_workers > 0) {

      if (excess_workers > infected_workers) {
        // assume infections are strictly less likely to attend work
        InfectionsByWorkplace[widi0] = 0;
        no_further_infections = true;
      } else {
        // no underflow: infected_workers > 0 (otherwise we would have
        // continue;'d above) and - won't go below zero otherwise we
        // would be in other branch
        InfectionsByWorkplace[widi0] -= excess_workers;
      }
    }

    if (Status[i] || no_further_infections) {
      continue;
    }
    unsigned char resistance_ = 255 - (255 - resistance1) / infected_workers;
    if (Resistance[i] < resistance_) {
      Status[i] = STATUS_NOSYMP;
      InfectedOn[i] = yday;
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
                   IntegerVector SA2,
                   const unsigned char state_by_school[],
                   const std::array<bool, NSTATES1> &areSchoolsLockedDown,
                   unsigned char state_trigger_pulled[],
                   int lockdown_trigger_schools_with_infections,
                   int lockdown_trigger_schools_with_infections_geq,
                   int lockdown_trigger_schools_with_infections_duration_of_lockdown,
                   int lockdown_trigger_schools_any_critical,
                   int lockdown_trigger_schools_any_critical_duration_of_lockdown,
                   const std::vector<int> &schoolsIndex,
                   const std::vector<unsigned char> &Erand,
                   IntegerVector Srand,
                   double q_school_dbl,
                   bool only_Year12,
                   List school_days_per_wk,
                   const int nThread,
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

  }

  bool tmp_all_states_in_school = true;
  bool tmp_any_states_in_school = false;
  for (int state = 1; state <= 8; ++state) {
    tmp_all_states_in_school = tmp_all_states_in_school && during_school_term(state, yday);
    tmp_any_states_in_school = tmp_any_states_in_school || during_school_term(state, yday);
  }

  const bool all_states_in_school = tmp_all_states_in_school;
  const bool any_states_in_school = tmp_any_states_in_school;

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


  const bool all_full_time =
    school_days_per_wk.containsElementNamed("all_full_time") &&
    school_days_per_wk["all_full_time"];


  constexpr int combn2[10][2] =
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

  constexpr int combn3[10][3] =
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
      int i = schoolsIndex[k];

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
      int statei = sa2_to_state(SA2[i]);
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
    int i = schoolsIndex[k];
    int statei = sa2_to_state(SA2[i]);
    if (anySchoolsLockedDown && areSchoolsLockedDown[statei]) {
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
    if (!all_states_in_school && !during_school_term(statei, yday)) {
      continue;
    }

    int schooli = School[i] - 1;
    if (!Erand[schooli]) {
      continue;
    }
    if (statusi == STATUS_NOSYMP) {
      i_visits[schooli] += 1;
    } else if ((statusi % ISOLATED_PLUS) == STATUS_CRITIC) {
      lockdownTriggeredByCritic[statei] = true;
    }
  }

  int newInfectionsBySchool[NSCHOOLS] = {};

  std::vector<unsigned char> Prand = do_lemire_char_par(NPUPILS, q_school_dbl, nThread, false);

#if defined _OPENMP && _OPENMP >= 201511
#pragma omp parallel for num_threads(nThread) reduction(+ :newInfectionsBySchool[:NSCHOOLS])
#endif
  for (int k = 0; k < NPUPILS; ++k) {
    int k5 = wday0 + (5 * k);
    if (!AttendsWday[k5]) {
      continue;
    }
    int i = schoolsIndex[k];
    int statei = sa2_to_state(SA2[i]);
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
                      const std::vector<unsigned char> &HouseholdSize,
                      const std::vector<int> &hhIndex,
                      const std::vector<unsigned char> &Resistance,
                      IntegerVector Age,
                      const int n_households,
                      const std::vector<unsigned char> &Hrand,
                      IntegerVector Srand,
                      const int yday,
                      const int N,
                      IntegerVector HouseholdInfectedToday,
                      const unsigned char resistance1,
                      const int q_household,
                      const int nThread) {
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
    const int nh = HouseholdSize[i] == 1;

    if (nh == 1) {
      // no transmission in single-person household
      continue;
    }

    const int statusi = Status[i];

    if (nh == 2) {
      // in this case, we only need to check adjacent
      int statusi_partner = Status[i + 1];
      const bool household_infected = is_infected(statusi) || is_infected(statusi_partner);
      // Prima facie we have a race condition on Status, but in fact
      // we have skipped anyone who has Status[i] != 0 so the only way
      // the following assignment can occur is if the other thread is
      // skipping
      if (household_infected && Srand[i] < q_household) {
        unsigned char r = resistance1;
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



    bool household_infected = false;
    // loop through the household, stop once an infection detected
    for (int j = 0; j < nh; ++j) {
      int statusij = Status[i + j];
      household_infected = household_infected || is_infected(statusij);
    }

    if (household_infected) {
      // return to first person and infect as appropriate
      // (don't infect already infected)
      for (int j = 0; j < nh; ++j) {
        if (Srand[i] >= q_household) {
          continue;
        }
        unsigned char r = resistance1;
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
List do_au_simulate(IntegerVector StatusOriginal,
                    IntegerVector InfectedOnOriginal,
                    IntegerVector SA2,
                    IntegerVector hid,
                    IntegerVector Age,
                    IntegerVector School,
                    IntegerVector DZN,
                    IntegerVector wid,
                    IntegerVector nColleagues,
                    IntegerVector PlaceTypeBySA2,
                    IntegerVector LabourForceStatus,
                    IntegerVector SeedOriginal,
                    List Policy,
                    List nPlacesByDestType,
                    List Epi, /* Epidemiological parameters */
                    IntegerVector Incubation,
                    IntegerVector Illness,
                    IntegerVector nSupermarketsAvbl,
                    IntegerVector SupermarketTypical,
                    List minPlaceID_nPlacesByDestType,
                    const int yday_start,
                    const int days_to_sim,
                    const int N,
                    bool display_progress = true,
                    bool on_terminal = false,
                    bool by_state = true,
                    int returner = 0,
                    int console_width = 80,
                    int optionz = 0,
                    int nThread = 1) {

#ifdef _OPENMP
  if (nThread < 1 || nThread > omp_get_num_procs()) {
    stop("Internal error: nThread out of range");
  }
#endif

  Progress p(days_to_sim, display_progress && console_width <= 1);
  IntegerVector S = clone(SeedOriginal);
  uint64_t s64 = 0;
  for (int t = 0; t < 20; ++t) {
    s64 += S[t];
    s64 <<= 32;
    s64 += S[t + 1];
    s64 <<= 32;
  }
  update_seed(s64);

  IntegerVector Status = no_init(N);
  IntegerVector InfectedOn = no_init(N);
  // Status.reserve(N);
  // std::fill(Status.begin(), Status.end(), 0);
#pragma omp parallel for num_threads(nThread)
  for (int i = 0; i < N; ++i) {
    Status[i] = StatusOriginal[i];
    InfectedOn[i] = InfectedOnOriginal[i];
  }


  if (nPlacesByDestType.length() <= 98) {
    stop("Internal error: FreqsByDestType.length < 98");
  }

  IntegerVector nSupermarketsBySA2 = nPlacesByDestType[97];
  // IntegerVector SupermarketFreq = FreqsByDestType[97]; // Type_by_TypeInt.fst


  if (N != nSupermarketsAvbl.length()) {
    stop("Internal error: nSupermarketsAvbl.length mismatch");
  }
  if (NSA2 != nSupermarketsBySA2.length()) {
    stop("Internal error: nSupermarketsBySA2.length() != NSA2.");
  }

  if (PlaceTypeBySA2.length() > 1) {
    stop("Internal error: PlaceTypeBySA2 not implemented yet.");
  }







  // attach policy changes

  const bool supermarkets_open = Policy["supermarkets_open"];
  const bool use_mpps = Policy.length() && Policy.containsElementNamed("max_persons_per_supermarket");
  const int max_persons_per_supermarket = use_mpps ? Policy["max_persons_per_supermarket"] : 2e9;


  const bool cafes_open = Policy["cafes_open"];
  const int max_persons_per_cafe = 10; // TODO: all this needs to be IntegerVectors!
  // school policies
  const bool schools_open = Policy["schools_open"];
  const bool only_Year12  = Policy["only_Year12"];

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
  const int ct_days_before_test = Policy["contact_tracing_days_before_test"];
  const int ct_days_until_result = Policy["contact_tracing_days_until_result"];
  const double ct_success = Policy["contact_tracing_success"];




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

  const double a_household_rate = Epi["a_household_rate"];
  const double a_schools_rate = Epi["a_schools_rate"];
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

  int resistance1000 = Epi["resistance_threshold"];


  const double p_asympto = Epi["p_asympto"];
  const double p_critical = Epi["p_critical"];
  const double p_death = Epi["p_death"];

  std::vector<unsigned char> ProgInSymp = do_lemire_char_par(N, 1 - p_asympto, nThread, false);
  std::vector<unsigned char> ProgCritic = do_lemire_char_par(N, p_critical, nThread, false);
  std::vector<unsigned char> ProgKilled = do_lemire_char_par(N, p_death, nThread, false);


  const double q_workplace = Epi["q_workplace"];
  const double q_household = Epi["q_household"];
  const double q_school = Epi["q_school"];
  const double q_supermarket = Epi["q_supermarket"];

  IntegerVector Srand = do_lemire_rand_par(N, nThread);


  std::vector<unsigned char> seqN(N, 0);
  std::vector<unsigned char> HouseholdSize(N, 0);
  // inline from do_seqN_N
  seqN[0] = 1;

  // going down - +1 each time the hid stays the same
  // hid  1  1  1  2  2  3  4  4  4  4
  // 1st  1  2  3  1  2  1  1  2  3  4
  for (int i = 1; i < N; ++i) {
    seqN[i] = (hid[i] == hid[i - 1]) ? seqN[i - 1] + 1 : 1;
  }
  HouseholdSize[N - 1] = seqN[N - 1];
  // so that the final
  for (int i = N - 2; i >= 0; --i) {
    // index j is just i for the last person (which records the household)
    // size; otherwise it is the 'next' person in the household (which
    // by induction must have recorded the household size too)

    // hid  1  1  1  2  2  3  4  4  4  4
    // 1st  1  2  3  1  2  1  1  2  3  4
    // 2nd                          4<-4  (first iter)
    //                           4<-4  4
    if (hid[i] == hid[i + 1]) {
      HouseholdSize[i] = HouseholdSize[i + 1];
    } else {
      HouseholdSize[i] = seqN[i];
    }
  }


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

  std::vector<unsigned char> Resistance = do_lemire_char_par(N, 1, nThread, true);
  int r255 = 255 * resistance1000;
  unsigned char resistance_threshold = static_cast<unsigned char>(r255 / 1000);

  // Hrand is the probability of infecting 100% of your household
  std::vector<unsigned char> Hrand = do_lemire_char_par(n_households, a_household_rate, nThread, false);



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
        state_by_school[schooli] = sa2_to_state(SA2[i]);
      }
    }
  }
  std::vector<unsigned char> Erand = do_lemire_char_par(NSCHOOLS, a_schools_rate, nThread, false);


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
  int i_last_workplace = widIndex.back();
  int n_workers = wid.size();
  int n_workplaces = wid[i_last_workplace];
#pragma omp parallel for num_threads(nThread) reduction(max : n_workplaces)
  for (int k = 0; k < n_workers; ++k) {
    n_workplaces = (n_workplaces < wid[k]) ? wid[k] : n_workplaces;
  }
  ++n_workplaces; // 0-indexing


  std::vector<unsigned char> Wrand = do_lemire_char_par(WID_SUPREMUM, a_workplace_rate, nThread, false);

  // variables which will be updated on day = 0
  // int n_schools = -1;

  IntegerVector nInfected = no_init(days_to_sim);
  IntegerVector SupermarketTarget = no_init(N);

  // These could potentially be smaller vectors
  IntegerVector HouseholdInfectedToday = no_init(N); // was the household infected today?

  if (which_unsorted_int(SA2)) {
    stop("SA2 was unsorted.");
  }
  IntegerVector shortSA2 = shorten_sa2s_ordered(SA2);


  // Convert state to short_sa2 directly
  unsigned char stateShortSA2[NSA2];
  for (int i = 0; i < NSA2; ++i) {
    stateShortSA2[i] = sa2_to_state(sa2s[i]);
  }





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


  List FirstFinalsState = sa2_firsts_finals(SA2, NSTATES, true);
  IntegerVector State_firsts = FirstFinalsState[0];
  IntegerVector State_finals = FirstFinalsState[1];



  List FirstFinalsSA2 = sa2_firsts_finals(shortSA2, NSA2, false);
  IntegerVector SA2_firsts = FirstFinalsSA2[0];
  IntegerVector SA2_finals = FirstFinalsSA2[1];

  NumericVector notUsed = {1,2};
  // returner 0: Data frame of statuses
  DataFrame Statuses = DataFrame::create(Named("InitialStatus") = clone(Status));

  // returner 1: days by state by age by status
  //

  int o_r = 0;
  const int o_r_len =
    (returner == 3) ? days_to_sim * (NSTATES * (6 * 2)) :
    ((returner == 1) ? days_to_sim * 7 : 1);

  IntegerVector out1 = no_init(o_r_len); // 7 statuses for each
  IntegerVector out2(days_to_sim * NSTATES * 7); // preallocate


  for (int day = 0; day < days_to_sim; ++day) {
    update_seed(s64);
    s64 <<= 32;
    s64 += day;

    // Start with basic calendar information, needed for
    // opening hours, especially of schools
    const int yday = yday_start + day;

    assert_is_weekday(day);
    assert_is_monday(day);
    // i.e yday 1 was a Wednesday, make the week start on Monday
    const int wday = wday_2020[((yday - 1) % 7)];
    const bool is_weekday = yday2weekday(yday);
    const bool is_monday = yday2monday(yday);



    // Aggregate the information to return
    // Doing it the head rather than the tail of this loop means
    // we can (a) exit early if there are no infections and (b) potentially
    // supply these aggregates to other functions to provide early returns.
    int n_infected_today = 0;
#if defined _OPENMP && _OPENMP >= 201511
#pragma omp parallel for num_threads(nThread) reduction(+:n_infected_today)
#endif
    for (int i = 0; i < N; ++i) {
      int statusi = Status[i];
      if (is_infected(statusi)) {
        n_infected_today += 1;
      }
    }
    if (returner == 0) {

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
          int statusi0 = Status[i];
          if (is_infected(statusi0)) {
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

    if (returner == 3) {
      //  column          Day   x   state x status x isolated
      // uniquen  days_to_sim     NSTATES        6          2
      constexpr int buffn = NSTATES * 6 * 2;
      int tot_status[buffn] = {};
#if defined _OPENMP && _OPENMP >= 201511
#pragma omp parallel for num_threads(nThread) reduction (+ : tot_status[:buffn])
#endif
      for (int sa2i = 0; sa2i < NSA2; ++sa2i) {
        int state = stateShortSA2[sa2i] - 1;
        int first = SA2_firsts[sa2i];
        int final = SA2_finals[sa2i];
        for (int i = first; i < final; ++i) {
          int statusi = Status[i];
          int statusi2 = (statusi + 2) % 16;
          int j = state * (6 * 2) + statusi2 * 2 + is_isolated(statusi);
          tot_status[j] += 1;
        }
      }

      int o_r_j = 0;
      for (int s = 0; s < NSTATES; ++s) {
        for (int status = 0; status < 6; ++status) {
          out1[o_r++] = tot_status[o_r_j++];
          out1[o_r++] = tot_status[o_r_j++];
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
    if (returner == 0) {
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
        int statusi = Status[i];
        if (AgesLockdown[agei] && !is_isolated(statusi)) {
          Status[i] += ISOLATED_PLUS;
        }
      }
    }
    if (day == 0 && Incubation.length() != N) {
      stop("Internal error: Incubation.length() != N");
    }
    if (day == 0 && Illness.length() != N) {
      stop("Internal error: Incubation.length() != N");
    }

    // First, examine all individuals infected last night
    // and move them accordingly.
#if defined _OPENMP && _OPENMP >= 201511
#pragma omp parallel for num_threads(nThread)
#endif
    for (int i = first_infected_i; i < final_infected_i; ++i) {


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




        int incubation = Incubation[i];

        if (yday <= InfectedOn[i] + incubation) {
          // Today is before the incubation period is over
          // nothing to do -- keep whatever the status is
        } else {
          // This is reevaluated each day, but because they are uniform
          // this is okay. Nonetheless we don't second-guess the original
          // statusi
          bool prog_insymp = statusi >  STATUS_NOSYMP || ProgInSymp[i];
          bool prog_critic = statusi == STATUS_CRITIC || (prog_insymp && ProgCritic[i]);
          bool prog_killed = (prog_critic && ProgKilled[i]);
          bool in_isolation =
            statusi > (STATUS_KILLED + ISOLATED_PLUS) &&
            (
                (statusi == (STATUS_SUSCEP + ISOLATED_PLUS)) || // most common first
                  (statusi == (STATUS_HEALED + ISOLATED_PLUS)) ||
                  (statusi == (STATUS_KILLED + ISOLATED_PLUS)) ||
                  (statusi == (STATUS_NOSYMP + ISOLATED_PLUS)) ||
                  (statusi == (STATUS_INSYMP + ISOLATED_PLUS)) ||
                  (statusi == (STATUS_CRITIC + ISOLATED_PLUS))
            );

          int illness = Illness[i];
          // Today is after the incubation, during the illness.
          // Assumption: if the person becomes critical, it happens immediately.
          if (yday <= InfectedOn[i] + incubation + illness) {
            if (prog_insymp) {
              Status[i] = STATUS_INSYMP + prog_critic * CRITIC_MINUS_INSYMP;
              if (in_isolation) {
                Status[i] += ISOLATED_PLUS;
              }
            } else {
              // nothing to do: they're still ill, but at Status 1.
            }
          } else {
            // Today is after the illness has run its course
            Status[i] = (prog_killed) ? STATUS_KILLED : STATUS_HEALED;
          }
        }
      }
    }

    // This function actually performs the interactions and infections


    if (supermarkets_open) {
      infect_supermarkets(Status,
                          InfectedOn,
                          shortSA2,
                          SA2_firsts,
                          SA2_finals,
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
                          q_supermarket,
                          TodayHz,
                          resistance_threshold,
                          max_persons_per_supermarket,
                          false);
    }

    // infect cafes
    if (cafes_open) {
      // perhaps do for (int p = 0; p < 105; ++p) but not yet
      infect_place(15 - 1, // 15 is place id for cafe -1 for 0-index
                   Status,
                   InfectedOn,
                   shortSA2,
                   SA2_firsts,
                   SA2_finals,
                   Age,
                   nThread,
                   minPlaceID_nPlacesByDestType,
                   day,
                   wday,
                   yday,
                   N,
                   Resistance,
                   resistance_threshold,
                   TodayHz,
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
            if (optionz) Rcout << "triggered";
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
                    SA2,
                    state_by_school,
                    areSchoolsLockedDown,
                    state_trigger_pulled,
                    lockdown_trigger_schools_with_infections,
                    lockdown_trigger_schools_with_infections_geq,
                    lockdown_trigger_schools_with_infections_duration_of_lockdown,
                    lockdown_trigger_schools_with_any_critical,
                    lockdown_trigger_schools_with_any_critical_duration_of_lockdown,
                    schoolsIndex,
                    Erand,
                    Srand,
                    q_school,
                    only_Year12,
                    school_days_per_wk,
                    nThread,
                    optionz,
                    -99);
    }
    if (workplaces_open) {
      infect_dzn(Status, InfectedOn,
                 DZN, wid,
                 n_workplaces,
                 widIndex,
                 LabourForceStatus, nColleagues,
                 day, wday, yday, N,
                 resistance_threshold,
                 Wrand,
                 workplaces_open,
                 workplace_size_max,
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
                      Age,
                      day,
                      yday,
                      seqN,
                      HouseholdSize,
                      TestsAvbl,
                      by_state,
                      SA2,
                      hid,
                      HouseholdInfectedToday,
                      N,
                      School,
                      schoolsIndex,
                      ptest_per_mille_asympto,
                      ptest_per_mille_sympto,
                      ct_success,
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
  if (returner == 3) {
    return List::create(Named("Status12") = out1);
  }


  return Statuses;
}



