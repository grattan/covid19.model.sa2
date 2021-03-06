#include "covid19model.h"
#include <Rcpp.h>
#include <Rcpp/Benchmark/Timer.h>
using namespace Rcpp;

#define N_SUPERMARKETS 7487

// # nocov start

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
int source_stadia() {
  return SOURCE_STADIA;
}
// [[Rcpp::export(rng = false)]]
int isolated_plus() {
  return ISOLATED_PLUS;
}

// [[Rcpp::export(rng = false)]]
int supermarket_weekday_hrs() {
  return SUPERMARKET_WEEKDAY_HRS;
}
// [[Rcpp::export(rng = false)]]
int supermarket_weekend_hrs() {
  return SUPERMARKET_WEEKEND_HRS;
}

// const int SOURCE_ABROAD =  8;
// const int SOURCE_SUPERM = 17;
// const int SOURCE_PLACES = 18;
// const int SOURCE_WORKPL = 19;
// const int SOURCE_SCHOOL = 20;
// const int SOURCE_HOUSEH = 21;
// const int SOURCE_OTHSA2 = 22;
// const int SOURCE_STADIA = 23;

// [[Rcpp::export(rng = false)]]
int source_workplace() {
  return SOURCE_WORKPL;
}
// [[Rcpp::export(rng = false)]]
int source_school() {
  return SOURCE_SCHOOL;
}
// [[Rcpp::export(rng = false)]]
int source_household() {
  return SOURCE_HOUSEH;
}
// [[Rcpp::export(rng = false)]]
int source_other_sa2() {
  return SOURCE_OTHSA2;
}

// # nocov end

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
  return statusi > 0 && (statusi % 32) >= STATUS_NOSYMP && (statusi % 32) <= STATUS_CRITIC;
}

inline bool is_infectious(const int & statusi) {
  return statusi == STATUS_NOSYMP;
}



int array3k(int x, int y, int z, int ny, int nz) {
  return x * (ny * nz) + y * (nz) + z;
}

int array4k(int w, int x, int y, int z, int nx, int ny, int nz) {
  return w * (nx * ny * nz) + x * (ny * nz) + y * nz + z;
}

bool prod_ge_intmax(int a, int b, int c, int d) {
  double prod = 1;
  int inputs[4] = {a, b, c, d};
  for (int i = 0; i < 4; ++i) {
    prod *= ((double) inputs[i]);
    if (prod > INT_MAX) {
      return true;
    }
  }
  return false;
}

// [[Rcpp::export]]
IntegerVector test_array4k(IntegerVector w, IntegerVector x, IntegerVector y, IntegerVector z,
                           int nw, int nx, int ny, int nz) {
  if (prod_ge_intmax(nw, nx, ny, nz)) {
    stop("test_array4k only available for integer-length outputs.");
  }
  // purely to test array4k
  R_xlen_t n = (nw * nx) * (ny * nz);

  if (n != w.length() ||
      n != x.length() ||
      n != y.length() ||
      n != z.length()) {
    stop("Internal error: lengths differ."); // # nocov
  }
  IntegerVector out = no_init(nw * nx * ny * nz);
  for (R_xlen_t i = 0; i < n; ++i) {
    int wi = w[i];
    int xi = x[i];
    int yi = y[i];
    int zi = z[i];
    if (wi >= nw || xi >= nx || yi >= ny || zi >= nz) {
      stop("Internal error: wi >= nw || xi >= nx || yi >= ny || zi >= nz"); // # nocov
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
    stop("Internal error: nThread out of range"); // # nocov
  }
#endif

  if (day == 0) {
    int n_pupils = schoolsIndex.size();
    if (n_pupils != NPUPILS && N == 21364885) {
      stop("Internal error(contact tracing): n_pupils != NPUPILS"); // # nocov
    }
    if (ct_success < 0 || ct_success > 1) {
      stop("Internal error(contact tracing): ct_success < 0 || ct_success"); // # nocov
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
  const int yday_result = yday + days_before_test + days_to_notify;
  const int NSTATES1 = NSTATES + 1;

  int tests_avbl[NSTATES1] = {0};

  for (int s = 0; s < NSTATES1; ++s) {
    if (TestsAvbl[s] < 0) {
      stop("Internal error(contact tracing): TestsAvbl[s] < 0 was negative."); // # nocov
    }
    tests_avbl[s] = TestsAvbl[s];
  }

  // tests_performed[0] = all of australia
  int tests_performed[NSTATES1] = {0};
  if (TestsAvbl.length() != NSTATES1) {
    stop("Internal error(contact tracing): TestsAvbl.length() != NSTATES + 1."); // # nocov
  }
  if (NSTATES1 != 10) {
    stop("Internal error(contact tracing): NSTATES1 != 10"); // # nocov
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
      const int statei = sa2_to_state(SA2[i]);

      if (statei == 1) t_perf1 += 1;
      else if (statei == 2) t_perf2 += 1;
      else if (statei == 3) t_perf3 += 1;
      else if (statei == 4) t_perf4 += 1;
      else if (statei == 5) t_perf5 += 1;
      else if (statei == 6) t_perf6 += 1;
      else if (statei == 7) t_perf7 += 1;
      else if (statei == 8) t_perf8 += 1;
      else if (statei == 9) t_perf9 += 1;

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
    int statei = sa2_to_state(SA2[i]);

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
  std::vector<unsigned char> CRand = q_lemire_32(t_perf0_binary_ceil, ct_success, nThread);



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

void infect_supermarkets(IntegerVector Status,
                         IntegerVector InfectedOn,
                         IntegerVector Source,
                         IntegerVector shortSA2,
                         IntegerVector SA2_starts,
                         IntegerVector SA2_finals,
                         const int nThread,
                         IntegerVector Age,
                         IntegerVector Employment,
                         IntegerVector nSupermarketsBySA2,
                         IntegerVector nSupermarketsAvbl,
                         const std::vector<unsigned char> &Resistant,
                         const int day,
                         const int wday,
                         const int yday,
                         const int N,
                         IntegerVector SupermarketTypical,
                         const double q_supermarket,
                         IntegerVector TodaysHz,
                         const int max_persons_per_supermarket,
                         const double supermarket_beta_shape1,
                         const double supermarket_beta_shape2,
                         const bool verbose = false) {
  if (day < 0) {
    stop("Internal error(infect_supermarkets): day < 0"); // # nocov
  }
  if (day == 0) {
    // poor man's assert()
    if (SupermarketTypical.length() != N) {
      stop("Internal error(infect_supermarkets): SupermarketTypical.length() != N"); // # nocov
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
      stop("Internal error(infect_supermarkets): maxSupermarketBySA2 > MAXSUPERMARKETSBYSA2"); // # nocov
    }

    if (SUPERMARKET_WEEKDAY_HRS < SUPERMARKET_WEEKEND_HRS) {
      stop("Internal error(infect_supermarkets): SUPERMARKET_WEEKDAY_HRS < SUPERMARKET_WEEKEND_HRS"); // # nocov
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
    double o = R::rbeta(supermarket_beta_shape1, supermarket_beta_shape2);
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
  std::vector<unsigned char> Q_Supermarket = q_lemire_32(N, q_supermarket, nThread);


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

      if (Status[i] ||
          !nSupermarketsAvbl[i] ||
          TodaysHz[(i * 11 + yday) % NTODAY] > SupermarketFreq[i % nFreqs]) {
        continue;
      }
      if (check_max_persons) {
        if (s_supermarket[supermarketi][hr] >= max_persons_per_supermarket) {
          break;
        }
        s_supermarket[supermarketi][hr] += 1;
      }

      // not transmitted
      if (!Q_Supermarket[i]) {
        continue;
      }

      if (Resistant[i]) {
        continue;
      }
      // i_supermarkets[sa2i][supermarketi][hr] -= 1;
      Status[i] = STATUS_NOSYMP;
      InfectedOn[i] = yday;
      Source[i] = SOURCE_SUPERM;
    }
  }
}

void infect_place(int place_id,
                  IntegerVector Status,
                  IntegerVector InfectedOn,
                  IntegerVector Source,
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
                  const std::vector<unsigned char> &Resistant,
                  const double q_places,
                  IntegerVector TodaysHz,
                  const int max_persons_per_place,
                  IntegerVector TodaysK) {

  if (place_id == PLACEID_ESTABLISHMENT ||
      place_id == PLACEID_POINT_OF_INTEREST ||
      place_id == PLACEID_STORE) {
    // don't support these
    // void function so just return early
    return; // # nocov
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
      stop("Internal error(infect_place): SA2_firsts.length() != NSA2 || SA2_finals.length() != NSA2"); // # nocov
    }

    // make sure there is an entry for every SA2, not just those with places
    if (nPlacesBySA2.length() != NSA2) {
      stop("Internal error(infect_place): nPlacesBySA2.length() != NSA2"); // # nocov
    }
    if (minPlaceIdBySA2.length() != NSA2) {
      stop("Internal error(infect_place): minPlaceIdBySA2.length() != NSA2"); // # nocov
    }

    int n_places = 0;
    for (int sa2i = 0; sa2i < NSA2; ++sa2i) {
      n_places += nPlacesBySA2[sa2i];
    }

    if (n_places > MAX_N_PLACES_TOTAL) {
      stop("Internal error(infect_place): n_places > MAX_N_PLACES_TOTAL"); // # nocov
    }

    if (NTODAY != TodaysHz.length()) {
      stop("Internal error(infect_place): NTODAY != TodaysHz.length()."); // # nocov
    }
    if (NTODAY != TodaysK.length()) {
      stop("Internal error(infect_place): NTODAY != TodaysK.length()."); // # nocov
    }

    if (Age.length() != N) {
      stop("Internal error(infect_place): Age.length() != N."); // # nocov
    }

    int observed_min_age = 100;
    int observed_max_age = 0;
#pragma omp parallel for num_threads(nThread) reduction(min : observed_min_age) reduction(max : observed_max_age)
    for (int i = 0; i < N; ++i) {
      observed_min_age = (Age[i] < observed_min_age) ? Age[i] : observed_min_age;
      observed_max_age = (Age[i] > observed_max_age) ? Age[i] : observed_max_age;
    }
    if (observed_min_age != 0 || observed_max_age != 100) {
      stop("Internal error(infect_place): Age was not in 0:100."); // # nocov
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
    stop("Internal error: max_persons_per_place > 255, exceeding unsigned char limit."); // # nocov
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

  std::vector<unsigned char> Q_Place = q_lemire_32(N, q_places, nThread);

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
      continue; // # nocov
    }

    int min_place_id = minPlaceIdBySA2[sa2i]; // 0-indexed

    for (int i = start_sa2i; i < end_sa2i; ++i) {
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

      if (n_infections_here && Q_Place[i]) {
        // increase resistance asymptotically for more and more infections
        if (!Resistant[i]) {
          Status[i] = STATUS_NOSYMP;
          InfectedOn[i] = yday;
          Source[i] = SOURCE_PLACES;
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
                IntegerVector Source,
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
                const double a_workplace_rate,
                const double q_workplace,
                double workplaces_open,
                const int workplace_size_max,
                IntegerVector TodaysK,
                const std::vector<unsigned char> &Resistant,
                const int zero,
                const int optionz,
                const int nThread) {
  if (zero != 0) {
    stop("zero != 0"); // # nocov
  }

  if (day == 0) {

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
      stop("Internal error(infect_dzn): wmax_dzn >= NDZN"); // # nocov
    }

    if (max_wid >= n_workplaces) {
      stop("Internal error(infect_dzn): wid_supremeum0 > WID_SUPREMUM"); // # nocov
    }
    if (TodaysK.length() != NTODAY) {
      stop("Internal error(infect_dzn): TodaysK.length() != NTODAY"); // # nocov
    }
    if (nColleagues.length() != N) {
      stop("Internal error(infect_dzn): nColleagues.length() != N"); // # nocov
    }
  }

  // can't use array as overflow risk is real
  std::vector<unsigned char> InfectionsByWorkplace(n_workplaces, 0);

  // for (int w = 0; w < WID_SUPREMUM; ++w) {
  //   InfectionsByWorkplace[w] = 0;
  // }

  unsigned int widIndexSize = widIndex.size();


  for (unsigned int k = 0; k < widIndexSize; ++k) {
    int i = widIndex[k];
    if (Status[i] != STATUS_NOSYMP) {
      continue;
    }

    int widi = wid[i]; // if wid[i] is NA widi[i] - 1 is UBD
    if (day == 0 && widi <= 0) {
      stop("Internal error (day == 0 && widi <= 0))."); // # nocov
    }
    int widi0 = widi - 1;

    InfectionsByWorkplace[widi0] += 1;
  }

  // A_Workplace = % chance of a workplace being infected
  // Q_Workplace = % chance of a person coming in to contact with an infected
  //               person given the infected person attends the same workplace
  std::vector<unsigned char> A_Workplace = q_lemire_32(n_workplaces, a_workplace_rate, nThread);
  std::vector<unsigned char> Q_Workplace = q_lemire_32(N, q_workplace, nThread);

  // reinfection
  for (unsigned int k = 0; k < widIndexSize; ++k) {
    int i = widIndex[k];
    int widi0 = wid[i] - 1;
    if (InfectionsByWorkplace[widi0] == 0) {
      continue;
    }
    if (!A_Workplace[widi0]) {
      continue;
    }
    if (nColleagues[i] <= 1) {
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
    if (Q_Workplace[i] && !Resistant[i]) {
      Status[i] = STATUS_NOSYMP;
      InfectedOn[i] = yday;
      Source[i] = SOURCE_WORKPL;
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
                   IntegerVector Source,
                   IntegerVector School,
                   IntegerVector Age,
                   IntegerVector AttendsWday,
                   const int &days_since_new_policy,
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
                   IntegerVector Srand,
                   double a_schools_rate,
                   double q_school_dbl,
                   bool only_Year12,
                   List school_days_per_wk,
                   const int nThread,
                   int optionz = 0,
                   int zero = 0) {
  if (zero != -99) {
    stop("zero expected."); // # nocov
  }

  // infect people within a school

  // Cube: number of visits by School x Age
  // First array index is the total, following indices are the age-based infections
  // Teachers are all aged '20'.



  // first school day may not be day 0
  if (day == 0 || (day < 3 && wday == 1)) {
    if (!school_days_per_wk.containsElementNamed("week15combns")) {
      stop("Internal error: school_days_per_wk did not contain 'week15combns'."); // # nocov
    }
    if (school_days_per_wk.length() < NSTATES1) {
      Rcerr << "school_days_per_wk.length() = " << school_days_per_wk.length();
      stop("Internal error: school_days_per_wk had wrong length. Must be school_days_per_wk.length() < NSTATES1"); // # nocov
    }
    if (AttendsWday.length() != (NPUPILS * 5)) {
      stop("Internal error: AttendsWday.length() != (NPUPILS * 5)"); // # nocov
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
      stop("Internal error(infect_schools): min_too_small."); // # nocov
    }
    if (max_too_large) {
      stop("Internal error(infect_schools): max_too_large"); // # nocov
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
    return; // # nocov
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
      stop("Internal error: sDaysPerWk.length() != 21."); // # nocov
    }
    for (int a = 0; a < 21; ++a) {
      int da = sDaysPerWk[a];
      DaysPerWk[s][a] = da;
    }
  }

  const int wday0 = wday - 1;
  if (wday0 < 0 || wday0 >= 5) {
    stop("Internal error(infect_school): 'wday' out of range (1:5)."); // # nocov
  }


  if ((day < 7 && wday < 6) || (days_since_new_policy < 7)) {
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

  std::vector<unsigned char> A_School = do_lemire_char_par(NSCHOOLS, a_schools_rate, nThread, false);

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
    if (!A_School[schooli]) {
      continue;
    }
    if (statusi == STATUS_NOSYMP) {
      i_visits[schooli] += 1;
    } else if ((statusi % ISOLATED_PLUS) == STATUS_CRITIC) {
      lockdownTriggeredByCritic[statei] = true;
    }
  }

  int newInfectionsBySchool[NSCHOOLS] = {};

  std::vector<unsigned char> Q_School = q_lemire_32(NPUPILS, q_school_dbl, nThread);

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
    if (!A_School[schooli]) {
      continue;
    }

    // N.B. This logic means the 'first' people in the table get infected
    // first.  We could randomize this, but I don't think it matters.

    // TODO: make students of the same age more likely/first to be infected
    if (i_visits[schooli] && Q_School[k]) {
      Status[i] = STATUS_NOSYMP;
      InfectedOn[i] = yday;
      Source[i] = SOURCE_SCHOOL;
      newInfectionsBySchool[schooli] += 1;
    }
  }

  int schoolsWithGeqInfections[NSTATES1] = {};
  const int b = lockdown_trigger_schools_with_infections_geq;
  for (int schooli = 0; schooli < NSCHOOLS; ++schooli) {
    if (newInfectionsBySchool[schooli] >= b) {
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
                      IntegerVector Source,
                      IntegerVector shortSA2,
                      IntegerVector hid,
                      const std::vector<unsigned char> &HouseholdSize,
                      const std::vector<int> &hhIndex,
                      const std::vector<unsigned char> &Resistant,
                      IntegerVector Age,
                      const int n_households,
                      double a_household_rate,
                      const int yday,
                      const int N,
                      IntegerVector HouseholdInfectedToday,
                      const double q_household,
                      const int nThread) {

  std::vector<unsigned char> A_Household = do_lemire_char_par(n_households, a_household_rate, nThread, false);
  std::vector<unsigned char> Q_Household = do_lemire_char_par(N, q_household, nThread, false);

#pragma omp parallel for num_threads(nThread)
  for (int k = 0; k < n_households; ++k) {
    // if the household is not infectible
    // skip immediately
    if (!A_Household[k]) {
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
      if (household_infected && Q_Household[i]) {
        if (Status[i] == 0 && !Resistant[i]) {
          Status[i] = STATUS_NOSYMP;
          InfectedOn[i] = yday + 1;
          Source[i] = SOURCE_HOUSEH;
        }

        if (Status[i + 1] == 0 && !Resistant[i + 1]) {
          Status[i + 1] = STATUS_NOSYMP;
          InfectedOn[i + 1] = yday + 1;
          Source[i + 1] = SOURCE_HOUSEH;
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
        int ij = i + j;
        if (Q_Household[ij] && Status[ij] == 0 && !Resistant[ij]) {
          Status[ij] = STATUS_NOSYMP;
          Source[ij] = SOURCE_HOUSEH;
          InfectedOn[ij] = yday + 1;
        }
      }
      HouseholdInfectedToday[i] = 1;
    }
  }
  // void
}


void infect_other_sa2(IntegerVector Status,
                      IntegerVector InfectedOn,
                      IntegerVector Source,
                      IntegerVector shortSA2,
                      IntegerVector SA2_firsts,
                      IntegerVector SA2_finals,
                      unsigned char stateShortSA2[],
                      int day,
                      int wday,
                      int yday,
                      int nThread,
                      int N,
                      double p_goes_outside = 0.01,
                      double rate = 1) {
  if (p_goes_outside < 0 || p_goes_outside > 1) {
    stop("Internal error(infect_other_sa2): wrong range of p.");  // # nocov
  }
  if (p_goes_outside == 0) {
    return;
  }

  int i_visitors[NSA2] = {};
  int tot_i_visitors = 0;

  std::vector<double> MaxTravelDist = Rexp(N, rate, nThread);

#if defined _OPENMP && _OPENMP >= 201511
#pragma omp parallel for num_threads(nThread) reduction(+ : i_visitors[:NSA2]) reduction(+:tot_i_visitors)
#endif
  for (int i = 0; i < N; ++i) {
    if (Status[i] != STATUS_NOSYMP && InfectedOn[i] < yday) {
      continue;
    }
    int sa2i = shortSA2[i];
    int statei = stateShortSA2[sa2i];
    int dd = 0;
    double dist_allowed = MaxTravelDist[i];
    // check the next sa2 if it's further away that this,
    // we conclude that no travel occurs
    int dest_sa2i = sa2i + 1;
    double dist2sa2 = haversine_distance_sa2(sa2i, dest_sa2i % NSA2);
    if (dist2sa2 >= dist_allowed) {
      continue;
    }
    // else the person visits

    for (int d = 2; d < NSA2; ++d) {
      dd += (d % 2) ? -d : d;
      dest_sa2i = (sa2i + dd) % NSA2;
      double dist = haversine_distance_sa2(sa2i, dest_sa2i);
      double dist_allowed = MaxTravelDist[i];
      if (dist_allowed < dist) {
        break;
      }
    }
    int dest_statei = stateShortSA2[dest_sa2i];
    // we don't model interstate travel here
    if (dest_statei != statei) {
      continue;
    }

    i_visitors[dest_sa2i] += 1;
    tot_i_visitors += 1;
  }

  if (!tot_i_visitors) {
    return;
  }


  for (int sa2i = 0; sa2i < NSA2; ++sa2i) {

    int infected_visitors = i_visitors[sa2i];
    if (!infected_visitors) {
      continue;
    }

    int sa2_first = SA2_firsts[sa2i];
    int sa2_final = SA2_finals[sa2i];
    int d = sa2_final - sa2_first;
    if (d <= 1) {
      // don't model potentially small sa2s
      continue;
    }
    for (int j = 0; j < infected_visitors; ++j) {
      int s = 0;
#ifdef _OPENMP
      s = omp_get_thread_num();
#endif
      int ii = do_one_unif(0, infected_visitors - 1, true, s);
      if (Status[ii] == STATUS_SUSCEP) {
        Status[ii] = STATUS_NOSYMP;
        InfectedOn[ii] = yday;
        Source[ii] = SOURCE_OTHSA2;
      }
    }
  }
}


void infect_major_event(IntegerVector Status,
                        IntegerVector InfectedOn,
                        IntegerVector Source,
                        IntegerVector nPersonsByEvent,
                        double q_major_event,
                        int max_persons_per_event,
                        int day,
                        int wday,
                        int yday,
                        const std::vector<unsigned char> &Resistant,
                        int nThread,
                        int N,
                        int optionz = 0) {
  int n_events_today = nPersonsByEvent.length();

  for (int e = 0; e < n_events_today; ++e) {
    double p_attends_e = ((double) nPersonsByEvent[e] / (double)N);
    std::vector<unsigned char> Attends = do_lemire_char_par(N, p_attends_e, nThread, false);
    int i_event = 0;
#pragma omp parallel for num_threads(nThread) reduction(+:i_event)
    for (int i = 0; i < N; ++i) {
      int statusi = Status[i];
      if (Attends[i] && is_infectious(statusi)) {
        i_event += 1;
      }
    }
    if (i_event == 0) {
      continue;
    }
    std::vector<unsigned char> GetsInfected = do_lemire_char_par(N, q_major_event, nThread, false);

#pragma omp parallel for num_threads(nThread)
    for (int i = 0; i < N; ++i) {
      if (!GetsInfected[i]) {
        continue;
      }
      int statusi = Status[i];
      if (statusi) {
        continue;
      }
      if (Resistant[i]) {
        continue;
      }
      Status[i] = STATUS_NOSYMP;
      InfectedOn[i] = yday;
      Source[i] = SOURCE_STADIA;
    }
  }


}

void add_unseen_infections(IntegerVector Status,
                           IntegerVector InfectedOn,
                           IntegerVector Source,
                           int day,
                           int wday,
                           int yday,
                           const std::vector<unsigned char> &Resistant,
                           int nThread,
                           int N,
                           int n_unseen_infections) {
  IntegerVector iNewInfs = Rcpp::sample(N, n_unseen_infections);
  for (int k = 0; k < n_unseen_infections; ++k) {
    int i = iNewInfs[k];
    if (Resistant[i] || Status[i]) {
      continue;
    }
    Status[i] = STATUS_NOSYMP;
    InfectedOn[i] = yday;
    Source[i] = SOURCE_UNSEEN;
  }
}

void add_overseas_arrivals(IntegerVector Status,
                           IntegerVector InfectedOn,
                           IntegerVector Source,
                           int day,
                           int wday,
                           int yday,
                           const std::vector<unsigned char> &Resistant,
                           int nThread,
                           int N,
                           int n_overseas_arrivals) {
  IntegerVector iNewInfs = Rcpp::sample(N, n_overseas_arrivals);
  for (int k = 0; k < n_overseas_arrivals; ++k) {
    int i = iNewInfs[k];
    if (Resistant[i] || Status[i]) {
      continue;
    }
    Status[i] = STATUS_NOSYMP + ISOLATED_PLUS;
    InfectedOn[i] = yday;
    Source[i] = SOURCE_ABROAD;
  }
}











void progress_bar(int console_width,
                  int days_to_sim,
                  int day,
                  int yday_start,
                  bool on_terminal,
                  int n_infected_today) {
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

void validate_policy(List Policy, int m) {
  if (!Policy.containsElementNamed("yday_start")) {
    Rcerr << "(" << m << "): " << "Policy did not contain element 'yday_start'\n";
    stop("Policy invalidated.");
  }

  if (!Policy.containsElementNamed("travel_outside_sa2")) {
    Rcerr << "(" << m << "): " << "Policy did not contain element 'travel_outside_sa2'\n";
    stop("Policy invalidated.");
  }
  if (!Policy.containsElementNamed("schools_open")) {
    Rcerr << "(" << m << "): " << "Policy did not contain element 'schools_open'\n";
    stop("Policy invalidated.");
  }
  if (!Policy.containsElementNamed("workplaces_open")) {
    Rcerr << "(" << m << "): " << "Policy did not contain element 'workplaces_open'\n";
    stop("Policy invalidated.");
  }
}


void validate_multipolicy(List MultiPolicy) {
  if (!MultiPolicy.length()) {
    return;
  }
  if (MultiPolicy.length() >= 255) {
    stop("MultiPolicy.length() >= 255.");
  }
  int n_multipolicies = MultiPolicy.length();
  for (int m = 0; m < n_multipolicies; ++m) {
    List PolicyM = MultiPolicy[m];
    validate_policy(PolicyM, m);
  }
}



std::vector<unsigned char> do_Resistance(IntegerVector Age,
                                         DoubleVector ResistanceByAge,
                                         int nThread = 1) {
  int N = Age.length();
  std::vector<unsigned char> out;
  out.reserve(N);
  std::fill(out.begin(), out.end(), 0);

  if (ResistanceByAge.length() != 101) {
    stop("Internal error(do_Resistance): ResistanceByAge.length() != 101.");
  }


  IntegerVector Rand = do_lemire_rand_par(N, nThread);

#pragma omp parallel for num_threads(nThread)
  for (int i = 0; i < N; ++i) {
    int agei = Age[i];
    double r = ResistanceByAge[agei];
    int p = percentage_to_int(r);
    out[i] = Rand[i] < p;
  }
  return out;
}

// [[Rcpp::export]]
LogicalVector test_resistance(IntegerVector Age, DoubleVector ResistanceByAge, int nThread) {
  std::vector<unsigned char> Resistance = do_Resistance(Age, ResistanceByAge, nThread);
  int N = Age.length();
  LogicalVector out = no_init(N);
#pragma omp parallel for num_threads(nThread)
  for (int i = 0; i < N; ++i) {
    out[i] = Resistance[i] != 0;
  }
  return out;
}





//' @title do_au_simulate
//' @name do_au_simulate
//' @description The internal mechanism of the \code{\link{simulate_sa2}} function
//'
//' @section Vectors which are modified:
//' @param Status An integer vector that is modified by the function.
//' @param InfectedOn The \code{yday} when the individual was infected.
//' @param Source The source of the infection. See the header file SOURCE.
//'
//'
//'
//'
//' @noRd


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
                    List MultiPolicy,
                    List nPlacesByDestType,
                    List Epi, /* Epidemiological parameters */
                    IntegerVector unseen_infections,
                    IntegerVector overseas_arrivals,
                    IntegerVector Incubation,
                    IntegerVector Illness,
                    IntegerVector nSupermarketsAvbl,
                    IntegerVector SupermarketTypical,
                    List minPlaceID_nPlacesByDestType,
                    const int yday_start,
                    const int days_to_sim,
                    const int N,
                    int returner = 0,
                    bool display_progress = true,
                    bool on_terminal = false,
                    int console_width = 80,
                    int optionz = 0,
                    int nThread = 1) {

#ifdef _OPENMP
  if (nThread < 1 || nThread > omp_get_num_procs()) {
    stop("Internal error: nThread out of range"); // # nocov
  }
#endif

  IntegerVector S = clone(SeedOriginal);

  // Seed lemire rng
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
  IntegerVector Source = no_init(N);
  // Status.reserve(N);
  // std::fill(Status.begin(), Status.end(), 0);
#pragma omp parallel for num_threads(nThread)
  for (int i = 0; i < N; ++i) {
    Status[i] = StatusOriginal[i];
    InfectedOn[i] = InfectedOnOriginal[i];
    Source[i] = 0;
  }


  if (nPlacesByDestType.length() <= 98) {
    stop("Internal error: FreqsByDestType.length < 98"); // # nocov
  }

  IntegerVector nSupermarketsBySA2 = nPlacesByDestType[97];
  // IntegerVector SupermarketFreq = FreqsByDestType[97]; // Type_by_TypeInt.fst


  if (N != nSupermarketsAvbl.length()) {
    stop("Internal error: nSupermarketsAvbl.length mismatch"); // # nocov
  }
  if (NSA2 != nSupermarketsBySA2.length()) {
    stop("Internal error: nSupermarketsBySA2.length() != NSA2."); // # nocov
  }

  if (PlaceTypeBySA2.length() > 1) {
    stop("Internal error: PlaceTypeBySA2 not implemented yet."); // # nocov
  }



  // attach policy changes

  const bool travel_outside_sa2 = Policy["travel_outside_sa2"];
  const bool supermarkets_open = Policy["supermarkets_open"];
  const bool use_mpps = Policy.length() && Policy.containsElementNamed("max_persons_per_supermarket");
  const int max_persons_per_supermarket = use_mpps ? Policy["max_persons_per_supermarket"] : 2e9;


  const bool cafes_open = Policy["cafes_open"];
  const int max_persons_per_cafe = 10;
  // school policies
  bool schools_open = Policy["schools_open"];
  const bool only_Year12  = Policy["only_Year12"];

  if (!Policy.containsElementNamed("school_days_per_wk")) {
    stop("Internal error: 'Policy' did not contain element 'school_days_per_wk'."); // # nocov
  }
  List school_days_per_wk = Policy["school_days_per_wk"];

  // Dynamic modelling relating to schools
  const bool school_lockdown_triggers_exist =
    Policy.containsElementNamed("school_lockdown_triggers_exist") &&
    Policy["school_lockdown_triggers_exist"];
  int lockdown_trigger_schools_with_infections = NSCHOOLS;
  int lockdown_trigger_schools_with_infections_geq = NPUPILS;
  int lockdown_trigger_schools_with_infections_duration_of_lockdown = days_to_sim;
  int lockdown_trigger_schools_with_any_critical = NSCHOOLS;
  int lockdown_trigger_schools_with_any_critical_duration_of_lockdown = NPUPILS;
  if (school_lockdown_triggers_exist &&
      Policy.containsElementNamed("lockdown_triggers__schools")) {
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

  double workplaces_open = Policy["workplaces_open"];
  int workplace_size_max = Policy["workplace_size_max"];

  IntegerVector TestedOn = no_init(N);

  // TODO: make user-avbl
  int ptest_per_mille_sympto = 1000; // 100%
  int ptest_per_mille_asympto = 10; // 1%

  // Age-based lockdown
  bool age_based_lockdown = false;
  IntegerVector AgesLockdown(100);
  if (Policy.containsElementNamed("age_based_lockdown")) {
    AgesLockdown = Policy["age_based_lockdown"];
    if (AgesLockdown.length() == 101) {
      for (int i = 0; i < 101; ++i) {
        if (AgesLockdown[i] != 0) {
          age_based_lockdown = true;
          break;
        }
      }
    }
  }

  // Events (i.e. large scale events)
  int max_persons_per_event = Policy["max_persons_per_event"];
  int n_major_events_weekday = Policy["n_major_events_weekday"];
  int n_major_events_weekend = Policy["n_major_events_weekend"];

  IntegerVector nPersonsByEvent = Policy["nPersonsByEvent"];
  double p_visit_major_event = Epi["p_visit_major_event"];
  if (p_visit_major_event < 0 || p_visit_major_event > 1) {
    stop("p_visit_major_event not (0, 1)");
  }


  // Multi policies
  // These are designed to reflect multiple policies throught the simulation period
  validate_multipolicy(MultiPolicy);
  unsigned char n_multipolicies = MultiPolicy.length();
  const bool use_multipolicy = MultiPolicy.length() > 0;
  unsigned char multipolicy = 0; // the index of the multipolicy, increments on policy change
  int multipolicy_changes_yday[255] = {};
  for (unsigned char m = 0; m < n_multipolicies; ++m) {
    if (use_multipolicy) {
      List PolicyM = MultiPolicy[m];
      int yday_startm = PolicyM["yday_start"];
      multipolicy_changes_yday[m] = yday_startm;
    } else {
      multipolicy_changes_yday[m] = INT_MAX;
    }
  }

  // 3 options:
  // yday_start <= second multipolicy  --->  nothing to do
  // second < yday_start < nth some n  ---> go to n - 1
  // yday_start > all -- > go to n
  if (use_multipolicy) {
    if (yday_start >= multipolicy_changes_yday[n_multipolicies - 1]) {
      multipolicy = n_multipolicies - 1;
    } else {
      if (n_multipolicies > 2 && yday_start > multipolicy_changes_yday[1]) {
        for (unsigned char m = 0; m < n_multipolicies; ++m) {
          if (yday_start > multipolicy_changes_yday[m]) {
            multipolicy = m - 1;
            break;
          }
        }
      }
    }
  }

  // Some policies use precalculated values (like the days of attendance of a
  // school). If policy chances (as through a multipolicy) we need to track
  // this occurrence
  int days_since_new_policy = 0;


//
//   LogicalVector multi_schools_open =
//     (use_multipolicy) ? MultiPolicy["schools_open"] : Policy["schools_open"];



  IntegerVector Srand = do_lemire_rand_par(N, nThread);




  std::vector<unsigned char> seqN(N, 0);
  std::vector<unsigned char> HouseholdSize(N, 0);
  // inline from do_seqN_N
  seqN[0] = 1;

  // going down - +1 each time the hid stays the same
  // hid  1  1  1  2  2  3  4  4  4  4
  // 1st  1  2  3  1  2  1  1  2  3  4  (seqN)
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
    // 1st  1  2  3  1  2  1  1  2  3  4  (seqN)
    // 2nd                          4<-4  (HouseholdSize, 1st iter)
    //                           4<-4  4  (HouseholdSize, 2nd iter)
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
  std::vector<int> hhIndex; // head of household index (i.e. seqN = 1)
  hhIndex.reserve(n_households);
  {
    int i = 0;
    while (i < N) {
      if (i == 0 || hid[i - 1] != hid[i]) {
        hhIndex.push_back(i);
        i += HouseholdSize[i];
      } else {
        stop("Internal error: hhIndex/HouseholdSize mismatch."); // # nocov
      }
    }
  }





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



  if (n_pupils != NPUPILS && N == 21364885) {
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


  // Now we can attach Epipars and create vectors that
  // supply the stochasticity in the model.

  // Unlike the p_ and q_ values below, these apply to the workplace/house/school
  // as a whole
  const double a_workplace_rate = Epi["a_workplace_rate"];
  const double a_household_rate = Epi["a_household_rate"];
  const double a_schools_rate = Epi["a_schools_rate"];


  // Personal epidemiological parameters
  DoubleVector ResistanceByAge = Epi["ResistanceByAge"];

  const std::vector<unsigned char> Resistant = do_Resistance(Age, ResistanceByAge, nThread);


  const double p_asympto = Epi["p_asympto"];
  DoubleVector p_critical = Epi["p_critical"];
  DoubleVector p_death = Epi["p_death"];

  const std::vector<unsigned char> ProgInSymp = do_lemire_char_par(N, 1 - p_asympto, nThread, false);
  const std::vector<unsigned char> ProgCritic     = do_lemire_char_par(N, p_critical[0], nThread, false);
  const std::vector<unsigned char> ProgCritic5064 = do_lemire_char_par(N, p_critical[1], nThread, false);
  const std::vector<unsigned char> ProgCritic6599 = do_lemire_char_par(N, p_critical[2], nThread, false);
  const std::vector<unsigned char> ProgKilled     = do_lemire_char_par(N, p_death[0], nThread, false);
  const std::vector<unsigned char> ProgKilled5064 = do_lemire_char_par(N, p_death[1], nThread, false);
  const std::vector<unsigned char> ProgKilled6599 = do_lemire_char_par(N, p_death[2], nThread, false);


  const double q_workplace = Epi["q_workplace"];
  const double q_household = Epi["q_household"];
  const double q_school = Epi["q_school"];
  const double q_supermarket = Epi["q_supermarket"];
  const double q_places = Epi["q_places"];
  const double q_major_event = Epi["q_major_event"];

  const double supermarket_beta_shape1 = Epi["supermarket_beta_shape1"];
  const double supermarket_beta_shape2 = Epi["supermarket_beta_shape2"];



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

  const double isol_compliance = Policy["isol_compliance"];




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

  IntegerVector NewInfections(days_to_sim);
  IntegerVector NewInfectionsByState(days_to_sim * NSTATES);

  if (unseen_infections.length() != days_to_sim) {
    Rcerr << "unseen_infections.length() = " << unseen_infections.length();
    Rcerr << "but must be equal to days_to_sim " << days_to_sim;
    stop("unseen_infections.length() != days_to_sim. ");
  }





  // Start the simulation //


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
        stop("Internal error: NSTATES != 9"); // # nocov
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
          out2[array3k(day, state, statusi, NSTATES, 7)] += 1;
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

      int new_infections = 0;
      int new_infections_by_state[NSTATES] = {};
#pragma omp parallel for num_threads(nThread)
      for (int i = 0; i < N; ++i) {
        int statusi = Status[i];
        int infected_on = InfectedOn[i];
        int statei = sa2_to_state(SA2[i]) - 1;
        if (is_infected(statusi) && infected_on == yday - 1) {
          new_infections += 1;
          new_infections_by_state[statei] += 1;
        }
      }
      NewInfections[day] = new_infections;
      for (int s = 0; s < NSTATES; ++s) {
        NewInfectionsByState[day * NSTATES + s] = new_infections_by_state[s];
      }



    }





    // Progress bar
    if (display_progress) {
      progress_bar(console_width,
                   days_to_sim,
                   day,
                   yday_start,
                   on_terminal,
                   n_infected_today);
    }


    // Multipolicy?
    // Do we need to change our policy parameters?
    //  1. Is a multipolicy in effect?
    //  2. Is the first date past?
    //  3. Apply changes as required
    ++days_since_new_policy;
    if (use_multipolicy) {
      // multipolicy may be after given yday
      if (multipolicy_changes_yday[multipolicy] == yday) {
        days_since_new_policy = 0;
        List NewPolicy = MultiPolicy[multipolicy];
        if (NewPolicy.containsElementNamed("schools_open")) {
          schools_open = NewPolicy["schools_open"];
        }
        if (NewPolicy.containsElementNamed("school_days_per_wk")) {
          school_days_per_wk = NewPolicy["school_days_per_wk"];
        }
        workplaces_open = NewPolicy["workplaces_open"];
        workplace_size_max = NewPolicy["workplace_size_max"];
        max_persons_per_event = NewPolicy["max_persons_per_event"];
        ++multipolicy;
      }
    }







    // no more infections?
    if (n_infected_today == 0) {
      continue;
    }
    if (returner == 0) {
      Statuses.push_back(clone(Status));
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
      stop("Internal error: Incubation.length() != N"); // # nocov
    }
    if (day == 0 && Illness.length() != N) {
      stop("Internal error: Incubation.length() != N"); // # nocov
    }

    std::vector<unsigned char> Q_isol_compliance = q_lemire_32(N, isol_compliance, nThread);
    // First, examine all individuals infected last night
    // and move them accordingly.
#if defined _OPENMP && _OPENMP >= 201511
#pragma omp parallel for num_threads(nThread)
#endif
    for (int i = first_infected_i; i < final_infected_i; ++i) {
      int statusi = Status[i];
      if (is_infected(statusi)) {
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
          int age012 = (Age[i] >= 50) + (Age[i] >= 65);

          bool prog_insymp = statusi >  STATUS_NOSYMP || ProgInSymp[i];
          bool prog_critic =
            statusi == STATUS_CRITIC ||
            (prog_insymp && ((age012 == 0) ? ProgCritic[i] : ((age012 == 1) ? ProgCritic5064[i] : ProgCritic6599[i])));
          bool prog_killed =
             prog_critic && ((age012 == 0) ? ProgKilled[i] : ((age012 == 1) ? ProgKilled5064[i] : ProgKilled6599[i]));
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
        int statusii = Status[i];
        if (is_isolated(statusii)) {
          if (!Q_isol_compliance[i]) {
            Status[i] -= ISOLATED_PLUS;
          }
        }

      }
    }

    // Now actually performs the interactions and infections

    // To avoid problems arising from some forms of infection always occuring
    // before others, we randomize the order each day.  If this remains a problem
    // we would need to do this by person.  (Pass ExecutionOrder and eo to
    // each infector.)
    IntegerVector ExecutionOrder = Rcpp::sample(6, 6);
    for (int eo = 0; eo < 6; ++eo) {

      // Anti-pattern?
      // https://en.wikipedia.org/wiki/Loop-switch_sequence
      // No: because the order is not known at compile-time. In particular,
      // it's different each day.

      switch(ExecutionOrder[eo]) {


      case 1:
        // Infect other sa2 (then return instantly -- i.e. continue to infect)
        if (travel_outside_sa2) {
          infect_other_sa2(Status,
                           InfectedOn,
                           Source,
                           shortSA2,
                           SA2_firsts,
                           SA2_finals,
                           stateShortSA2,
                           day,
                           wday,
                           yday,
                           nThread,
                           N);
        }
        continue;

      case 2:
        if (supermarkets_open) {
          infect_supermarkets(Status,
                              InfectedOn,
                              Source,
                              shortSA2,
                              SA2_firsts,
                              SA2_finals,
                              nThread,
                              Age,
                              LabourForceStatus,
                              nSupermarketsBySA2,
                              nSupermarketsAvbl,
                              Resistant,
                              day,
                              wday,
                              yday,
                              N,
                              SupermarketTypical,
                              q_supermarket,
                              TodayHz,
                              max_persons_per_supermarket,
                              supermarket_beta_shape1,
                              supermarket_beta_shape2,
                              false);
        }
        continue;

      case 3:
        // infect cafes
        if (cafes_open) {
          // perhaps do for (int p = 0; p < 105; ++p) but not yet
          infect_place(15 - 1, // 15 is place id for cafe -1 for 0-index
                       Status,
                       InfectedOn,
                       Source,
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
                       Resistant,
                       q_places,
                       TodayHz,
                       max_persons_per_cafe,
                       TodaysK);
        }
        continue;

      case 4:
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
          infect_school(Status, InfectedOn, Source,
                        School, Age,
                        AttendsWday,
                        days_since_new_policy,
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
                        Srand,
                        a_schools_rate,
                        q_school,
                        only_Year12,
                        school_days_per_wk,
                        nThread,
                        optionz,
                        -99);
        }
        continue;

      case 5:
        if (workplaces_open > 0) {
          infect_dzn(Status, InfectedOn, Source,
                     DZN, wid,
                     n_workplaces,
                     widIndex,
                     LabourForceStatus, nColleagues,
                     day, wday, yday, N,
                     a_workplace_rate,
                     q_workplace,
                     workplaces_open,
                     workplace_size_max,
                     TodaysK, Resistant, 0, optionz, nThread);
        }
        continue;

      case 6:
        int n_major_events_today =
          (is_weekday) ? n_major_events_weekday : n_major_events_weekend;

        if (n_major_events_today && max_persons_per_event > 1000) {
          infect_major_event(Status, InfectedOn, Source,
                             nPersonsByEvent,
                             q_major_event, max_persons_per_event,
                             day,
                             wday, yday, Resistant, nThread, N, optionz);
        }
        continue;
      }
    }


    // finally
    infect_household(Status, InfectedOn, Source,
                     shortSA2,
                     hid, HouseholdSize,
                     hhIndex,
                     Resistant, Age,
                     n_households,
                     a_household_rate,
                     yday, N, HouseholdInfectedToday,
                     q_household,
                     nThread);



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

    int n_unseen_infections = unseen_infections[day];
    if (n_unseen_infections) {
      add_unseen_infections(Status,
                            InfectedOn,
                            Source,
                            day,
                            wday,
                            yday,
                            Resistant,
                            nThread,
                            N,
                            n_unseen_infections);
    }
    int n_overseas_arrivals = overseas_arrivals[day];
    if (n_overseas_arrivals) {
      add_overseas_arrivals(Status,
                            InfectedOn,
                            Source,
                            day,
                            wday,
                            yday,
                            Resistant,
                            nThread,
                            N,
                            n_overseas_arrivals);
    }
  }

  if (returner == 0) {
    Statuses.push_back(InfectedOn);
    Statuses.push_back(Source);
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
    return List::create(Named("Status12") = out1,
                        Named("NewInfections") = NewInfections,
                        Named("NewInfectionsByState") = NewInfectionsByState,
                        Named("InfectionSource") = Source);
  }
  if (returner == 4) {
    return List::create(Named("Status") = Status,
                        Named("InfectedOn") = InfectedOn,
                        Named("InfectionSource") = Source);
  }


  return Statuses;
}




