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

int r_Rand(double m, double s, int d) {
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
  return x * (ny + nz) + y * (nz) + z;
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
                         int maxSupermarketsBySA2,
                         IntegerVector nSupermarketsAvbl,
                         IntegerVector Resistance,
                         int wday,
                         int yday,
                         int N,
                         IntegerVector SupermarketTypical,
                         double r_location,
                         double r_scale,
                         int r_d,
                         IntegerVector SupermarketFreq,
                         IntegerVector TodaysHz,
                         int resistance1,
                         int max_persons_per_supermarket,
                         int resistance2 = 3,
                         double r_div = 36, // divide r_ by this // TODO: model is highly sensitive to this par!
                         bool verbose = false) {
  // array of supermarkets: SA2 x Supermarkets each SA2 x hour
  // (some sa2s have 17 supermarkets; others will just record 0 there)

  // int i_supermarkets[NSA2][maxSupermarketsBySA2][hrs_open];

  if (resistance1 < 0 || resistance1 > 1000) {
    stop("Internal error: resistance1 was not in [0, 1]");
  }

  const int hrs_open = (wday < 6) ? SUPERMARKET_WEEKDAY_HRS : SUPERMARKET_WEEKEND_HRS;

  int i_supermarkets[NSA2][maxSupermarketsBySA2][hrs_open];
  memset(i_supermarkets, 0, sizeof i_supermarkets);

  bool check_max_persons = max_persons_per_supermarket < 10e3;


#pragma omp parallel for num_threads(nThread) reduction(+:i_supermarkets[:NSA2][:maxSupermarketsBySA2][:hrs_open])
  for (int i = 0; i < N; ++i) {
    if (Status[i] != STATUS_NOSYMP || TodaysHz[(i * 11 + yday) % NTODAY] > SupermarketFreq[i]) {
      continue;
    }
    int sa2i = shortSA2[i];
    int supermarketi = SupermarketTypical[i];
    int hri = i % hrs_open;
    if (sa2i >= NSA2 || supermarketi >= maxSupermarketsBySA2 || hri >= hrs_open) {
      continue;
    }
    i_supermarkets[sa2i][supermarketi][hri] += r_Rand(r_location, r_scale, r_d);
  }



#pragma omp parallel for num_threads(nThread)
  for (int hr = 0; hr < hrs_open; ++hr) {
    int s_supermarkets[NSA2][maxSupermarketsBySA2];
    memset(s_supermarkets, 0, sizeof s_supermarkets);
    for (int i = hr; i < N; i += hrs_open) {
      int sa2i = shortSA2[i];
      int supermarketi = SupermarketTypical[i];
      if (i_supermarkets[sa2i][supermarketi][hr] <= 0) {
        continue;
      }
      if (Status[i] || !nSupermarketsAvbl[i] || TodaysHz[(i * 11 + yday) % NTODAY] > SupermarketFreq[i]) {
        continue;
      }


      s_supermarkets[sa2i][supermarketi] += 1;
      int s_supermarketi = s_supermarkets[sa2i][supermarketi];

      if (check_max_persons && s_supermarketi > max_persons_per_supermarket) {
        continue;
      }

      if (i_supermarkets[sa2i][supermarketi][hr]) {
        if (Resistance[i] < resistance1) {
          // i_supermarkets[sa2i][supermarketi][hr] -= 1;
          Status[i] = STATUS_NOSYMP;
          InfectedOn[i] = yday;
        }
      }
    }
  }
}

void infect_dzn(IntegerVector Status,
                IntegerVector DZN,
                IntegerVector nColleagues,
                IntegerVector LabourForceStatus,
                int day,
                int wday,
                int yday,
                int N,
                double r_location,
                double r_scale,
                int r_d,
                IntegerVector TodaysK,
                int nThread = 1) {

  IntegerVector InfectionsByDZN = no_init(NDZN);

  for (int i = 0; i < N; ++i) {
    if (Status[i] != STATUS_INSYMP) {
      continue;
    }

    int dzni = DZN[i];
    // if zero or NA
    if (dzni <= 0) {
      continue;
    }

    // unlikely at this point so don't worry about branches
    if (LabourForceStatus[i] != LFS_FULLTIME &&
        LabourForceStatus[i] != LFS_PARTTIME) {
      continue;
    }



    if ((dzni % omp_get_num_threads()) != omp_get_thread_num()) {
      continue;
    }


    InfectionsByDZN[dzni] += 1;





  }



  // void infect_dzn
}



void infect_school(IntegerVector Status,
                   IntegerVector InfectedOn,
                   IntegerVector School,
                   IntegerVector Age,
                   IntegerVector AttendsWday,
                   int day,
                   int wday,
                   int yday,
                   int N,
                   IntegerVector State,
                   const std::vector<int>& schoolIndices,
                   double r_location,
                   double r_scale,
                   int r_d,
                   IntegerVector TodaysK,
                   bool only_Year12,
                   List school_days_per_wk,
                   int n_schools,
                   int n_pupils,
                   int nThread = 1) {

  // infect people within a school

  // Cube: number of visits by School x Age
  // First array index is the total, following indices are the age-based infections
  // Teachers are all aged '20'.
  int s_visits[n_schools][21];
  int i_visits[n_schools][21];
  memset(s_visits, 0, sizeof s_visits);
  memset(i_visits, 0, sizeof i_visits);

  if (day == 0) {
    if (!school_days_per_wk.containsElementNamed("week15combns")) {
      stop("Internal error: school_days_per_wk did not contain 'week15combns'.");
    }
    if (school_days_per_wk.length() < 9) {
      stop("school_days_per_wk had wrong length");
    }
    if (AttendsWday.length() != (NPUPILS * 5)) {
      stop("Internal error: AttendsWday.length() != (NPUPILS * 5)");
    }
  }

  List week15combns = school_days_per_wk["week15combns"];

  bool all_full_time =
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
    for (int a = 0; a < 21; ++a) {
      int da = sDaysPerWk[a];
      DaysPerWk[s][a] = da;
    }
  }

  const int wday0 = wday - 1;
  if (wday0 < 0 || wday0 >= 5) {
    stop("'wday' out of range (1:5).");
  }




  // This should be parallelizable!!
  // But accessing a list is not thread safe so can't be done directly.
  if (day < 7 && wday < 6) {
#pragma omp parallel for num_threads(nThread)
    for (int k = 0; k < n_pupils; ++k) {
      int k5 = wday0 + (5 * k);
      int i = schoolIndices[k];
      int schooli = School[i] - 1;
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
      AttendsWday[k5] = attends_today;
    }
  }




  for (int k = 0; k < n_pupils; ++k) {
    int k5 = wday0 + (5 * k);
    if (!AttendsWday[k5]) {
      continue;
    }
    int i = schoolIndices[k];
    int schooli = School[i] - 1;
    int Agei = (Age[i] > 20) ? 20 : Age[i];

    s_visits[schooli][0] += 1;
    s_visits[schooli][Agei] += 1;
    // rcauchy relates to the single day
    if (Status[i] == STATUS_NOSYMP) {
      int infectedi = r_Rand(r_location, r_scale, r_d);
      i_visits[schooli][0] += infectedi;
      i_visits[schooli][Agei] += infectedi;
    }
  }

  for (int k = 0; k < n_pupils; ++k) {
    int k5 = wday0 + (5 * k);
    if (!AttendsWday[k5]) {
      continue;
    }
    int i = schoolIndices[k];
    if (Status[i]) {
      continue;
    }
    int schooli = School[i] - 1;

    // N.B. This logic means the 'first' people in the table get infected
    // first.  We could randomize this, but I don't think it matters.

    // TODO: make students of the same age more likely/first to be infected
    if (i_visits[schooli][0]) {
      Status[i] = STATUS_NOSYMP;
      InfectedOn[i] = yday;
      i_visits[schooli][0] -= 1;
    }
  }
  // void
}


void infect_household(IntegerVector Status,
                      IntegerVector InfectedOn,
                      IntegerVector hid,
                      IntegerVector seqN,
                      IntegerVector HouseholdSize,
                      IntegerVector Resistance,
                      IntegerVector Age,
                      int yday,
                      int N,
                      IntegerVector HouseholdInfectedToday,
                      int resistance1,
                      int nThread = 1,
                      int resistance_penalty = 400) {
  // resistance_penalty = penalty against Resistance[i]
  // that makes infection more likely. Higher penalties
  // make infection more likely among otherwise resistant
  // individuals

#pragma omp parallel for num_threads(nThread)
  for (int i = 0; i < N; ++i) {
    // separate household ids into the same thread
    if (seqN[i] != 1) {
      continue;
    }
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
      if (household_infected) {
        int r = resistance1 + unifRand(-1, resistance_penalty);
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
    if (seqN[i] != 1) {
      continue;
    }

    int nh = HouseholdSize[i];
    bool household_infected = Status[i] > 0;

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
        int r = resistance1 + unifRand(-1, resistance_penalty);
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
                    IntegerVector SA2,
                    IntegerVector State,
                    IntegerVector hid,
                    IntegerVector seqN,
                    IntegerVector HouseholdSize,
                    IntegerVector Age,
                    IntegerVector School,
                    IntegerVector PlaceTypeBySA2,
                    IntegerVector Employment,
                    IntegerVector Resistance,
                    List Policy,
                    List nPlacesByDestType,
                    List FreqsByDestType,
                    List Epi, /* Epidemiological parameters */
                    IntegerVector nSupermarketsAvbl,
                    IntegerVector SupermarketTypical,
                    int yday_start,
                    int days_to_sim,
                    int N = 25e6,
                    bool display_progress = true,
                    bool by_state = true,
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

  int maxSupermarketsBySA2 = 8;



  // for (int i = 1; i < NSA2; ++i) {
  //   maxSupermarketsBySA2 = maxii(maxSupermarketsBySA2, nSupermarketsBySA2[i]);
  // }


  // attach policy changes

  bool supermarkets_open = true;
  if (Policy.length() && Policy.containsElementNamed("supermarkets_open")) {
    supermarkets_open = Policy["supermarkets_open"];
  }
  bool use_mpps = Policy.length() && Policy.containsElementNamed("max_persons_per_supermarket");
  const int max_persons_per_supermarket =
    use_mpps ? Policy["max_persons_per_supermarket"] : 2e9;

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


  IntegerVector TestedOn = no_init(N);

  // TODO: make user-avbl
  int ptest_per_mille_sympto = 1000; // 100%
  int ptest_per_mille_asympto = 10; // 1%


  // attach epipars
  double incubation_m = Epi["incubation_mean"];
  double incubation_s = Epi["incubation_sigma"];
  double illness_m = Epi["illness_mean"];
  double illness_s = Epi["illness_sigma"];
  // double r_location = Epi["r_location"];
  double r_schools_location = Epi["r_schools_location"];
  double r_supermarket_location = Epi["r_supermarket_location"];
  double r_scale = Epi["r_scale"];
  int resistance_threshold = Epi["resistance_threshold"];
  int p_asympto = Epi["p_asympto"];
  int p_critical = Epi["p_critical"];
  int p_death = Epi["p_death"];

  int incubation_d = Epi["incubation_distribution"];
  int illness_d = Epi["illness_distribution"];
  int r_d = Epi["r_distribution"];

  int n_pupils = 0;
  std::vector<int> schoolsIndex;
  schoolsIndex.reserve(NPUPILS);
  for (int i = 0; i < N; ++i) {
    if (School[i] > 0) {
      ++n_pupils;
      schoolsIndex.push_back(i);
    }
  }
  if (n_pupils != NPUPILS) {
    Rcout << NPUPILS << "\n";
    Rcout << n_pupils << "\n";
    stop("n_pupils much larger than expected: likely an overestimate of schools.");
  }

  // 5 days (at most) in a school week
  IntegerVector AttendsWday = no_init(NPUPILS * 5);
  // memset(AttendsWday, 0, sizeof AttendsWday);



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

  DataFrame Statuses = DataFrame::create(Named("InitialStatus") = clone(Status));

  IntegerVector PlaceId = clone(SupermarketTarget);

  // For example, SupermarketFreq[i] = 365  => every day
  // SupermarketFreq[i] = 1 every year.  So we create a vector
  // of 1:366 and compare that to the individual's tendency to
  // visit. So if TodaysHz[i] = 366 they will not visit anything
  // regardless; if TodaysHz[i] = 1 they will visit everything.

  // Note these are < N
  IntegerVector TodaysK = dqsample_int2(1000, 262144);
  IntegerVector TodayHz = dqsample_int2(365, 262144);






  for (int day = 0; day < days_to_sim; ++day) {
    int yday = yday_start + day;

    //                  yday  1, 2, 3, 4, 5, 6, 7, ...
    const int wday_2020[7] = {3, 4, 5, 6, 7, 1, 2};
    // i.e yday 1 was a Wednesday

    int wday = wday_2020[((yday - 1) % 7)];
    bool is_weekday = wday < 6;


    int n_infected_today = 0;

#pragma omp parallel for num_threads(nThread) reduction(+:n_infected_today)
    for (int i = 0; i < N; ++i) {
      int statusi = Status[i];
      n_infected_today += (statusi > 0) && (statusi != ISOLATED_PLUS);
    }

    nInfected[day] = n_infected_today;

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
          int w_yday = w_d + yday_start + 0.5;
          int w_wday = wday_2020[((w_yday - 1) % 7)];
          w_d += di;
          r_d += di;
          int max_reds = 2;
          while (r_d > 1 && w < 1024) {
            ++w;
            r_d -= 1;
            if ((w_d + 0.5) < b_d) {
              if (w_wday < 6 || max_reds <= 0) {
                Rcout << "=";
              } else {
                --max_reds;
                Rcout << "\033[31m" << "=" << "\033[39m";
              }
            } else {
              Rcout << "_";
            }
          }
        }
        Rcout << " | ";

        // int last_yday = (days_to_sim + yday_start);

        Rcout << "day = " << day << "/" << days_to_sim << " ";

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
    if (optionz != 2) {
      Statuses.push_back(clone(Status));
    }
    if (optionz == 3) {
      continue;
    }




#pragma omp parallel for num_threads(nThread)
    for (int i = 0; i < N; ++i) {

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
                          Employment,
                          nSupermarketsBySA2,
                          maxSupermarketsBySA2,
                          nSupermarketsAvbl,
                          Resistance,
                          wday,
                          yday,
                          N,
                          SupermarketTypical,
                          r_supermarket_location, r_scale, r_d,
                          SupermarketFreq,
                          TodayHz,
                          resistance_threshold,
                          max_persons_per_supermarket);
    }



    if (is_weekday && schools_open) {
      infect_school(Status, InfectedOn, School, Age,
                    AttendsWday,
                    day, wday, yday,
                    N,
                    State,
                    schoolsIndex,
                    r_schools_location, r_scale, r_d,
                    TodaysK,
                    only_Year12,
                    school_days_per_wk,
                    NSCHOOLS,
                    n_pupils,
                    nThread);
    }



    // finally

    infect_household(Status, InfectedOn, hid, seqN, HouseholdSize, Resistance, Age,
                     yday, N, HouseholdInfectedToday,
                     resistance_threshold,
                     nThread);

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

  return Rcpp::List::create(Named("nInfected") = nInfected,
                            Named("Statuses") = Statuses,
                            Named("TestedOn") = TestedOn);
}



