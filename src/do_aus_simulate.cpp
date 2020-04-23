#include "covid19model.h"
#include <Rcpp.h>
#include <Rcpp/Benchmark/Timer.h>
using namespace Rcpp;

#define N_SUPERMARKETS 7487

// [[Rcpp::export]]
int status_killed() {
  return STATUS_KILLED;
}
// [[Rcpp::export]]
int status_healed() {
  return STATUS_HEALED;
}
// [[Rcpp::export]]
int status_suscep() {
  return STATUS_SUSCEP;
}
// [[Rcpp::export]]
int status_nosymp() {
  return STATUS_NOSYMP;
}
// [[Rcpp::export]]
int status_insymp() {
  return STATUS_INSYMP;
}
// [[Rcpp::export]]
int status_critic() {
  return STATUS_CRITIC;
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
                     int yday,
                     IntegerVector seqN,
                     IntegerVector HouseholdSize,
                     int tests_avbl,
                     IntegerVector hid,
                     IntegerVector HouseholdInfectedToday,
                     int N,
                     IntegerVector School,
                     IntegerVector PlaceId,
                     int ptest_per_mille_sympto,
                     int ptest_per_mille_asympto,
                     IntegerVector Todays2B,
                     int days_to_notify = 3,
                     int nThread = 1) {
  if (nThread < 1 || nThread > omp_get_num_procs()) {
    // lots of large ints nearby!
    stop("Internal error: nThread out of range");
  }

  // isTested -> isIdentified -> responseReq -> LockdownTomorrw

  // loop through each person who has been infected
  // does the person get a test?
  // is the test positive?
  // is the person part of a household
  // are any tests left?
  // test each person in the household

  // TestedOn = Day at which the person is tested and notified
  IntegerVector TestedOn = no_init(N);
  const int test_array[3] = {0, ptest_per_mille_asympto, ptest_per_mille_sympto};
  int tests_performed = 0;
#pragma omp parallel for num_threads(nThread) reduction(+:tests_performed)
  for (int i = 0; i < N; ++i) {
    int test_k = ((Status[i] == STATUS_NOSYMP) + (Status[i] == STATUS_INSYMP));
    int ptest_per_mille = test_array[test_k];

    // test_outcome = -1 (negative), 0 (no test), 1 (positive)
    int test_outcome = (Todays2B[i] % 1000) < ptest_per_mille;
    tests_performed += test_outcome;

    test_outcome -= 2 * (((Todays2B[i] / 13) % 1000) > SENSITIVITY);

    // encode negative and time of test in one variable
    TestedOn[i] = (yday + days_to_notify) * test_outcome;
  }

  // At this point we know how many tests have been performed
  // and who has been identified.

  // Isolate everyone who is in the same household
  // as a person who is 'TestedOn' today
  for (int i = 0; i < N; ++i) {
    if (seqN[i] != 1) {
      continue;
    }

    bool notified_positive_today = TestedOn[i] == yday;

    if (HouseholdSize[i] == 1) {
      if (notified_positive_today) {
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
    if (notified_positive_today) {
      for (int j = 0; j < nh; ++j) {
        Status[i + j] += ISOLATED_PLUS;
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
                         int yday,
                         int N,
                         IntegerVector SupermarketTypical,
                         IntegerVector SupermarketHour,
                         double r_location,
                         double r_scale,
                         int r_d,
                         IntegerVector SupermarketFreq,
                         IntegerVector TodaysHz,
                         int hrs_open = 8,
                         int resistance1 = 400,
                         int resistance2 = 3,
                         double r_div = 36, // divide r_ by this // TODO: model is highly sensitive to this par!
                         bool verbose = false) {
  // array of supermarkets: SA2 x Supermarkets each SA2 x hour
  // (some sa2s have 17 supermarkets; others will just record 0 there)

  // int i_supermarkets[NSA2][maxSupermarketsBySA2][hrs_open];
  // memset(i_supermarkets, 0, sizeof i_supermarkets);

  int nInfections_len = NSA2 * hrs_open;
  nInfections_len *= 8;
  IntegerVector nInfections = no_init(nInfections_len);

#pragma omp parallel for num_threads(nThread)
  for (int k = 0; k < nInfections_len; ++k) {
    nInfections[k] = 0;
  }

#pragma omp parallel for num_threads(nThread)
  for (int i = 0; i < N; ++i) {
    if (Status[i] != STATUS_NOSYMP || TodaysHz[i] > SupermarketFreq[i]) {
      continue;
    }
    int supermarketi = SupermarketTypical[i];
    int random32 = unifRand(0, 31);
    int sa2i = shortSA2[i];
    int k = 0;

    // Mix if supermarket not defined then shift it around
    if (!nSupermarketsAvbl[i]) {
      // most of the time we do just assume isolated
      if ((random32 >> 2)) {
        continue;
      }
      // otherwise we choose the 'central' supermarket
      k += array3k(sa2i, supermarketi, 0, 0, 0) + random32;
    } else {
      k += array3k(sa2i, supermarketi, SupermarketHour[i], 8, hrs_open);
      if ((random32 >> 4)) {
        k -= (i % 4) + 1;
      }
    }

    if (k >= nInfections_len || k < 0) {
      continue;
    }

    // threadsafety
#ifdef _OPENMP
    if ((k % omp_get_num_threads()) != omp_get_thread_num()) {
      continue;
    }
#endif

    double loc = r_location / r_div;
    double sca = r_scale / r_div;

    nInfections[k] += r_Rand(loc, sca, r_d);
  }

#pragma omp parallel for num_threads(nThread)
  for (int i = 0; i < N; ++i) {
    if (Status[i] || !nSupermarketsAvbl[i] || TodaysHz[i] > SupermarketFreq[i]) {
      continue;
    }

    int sa2i = shortSA2[i];

    int k = array3k(sa2i, SupermarketTypical[i], SupermarketHour[i], 8, hrs_open);


    // threadsafety
#ifdef _OPENMP
    if ((k % omp_get_num_threads()) != omp_get_thread_num()) {
      continue;
    }
#endif

    if (nInfections[k]) {
      nInfections[k] -= 1;
      if (Resistance[i] < resistance1) {
        Status[i] = STATUS_NOSYMP;
        InfectedOn[i] = yday;
      }
    }
  }
}

void infect_school(IntegerVector Status,
                   IntegerVector InfectedOn,
                   IntegerVector School,
                   IntegerVector Age,
                   int yday,
                   int N,
                   const std::vector<int>& schoolIndices,
                   double r_location,
                   double r_scale,
                   int r_d,
                   bool only_Year12,
                   int n_schools,
                   int n_pupils) {

  // infect people within a school

  // Cube: number of visits by School x Age
  // First array index is the total, following indices are the age-based infections
  // Teachers are all aged '20'.
  int s_visits[n_schools][21];
  int i_visits[n_schools][21];
  memset(s_visits, 0, sizeof s_visits);
  memset(i_visits, 0, sizeof i_visits);

  for (int k = 0; k < n_pupils; ++k) {
    int i = schoolIndices[k];
    int schooli = School[i] - 1;
    int Agei = (Age[i] > 20) ? 20 : Age[i];
    if (only_Year12 && Agei < 17) {
      continue;
    }
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
    int i = schoolIndices[k];
    if (Status[i]) continue;
    int schooli = School[i] - 1;
    int Agei = (Age[i] > 20) ? 20 : Age[i];
    if (only_Year12 && Agei > 17) {
      continue;
    }

    // N.B. This logic means the 'first' people in the table get infected
    // first.  We could randomize this, but I don't think it matters.

    // TODO: make students of the same age more likely/first to be infected
    if (i_visits[schooli][0]) {
      Status[i] = STATUS_NOSYMP;
      InfectedOn[i] = yday;
      // i_visits[schooli][Agei] -= i_visits[schooli][Agei] > 0;
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
                      int nThread = 1,
                      int resistance1 = 400,
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
                    IntegerVector SupermarketHour,
                    int yday_start,
                    int days_to_sim,
                    int N = 25e6,
                    bool display_progress = true,
                    int nThread = 1) {

  Progress p(days_to_sim, display_progress);
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


  int maxSupermarketsBySA2 = 8;



  // for (int i = 1; i < NSA2; ++i) {
  //   maxSupermarketsBySA2 = maxii(maxSupermarketsBySA2, nSupermarketsBySA2[i]);
  // }


  // attach policy changes

  bool supermarkets_open = true;
  if (Policy.length() && Policy.containsElementNamed("supermarkets_open")) {
    supermarkets_open = Policy["supermarkets_open"];
  }

  bool schools_open = false;
  bool only_Year12  = false;
  if (Policy.length() && Policy.containsElementNamed("schools_open")) {
    schools_open = Policy["schools_open"];
  }
  if (Policy.length() && Policy.containsElementNamed("only_Year12")) {
    only_Year12 = Policy["only_Year12"];
  }

  bool do_contact_tracing = true;
  if (Policy.length() && Policy.containsElementNamed("do_contact_tracing")) {
    do_contact_tracing = Policy["do_contact_tracing"];
  }

  // TODO: make user-avbl
  int ptest_per_mille_sympto = 1000; // 100%
  int ptest_per_mille_asympto = 10; // 1%
  int days_to_notify = 3;
  int tests_avbl = 100e3;


  // attach epipars
  double incubation_m = Epi["incubation_mean"];
  double incubation_s = Epi["incubation_sigma"];
  double illness_m = Epi["illness_mean"];
  double illness_s = Epi["illness_sigma"];
  double r_location = Epi["r_location"];
  double r_scale = Epi["r_scale"];
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
  if (n_pupils > (2 * NPUPILS)) {
    Rcout << NPUPILS << "\n";
    Rcout << n_pupils << "\n";
    stop("n_pupils much larger than expected: likely an overestimate of schools.");
  }

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
  }

  if (which_unsorted_int(SA2)) {
    stop("SA2 was unsorted.");
  }
  IntegerVector shortSA2 = shorten_sa2s_ordered(SA2);

  DataFrame Statuses = DataFrame::create(Named("InitialStatus") = clone(Status));

  IntegerVector PlaceId = clone(SupermarketTarget);



  for (int day = 0; day < days_to_sim; ++day) {
    int yday = yday_start + day;

    const int wday_2020[7] = {3, 4, 5, 6, 7, 1, 2};
    int wday = wday_2020[(yday % 7)];
    bool is_weekday = wday != 0 && wday != 7;

    p.increment();

    int n_infected_today = 0;

#pragma omp parallel for num_threads(nThread) reduction(+:n_infected_today)
    for (int i = 0; i < N; ++i) {
      int statusi = Status[i];
      n_infected_today += (statusi > 0) && (statusi != ISOLATED_PLUS);
    }

    nInfected[day] = n_infected_today;

    Statuses.push_back(clone(Status));

    // no more infections?
    if (n_infected_today == 0) {
      continue;
    }


    // For example, SupermarketFreq[i] = 365  => every day
    // SupermarketFreq[i] = 1 every year.  So we create a vector
    // of 1:366 and compare that to the individual's tendency to
    // visit. So if TodaysHz[i] = 366 they will not visit anything
    // regardless; if TodaysHz[i] = 1 they will visit everything.
    IntegerVector TodaysHz = rep_len(dqsample_int2(365, wday * 23456), N);

    // We also want to draw quickly from a sample every day
    IntegerVector TodaysK = rep_len(dqsample_int2(1000, wday * 54321), N);

    // Use this with modulo
    IntegerVector Todays2B = rep_len(dqsample_int2(2e9, wday * 54321), N);


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
                          yday,
                          N,
                          SupermarketTypical,
                          SupermarketHour,
                          r_location, r_scale, r_d,
                          SupermarketFreq,
                          TodaysHz);
    }

    if (is_weekday && schools_open) {
      infect_school(Status, InfectedOn, School, Age,
                    yday, N,
                    schoolsIndex,
                    r_location, r_scale, r_d,
                    only_Year12,
                    NSCHOOLS,
                    n_pupils);
    }


    // finally

    infect_household(Status, InfectedOn, hid, seqN, HouseholdSize, Resistance, Age,
                     yday, N, HouseholdInfectedToday, nThread);

    if (do_contact_tracing) {
      contact_tracing(Status,
                      InfectedOn,
                      Incubation,
                      yday,
                      seqN,
                      HouseholdSize,
                      tests_avbl,
                      hid,
                      HouseholdInfectedToday,
                      N,
                      School,
                      PlaceId,
                      ptest_per_mille_asympto,
                      ptest_per_mille_sympto,
                      Todays2B,
                      days_to_notify,
                      nThread);
    }
  }

  return Rcpp::List::create(Named("nInfected") = nInfected,
                            Named("Statuses") = Statuses);
}
