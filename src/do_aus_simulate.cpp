#include "covid19model.h"
#include <Rcpp.h>
#include <Rcpp/Benchmark/Timer.h>
using namespace Rcpp;

#define N_SUPERMARKETS 7487

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
  NumericVector o = Rcpp::rcauchy(1, l, s);
  double out = o[0];
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
    return cauchyRand(m, s);

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
    return cauchyRand(m, s);

  return m;
}


// https://stackoverflow.com/questions/1577475/c-sorting-and-keeping-track-of-indexes


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
                         IntegerVector SA2,
                         int nThread,
                         IntegerVector Age,
                         IntegerVector Employment,
                         IntegerVector nSupermarketsBySA2,
                         int maxSupermarketsBySA2,
                         IntegerVector nSupermarketsAvbl,
                         IntegerVector Resistance,
                         int yday,
                         int N,
                         double r_location,
                         double r_scale,
                         IntegerVector SupermarketFreq,
                         IntegerVector TodaysHz,
                         int hr_open = 9,
                         int hr_close = 22,
                         int resistance1 = 400,
                         int resistance2 = 3,
                         double r_div = 36, // divide r_ by this // TODO: model is highly sensitive to this par!
                         bool verbose = false) {
  // array of supermarkets: SA2 x Supermarkets each SA2 x hour
  // (some sa2s have 17 supermarkets; others will just record 0 there)

  int i_supermarkets[NSA2][maxSupermarketsBySA2][24];
  memset(i_supermarkets, 0, sizeof i_supermarkets);

  // for early return in second loop
  int new_infections = 0;

  for (int i = 0; i < N; ++i) {
    if (Status[i] != 1) {
      continue;
    }
    int sa2i = short_sa2(SA2[i]);
    int n_supermarkets_avbl = nSupermarketsAvbl[i];
    if (!n_supermarkets_avbl || TodaysHz[i] > SupermarketFreq[i]) {
      continue;
    }
    int sj = 0;
    if (n_supermarkets_avbl > 1) {
      // sj = unifRand(0, n_supermarkets_avbl - 1);
      sj = (yday % 8) ? i % n_supermarkets_avbl : unifRand(0, n_supermarkets_avbl - 1);
    }
    // hour of visit today
    int hr = unifRand(hr_open, hr_close);

    // derived index of SA2SupermarketHour
    //
    // int k = cumSupermarketsBySA2[sa2i] * 24 + sj * 24 + hr;
    int infections_from_i = cauchyRand0(r_location / r_div, r_scale / r_div);
    i_supermarkets[sa2i][sj][hr] += infections_from_i;
    new_infections += infections_from_i;
  }

  for (int i = 0; i < N; ++i) {
    // if no new infections or the person is not susceptible, keep playing
    if (!new_infections || Status[i]) {
      continue;
    }
    int sa2i = short_sa2(SA2[i]);
    int n_supermarkets_avbl = nSupermarketsAvbl[i];
    if (!n_supermarkets_avbl) {
      continue;
    }
    // This time we skip if the person is infected.

    int sj = 0;
    if (n_supermarkets_avbl > 1) {
      // sj = unifRand(0, n_supermarkets_avbl - 1);
      sj = i % n_supermarkets_avbl;
    }
    // hour of visit today
    int hr = unifRand(hr_open, hr_close);
    // int k = cumSupermarketsBySA2[sa2i] * 24 + sj * 24 + hr;
    if (i_supermarkets[sa2i][sj][hr]) {
      if (Resistance[i] < resistance1) {
        Status[i] = 1;
        InfectedOn[i] = yday;
      }
      i_supermarkets[sa2i][sj][hr] -= -1;
      --new_infections;
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
  // void function so this run at most once
  if (n_schools < 0) {
    std::set<int> SchoolSet;
    for (int i = 0; i < N; ++i) {
      SchoolSet.insert(School[i]);
    }
    n_schools = SchoolSet.size();
  }
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
    if (Status[i] == 1) {
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
      Status[i] = 1;
      InfectedOn[i] = yday;
      i_visits[schooli][Agei] -= i_visits[schooli][Agei] > 0;
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
                      int &maxHouseholdSize,
                      int nThread = 1,
                      int resistance1 = 400,
                      int resistance_penalty = 100) {
  // resistance_penalty = penalty against Resistance[i]
  // that makes infection more likely. Higher penalties
  // make infection more likely among otherwise resistant
  // individuals
  if (maxHouseholdSize <= 0) {
    maxHouseholdSize = do_max_par_int(HouseholdSize, nThread);
  }

#pragma omp parallel for num_threads(nThread)
  for (int i = 0; i < N; ++i) {
    // separate household ids into the same thread
    if ((hid[i] % omp_get_num_threads()) != omp_get_thread_num()) {
      continue;
    }
    if (HouseholdSize[i] == 1) {
      // no transmission in single-person household
      continue;
    }

    if (HouseholdSize[i] == 2) {
      if (Status[i]) {
        // if not susceptible
        continue;
      }

      // in this case, we only need to check adjacent
      int household_infected = 0;
      if (seqN[i] == 1) {
        household_infected = Status[i + 1] > 0;
      } else {
        household_infected = Status[i - 1] > 0;
      }
      // Prima facie we have a race condition on Status, but in fact
      // we have skipped anyone who has Status[i] != 0 so the only way
      // the following assignment can occur is if the other thread is
      // skipping
      if (household_infected) {
        int r = resistance1 + unifRand(-1, resistance_penalty);
        if (Resistance[i] < r) {
          Status[i] = 1;
          InfectedOn[i] = yday + 1;
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
    int j = 1;
    // loop through the household, stop once an infection detected
    for (; j < nh && !household_infected; ++j) {
      household_infected = Status[i + j] > 0;
    }
    if (household_infected) {
      // return to first person and infect as appropriate
      // (don't infect already infected)
      j = 0;
      for (; j < nh; ++j) {
        int r = resistance1 + unifRand(-1, resistance_penalty);
        int ij = i + j;
        if (Status[ij] == 0 && Resistance[ij] < r) {
          Status[ij] = 1;
          InfectedOn[ij] = yday + 1;
        }
      }
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


  int maxSupermarketsBySA2 = 1;
  int nSupermarkets = nSupermarketsBySA2[0];


  for (int i = 1; i < NSA2; ++i) {
    maxSupermarketsBySA2 = maxii(maxSupermarketsBySA2, nSupermarketsBySA2[i]);
    nSupermarkets += nSupermarketsBySA2[i];
  }


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
  // schoolsIndex.reserve(N);
  for (int i = 0; i < N; ++i) {
    if (School[i] > 0) {
      ++n_pupils;
      schoolsIndex.push_back(i);
    }
  }

  // variables which will be updated on day = 0
  int n_schools = -1;
  int maxHouseholdSize = -1;

  IntegerVector nInfected = no_init(days_to_sim);
  IntegerVector Incubation = no_init(N);
  IntegerVector Illness = no_init(N);
  IntegerVector SupermarketTarget = no_init(N);
#pragma omp parallel for num_threads(nThread)
  for (int i = 0; i < N; ++i) {
    Incubation[i] = 0;
    Illness[i] = 0;
    SupermarketTarget[i] = 0;
  }


  DataFrame Statuses = DataFrame::create(Named("Status") = clone(Status));
  for (int day = 0; day < days_to_sim; ++day) {
    int yday = yday_start + day;
    p.increment();

    int n_infected_today = 0;

#pragma omp parallel for num_threads(nThread) reduction(+:n_infected_today)
    for (int i = 0; i < N; ++i) {
      n_infected_today += (Status[i] >= 1);
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
    IntegerVector TodaysHz = Rcpp::rep_len(Rcpp::sample(365, 365, true), N);



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
          bool becomes_symptomatic = statusi >= 2 || (unifRand(0, 1000) > p_asympto);
          bool becomes_critical = statusi == 3 || (becomes_symptomatic && unifRand(0, 1000) < p_critical);
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
              Status[i] = 2 + becomes_critical;
            } else {
              // nothing to do: they're still ill, but at Status 1.
            }
          } else {
            // Today is after the illness has run its course
            Status[i] = -1 - dies;
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
                          SA2,
                          nThread,
                          Age,
                          Employment,
                          nSupermarketsBySA2,
                          maxSupermarketsBySA2,
                          nSupermarketsAvbl,
                          Resistance,
                          yday,
                          N,
                          r_location,
                          r_scale,
                          SupermarketFreq,
                          TodaysHz);
    }

    if (schools_open) {
      infect_school(Status, InfectedOn, School, Age, yday, N, schoolsIndex,
                    r_location, r_scale, r_d,
                    only_Year12,
                    n_schools,
                    n_pupils);
    }


    // finally

    infect_household(Status, InfectedOn, hid, seqN, HouseholdSize, Resistance, Age, yday,
                     N, maxHouseholdSize, nThread);
  }

  return Rcpp::List::create(Named("nInfected") = nInfected,
                            Named("Statuses") = Statuses);
}
