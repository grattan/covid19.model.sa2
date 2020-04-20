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

void do_1day_supermarket(IntegerVector Status,
                         IntegerVector InfectedOn,
                         IntegerVector SA2,
                         IntegerVector Age,
                         IntegerVector Employment,
                         IntegerVector SupermarketTarget,
                         IntegerVector Resistance,
                         int yday,
                         int N = 25e6,
                         bool check_sa2_key = true,
                         int returner = 0,
                         double r0_supermarket = 2.5,
                         int resistance1 = 400,
                         int resistance2 = 3,
                         bool verbose = false,
                         int nThread = 1) {
  const int PERSONS_PER_SUPERMARKET = N / N_SUPERMARKETS;

  Timer timer;

  // Idea: First, count number of people and infected people
  // who visited each supermarket.  Then, if these numbers
  // are large enough, create new infections among the
  // susceptible.

  if (N != Status.length() ||
      N != SA2.length() ||
      N != Age.length() ||
      N != Employment.length() ||
      N != SupermarketTarget.length() ||
      N != Resistance.length()) {
    Rcout << "SupermarketTarget " << SupermarketTarget.length() << "\n";
    Rcout << "N " << N << "\n";
    Rcout << "Status " << Status.length() << "\n";
    Rcout << "SA2 " << SA2.length() << "\n";
    Rcout << "Age " << Age.length() << "\n";
    Rcout << "Employment " << Employment.length() << "\n";
    Rcout << "Resistance " << Resistance.length() << "\n";
    stop("Internal error: lengths differ.");
  }

  // assume that SA2 is keyed
  timer.step("which_unsorted_int");

  timer.step("allocate_n_supermarkets");
  IntegerVector nInfectedVisitorsBySupermarket(N_SUPERMARKETS);
  IntegerVector nVisitorsBySupermarket(N_SUPERMARKETS);

  timer.step("supermarket_j");
  // supermarket_j is the shift from
  int supermarket_j = 0;
  int infected_visitors = 0;
  for (int i = 0; i < N; ++i) {
    int supermarket_target = SupermarketTarget[i];
    if (supermarket_target > 0) {
      // SupermarketTarget is 1-indexed.
      nVisitorsBySupermarket[supermarket_target - 1] += 1;
      nInfectedVisitorsBySupermarket[supermarket_target - 1] += (Status[i] > 0) ? rcauchy_int(2, 0.001) : 0;
    }
  }

  IntegerVector NewInfectionsBySupermarket = no_init(N_SUPERMARKETS);
  for (int k = 0; k < N_SUPERMARKETS; ++k) {
    NewInfectionsBySupermarket[k] = (int)(r0_supermarket * nInfectedVisitorsBySupermarket[k]);
  }

  IntegerVector nInfected = no_init(N);
  timer.step("reinfect");
  // reinfect
  for (int i = 0; i < N; ++i) {
    if (Status[i]) {
      continue; // already infected
    }

    int supermarket_target = SupermarketTarget[i];
    // if the person visited a supermarket they become infected
    // probablistically
    if (supermarket_target > 0) {
      int n_new_cases = NewInfectionsBySupermarket[supermarket_target - 1];
      if (n_new_cases) {
        // Resistance determines infections and Age determines criticality
        Status[i] = (Resistance[i] < resistance1) + (Resistance[i] < (Age[i] * resistance2));
        if (Status[i] > 0) {
          InfectedOn[i] = yday;
        }
        // Used
        NewInfectionsBySupermarket[supermarket_target - 1] -= 1;
      }

    }
  }
  timer.step("finish");
  if (verbose) {
    DoubleVector res(timer);
    for (int i = 0; i < res.size(); ++i) {
      res[i] = res[i] / 1e6;
    }

    Rcout << res << std::endl;
  }

  // void
}

void infect_school(IntegerVector Status,
                   IntegerVector InfectedOn,
                   IntegerVector School,
                   IntegerVector Age,
                   int yday,
                   int N,
                   std::vector<int> schoolIndices,
                   double cauchy_loc,
                   double cauchy_scale,
                   bool only_Year12 = false,
                   int n_schools = -1) {
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

  for (int k = 0; k < schoolIndices.size(); ++k) {
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
      int infectedi = rcauchy_int(2, 0.01);
      i_visits[schooli][0] += infectedi;
      i_visits[schooli][Agei] += infectedi;
    }
  }
  for (int k = 0; k < schoolIndices.size(); ++k) {
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
                      int maxHouseholdSize = -1,
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
      // single-person household can't infect anyone else
      continue;
    }
    if (Status[i]) {
      // if not susceptible
      continue;
    }
    if (HouseholdSize[i] == 2) {
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
    int household_infected = 0;
    for (int j = 0; j < nh; ++j) {
      household_infected += Status[i + j] > 0;
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

  // attach policy changes

  bool schools_open = false;
  bool only_Year12  = false;
  if (Policy.length() && Policy.containsElementNamed("schools_open")) {
    schools_open = Policy["schools_open"];
  }
  if (Policy.length() && Policy.containsElementNamed("only_Year12")) {
    only_Year12 = Policy["only_Year12"];
  }



  // attach epipars
  // TODO: redundant if using set_epi_pars at R level?
  bool useEpi = Epi.length() > 0;
  double asympto = 0.48;
  int duration_active = 13;
  int lambda_infectious = 9;
  if (useEpi && Epi.containsElementNamed("asympto")) {
    asympto = Epi["asympto"];
  }
  if (useEpi && Epi.containsElementNamed("duration_active")) {
    duration_active = Epi["duration_active"];
  }
  if (useEpi && Epi.containsElementNamed("lambda_infectious")) {
    lambda_infectious = Epi["lambda_infectious"];
  }

  double cau_l = 2;
  double cau_s = 0.01;
  cau_l = (useEpi && Epi.containsElementNamed("cau_l") ? Epi["cau_l"] : cau_l);
  cau_s = (useEpi && Epi.containsElementNamed("cau_s") ? Epi["cau_s"] : cau_s);

  double incubation_m = Epi["incubation_m"];

  std::vector<int> schoolsIndex;
  for (int i = 0; i < N; ++i) {
    if (School[i] > 0) schoolsIndex.push_back(i);
  }

  IntegerVector nInfected = no_init(days_to_sim);
  DataFrame Statuses = DataFrame::create(Named("Status") = Status);
  for (int day = 0; day < days_to_sim; ++day) {
    int yday = yday_start + day;
    p.increment();

    // For example, SupermarketFreq[i] = 365  => every day
    // SupermarketFreq[i] = 1 every year.  So we create a vector
    // of 1:366 and compare that to the individual's tendency to
    // visit. So if TodaysHz[i] = 366 they will not visit anything
    // regardless; if TodaysHz[i] = 1 they will visit everything.
    IntegerVector TodaysHz = Rcpp::rep_len(Rcpp::sample(365, 365, true), N);


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

    double incubation_mu = m2mu(incubation_m, 0.44);

#pragma omp parallel for num_threads(nThread)
    for (int i = 0; i < N; ++i) {
      if (Status[i] > 0 && (InfectedOn[i] + dbl2int(3 * lnormRand(incubation_mu, 0.44)) > yday)) {
        Status[i] = -1;
      }
      // did they go outside
      // 1 -> infectious but showing no symptoms
      // -1 healed potentially go outside (but not to infect or be infected)
      bool goes_outside = Status[i] == 0 || Status[i] == 1;
      // goes_outside = true;
      // bool contagious = Status[i] > 0;

      // int ssa2 = short_sa2(SA2[i]);
      // bool sa2_change = ((i > 0) && SA2[i] != SA2[i - 1]);
      // if (sa2_change) {
      //   // on the sa2 change we incremenent supermarket_cumj
      //   // by the previous SA2's number of supermarket
      //   // This moves along the array of supermarkets
      //   // so that rand % n_supermarkets_avbl moves within
      //   // the SA2's supermarket index for SupermarketTarget.
      //   supermarket_cumj += n_supermarkets_avbl;
      //   n_supermarkets_avbl = nSupermarketsBySA2[ssa2];
      // }
      int n_supermarkets_avbl = nSupermarketsAvbl[i];



      if (goes_outside) {
        bool moves_sa2 = false;
        if (moves_sa2) {
          int new_sa2 = 0;
          // don't get infected
          SA2[i] = new_sa2;
        } else {
          // bool commutes = false; // goes to work
          // bool is_pupil = false;
          // int destination_type = 0; // 1-106

          // TODO: n_supermarkets_avbl needs to be loosened
          if (n_supermarkets_avbl && SupermarketFreq[i] > TodaysHz[i]) {
            // they will visit a supermarket
            // fast_basic_rand is both deterministic and non-uniform
            // but neither is important here: we just want some allocation
            // to one of the supermarkets that allows mixing.
            int supermarket_visited = unifRand(0, n_supermarkets_avbl - 1);
            SupermarketTarget[i] = supermarket_visited + supermarket_cumj;
          }

        }

      }
    }

    // This function actually performs the interactions and infections
    do_1day_supermarket(Status,
                        InfectedOn,
                        SA2,
                        Age,
                        Employment,
                        SupermarketTarget,
                        Resistance,
                        yday,
                        N,
                        /* check_sa2_key = */ day == 0);


    if (schools_open) {
      infect_school(Status, InfectedOn, School, Age, yday, N, schoolsIndex, cau_l, cau_s,
                    only_Year12);
    }

    int maxHouseholdSize = -1;
    // finally
    infect_household(Status, InfectedOn, hid, seqN, HouseholdSize, Resistance, Age, yday,
                     N, maxHouseholdSize, nThread);

    int n_infected_today = 0;

#pragma omp parallel for num_threads(nThread) reduction(+:n_infected_today)
    for (int i = 0; i < N; ++i) {
      n_infected_today += (Status[i] == 1);
    }

    nInfected[day] = n_infected_today;

    Statuses.push_back(clone(Status));
  }

  return Rcpp::List::create(Named("nInfected") = nInfected,
                            Named("Statuses") = Statuses);
}






