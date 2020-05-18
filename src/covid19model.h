#ifndef covid19model_H
#define covid19model_H

#include "covid19model.h"
#include <vector>
#include <array>
#include <random>
#include <numeric>      // std::iota
#include <algorithm>
// [[Rcpp::depends(RcppProgress)]]
#include <progress.hpp>
#include <progress_bar.hpp>
#include <Rcpp.h>
// [[Rcpp::plugins(openmp)]]
#ifdef _OPENMP
#include <omp.h>
#endif
using namespace Rcpp;

const int NSTATES = 9;
const int NSTATES1 = 10;
const int NSA2 = 2310;
const int NSCHOOLS = 9501;
const int NPUPILS = 3135825;
const int NDZN = 9077; // includes NA

const int NCAFES = 9730;
const int NRESTAURANTS = 20026;
const int MAXSUPERMARKETSBYSA2 = 8;

const int MAX_N_PLACES_TOTAL = 42689; // "TypeInt = 38, Type = food"
const int MAX_N_PLACES_PER_SA2 = 242; // "TypeInt = 47, Type = health, shortSA2 = 2026"
const int PLACEID_ESTABLISHMENT = 33;
const int PLACEID_POINT_OF_INTEREST = 78;
const int PLACEID_STORE = 94;
const int PLACES_HRS_OPEN = 8;




const int NTODAY = 261167; // prime just below a good cache length (262144)
const int MWORKPLACES = 5e6; // number of workpalces approximately

const int WID_SUPREMUM = 1048576; // (sufficiently high approximate number of WIDs)

const int STATUS_KILLED = -2;
const int STATUS_HEALED = -1;
const int STATUS_SUSCEP =  0;
const int STATUS_NOSYMP =  1;
const int STATUS_INSYMP =  2;
const int STATUS_CRITIC =  3;

const int ISOLATED_PLUS = 32;

const int NSTATUSES = 12;

// We want to avoid branching and prefer + or - to if statements
// e.g.
//  status = if (dies) -2 else -1;
//  status = -1 - dies;
const int HEALED_MINUS_KILLED = 1;
const int INSYMP_MINUS_NOSYMP = 1;
const int CRITIC_MINUS_INSYMP = 1;

const int SPECIFICITY = 992;
const int SENSITIVITY = 800;

// LFS = Labour force status
const int LFS_NOTINLFS = -1;
const int LFS_UNEMPLOY = 0;
const int LFS_PARTTIME = 1;
const int LFS_FULLTIME = 2;

const int SUPERMARKET_WEEKDAY_HRS = 16;
const int SUPERMARKET_WEEKEND_HRS = 12;

//// https://www.education.gov.au/school-term-dates-2020
const int NSW_TERM2_START_YDAY = 118; /// yday(as.Date("2020-04-27"))
const int VIC_TERM2_START_YDAY = 105;
const int QLD_TERM2_START_YDAY = 111;
const int  SA_TERM2_START_YDAY = 118;
const int  WA_TERM2_START_YDAY = 119;
const int TAS_TERM2_START_YDAY = 118;
const int ACT_TERM2_START_YDAY = 118;
const int  NT_TERM2_START_YDAY = 111;

const int NSW_TERM2_FINAL_YDAY = 185;
const int VIC_TERM2_FINAL_YDAY = 178;
const int QLD_TERM2_FINAL_YDAY = 178;
const int  SA_TERM2_FINAL_YDAY = 185;
const int  WA_TERM2_FINAL_YDAY = 185;
const int TAS_TERM2_FINAL_YDAY = 185;
const int ACT_TERM2_FINAL_YDAY = 185;
const int  NT_TERM2_FINAL_YDAY = 178;

const int NSW_TERM3_START_YDAY = 202;
const int VIC_TERM3_START_YDAY = 195;
const int QLD_TERM3_START_YDAY = 195;
const int  SA_TERM3_START_YDAY = 202;
const int  WA_TERM3_START_YDAY = 202;
const int TAS_TERM3_START_YDAY = 202;
const int ACT_TERM3_START_YDAY = 202;
const int  NT_TERM3_START_YDAY = 203;

const int NSW_TERM3_FINAL_YDAY = 269;
const int VIC_TERM3_FINAL_YDAY = 262;
const int QLD_TERM3_FINAL_YDAY = 262;
const int  SA_TERM3_FINAL_YDAY = 269;
const int  WA_TERM3_FINAL_YDAY = 269;
const int TAS_TERM3_FINAL_YDAY = 269;
const int ACT_TERM3_FINAL_YDAY = 269;
const int  NT_TERM3_FINAL_YDAY = 269;

// weekdays in 2020
//                  yday  1, 2, 3, 4, 5, 6, 7, ...
const int wday_2020[7] = {3, 4, 5, 6, 7, 1, 2};

bool yday2weekday(const int & yday);
bool yday2monday(const int & yday);

void assert_is_weekday(const int & day);
void assert_is_monday(const int & day);



int which_unsorted_int(IntegerVector x);
bool do_is_unsorted_pint(IntegerVector x, int nThread);

int short_sa2(int sa2);

IntegerVector shorten_sa2s_ordered(IntegerVector SA2);

double m2mu(double m, double s);

int poisRand(const int & lambda);

IntegerVector dqsample_int2(int m, int n);
int unifRand(const int & a, const int & b);

double lnormRand(const double & a, const double & b);

double cauchyRand(const double & a, const double & b);
int cauchyRand0(const double & a, const double & b);

int geomRand(const double & lambda);

int dbl2int(double x);

int maxii(int &a, int &b);
int minii(int &a, int &b);
int max0(int x);
int max0(double x);

// const int out1d_len = NSA2 * 101 * NSTATUSES;
const int out1d_len = 2799720;
const int NAGES_X_NSTATUSES = 1212;

IntegerVector do_minmax_par(IntegerVector x, int nThread);
IntegerVector do_lag_int(IntegerVector s, int nThread);
IntegerVector modulo(IntegerVector x, int m, int d, int nThread);
IntegerVector prlnorm_int(int n, double a, double b, int nThread);
IntegerVector RCauchy(IntegerVector U, double location, double scale, int nThread);
IntegerVector do_lemire_rand_par(int n, IntegerVector S, int nThread);
int lehmer32();
std::vector<char> do_lemire_char_par(int n, double p, IntegerVector S, int nThread);

IntegerVector get_nColleagues(int nr, int N,
                              IntegerVector LabourForceStatus,
                              int nThread,
                              int c_d,
                              double beta,
                              double mu,
                              double sigma);
List sa2_firsts_finals(IntegerVector SA2, int nsa2);


#endif
