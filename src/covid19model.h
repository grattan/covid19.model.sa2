#ifndef covid19model_H
#define covid19model_H

#include "covid19model.h"
#include <vector>
#include <numeric>      // std::iota
#include <algorithm>
// [[Rcpp::depends(RcppProgress)]]
#include <progress.hpp>
#include <progress_bar.hpp>
#include <Rcpp.h>
using namespace Rcpp;
#ifdef _OPENMP
#include <omp.h>
#endif

const int NSTATES = 9;
const int NSTATES1 = 10;
const int NSA2 = 2310;
const int NSCHOOLS = 9501;
const int NPUPILS = 3135825;

const int NTODAY = 262144; // good cache length

const int STATUS_KILLED = -2;
const int STATUS_HEALED = -1;
const int STATUS_SUSCEP =  0;
const int STATUS_NOSYMP =  1;
const int STATUS_INSYMP =  2;
const int STATUS_CRITIC =  3;

const int ISOLATED_PLUS = 32;

// We want to avoid branching and prefer + or - to if statements
// e.g.
//  status = if (dies) -2 else -1;
//  status = -1 - dies;
const int HEALED_MINUS_KILLED = 1;
const int INSYMP_MINUS_NOSYMP = 1;
const int CRITIC_MINUS_INSYMP = 1;

const int SPECIFICITY = 992;
const int SENSITIVITY = 800;

int which_unsorted_int(IntegerVector x);

int short_sa2(int sa2);

IntegerVector shorten_sa2s_ordered(IntegerVector SA2);

double m2mu(double m, double s);

int poisRand(const int & lambda);

IntegerVector dqsample_int2(int m, int n);
int unifRand(const int & a, const int & b);

double lnormRand(const double & a, const double & b);

double cauchyRand(const double & a, const double & b);
int cauchyRand0(const double & a, const double & b);

int dbl2int(double x);

int maxii(int a, int b);
int max0(int x);
int max0(double x);

IntegerVector do_lag_int(IntegerVector s, int nThread);
IntegerVector modulo(IntegerVector x, int m, int d, int nThread);


#endif
