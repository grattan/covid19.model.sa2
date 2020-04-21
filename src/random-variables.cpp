#include "covid19model.h"
#include <random>
#include <Rcpp.h>
using namespace Rcpp;


double m2mu(double m, double s) {
  return log(m) - s/2;
}

// thread-safe random variables
int unifRand(const int & a, const int & b) {
  static thread_local std::mt19937 generator;
  std::uniform_int_distribution<int> distribution(a, b);
  return distribution(generator);
}

int poisRand(const int & lambda) {
  static thread_local std::mt19937 generator;
  std::poisson_distribution<int> distribution(lambda);
  return distribution(generator);
}


double lnormRand(const double & a, const double & b) {
  static thread_local std::mt19937 generator;
  std::lognormal_distribution<double> distribution(a, b);
  return distribution(generator);
}

double cauchyRand(const double & a, const double & b) {
  static thread_local std::mt19937 generator;
  std::cauchy_distribution<double> distribution(a, b);
  return distribution(generator);
}

int cauchyRand0(const double & a, const double & b) {
  double out = cauchyRand(a, b);
  return (out > 1) ? out : 0;
}

int dbl2int(double x) {
  // if x is NaN then *both* of the following are false
  bool inrange = (x > INT_MIN) && (x < INT_MAX);
  return inrange ? ((int)x) : 0;
}

// use this for thread safety checks

// [[Rcpp::export(rng = false)]]
DoubleVector prlnorm_dbl(int n, double a, double b, int nThread = 1) {
  DoubleVector out = no_init(n);
#pragma omp parallel for num_threads(nThread)
  for (int i = 0; i < n; ++i) {
    out[i] = lnormRand(a, b);
  }
  return out;
}

// [[Rcpp::export(rng = false)]]
IntegerVector prlnorm_int(int n, double a, double b, int nThread = 1) {
  IntegerVector out = no_init(n);
#pragma omp parallel for num_threads(nThread)
  for (int i = 0; i < n; ++i) {
    out[i] = dbl2int(lnormRand(a, b));
  }
  return out;
}

// [[Rcpp::export(rng = false)]]
DoubleVector prcauchy(int n, double a, double b, int nThread = 1) {
  DoubleVector out = no_init(n);
#pragma omp parallel for num_threads(nThread)
  for (int i = 0; i < n; ++i) {
    out[i] = cauchyRand(a, b);
  }
  return out;
}
