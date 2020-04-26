#include "covid19model.h"
#include <Rcpp.h>
using namespace Rcpp;

int maxii(int a, int b) {
  return (a < b) ? b : a;
}

int max0(int x) {
  return (x > 0) ? x : 0;
}

int max0(double x) {
  return (x > 0) ? x : 0;
}

// [[Rcpp::export]]
IntegerVector do_minmax_par(IntegerVector x, int nThread = 10) {
  int N = x.length();
  int xmin = x[0];
  int xmax = x[0];
#pragma omp parallel for num_threads(nThread) reduction(min : xmin) reduction(max : xmax)
  for (int i = 1; i < N; ++i) {
    int xi = x[i];
    if (xi < xmin) {
      xmin = xi;
    } else if (xi > xmax) {
      xmax = xi;
    }
  }
  IntegerVector out(2);
  out[0] = xmin;
  out[1] = xmax;
  return out;
}

// [[Rcpp::export]]
IntegerVector do_ModuloIndex(IntegerVector x, int d, int m, int nThread = 1) {
  std::vector<int> y;
  y.reserve(m);
  for (int j = 0; j < m; ++j) {
    y.push_back(j % d);
  }
  int n = x.length();
  IntegerVector out = no_init(n);
#pragma omp parallel for num_threads(nThread)
  for (int i = 0; i < n; ++i) {
    out[i] = y[x[i]];
  }
  return out;
}


// [[Rcpp::export]]
IntegerVector do_modulo_d(IntegerVector x, int m, int d, int nThread = 1) {
  if (m < 1) {
    return x;
  }
  int n = x.length();
  IntegerVector out = no_init(n);
  if (d <= 1) {
#pragma omp parallel for num_threads(nThread)
    for (int i = 0; i < n; ++i) {
      out[i] = x[i] % m;
    }
    return out;
  }

#pragma omp parallel for num_threads(nThread)
  for (int i = 0; i < n; ++i) {
    out[i] = (x[i] / d) % m;
  }
  return out;
}
