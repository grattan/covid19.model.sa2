#include "covid19model.h"
#include <Rcpp.h>
using namespace Rcpp;

int maxii(int &a, int &b) {
  return (a < b) ? b : a;
}
int minii(int &a, int &b) {
  return (a < b) ? a : b;
}

int max0(int x) {
  return (x > 0) ? x : 0;
}

int max0(double x) {
  return (x > 0) ? x : 0;
}

// [[Rcpp::export]]
IntegerVector do_minmax_par(IntegerVector x, int nThread = 1) {
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
IntegerVector do_ModuloIndex(IntegerVector x, int mod, int max, int nThread = 1) {
  std::vector<int> y;
  y.reserve(max);
  for (int j = 0; j < max; ++j) {
    y.push_back(j % mod);
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
IntegerVector do_modulo_d(IntegerVector x, int m, int divisor, int nThread = 1) {
  if (m < 1) {
    return x;
  }
  int n = x.length();
  IntegerVector out = no_init(n);
  if (divisor <= 1) {
#pragma omp parallel for num_threads(nThread)
    for (int i = 0; i < n; ++i) {
      out[i] = x[i] % m;
    }
    return out;
  }

#pragma omp parallel for num_threads(nThread)
  for (int i = 0; i < n; ++i) {
    out[i] = (x[i] / divisor) % m;
  }
  return out;
}

// [[Rcpp::export]]
IntegerVector do_modulo_16(IntegerVector x, int nThread = 1) {
  int n = x.length();
  IntegerVector out = no_init(n);

#pragma omp parallel for num_threads(nThread)
  for (int i = 0; i < n; ++i) {
    out[i] = x[i] % 16;
  }


  return out;
}


// [[Rcpp::export]]
IntegerVector do_lag_in_place(IntegerVector x) {
  int N = x.length();
  int x0 = x[0];
  for (int i = 0; i < N - 1; ++i) {
    x[i] = x[i + 1];
  }
  x[N - 1] = x0;
  return x;
}

// [[Rcpp::export]]
IntegerVector do_pminCppp(IntegerVector x, int a = 0, int nThread = 1) {
  int N = x.length();
  IntegerVector out = no_init(N);
#pragma omp parallel for num_threads(nThread)
  for (int i = 0; i < N; ++i) {
    out[i] = (x[i] > a) ? a : x[i];
  }
  return out;
}



// [[Rcpp::export]]
List sa2_firsts_finals(IntegerVector SA2, int nsa2 = 2310) {
  IntegerVector SA2_firsts = no_init(nsa2);
  IntegerVector SA2_finals = no_init(nsa2);
  int s = 0;
  int N = SA2.length();
  SA2_firsts[s] = 0;
  SA2_finals[nsa2 - 1] = N - 1;

  for (int i = 1; i < N; ++i) {
    int d = SA2[i] - SA2[i - 1];
    // if nonzero continue;
    // if d == 1 great! just a regular increment
    // if d == 2 then next SA2 doesn't appear so
    // it both starts and stops here
    while (d > 0 && ++s < nsa2) {
      --d;
      SA2_firsts[s] = i;
      SA2_finals[s - 1] = i;
    }
  }
  while (++s < nsa2) {
    SA2_firsts[s] = N - 1;
    SA2_finals[s - 1] = N - 1;
  }
  return List::create(SA2_firsts, SA2_finals);
}





