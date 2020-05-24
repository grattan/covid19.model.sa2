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





bool yday2weekday(const int & yday) {
  int wday = wday_2020[((yday - 1) % 7)];
  return wday < 6;
}

bool yday2monday(const int & yday) {
  int wday = wday_2020[((yday - 1) % 7)];
  return wday == 1;
}










