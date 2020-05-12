#include "covid19model.h"
#include <random>
#include <Rcpp.h>
#include <stdint.h>
#include <dqrng.h>
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

int geomRand(const double & lambda) {
  static thread_local std::mt19937 generator;
  std::geometric_distribution<int> distribution(lambda);
  return distribution(generator);
}

int cauchyRand0(const double & a, const double & b) {
  double out = cauchyRand(a, b);
  return (out > 1 && out < 1024) ? out : 0;
}

int dbl2int(double x) {
  // if x is NaN then *both* of the following are false
  bool inrange = (x > INT_MIN) && (x < INT_MAX);
  return inrange ? ((int)x) : 0;
}

// use this for thread safety checks

// [[Rcpp::export]]
IntegerVector punif_int(int n, int a, int b, int nThread = 1) {
  IntegerVector out = no_init(n);
#pragma omp parallel for num_threads(nThread)
  for (int i = 0; i < n; ++i) {
    out[i] = unifRand(a, b);
  }
  return out;
}

// [[Rcpp::export]]
IntegerVector dqsample_int2(int m, int n) {
  return dqrng::dqsample_int(m, n, true, R_NilValue, 0);
}

// [[Rcpp::export]]
DoubleVector prlnorm_dbl(int n, double a, double b, int nThread = 1) {
  DoubleVector out = no_init(n);
#pragma omp parallel for num_threads(nThread)
  for (int i = 0; i < n; ++i) {
    out[i] = lnormRand(a, b);
  }
  return out;
}

// [[Rcpp::export]]
IntegerVector prlnorm_int(int n, double a, double b, int nThread = 1) {
  IntegerVector out = no_init(n);
#pragma omp parallel for num_threads(nThread)
  for (int i = 0; i < n; ++i) {
    out[i] = dbl2int(lnormRand(a, b));
  }
  return out;
}

// [[Rcpp::export]]
DoubleVector prcauchy(int n, double a, double b, int nThread = 1) {
  DoubleVector out = no_init(n);
#pragma omp parallel for num_threads(nThread)
  for (int i = 0; i < n; ++i) {
    out[i] = cauchyRand(a, b);
  }
  return out;
}

/**
 * D. H. Lehmer, Mathematical methods in large-scale computing units.
 * Proceedings of a Second Symposium on Large Scale Digital Calculating
 * Machinery;
 * Annals of the Computation Laboratory, Harvard Univ. 26 (1951), pp. 141-146.
 *
 * P L'Ecuyer,  Tables of linear congruential generators of different sizes and
 * good lattice structure. Mathematics of Computation of the American
 * Mathematical
 * Society 68.225 (1999): 249-260.
 */


#include <stdint.h>
#include <cstdint>

#if INTPTR_MAX == INT64_MAX

bool is64bit = true;

__uint128_t g_lehmer64_state = 353;

uint64_t lehmer64() {
  g_lehmer64_state *= 0xda942042e4dd58b5;
  return g_lehmer64_state >> 64;
}

#else
bool is64bit = false;
#endif



// original documentation by Vigna:
/* This is a fixed-increment version of Java 8's SplittableRandom generator
 See http://dx.doi.org/10.1145/2714064.2660195 and
 http://docs.oracle.com/javase/8/docs/api/java/util/SplittableRandom.html
 It is a very fast generator passing BigCrush, and it can be useful if
 for some reason you absolutely want 64 bits of state; otherwise, we
 rather suggest to use a xoroshiro128+ (for moderately parallel
 computations) or xorshift1024* (for massively parallel computations)
 generator. */

#if INTPTR_MAX == INT64_MAX
// state for splitmix64
uint64_t splitmix64_x; /* The state can be seeded with any value. */

// call this one before calling splitmix64
static inline void splitmix64_seed(uint64_t seed) { splitmix64_x = seed; }

// returns random number, modifies splitmix64_x
// compared with D. Lemire against
// http://grepcode.com/file/repository.grepcode.com/java/root/jdk/openjdk/8-b132/java/util/SplittableRandom.java#SplittableRandom.0gamma
static inline uint64_t splitmix64(void) {
  uint64_t z = (splitmix64_x += UINT64_C(0x9E3779B97F4A7C15));
  z = (z ^ (z >> 30)) * UINT64_C(0xBF58476D1CE4E5B9);
  z = (z ^ (z >> 27)) * UINT64_C(0x94D049BB133111EB);
  return z ^ (z >> 31);
}

// returns the 32 least significant bits of a call to splitmix64
// this is a simple function call followed by a cast
static inline uint32_t splitmix64_cast32(void) {
  return (uint32_t)splitmix64();
}

// same as splitmix64, but does not change the state, designed by D. Lemire
static inline uint64_t splitmix64_stateless(uint64_t index) {
  uint64_t z = (index + UINT64_C(0x9E3779B97F4A7C15));
  z = (z ^ (z >> 30)) * UINT64_C(0xBF58476D1CE4E5B9);
  z = (z ^ (z >> 27)) * UINT64_C(0x94D049BB133111EB);
  return z ^ (z >> 31);
}



static __uint128_t g_lehmer64_states[20];


uint64_t lehmer64_states(int s = 0) {
  g_lehmer64_states[s] *= 0xda942042e4dd58b5;
  return g_lehmer64_states[s] >> 64;
}

// https://github.com/lemire/testingRNG/blob/master/source/lehmer64.h
static inline void lehmer64_seed(uint64_t seed) {
  g_lehmer64_state = (((__uint128_t)splitmix64_stateless(seed)) << 64) +
    splitmix64_stateless(seed + 1);
}

int ensign(unsigned int x) {
  unsigned int INT_MIN64 = (unsigned)(-2147483647);
  if (x <= 2147483647) {
    return static_cast<int>(x);
  }
  if (x >= INT_MIN64) {
    return static_cast<int>(x - INT_MIN) + INT_MIN;
  }
  return INT_MIN64;
}

// [[Rcpp::export]]
IntegerVector do_lemire_rand(int n, IntegerVector S) {
  if (n <= 0 || (n % 2)) {
    stop("n must be positive and even.");
  }
  if (S.length() < 5) {
    stop("S must be longer than 5.");
  }

  if (S[0] > 0 &&
      S[1] > 0 &&
      S[2] > 0 &&
      S[3] > 0 &&
      S[4] > 0) {
    union {
        struct {
          uint32_t v1;
          uint32_t v2;
          uint32_t v3;
          uint32_t v4;
        } __attribute__((packed));
        __uint128_t i128;
      } t128;
    t128.v1 = S[1];
    t128.v2 = S[2];
    t128.v3 = S[3];
    t128.v4 = S[4];

    g_lehmer64_state = t128.i128;
  }
  if (S[0]) {
    union {
    struct {
      int v1;
      int v2;
    } __attribute__((packed));
    uint64_t ui64;
  } t64;

    t64.v1 = S[5];
    t64.v2 = S[6];
    lehmer64_seed(t64.ui64);
  }
  IntegerVector out = no_init(n);
  for (int i = 0; i < n; i += 2) {
    uint64_t L = lehmer64();
    unsigned int ux0 = L & 0xFFFFFFFF;
    unsigned int ux1 = static_cast<int32_t>((L & 0xFFFFFFFF00000000LL) >> 32);
    out[i] = ensign(ux0);
    out[i + 1] = ensign(ux1);
  }
  return out;
}

// [[Rcpp::export]]
IntegerVector do_lemire_rand_par(int n, IntegerVector S, int nThread = 1) {

  if (nThread > 20) {
    nThread = 20;
  }
  if (S.length() < 105) {
    stop("S must have length > 105.");
  }

  if (S[0] > 0 &&
      S[1] > 0 &&
      S[2] > 0 &&
      S[3] > 0 &&
      S[4] > 0) {
    for (int t = 0; t < 20; ++t) {
      union {
      struct {
        uint32_t v1;
        uint32_t v2;
        uint32_t v3;
        uint32_t v4;
      } __attribute__((packed));
      __uint128_t i128;
    } t128;

      t128.v1 = S[t * 5 + 1];
      t128.v2 = S[t * 5 + 2];
      t128.v3 = S[t * 5 + 3];
      t128.v4 = S[t * 5 + 4];

      g_lehmer64_states[t] = t128.i128;
    }
  }
  IntegerVector out = no_init(n);

#pragma omp parallel for num_threads(nThread)
  for (int i = 0; i < n; i += 2) {
    int s = omp_get_thread_num();
    uint64_t L = lehmer64_states(s);
    unsigned int ux0 = L & 0xFFFFFFFF;
    unsigned int ux1 = static_cast<int32_t>((L & 0xFFFFFFFF00000000LL) >> 32);
    out[i] = ensign(ux0);
    out[i + 1] = ensign(ux1);
  }

  return out;
}

#else


IntegerVector do_lemire_rand(int n, IntegerVector S) {
  warning("Unable.");
  return S;
}


IntegerVector do_lemire_rand_par(int n, IntegerVector S, int nThread = 1) {
  warning("Unable.");
  return S;
}


#endif


// [[Rcpp::export]]
IntegerVector cf_sample(int n, int m, IntegerVector x, IntegerVector S) {
  int slen = S.length();
  int N = x.length();
  if (N != slen) {
    stop("N != slen");
  }
  IntegerVector out = no_init(N);
  if (m) {
    for (int i = 0; i < N; ++i) {
      out[i] = S[i] > 455;
    }
  } else {
    for (int i = 0; i < N; ++i) {
      out[i] = S[((i * 13) + 17) % slen] > 455;
    }
  }
  return out;
}





