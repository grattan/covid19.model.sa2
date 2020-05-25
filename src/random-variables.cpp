#include "covid19model.h"
#include <random>
#include <Rcpp.h>
#include <Rcpp/Benchmark/Timer.h>
#include <stdint.h>
#include <dqrng.h>
using namespace Rcpp;

double m2mu(double m, double s) {
  return log(m) - s/2;
}

// thread-safe random variables
// but not inheriting from R's RNG (for better or worse)
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



#if INTPTR_MAX == INT64_MAX

bool is64bit = true;

__uint128_t g_lehmer64_state;

uint64_t lehmer64() {
  g_lehmer64_state *= 0xda942042e4dd58b5;
  return g_lehmer64_state >> 64;
}

int lehmer32() {
  uint64_t L = lehmer64();
  return ensign(L & 0xFFFFFFFF);
}

#else
bool is64bit = false;

int lehmer32() {
  return dqsample_int2(INT_MAX, 1)[0];
}

int g_lehmer64_state = 353;

std::vector<unsigned char> do_lemire_char_par(int n, double p,  int nThread, bool return_char = false) {
  warning("do_lemire_char_par not available for 32-bit R.");
  std::vector<unsigned char> out = {};
  return out;
}

LogicalVector lemire_char(int n, double p,  int nThread, int m) {
  warning("lemire_char not available for 32-bit R.");
  LogicalVector out(n);
  return out;
}

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



static __uint128_t g_lehmer64_states[20] =
  {2136543473, 8362623451, 1217309610, 1392472685,
   5806475900, 7417877733, 8442120624, 1676887235,
   1596567492, 1740086218, 1665692500, 1245331274,
   1636032902, 1252017648, 1040518150, 1791876209,
   1302144241, 4878760597, 996898307, 1791876253};


uint64_t lehmer64_states(int s = 0) {
  g_lehmer64_states[s] *= 0xda942042e4dd58b5;
  return g_lehmer64_states[s] >> 64;
}

// https://github.com/lemire/testingRNG/blob/master/source/lehmer64.h
static inline void lehmer64_seed(uint64_t seed) {
  g_lehmer64_state = (((__uint128_t)splitmix64_stateless(seed)) << 64) +
    splitmix64_stateless(seed + 1);
}

static inline void lehmer64_seeds(uint64_t seed) {
  for (int t = 0; t < 20; ++t) {
    g_lehmer64_states[t] = (((__uint128_t)splitmix64_stateless(seed)) << 64) +
      splitmix64_stateless(seed + 1);
  }
}

void update_seed(uint64_t s64) {
  lehmer64_seed(s64);
  lehmer64_seeds(s64);
  s64 += 1;
}


// [[Rcpp::export]]
IntegerVector do_lemire_rand(int n) {
  if (n <= 0 || (n % 2)) {
    stop("n must be positive and even.");
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
IntegerVector do_lemire_rand_par(int n,
                                 int nThread = 1) {

  nThread = (nThread > 20) ? 20 : nThread;



  IntegerVector out = no_init(n);

#pragma omp parallel for num_threads(nThread)
  for (int i = 0; i < n; i += 2) {
    int s = 0;
#ifdef _OPENMP
    s = omp_get_thread_num();
#endif
    uint64_t L = lehmer64_states(s);
    unsigned int ux0 = L & 0xFFFFFFFF;
    unsigned int ux1 = static_cast<int32_t>((L & 0xFFFFFFFF00000000LL) >> 32);
    out[i] = ensign(ux0);
    out[i + 1] = ensign(ux1);
  }

  return out;
}

std::vector<unsigned char> do_lemire_char_par(int nn,
                                              double p,
                                              int nThread = 1,
                                              bool return_char = false) {
  if (p > 1 || p < 0) {
    stop("Internal error p must be in [0, 1]");
  }
  int pint = p * 255;
  unsigned char threshold = 0;
  if (p == 1) {
    threshold = 255;
  } else if (p > 0) {
    threshold = static_cast<unsigned char>(pint);
  }

  if (nn < 8) {
    std::vector<char> early_out;
    early_out.reserve(8);
    for (int i = 0; i < nn; ++i) {
      // not exact but only 8 elements so...
      early_out.push_back((lehmer32() & 255) < threshold);
    }
  }

  int n = nn;
  while (n & 7) {
    n += 1;
  }

  nThread = (nThread > 20) ? 20 : nThread;

  // Hugh: this is trivial performance
  std::vector<unsigned char> out;
  out.reserve(n);
  std::fill(out.begin(), out.end(), 0);

#pragma omp parallel for num_threads(nThread)
  for (int i = 7; i < n; i += 8) {
    int s = 0;
#ifdef _OPENMP
    s = omp_get_thread_num();
#endif
    uint64_t L = lehmer64_states(s);
    unsigned int ux0 = L & 0xFFFFFFFF;
    unsigned int ux1 = static_cast<int32_t>((L & 0xFFFFFFFF00000000LL) >> 32);
    unsigned char bytes[8] = {};
    bytes[0] = static_cast<unsigned char>((ux0 >> 24) & 0xFF);
    bytes[1] = static_cast<unsigned char>((ux0 >> 16) & 0xFF);
    bytes[2] = static_cast<unsigned char>((ux0 >> 8) & 0xFF);
    bytes[3] = static_cast<unsigned char>((ux0 & 0xFF));
    bytes[4] = static_cast<unsigned char>((ux1 >> 24) & 0xFF);
    bytes[5] = static_cast<unsigned char>((ux1 >> 16) & 0xFF);
    bytes[6] = static_cast<unsigned char>((ux1 >> 8) & 0xFF);
    bytes[7] = static_cast<unsigned char>((ux1 & 0xFF));
    if (return_char) {
#pragma omp simd
      for (int b = 0; b < 8; ++b) {
        out[i - b] = bytes[b];
      }
    } else {
#pragma omp simd
      for (int b = 0; b < 8; ++b) {
        out[i - b] = (bytes[b] < threshold) ? 1 : 0;
      }
    }
  }

  return out;
}

// [[Rcpp::export]]
LogicalVector lemire_char(int N, double p, int return_early = 0,
                          int nThread = 1) {
  std::vector<unsigned char> the_lemire_char = do_lemire_char_par(N, p, nThread, false);

  if (return_early) {
    LogicalVector out(1);
    out[0] = the_lemire_char[0] != 0;
    return out;
  }
  LogicalVector out = no_init(N);
  for (int i = 0; i < N; ++i) {
    out[i] = the_lemire_char[i] > 0;
  }
  return out;
}

#else

void update_seed(int s64) {
  ++s64;
}

IntegerVector do_lemire_rand(int n) {
  warning("Unable.");
  return IntegerVector(1);
}


IntegerVector do_lemire_rand_par(int n,  int nThread = 1) {
  warning("Unable.");
  return IntegerVector(1);
}


#endif


// [[Rcpp::export]]
IntegerVector cf_sample(int n, int m, IntegerVector S, IntegerVector x) {
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

// [[Rcpp::export]]
int cf_mod_lemire(int n, double p, int m = 0, int nThread = 1) {
  int out = 0;
  if (m) {
    std::vector<unsigned char> Srand1 = do_lemire_char_par(n, p, nThread, false);
    std::vector<unsigned char> Srand2 = do_lemire_char_par(n, p, nThread, false);
    for (int i = 0; i < n; ++i) {
      if (Srand1[i] || Srand2[i]) {
        ++out;
      }
    }
  } else {
    IntegerVector TodaysS = dqsample_int2(1000, NTODAY);
    int P1000 = 1000 * p;
    for (int i = 0; i < n; ++i) {
      if (TodaysS[(i * 3 + 13) % NTODAY] < P1000 ||
          TodaysS[(i * 11 + 7) % NTODAY] < P1000) {
        ++out;
      }
    }
  }

  return out;
}

inline double scale2radius(int x, double r) {
  // from [INT_MIN, INT_MAX] -> [-r, r]
  double xr = x * r;
  double d = (double)(INT_MAX);
  return xr / d;
}

// [[Rcpp::export]]
IntegerVector RCauchy(IntegerVector U, double location, double scale, int nThread = 1) {
  // U has domain [-INT_MIN, INT_MAX]
  // output -
  int N = U.length();


  IntegerVector out = no_init(N);
#pragma omp parallel for num_threads(nThread)
  for (int i = 0; i < N; ++i ) {
    double theta = scale2radius(U[i] / 2, M_PI);
    double oi = scale * std::tan(theta) + location;
    if (oi > INT_MIN && oi < INT_MAX) {
      out[i] = (int)oi;
    } else {
      out[i] = (oi <= INT_MIN) ? INT_MIN : INT_MAX;
    }
  }
  return out;

}

inline double u2exp(uint64_t x, double k) {
  if (x == 0) return 0;
  double antilog = ((double)x) / (double(UINT64_MAX));
  double logarithm = std::log(antilog);
  return (-1 / k) * logarithm;
}


std::vector<double> Rexp(int N, double k, int nThread) {
  nThread = (nThread > 20) ? 20 : nThread;

  std::vector<double> out;
  out.reserve(N);
  std::fill(out.begin(), out.end(), 0);

#pragma omp parallel for num_threads(nThread)
  for (int i = 0; i < N; i += 1) {
    int s = 0;
#ifdef _OPENMP
    s = omp_get_thread_num();
#endif
    uint64_t ux0 = lehmer64_states(s);
    double rx0 = u2exp(ux0, k);
    out[i] = rx0;
  }

  return out;
}



// [[Rcpp::export]]
IntegerVector updateLemireSeedFromR(IntegerVector S) {
  if (S.length() <= 42) {
    stop("S.length() <= 42.");
  }
  // Seed lemire rng
  uint64_t s64 = 0;
  for (int t = 0; t < 20; ++t) {
    s64 += S[t];
    s64 <<= 32;
    s64 += S[t + 1];
    s64 <<= 32;
  }
  update_seed(s64);

  return S;
}


