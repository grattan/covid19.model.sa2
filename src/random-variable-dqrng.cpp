
// Not actually faster than just non-parallel versions

/*
#include <Rcpp.h>
// [[Rcpp::depends(dqrng, BH, sitmo)]]
#include <xoshiro.h>
#include <dqrng_distribution.h>
// [[Rcpp::plugins(cpp11)]]
// [[Rcpp::plugins(openmp)]]
#include <omp.h>

// [[Rcpp::export]]
Rcpp::DoubleVector parallel_random_sum(int n, int m, int ncores, int s) {
  dqrng::uniform_distribution dist(0, 1); // Uniform distribution [0,1)
  dqrng::xoshiro256plus rng(s);              // properly seeded rng
  std::vector<double> res(m); // ok to use rng here

#pragma omp parallel num_threads(ncores)
{
  dqrng::xoshiro256plus lrng(rng);      // make thread local copy of rng
  lrng.long_jump(omp_get_thread_num() + 1);  // advance rng by 1 ... ncores jumps

#pragma omp for
  for (int i = 0; i < m; ++i) {
    res[i] = dist(lrng);
  }
}
// ok to use rng here
return Rcpp::wrap(res);
}

*/
