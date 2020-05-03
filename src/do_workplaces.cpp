#include "covid19model.h"
#include <Rcpp.h>

using namespace Rcpp;


// [[Rcpp::export]]
IntegerVector get_nColleagues(int nr, int N,
                              IntegerVector LabourForceStatus,
                              int nThread = 1,
                              int c_d = 0,
                              double beta = 27,
                              double mu = 2.2,
                              double sigma = 0.62) {
  // N = vector length desired, the number of individuals
  // nr intermediate estimate for number of lognormal draws
  // plausibly the number of 'workplaces'
  if (N != LabourForceStatus.length()) {
    stop("Internal error: get_nColleagues(): N != LabourForceStatus.length().");
  }

  IntegerVector r = no_init(nr);
  int n = 0;

  // possibly neater to put this inside a loop but (a) I don't think my
  // compiler is that smart and (b) 'breaks' and 'continues' make me nervous
  switch(c_d) {
  case 0:
#pragma omp parallel for num_threads(nThread) reduction(+:n)
    for (int i = 0; i < nr; ++i) {
      int ri = geomRand(1 / beta); // beta = average so 1/beta gives the 'rate'
      if (ri <= 1) {
        // partners are far more common than single-person employers
        if ((i % 4)) {
          ri = 2;
        } else {
          ri = 1;
        }
      }
      n += ri;
      r[i] = ri;
    }
    break;
  case 1:
#pragma omp parallel for num_threads(nThread) reduction(+:n)
    for (int i = 0; i < nr; ++i) {
      int ri = dbl2int(lnormRand(mu, sigma));
      n += ri;
      r[i] = ri;
    }
    break;
  default:
    stop("Internal error: unsupported c_d.");
  }

  IntegerVector out = no_init(N);
  // j index of out
  // i index of workplaces (new i => new number of colleagues)
  // k new position within same workplace

  int j = 0;
  for (int i = 0, k = 0; i < nr && j < N; ++i) {
    int ri = r[i];
    while (k < ri && j < N) {
      if (LabourForceStatus[j] != LFS_FULLTIME &&
          LabourForceStatus[j] != LFS_PARTTIME) {
        out[j] = 0;
        // don't increment k because we need to keep it for next employee
      } else {
        out[j] = ri;
        ++k;
      }
      ++j;
    }
    k = 0;
  }
  if (j < N) {
    // just select from the lognormal repeater already created
    for (int i = 0, k = 0; i < nr && j < N; ++i) {
      int ri = r[i];
      while (k < ri && j < N) {
        if (LabourForceStatus[j] != LFS_FULLTIME &&
            LabourForceStatus[j] != LFS_PARTTIME) {
          out[j] = 0;
        } else {
          out[j] = ri;
          ++k;
        }
        ++j;
      }
      k = 0;
    }
  }
  return out;
}

// [[Rcpp::export]]
List do_workplaces(List AusByDZN,
                   int nThread = 1,
                   int c_d = 0,
                   double beta = 15,
                   double mu = 2.2,
                   double sigma = 0.62) {
  // assign the number of colleagues in each DZN
  // use nColleagues until you
  IntegerVector pid = AusByDZN["pid"];
  IntegerVector DZN = AusByDZN["dzn"];
  IntegerVector NDz = AusByDZN["NDz"]; // number of persons in the dzns
  IntegerVector LabourForceStatus = AusByDZN["LabourForceStatus"];
  if (do_is_unsorted_pint(DZN, nThread)) {
    stop("Internal error: DZN should have been sorted.");
  }

  int N = pid.length();
  int nr = MWORKPLACES + 500e3; // overestimate

  if (N != LabourForceStatus.length()) {
    stop("Internal error: get_nColleagues(): N != LabourForceStatus.length().");
  }

  IntegerVector r = no_init(nr);
  int n = 0;

  // possibly neater to put this inside a loop but (a) I don't think my
  // compiler is that smart and (b) 'breaks' and 'continues' make me nervous
  switch(c_d) {
  case 0:
#pragma omp parallel for num_threads(nThread) reduction(+:n)
    for (int i = 0; i < nr; ++i) {
      int ri = geomRand(1 / beta); // beta = average so 1/beta gives the 'rate'
      if (ri <= 1) {
        // partners are far more common than single-person employers
        if ((i % 4)) {
          ri = 2;
        } else {
          ri = 1;
        }
      }
      n += ri;
      r[i] = ri;
    }
    break;
  case 1:
#pragma omp parallel for num_threads(nThread) reduction(+:n)
    for (int i = 0; i < nr; ++i) {
      int ri = dbl2int(lnormRand(mu, sigma));
      n += ri;
      r[i] = ri;
    }
    break;
  default:
    stop("Internal error: unsupported c_d.");
  }

  IntegerVector nColleagues = no_init(N);
  IntegerVector wid = no_init(N); // workplace id
  wid[0] = 1; // 1-indexed
  // j index of out
  // i index of workplaces (new i => new number of colleagues)
  // k new position within same workplace
  // z index within DZN

  int j = 0;
  int z = 0;
  for (int i = 0, k = 0; i < nr && j < N; ++i) {
    if (j == 0 || DZN[j - 1] != DZN[j]) {
      z = 0;
    }


    int dzn_remaining = NDz[j] - z;
    // take the generated number of workers, but only if enough
    // workers in the dzn remain
    int ri = minii(r[i], dzn_remaining);

    while (k < ri && j < N) {
      if (LabourForceStatus[j] != LFS_FULLTIME &&
          LabourForceStatus[j] != LFS_PARTTIME) {
        nColleagues[j] = 0;
        wid[j] = 0;
        // don't increment k because we need to keep it for next employee
      } else {
        nColleagues[j] = ri;
        wid[j] = i + 1;
        ++k;
        ++z;
      }
      ++j;
    }
    k = 0;
  }

  if (j < N) {
    // just select from the lognormal repeater already created
    for (int i = 0, k = 0; i < nr && j < N; ++i) {
      if (j == 0 || DZN[j - 1] != DZN[j]) {
        z = 0;
      }
      int dzn_remaining = NDz[j] - z;
      int ri = minii(r[i], dzn_remaining);
      while (k < ri && j < N) {
        if (LabourForceStatus[j] != LFS_FULLTIME &&
            LabourForceStatus[j] != LFS_PARTTIME) {
          nColleagues[j] = 0;
          wid[j] = 0;
        } else {
          nColleagues[j] = ri;
          wid[j] = i + nr;
          ++k;
          ++z;
        }
        ++j;
      }
      k = 0;
    }
  }
  return List::create(Named("wid") = wid, Named("nColleagues") = nColleagues);
}
