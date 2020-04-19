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

#define NSA2 2310

int which_unsorted_int(IntegerVector x);

int short_sa2(int sa2);


#endif
