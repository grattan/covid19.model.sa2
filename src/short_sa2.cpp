#include "covid19model.h"
#include <Rcpp.h>
using namespace Rcpp;



// [[Rcpp::export(rng = false)]]
int short_sa2(int sa2) {

  // quintrature less branching
  int q = (sa2 > 123021442) + (sa2 > 212051323) + (sa2 > 312021343) + (sa2 > 505031100);
  int start = 462 * q;
  int end = 462 * (q + 1);

  for (int i = start; i < end; ++i) {
    if (sa2s[i] == sa2) {
      return i;
    }
  }
  return -1;
}


// [[Rcpp::export(rng = false)]]
IntegerVector shorten_sa2s_ordered(IntegerVector SA2) {
  int j = 0;
  int start = short_sa2(SA2[j]);
  int n = SA2.length();
  IntegerVector out = no_init(n);
  out[0] = start;
  int i = start;
  int end = NSA2;
  while (++j < n && i < end) {
    if (sa2s[i] == SA2[j]) {
      out[j] = i;
      continue;
    }
    --j; // loop has incremented j but failed; retreat
    ++i;
  }
  if (j < n) {
    stop("Internal error (SA2 likely unsorted): j < n."); // nocov
  }
  return out;
}

// [[Rcpp::export]]
List sa2_firsts_finals(IntegerVector SA2, int nsa2 = 2310, bool is_state = false) {

  IntegerVector SA2_firsts = no_init(nsa2);
  IntegerVector SA2_finals = no_init(nsa2);
  int s = 0;
  int N = SA2.length();
  SA2_firsts[s] = 0;
  SA2_finals[nsa2 - 1] = N;

  for (int i = 1; i < N; ++i) {
    int SA2i = SA2[i];
    int SA2p = SA2[i - 1];
    if (is_state) {
      SA2i = sa2_to_state(SA2i);
      SA2p = sa2_to_state(SA2p);
    }

    int d = SA2i - SA2p;
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
    SA2_firsts[s] = N;
    SA2_finals[s - 1] = N;
  }
  return List::create(SA2_firsts, SA2_finals);
}

int sa2_to_state(int sa2) {
  return sa2 / 100000000;
}

// distances between sa2s

double sinhalfsq (double x) {
  const double o = sin(x / 2);
  return o * o;
}

double haversine_distance(double olat1, double olon1, double olat2, double olon2) {
  // double pi = 3.1415926535897;
  const double lat1 = olat1 * (M_PI / 180) ;
  const double lat2 = olat2 * (M_PI / 180) ;
  const double lon1 = olon1 * (M_PI / 180) ;
  const double lon2 = olon2 * (M_PI / 180) ;

  const double delta_lat = std::fabs(lat1 - lat2);
  const double delta_lon = std::fabs(lon1 - lon2);

  double out = 0;
  double den = cos(lat1) * cos(lat2) * sinhalfsq(delta_lon);
  out = sinhalfsq(delta_lat);
  out += den;
  out = sqrt(out);
  out = asin(out);
  out *= 6371;
  out *= 2;
  return out;
}






