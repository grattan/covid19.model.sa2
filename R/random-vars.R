

# faster rlnorm?
dq_rnlorm <- function(n, m, sigma = 1, nThread = getOption("covid19.model.sa2_nThread", 1L)) {
  mu <- m2mu(m, sigma)
  do_exp_dbl2int(dqrng::dqrnorm(n, mu, sigma), nThread)
}

# average -> lognormal parameter
m2mu <- function(m, s = 1) {
  log(m) - s/2
}
