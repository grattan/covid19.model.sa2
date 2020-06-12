<!-- badges: start -->
<h3 id="experimental"><img src="https://img.shields.io/badge/lifecycle-experimental-orange.svg" alt=""></h3>
  [![Codecov test coverage](https://codecov.io/gh/grattan/covid19.model.sa2/branch/master/graph/badge.svg)](https://codecov.io/gh/grattan/covid19.model.sa2?branch=master)
  <!-- badges: end -->

This model is free to use but not yet mature enough to be relied upon or cited. Please refrain from attributing this model to Grattan Institute until this advice changes.

# covid19.model.sa2

A model of the COVID-19 pandemic stratified by SA2.


## Synopsis

Basic usage:

```r
S <- simulate_sa2(days_to_simulate = 5)
```

The default return value is a list of three components:
  * `integer(days_to_simulate)` The number of people infected on each day simulated
  * A `data.table` of 21,364,885 rows, one for each individual modelled, with columns
    `V1, ... Vd` where d = `days_to_simulate` and each column is the status of each
    individual on each day simulated.
  * A vector of test results.

(Other return types are available via `returner` but these are subject to change.)

### Modelling different epidemiological assumptions

To model different epidemiological assumptions -- such as the the duration
of the incubation period, how likely transmission is in certain places, and  
the severity of cases -- supply a list of parameters to `EpiPars`. 

The function `set_epipars()` returns a list of the required parameters with
reasonable defaults. 

```r
S <- simulate_sa2(EpiPars = set_epipars())

# Assume longer average incubation, 8 days
S_long_incubation <- 
  simulate_sa2(EpiPars = set_epipars(incubation_mean = 8))

# Assume everyone in the household gets infected the following day
# if any member does
S_high_household_transmission <- 
  simulate_sa2(EpiPars = set_epipars(q_household = 1))

# Assume no-one is naturally resistant
S_no_natural_resist <-
  simulate_sa2(EpiPars = set_epipars(resistance_threshold = 1000))
```

### Policy parameters

Like `EpiPars` use `set_policypars` to supply a list of `PolicyPars`, to 
change the assumptions about policies that restrict interaction. 

```r
# Open all schools
S_schools <- 
  simulate_sa2(PolicyPars = set_policypars(schools_open = TRUE))
  
## Isolate everyone over 65
S_ages_lockdown <-
  simulate_sa2(PolicyPars = set_policypars(age_based_lockdown = 65:100))
```

### Performance

Two arguments are available to improve the performance of the model, as well 
as corresponding options.

  * `nThread` the number of threads to use during the modelling
  * `use_dataEnv` can be set to `TRUE` to avoid boilerplate reading in and 
    preparation of the base data.


# Installation

## Mac

- Install `R 4.0.0`: https://cran.r-project.org/bin/macosx/
- Enable OpenMP by installing `rtools`: https://github.com/rmacoslib/r-macos-rtools/releases/tag/v3.2.2
