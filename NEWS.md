
# 0.26.0

* The historical multipolicy default eases workplace restrictions faster

# 0.25.0

* The (beta) distribution of supermarket visit frequency is now a user-visible parameter.


# 0.24.0

* bugfix (#58): Workplaces are much more likely to be sources of infection, 
  fixes a double-int roundtrip bug.

# 0.23.0

* `set_initial_by_state` and hence `simulate_sa2` now by default isolate approximately
  50\% of the cases set for the given start day.

# 0.22.0

* The execution order of infectors is now random, rather than supermarket
  infections coming first.

# 0.21.0

* Initial status can be set by state

# 0.20.0

* Support for infection via major events


# 0.19.0

* Simulation now infects other SA2s internally.


# 0.18.0

* `.first_yday` is now fussier and is passed to `set_initial_by_state` for
  retrosepctive analysis of model performance

# covid19.model.sa2 0.17.2

* Added a `NEWS.md` file to track changes to the package.
