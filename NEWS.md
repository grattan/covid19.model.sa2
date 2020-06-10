# 0.28.0

* Age-based lockdowns work for ages up to 100
* Default "historical" policy parameters are cumulative, rather than only including 
  the policies that recently changed.  Multipolicies which start before the first yday
  are now handled correctly (rather than waiting for the first multipolicy 
  which never arrives)
* Argument `myaus` is now used again after being silently dropped
* Fixed an error where a length-one `multipolicy' (i.e. a single change) was ignored 


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
