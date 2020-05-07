library(covid19.model.sa2)

options(optionz = 0)
S <- simulate_sa2(12, PolicyPars = set_policypars(workplace_size_max = 100, workplaces_open = 1), nThread = 10, returner = 2)
cat(typeof(S), "\n")

