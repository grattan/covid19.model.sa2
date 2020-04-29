
library(magrittr)
library(data.table)
library(hutils)

library(covid19.model.sa2)

Epi2 <-
  CJ(incubation_mean = c(4:7),
     illness_mean = c(13, 15, 17),
     r_distribution = c("cauchy", "lnorm", "pois"),
     r_location = c(1/5, 2/5, 3/5),
     r_scale = c(1/5, 2/5),
     r_schools_location = c(1/10, 2/10, 4/10, 5/10),
     resistance_threshold = c(300, 400, 500),
     schools_open = c(TRUE, FALSE),
     only_Year12 = c(TRUE, FALSE),
     contact_tracing_days_before_test = c(-1L, 0L, 1L),
     contact_tracing_days_until_result = c(3L, 4L)) %>%

  # Only consider these variables indepe  when schools are open
  .[(r_location != r_schools_location) %implies% schools_open] %>%
  .[only_Year12 %implies% schools_open] %>%
  .[, e := .I]

EpiSims <-
  if (file.exists("data-raw/EpiSims-2020-04-29.rds")) {
    readRDS("data-raw/EpiSims-2020-04-29.rds")
  } else {
    vector(mode = "list", length = nrow(Epi2))
  }

# Avoid lapply in case the function barfs and we have to restart
for (e in seq_along(EpiSims)) {
  if (!is.null(EpiSims[[e]]) && is.data.table(EpiSims[[e]])) {
    next # already done
  }
  if ((e %% 5) == 0) cat(crayon::bgCyan(e), "\n")
  Ee <- Epi2[e]
  Policye <-
    set_policypars(schools_open = Ee$schools_open,
                   only_Year12 = Ee$only_Year12,
                   contact_tracing_days_before_test = Ee$contact_tracing_days_before_test,
                   contact_tracing_days_until_result = Ee$contact_tracing_days_until_result)
  oute <-
    simulate_sa2(50,
                 PolicyPars = Policye,
                 EpiPars = set_epipars(incubation_mean = Ee$incubation_mean,
                                       illness_mean = Ee$illness_mean,
                                       r_distribution = Ee$r_distribution,
                                       r_location = Ee$r_location,
                                       r_scale = Ee$r_scale,
                                       r_schools_location = Ee$r_schools_location,
                                       resistance_threshold = Ee$resistance_threshold),
                 use_dataEnv = TRUE,
                 nThread = 8L)

    EpiSims[[e]] <-
      oute[[2]][, .N, keyby = c(intersect(names(oute[[2]]), paste0("V", 7 * (1:7))))]

}


read_fst("data-raw/EpiSims20200428-by-e.fst", as.data = T) %>% melt.data.table(measure.vars = patterns("^V")) %>% .[, Day := as.integer(sub("^V", "", variable))] %>% .[value > 0] %>% .[, .(N = sum(N)), keyby = .(e, Day)] %>% .[Epi2, on = "e", nomatch = 0L] %>% .[r_distribution == "cauchy"] %>% .[schools_open == T]  %>%  ggplot(aes(x = Day, y = N, group = e)) + geom_line(alpha = 0.4) + scale_y_continuous(labels = comma)  + facet_grid(r_scale ~ r_location) + ggtitle("Schools open")



