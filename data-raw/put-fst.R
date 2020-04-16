
# Read, join and build data ----------------------------------------------------


# Settings ---------------------------------------------------------------------
rebuild_people <- FALSE
rebuild_schools <- FALSE
rebuild_jobs <- FALSE



# Functions --------------------------------------------------------------------
library(hutils)
library(data.table)
library(tidyverse)
library(readxl)
library(absmapsdata) # remotes::install_github("wfmackey/absmapsdata")
library(sf)
library(fst)
library(janitor)


# Statistical area levels ------------------------------------------------------

sa1_codes <- absmapsdata::sa12016 %>%
  st_drop_geometry() %>%
  select(sa1 = sa1_7dig_2016,
         sa2_name = sa2_name_2016,
         sa2 = sa2_main_2016,
         sa3 = sa3_code_2016,
         sa4 = sa4_code_2016,
         state = state_code_2016)

write_fst(sa1_codes, "data-raw/int/sa1_codes.fst")



sa2_codes <- absmapsdata::sa22016 %>%
library(purrr)

`%nin%` <- Negate(`%in%`)
pm <- function(...) {message(paste(...))}




# Statistical area levels ------------------------------------------------------

sa2_codes <- sa22016 %>%
  st_drop_geometry() %>%
  select(sa2_name = sa2_name_2016,
         sa2 = sa2_main_2016,
         sa3 = sa3_code_2016,
         sa4 = sa4_code_2016,
         state = state_code_2016)

write_fst(sa2_codes, "data-raw/int/sa2_codes.fst")


# Home to work (hw) ------------------------------------------------------------

hw_sa2_dzn <- read_csv("data-raw/abs/sa2_live_dzn_work.zip", skip = 9) %>%
  rename(sa2_name = 1) %>%
  pivot_longer(-sa2_name, names_to = "work_dzn", values_to = "n") %>%
  filter(!is.na(n),
         n > 0, # drop file size
         sa2_name != "Total",
         work_dzn != "Total")

write_fst(hw_sa2_dzn, "data-raw/int/hw_sa2_dzn.fst")


hw_sa2_sa2 <- read_csv("data-raw/abs/sa2-sa2-work.zip", skip = 9,
                       col_types = "_ccd_",
                       col_names = c("sa2_name", "work_sa2_name", "n")) %>%
  filter(!is.na(n),
         n > 0, # drop file size
         sa2_name != "Total",
         work_sa2_name != "Total")

write_fst(hw_sa2_sa2, "data-raw/int/hw_sa2_sa2.fst")


# Non-private facilities -------------------------------------------------------

nonprivate_sa1 <- read_csv("data-raw/abs/sa1-residential-facilities.zip",
                           skip = 9, col_types = "_ccd_",
                           col_names = c("residence", "sa1", "n")) %>%
  filter(!is.na(n),
         residence != "Total",
         sa1 != "Total")

write_fst(nonprivate_sa1, "data-raw/int/nonprivate_sa1.fst")

# Schools ----------------------------------------------------------------------


schools_vars <- read_excel("data-raw/acara/school-profile.xlsx")


# Households -------------------------------------------------------------------
households_raw <- read_csv("data-raw/abs/sa2-households-families-persons.zip",
                           skip = 9, col_types = "_cccd_",
                           col_names = c("persons", "families", "sa2", "n")) %>%
  filter(!is.na(n),
         persons != "Total", families != "Total")


drop_families <- c(
  "Visitors only household", # hotels etc; NA given circumstances
  "Other non-classifiable household", # assume zero; but not much info on this
  "Not applicable"
)


households <- households_raw %>%
  mutate(
    persons = case_when(
      # make persons numeric
      persons == "Not applicable" ~ NA_real_,
      str_starts(persons, "One") ~ 1,
      str_starts(persons, "Two") ~ 2,
      str_starts(persons, "Three") ~ 3,
      str_starts(persons, "Four") ~ 4,
      str_starts(persons, "Five") ~ 5,
      str_starts(persons, "Six") ~ 6,
      str_starts(persons, "Seven") ~ 7,
      str_starts(persons, "Eight") ~ 8)) %>%  # assume 8+ is 8
  # drop ignored family groups
  filter(families %nin% drop_families) %>%
  # generate adult and child counts for each household
  mutate(
    family_count = case_when(
      str_starts(families, "Two") ~ 2,
      str_starts(families, "Three") ~ 3,
      TRUE ~ 1),
    couple1 = str_detect(families, "Couple family"),
    adult = case_when(
      families == "Lone person household" ~ 1,
      families == "Group household" ~ persons,
      family_count == 1 ~ 1 + couple1, # 1 for singles; 2 for couples
      family_count >  1 ~ 1 + couple1 + family_count),
    kid = persons - adult) %>%
  # get one observation per household
  uncount(n) %>%
  mutate(hid = row_number()) %>%
  select(hid, sa2, adult, kid)

# generate people count
people_raw <- households %>%
  pivot_longer(c(-hid, -sa2), names_to = "person", values_to = "n") %>%
  uncount(n) %>%
  mutate(pid = row_number())



# People demographics ----------------------------------------------------------
demog_raw <- read_csv("data-raw/abs/sa2-age-lfs-occ-edu.zip", skip = 9,
                      col_types = "_cccccd_",
                      col_names = c("edu", "lfs", "occ", "sa2", "age", "n")) %>%
  filter(!is.na(n)) %>%
  filter_all(all_vars(. != "Total"))

demog <- demog_raw %>%
  uncount(n) %>%
  mutate(edu = case_when(
    str_detect(edu, "University") ~ "University",
    str_detect(edu, "TAFE") ~ "TAFE",
    TRUE ~ edu),
    # from demography; define kids as under 20 (to be re-examined)
    post_sec = edu %in% c("University", "TAFE"),
    under20 = age %in% c("0-9 years", "10-19 years"),
    in20s = age %in% c("20-29 years"),
    person = if_else(
      under20 |
        ((in20s *  post_sec * runif(n())) > 0.8) |
        ((in20s * !post_sec * runif(n())) > 0.2),
      "kid", "adult")) %>%
  select(-post_sec, -under20, -in20s) %>%
  # make age numeric
  mutate(age = case_when(
    str_detect(age, "100 years") ~ 100,
    TRUE ~ (as.numeric(substr(age, 1, 1)) * 10 + runif(n(), 0, 9))),
    age = as.integer(age))



# Combine people demographics with household/person list -----------------------

# For each set of households in an SA2

apply_demographics <- function(area) {

  pm("Getting demographics for", area)

  p <- people_raw %>% filter(sa2 == area)
  d <- demog %>% filter(sa2 == area)

  # ignore SA2s with fewer than 20 people
  if (nrow(p) < 20) {
    pm("\tSkipping with small sample", nrow(p))
    return(p %>% select(hid, sa2))
  }

  # for each adult in houses, get demographics with sampling
  p_adults <- p %>% filter(person == "adult")

  d_adults <- d %>%
    filter(person == "adult") %>%
    select(-person, -sa2) %>%
    sample_n(nrow(p_adults), replace = TRUE)

  adults <- p_adults %>%
    bind_cols(d_adults)

  # for each kid in houses, get demographics with sampling
  p_kids <- p %>% filter(person == "kid")
  d_kids <- d %>%
    filter(person == "kid") %>%
    select(-person, -sa2) %>%
    sample_n(nrow(p_kids), replace = TRUE)

  kids <- p_kids %>%
    bind_cols(d_kids)

  ret <- adults %>%
    bind_rows(kids)

  return(ret)
}

sa2_list <- unique(sa2_codes$sa2_name)

if (rebuild_people) {
  people <- map_dfr(sa2_list, apply_demographics) %>%
    filter(!is.na(pid)) %>%
    select(-person) %>%
    rename(sa2_name = sa2)

  write_fst(people, "data-raw/int/people.fst", compress = 100)
}

# to do: should assume that people > 65 don't (or low prob) live with kids

people <- read_fst("data-raw/int/people.fst")


# Schools ----------------------------------------------------------------------

# Schools information

schools_profile <- read_excel("data-raw/acara/school-profile.xlsx",
                              sheet = 2) %>%
  clean_names() %>%
  select(school_name,
         school_id = acara_sml_id,
         state,
         postcode,
         sector = school_sector,
         governing_body,
         year_range,
         rmeoteness_area = geolocation,
         icsea_pc = icsea_percentile,
         teachers_n = teaching_staff,
         teachers_fte = full_time_equivalent_teaching_staff,
         non_teachers_n = non_teaching_staff,
         non_teachers_fte = full_time_equivalent_non_teaching_staff,
         students_n = total_enrolments,
         stdents_fte = full_time_equivalent_enrolments)

schools_location <- read_excel("data-raw/acara/school-locations.xlsx",
           sheet = 2) %>%
  clean_names() %>%
  select(school_name,
         school_id = acara_sml_id,
         lat = latitude,
         lon = longitude,
         sa1 = statistical_area_1,
         sa2 = statistical_area_2,
         sa2_name = name_of_statistical_area_2,
         sa3 = statistical_area_3,
         sa4 = statistical_area_4)

schools <- schools_profile %>%
  left_join(schools_location) %>%
  # do schools take primary/secondary?
  mutate(range = year_range %>%
           str_remove_all("[A-Z], ") %>%
           str_replace_all("[a-zA-Z]{1,4}", "0")) %>%
  separate(range, c("from", "to"), "-") %>%
  mutate(to = if_else(is.na(to), from, to),
         students_n = if_else(is.na(students_n), teachers_n * 15, students_n),
         takes_primary = from <= 4,
         takes_secondary = from >= 10 | to >= 10,
         primary_n = students_n * takes_primary / (takes_primary + takes_secondary),
         secondary_n = students_n * takes_secondary / (takes_primary + takes_secondary))


write_fst(schools, "data-raw/int/schools.fst")



# For each kid that attends school, put them in a nearby school
kids <- people %>%
  select(hid, pid, sa2_name, edu, age) %>%
  filter(edu %in% c("Primary", "Secondary"))

find_schools <- function(area) {


  pm("Finding schools for kids in", area)

  k <- kids %>% filter(sa2_name == area)

  if (nrow(k) == 0) {
    pm("\tNo school kids; skipping.")
    return(k)
  }

  kp <- k %>% filter(edu == "Primary")
  ks <- k %>% filter(edu == "Secondary")


  s <- schools %>%
    filter(sa2_name == area) %>%
    select(school_name, school_id,
           takes_primary, takes_secondary, primary_n, secondary_n)

  if (nrow(s) == 0) {
    pm("\tNo schools found")
    return(k)
  }

  # primary
  sp <- s %>% filter(takes_primary)

  if (nrow(kp) > 0 & nrow(sp) == 0) pm("\tNo primary schools found for primary school kids")

  if (nrow(kp) > 0 & nrow(sp) > 0) {
    sp <- sp %>%
      sample_n(nrow(kp), weight = primary_n, replace = TRUE)

    kp <- kp %>% bind_cols(sp)
  }

  # secondary
  ss <- s %>% filter(takes_secondary)

  if (nrow(ks) > 0 & nrow(ss) == 0) pm("\tNo secondary schools found for secondary school kids")

  if (nrow(ks) > 0 & nrow(ss) > 0) {
    ss <- ss %>%
      sample_n(nrow(ks), weight = secondary_n, replace = TRUE)

    ks <- ks %>% bind_cols(ss)
  }

  ret <- kp %>% bind_rows(ks)

  return(ret)

}

if (rebuild_schools) {
  school_spine <- map_dfr(unique(kids$sa2_name), find_schools) %>%
    select(pid, school_id)

  write_fst(school_spine, "data-raw/int/school_spine.fst")
}

school_spine <- read_fst("data-raw/int/school_spine.fst")

# Post-secondary study --------------------------------------

# to do


# Work -------------------------------------------------------------------------

# Home to work (hw) ------------------------------------------------------------

# this table is wide for some reason
hw_sa2_dzn <- read_csv("data-raw/abs/sa2-live-dzn-work.zip", skip = 9) %>%
  rename(sa2_name = 1) %>%
  pivot_longer(-sa2_name, names_to = "work_dzn", values_to = "n") %>%
  mutate(work_dzn = as.integer(work_dzn)) %>%
  filter(!is.na(n),
         !is.na(work_dzn),
         n > 0, # drop file size
         sa2_name != "Total",
         work_dzn != "Total")

write_fst(hw_sa2_dzn, "data-raw/int/hw_sa2_dzn.fst")

# get new data cutting by FT/PT work (excl on leave)


workers <- people %>%
  filter(lfs %in% c("Employed, worked full-time",
                    "Employed, worked part-time")) %>%
  select(pid, sa2_name)


find_work <- function(area) {


  pm("Finding job locations for workers in", area)

  w <- workers %>% filter(sa2_name == area)

  if (nrow(w) == 0) {
    pm("\tNo workers; skipping.")
    return(w)
  }

  j <- hw_sa2_dzn %>%
    filter(sa2_name == area)

  if (nrow(j) == 0) {
    pm("\tNo jobs found")
    return(w)
  }

  j <- j %>%
    sample_n(nrow(w), weight = n, replace = TRUE) %>%
    select(-sa2_name, -n)


  ret <- w %>% bind_cols(j)

  return(ret)

}

workers_jobs <- map_dfr(sa2_list, find_work)
work_spine <- workers_jobs %>% select(pid, work_dzn)

# Get distance for each unique combination
# to do








# Combine and export Australia -------------------------------------------------

australia_spine <- people %>%
  select(hid, pid) %>%
  left_join(school_spine) %>%
  left_join(work_spine)


write_fst(australia_spine, "data-raw/int/australia.fst", compress = 100)


person_demography <- people %>%
  select(hid, pid, age, edu, lfs) %>%
  mutate(edu = if_else(edu == "Not attending", NA_character_, edu) %>%
           as_factor(),
         lfs = if_else(lfs == "Not working", NA_character_, lfs) %>%
           as_factor())

write_fst(person_demography, "data-raw/int/person_demography.fst", compress = 100)


house <- people %>%
  select(hid, sa2_name) %>%
  distinct()


write_fst(house, "data-raw/int/house.fst", compress = 100)


# // data-raw/google/sa2_by_place_id.fst
if (!requireNamespace("ASGS", quietly = TRUE)) {
  message("ASGS not installed, skipping. Consider\n\t",
          'install.packages("ASGS.foyer")')
} else {
  read_fst("data-raw/google/TypeInt_by_place_id.fst",
           as.data.table = TRUE) %>%
    merge(fread("data-raw/latlon-by-placeid.csv", key = "place_id"),
          by = "place_id") %>%
    unique(by = "place_id") %>%
    .[complete.cases(lat),
      sa2_name := ASGS::latlon2SA(lat, lon, to = "SA2", return = "v")] %>%
    .[ASGS::SA2016_decoder, sa2 := i.SA2_MAIN16, on = "sa2_name==SA2_NAME16"] %>%
    .[, .(place_id, sa2)] %>%
    setkey(place_id) %T>%
    write_fst("data-raw/google/sa2_by_place_id.fst", compress = 100) %>%
    .[]

  ASGS::SA2016_decoder[, .(SA2_MAIN16, SA2_5DIG16)] %>%
    write_fst("data-raw/SA2_MAIN16-vsSA2_5DIG16.fst")
}



# Temporary for proof-of-concept with supermarkets
read_fst("data-raw/google/TypeInt_by_place_id.fst") %>%
  .[read_fst("data-raw/google/sa2_by_place_id.fst")] %>%
  .[read_fst("data-raw/google/Type_by_TypeInt.fst"), on = "TypeInt"] %>%
  .[TypeInt %in% c(43L, 98L)] %>%
  .[complete.cases(sa2)] %>%
  setkey(place_id) %>%
  .[, c("N", "seqN") := list(.N, seq_len(.N)), keyby = .(sa2)] %>%
  .[] %T>%
  write_fst(provide.file("data-raw/google/tmp/seqN-sa2--supermarket.fst")) %>%
  # right join so that SA2s that have no supermarkets appear (as zero)
  # instead of being absent
  .[read_fst("data-raw/int/sa2_codes.fst")[, sa2 := as.integer(sa2)], on = "sa2"] %>%
  .[, .(nSupermarkets = uniqueN(place_id, na.rm = TRUE)), keyby = .(sa2)] %T>%
  write_fst(provide.file("data-raw/google/tmp/nSupermarkets_by_sa2.fst")) %>%
  .[]


