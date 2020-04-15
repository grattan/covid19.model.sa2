
# Read, join and build data ----------------------------------------------------

library(data.table)
library(tidyverse)
library(readxl)
library(absmapsdata) # remotes::install_github("wfmackey/absmapsdata")
library(sf)
library(fst)
library(janitor)
library(purrr)

`%nin%` <- Negate(`%in%`)
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
  st_drop_geometry() %>%
  select(sa2_name = sa2_name_2016,
         sa2 = sa2_main_2016,
         sa3 = sa3_code_2016,
         sa4 = sa4_code_2016,
         state = state_code_2016)

write_fst(sa2_codes, "data-raw/int/sa2_codes.fst")


# Home to work (hw) ------------------------------------------------------------

hw_sa2_dzn <- read_csv("data-raw/abs/sa2-live-dzn-work.zip", skip = 9) %>%
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
         takes_primary = from <= 4,
         takes_secondary = from >= 10 | to >= 10,
         primary_n = students_n * takes_primary / (takes_primary + takes_secondary),
         secondary_n = students_n * takes_secondary / (takes_primary + takes_secondary))



write_fst(schools, "data-raw/int/schools.fst")

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

  message(paste("Getting demographics for", area))

  p <- people_raw %>% filter(sa2 == area)
  d <- demog %>% filter(sa2 == area)

  # ignore SA2s with fewer than 20 people
  if (nrow(p) < 20) {
    message(paste("\tSkipping with small sample", nrow(p)))
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

people <- map_dfr(sa2_list, apply_demographics) %>%
  select(-person) %>%
  rename(sa2_name = sa2)
# to do: should assume that people > 65 don't (or low prob) live with kids


# Schools ----------------------------------------------------------------------
# For each kid that attends school, put them in a nearby school

kids <- people %>%
  select(hid, pid, sa2_name, edu, age) %>%
  filter(edu %in% c("Primary", "Secondary"))

find_schools <- function(area) {

  message(paste("Finding schools for kids in", area))

  k <- kids %>% filter(sa2_name == area)

  kp <- k %>% filter(edu == "Primary")
  ks <- k %>% filter(edu == "Secondary")


  s <- schools %>%
    filter(sa2_name == area) %>%
    select(school_name, school_id,
           takes_primary, takes_secondary, primary_n, secondary_n)

  sp <- s %>%
    filter(takes_primary) %>%
    sample_n(nrow(kp), weight = primary_n, replace = TRUE)

  ss <- s %>%
    filter(takes_secondary) %>%
    sample_n(nrow(ks), weight = secondary_n, replace = TRUE)

  kp <- kp %>% bind_cols(sp)
  ks <- ks %>% bind_cols(ss)

  k <- kp %>% bind_rows(ks)

  return(k)

}

kids_schools <- map_dfr(unique(kids$sa2_name), find_schools)


# Where do people do post-secondary study --------------------------------------

# to do

# Where do people work ---------------------------------------------------------

# to do



# Combine and export Australia -------------------------------------------------

australia <- people %>%
  left_join(kids_schools)


write_fst(australia, "data-raw/int/people.fst", compress = 100)

