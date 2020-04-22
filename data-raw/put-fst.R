
# Read, join and build data ----------------------------------------------------


# Settings ---------------------------------------------------------------------
rebuild_people <- TRUE
rebuild_schools <- TRUE
rebuild_jobs <- TRUE
  rebuild_distance <- FALSE


# Functions --------------------------------------------------------------------
library(hutils)
library(data.table)
library(tidyverse)
library(readxl)
library(absmapsdata) # remotes::install_github("wfmackey/absmapsdata")
library(abscorr) # remotes::install_github("wfmackey/abscorr")
library(sf)
library(fst)
library(janitor)
library(purrr)
library(zoo)

`%nin%` <- Negate(`%in%`)
pm <- function(...) {message(paste(...))}



# Statistical area levels ------------------------------------------------------

sa2_codes <- absmapsdata::sa22016 %>%
  st_drop_geometry() %>%
  select(sa2_name = sa2_name_2016,
         sa2 = sa2_main_2016,
         sa3 = sa3_code_2016,
         sa4 = sa4_code_2016,
         state = state_code_2016)

sa2_codes %>%
  as.data.table() %>%
  write_fst("inst/extdata/sa2_codes.fst")

sa2_list <- absmapsdata::sa22016$sa2_name




# Home to work (hw) ------------------------------------------------------------

hw_sa2_dzn <- read_csv("data-raw/abs/sa2_live_dzn_work.zip", skip = 9) %>%
  rename(sa2_name = 1) %>%
  pivot_longer(-sa2_name, names_to = "work_dzn", values_to = "n") %>%
  filter(!is.na(n),
         n > 0, # drop file size
         sa2_name != "Total",
         work_dzn != "Total")

hw_sa2_dzn %>%
  as.data.table() %>%
  write_fst("inst/extdata/hw_sa2_dzn.fst")


# Households -------------------------------------------------------------------
households_raw <- read_csv("data-raw/abs/sa2-households-families-persons.zip",
                           skip = 9, col_types = "_cccd_",
                           col_names = c("persons", "families", "sa2", "n")) %>%
  filter(!is.na(n),
         persons != "Total", families != "Total")


drop_families <- c(
  "Visitors only household", # hotels etc; NA given circumstances
  "Other non-classifiable household", # assume zero; but not much info on this
  "Not applicable" # non-private houses that will be added separately
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
  left_join(rename(sa2_codes, sa2_code = sa2),
            by = c(sa2 = "sa2_name")) %>%
  arrange(state, sa4, sa3, sa2_code) %>%
  # get one observation per household
  uncount(n) %>%
  mutate(hid = row_number()) %>%
  select(hid, sa2, adult, kid)

# generate people count
people_raw <- households %>%
  pivot_longer(c(-hid, -sa2), names_to = "person", values_to = "n") %>%
  uncount(n) %>%
  mutate(pid = row_number())



# People -----------------------------------------------------------------------
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



# Put people in houses ---------------------------------------------------------

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

if (rebuild_people) {
  people <- map_dfr(sa2_list, apply_demographics) %>%
    filter(!is.na(pid)) %>%
    select(-person) %>%
    rename(sa2_name = sa2) %>%
    mutate_at(vars(sa2_name, edu, lfs, occ), ~as_factor(.)) # reduce file size

  write_fst(people, "data-raw/int/people.fst", compress = 100)
}

people <- read_fst("data-raw/int/people.fst")

# to do: should assume that people > 65 don't (or low prob) live with kids


# Add people in non-private facilities -----------------------------------------

nonprivate_sa1 <- read_csv("data-raw/abs/sa1-residential-facilities.zip",
                           skip = 9, col_types = "_ccd_",
                           col_names = c("residence", "sa1", "n")) %>%
  filter(!is.na(n),
         n > 0,
         residence %in% c("Hospital", "Prison"),
         sa1 != "Total") %>%
  mutate(sa1 = as.integer(sa1))


nonprivate_sa1 %>%
  as.data.table() %>%
  write_fst("inst/extdata/nonprivate_sa1.fst")










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
  filter(!is.na(year_range)) %>% # removes 'environmental centres' mostly
  mutate(range = year_range %>%
           str_remove_all("[A-Z], ") %>%
           str_replace_all("[a-zA-Z]{1,4}", "0")) %>%
  separate(range, c("from", "to"), "-") %>%
  mutate(to = if_else(is.na(to), from, to),
         students_n = if_else(is.na(students_n), teachers_n * 15, students_n),
         takes_primary = from <= 4,
         takes_secondary = from >= 10 | to >= 10,
         primary_n = students_n * takes_primary / (takes_primary + takes_secondary),
         secondary_n = students_n * takes_secondary / (takes_primary + takes_secondary)) %>%
  mutate(school_id = as.integer(school_id))


schools %>%
  as.data.table() %>%
  write_fst("inst/extdata/schools.fst")



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
    sp <- sp %>% sample_n(nrow(kp), weight = primary_n, replace = TRUE)

    kp <- kp %>% bind_cols(sp)
  }

  # secondary
  ss <- s %>% filter(takes_secondary)

  if (nrow(ks) > 0 & nrow(ss) == 0) pm("\tNo secondary schools found for secondary school kids")

  if (nrow(ks) > 0 & nrow(ss) > 0) {
    ss <- ss %>% sample_n(nrow(ks), weight = secondary_n, replace = TRUE)

    ks <- ks %>% bind_cols(ss)
  }

  ret <- kp %>% bind_rows(ks)

  return(ret)

}

if (rebuild_schools) {
  school_spine <- map_dfr(unique(kids$sa2_name), find_schools) %>%
    select(pid, school_id)

  school_spine %>%
    as.data.table() %>%
    write_fst("inst/extdata/school_spine.fst")
}

school_spine <- read_fst("inst/extdata/school_spine.fst") %>%
  as_tibble()



# Post-secondary study ---------------------------------------------------------

# to do





# Work -------------------------------------------------------------------------

# Where people live, and where they work
hw_sa2_dzn <- read_csv("data-raw/abs/sa2-live-dzn-work.zip", skip = 9) %>%
  rename(sa2_name = 1) %>%
  pivot_longer(-sa2_name, names_to = "work_dzn", values_to = "n") %>%
  mutate(work_dzn = as.integer(work_dzn)) %>%
  filter(!is.na(n),
         !is.na(work_dzn),
         n > 0, # drop file size
         sa2_name != "Total",
         work_dzn != "Total")

hw_sa2_dzn %>%
  as.data.table() %>%
  write_fst("inst/extdata/hw_sa2_dzn.fst")

# get new data cutting by mode of transport (coming from TableBuilder)


# Find each worker a job in a place
workers <- people %>%
  filter(lfs %in% c("Employed, worked full-time",
                    "Employed, worked part-time")) %>%
  select(pid, sa2_name)


find_work <- function(area) {


  pm("Finding jobs for people living in", area)

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

if (rebuild_jobs) {

  workers_jobs <- map_dfr(sa2_list, find_work)

  if (rebuild_distance) {
    # Get distance for each unique combination observed in data
    dzn_shape <- absmapsdata::dz2016 %>%
      mutate(work_dzn = as.integer(dz_code_2016),
             geometry = st_centroid(geometry)) %>%
      select(work_dzn)

    sa2_shape <- absmapsdata::sa22016 %>%
      mutate(geometry = st_centroid(geometry)) %>%
      select(sa2_name = sa2_name_2016)

    routes <- workers_jobs %>%
      distinct(sa2_name, work_dzn) %>%
      left_join(sa2_shape) %>%
      rename(sa2_geom = geometry) %>%
      left_join(dzn_shape) %>%
      rename(dzn_geom = geometry)

    get_distance <- function(x) {
    st_distance(routes$sa2_geom[x], routes$dzn_geom[x]) %>%
      as.numeric()
    }

    distances <- routes %>%
      # bah this takes 65 minutes
      mutate(distance_to_work = map_dbl(1:nrow(.), get_distance)) %>%
      select(-ends_with("geom"))

    distances <- distances %>%
      left_join(sa2_codes) %>%
      select(sa2, work_dzn, distance_to_work) %>%
      mutate(sa2 = as.integer(sa2))

    distances %>%
      as.data.table() %>%
    write_fst("inst/extdata/distances.fst")

  }

  distances <- read_fst("inst/extdata/distances.fst") %>%
    as_tibble()

  work_spine <- workers_jobs %>%
    select(pid, work_dzn)

  work_spine %>%
    as.data.table() %>%
    write_fst("inst/extdata/work_spine.fst", compress = 100)

}

work_spine <- read_fst("inst/extdata/work_spine.fst") %>%
  as_tibble()




# Combine and export Australia -------------------------------------------------

australia_spine <- people %>%
  left_join(sa2_codes) %>%
  select(state, sa2, hid, pid) %>%
  left_join(school_spine) %>%
  left_join(work_spine) %>%
  mutate(across(is.character, as.integer))

australia_spine %>%
  as.data.table() %>%
  setkey(state, sa2, hid, pid) %>%
  write_fst("inst/extdata/australia.fst", compress = 100)


person_demography <- people %>%
  select(hid, pid, age, edu, lfs)

person_demography %>%
  as.data.table() %>%
  setkey(hid, pid) %>%
  write_fst("inst/extdata/person_demography.fst", compress = 100)


house <- people %>%
  left_join(sa2_codes) %>%
  distinct(state, sa3, sa4, sa2, sa2_name, hid)

house %>%
  as.data.table() %>%
  setkey(hid) %>%
  write_fst("inst/extdata/house.fst", compress = 100)




# Census occupation and industry counts + inflation to 2019 with LFS ------------

# LFS
download_lfs <- FALSE

lfs_url <- "https://www.abs.gov.au/AUSSTATS/subscriber.nsf/log?openagent&eq08.xlsx&6291.0.55.003&Data%20Cubes&E60C5BD0531CEFF9CA2584D6000F5A4A&0&Nov%202019&23.12.2019&Latest"
lfs_path <- "data-raw/abs/lfs_eq08.xlsx"

if (download_lfs) download.file(lfs_url, lfs_path)

lfs_raw <- read_excel(lfs_path, sheet = "Data 1", skip = 3)

lfs <- lfs_raw %>%
  select(date = 1,
         sex = 2,
         state = 3,
         occ = 4,
         employed_000 = 5) %>%
  mutate(year = year(date),
         quarter = quarter(date),
         occ_code = parse_number(occ) %>% as.character(),
         occ = substring(occ, 6) %>%
           str_replace(" nfd", ", nfd") %>%
           str_replace("\\\\", "/") %>%
           str_replace("  ", " ")) %>%
  arrange(occ, occ_code, year, quarter) %>%
  group_by(occ, occ_code, year, quarter) %>%
  summarise(employed = 1000 * sum(employed_000, na.rm = TRUE)) %>%
  # 4q rolling
  group_by(occ) %>%
  arrange(year, quarter) %>%
  mutate(employed4q = rollmean(employed, 4, align = "right", fill = 0)) %>%
  select(-employed)


lfs_inflate <- lfs %>%
  filter(year == 2019 & quarter == 4) %>%
  select(occ, n2019 = employed4q)


# Read Census 6 digit occupation X 4 digit industry
occ_ind_raw <- read_csv("data-raw/abs/occ4_ind4.zip", skip = 9,
                    col_types = "__ccd_",
                    col_names = c("occ", "ind", "n2016")) %>%
  drop_na(n2016) %>%
  filter(across(c(occ, ind), ~ . %nin% c("Total", "Not applicable")))


# Inflate on two levels:
# to distribute `, nfd` occupations among subgroups

occ_inflate <- occ_ind_raw %>%
  group_by(occ) %>%
  summarise(n2016 = sum(n2016)) %>%
  left_join(lfs_inflate) %>%
  mutate(n2019 = if_else(is.na(n2019), n2016, n2019),
         scale2019 = n2019 / n2016) %>%
  select(occ, scale2019)

join_anzsco <- abscorr::anzsco %>%
  select(occ = anzsco4, occ_code = anzsco4_code) %>%
  mutate(occ_code = as.integer(occ_code)) %>%
  group_by(occ) %>%
  summarise(occ_code = min(occ_code))

join_anzsic <- abscorr::anzsic %>%
  select(ind = anzsic_class_title, ind_code = anzsic_class_code) %>%
  group_by(ind) %>%
  summarise(ind_code = min(ind_code))


occ_ind <- occ_ind_raw %>%
  left_join(occ_inflate) %>%
  left_join(join_anzsco) %>%
  left_join(join_anzsic) %>%
  mutate(n2019 = round(n2016 * scale2019)) %>%
  select(occ = occ_code, ind = ind_code, n2016, n2019) %>%
  group_by(occ, ind) %>%
  summarise(n2016 = sum(n2016),
            n2019 = sum(n2019)) %>%
  mutate(across(is.double, as.integer))

occ_ind %>%
  as.data.table() %>%
  write_fst("inst/extdata/occ_ind.fst")





## Maybe it should be a submodule?
try({
  # Not fst because it's small and insertions are easier on git and useful to see
  fread("https://github.com/pappubahry/AU_COVID19/raw/master/time_series_cases.csv") %T>%
    fwrite(provide.file("data-raw/pappubahry/AU_COVID19/time_series_cases.csv")) %>%
    .[, Date := as.Date(Date)] %>%
    setkey(Date) %>%
    write_fst("inst/extdata/time_series_cases.fst", compress = 100)

  fread("https://github.com/pappubahry/AU_COVID19/raw/master/time_series_recovered.csv") %T>%
    fwrite(provide.file("data-raw/pappubahry/AU_COVID19/time_series_recovered.csv")) %>%
    .[, Date := as.Date(Date)] %>%
    setkey(Date) %>%
    write_fst("inst/extdata/time_series_recovered.fst", compress = 100)

  fread("https://github.com/pappubahry/AU_COVID19/raw/master/time_series_deaths.csv") %T>%
    fwrite(provide.file("data-raw/pappubahry/AU_COVID19/time_series_deaths.csv")) %>%
    .[, Date := as.Date(Date)] %>%
    setkey(Date) %>%
    write_fst("inst/extdata/time_series_deaths.fst", compress = 100)
})

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
  .[read_fst("inst/extdata/sa2_codes.fst")[, sa2 := as.integer(sa2)], on = "sa2"] %>%
  .[, .(nSupermarkets = uniqueN(place_id, na.rm = TRUE)), keyby = .(sa2)] %T>%
  write_fst(provide.file("data-raw/google/tmp/nSupermarkets_by_sa2.fst")) %>%
  .[]


move_fst_to_inst <- function() {
  sapply(dir(path = "data-raw/int", pattern = "\\.fst$", full.names = TRUE),
         function(file.fst) {
           file.rename(file.fst,
                       provide.file(paste0("inst/extdata/", basename(file.fst))))
         })
}
move_fst_to_inst()




