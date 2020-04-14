
# Read, join and build data ----------------------------------------------------

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
  left_join(schools_location)


write_fst(schools, "data-raw/int/schools.fst")
