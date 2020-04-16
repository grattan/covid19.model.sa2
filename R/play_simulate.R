
# Read, join and build data ----------------------------------------------------


# Settings ---------------------------------------------------------------------
rebuild_people <- FALSE
rebuild_schools <- FALSE
rebuild_jobs <- FALSE



# Functions --------------------------------------------------------------------

library(data.table)
library(tidyverse)
library(readxl)
library(absmapsdata) # remotes::install_github("wfmackey/absmapsdata")
library(sf)
library(fst)
library(janitor)


# Read --------------------------------------------------------------------
# data sourced from put-fst.R

read_fst("data-raw/int/person_demography.fst")
