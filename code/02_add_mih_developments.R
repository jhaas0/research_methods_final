#### LOAD IN AND CLEAN MIGRATE DATA ####

# Jacob & Fabian
# December 2025


library(tidyverse)
library(janitor)
library(tigris)
library(sf)

#### ---- SETUP AND LOAD DATA ---- ####

setwd("C:/Users/Jacob/Documents/Wagner/RM/research_methods_final")

#remove scientific notation
options(scipen = 999)

options(tigris_use_cache = TRUE)

nyc_all <- read_rds("data/clean/nyc_all_years.rds")

mih_developments <- read_rds("data/clean/mih_developments_clean.rds")

block_groups_ny <- block_groups(state = "NY") %>% 
  st_transform(st_crs(mih_developments))

#### --- MERGE MIH DATA WITH MiGRATE DATA BY CBG ---- ####

mih_developments_with_census_bg <- mih_developments %>% 
  st_join(block_groups_ny, 
          join = st_intersects)
  



