#### LOAD IN AND CLEAN MIGRATE DATA ####

# Jacob & Fabian
# December 2025


library(tidyverse)
library(janitor)
library(tigris)
library(tidycensus)
library(sf)


#### ---- SETUP ---- ####

setwd("C:/Users/Jacob/Documents/Wagner/RM/research_methods_final")

#remove scientific notation
options(scipen = 999)

options(tigris_use_cache = TRUE)

years <- 2011:2019

nyc_all <- map_dfr(
  years,
  ~ read_rds(paste0("data/clean/", .x, "_nyc.rds")) %>%
    mutate(year = .x)
)

attach_acs <- function(household_df, fips, year = 2023) {
  
  acs_vars <- c(
    median_rent = "B25064_001",
    median_income = "B19013_001",
    poverty_rate = "",
    rental_units = "B25003_003", 
    occupied_units = "B25003_001", 
    # total_units = "B25002_001",
    # total_vac_units = "B25002_003",  
    vac_rate = "DP04_0005",
    renters_from_last_yr = "B07013_006", 
    total_renters_last_yr = "B07013_003", 
    race.latin = "B03002_012",
    race.total = "B03002_001",
    race.white = "B03002_003",
    race.black = "B03002_004",
    race.asian = "B03002_006"
  )
  
  if(nchar(fips)  == 5){
    acs_data <- get_acs(
      geography = "tract",
      variables = acs_vars,
      state = substr(fips, 1, 2), 
      county = substr(fips, 3, 5), 
      year = year,
      survey = "acs5",
      output = "wide", 
      key = Sys.getenv("CENSUS_API_KEY")
    )
  }else if(nchar(fips) == 2) {
    acs_data <- get_acs(
      geography = "tract",
      variables = acs_vars,
      state = substr(fips, 1, 2), 
      year = year,
      survey = "acs5",
      output = "wide" ,
      key = Sys.getenv("CENSUS_API_KEY")
    )
  }else{
    stop("Error - fips code is not in the correct format of 5 digits for a county or 2 digits for a state")
  }
  
  
  acs_clean <- acs_data %>%
    transmute(
      GEOID.Tract = GEOID,
      median_rent = median_rentE,
      median_income = median_incomeE,
      vac_rate = vac_rateE/100,
      rental_share = rental_unitsE / occupied_unitsE,
      rental_units = rental_unitsE,
      mobility_share = 1 - renters_from_last_yrE / total_renters_last_yrE,
      group = factor(ifelse(race.whiteE / race.totalE >.5, 1,
                            ifelse(race.blackE / race.totalE >.5, 2,
                                   ifelse(race.latinE / race.totalE > .5, 3, 4))))
    ) %>% 
    mutate(group = recode(group,
                          `1` = "White",
                          `2` = "Black",
                          `3` = "Hispanic/Latino",
                          `4` = "Other"))
  
  return(acs_clean)
}



