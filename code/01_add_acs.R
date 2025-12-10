

# Jacob & Fabian
# December 2025


library(tidyverse)
library(janitor)
library(tigris)
library(tidycensus)
library(sf)


#### ---- SETUP ---- ####

setwd("C:/Users/fl1351/Desktop/RM Project Local/research_methods_final")

#remove scientific notation
options(scipen = 999)

options(tigris_use_cache = TRUE)

years <- 2011:2019

nyc_all <- map_dfr(
  years,
  ~ read_rds(paste0("data/clean/", .x, "_nyc.rds")) %>%
    mutate(year = .x)
)

acs_vars <- c(
  median_rent = "B25064_001",
  median_income = "B19013_001",
  poverty_rate = "S1701_C03_001",
  race.latin = "B03002_012",
  race.total = "B03002_001",
  race.white = "B03002_003",
  race.black = "B03002_004",
  race.asian = "B03002_006"
)

acs_data <- get_acs(
  geography = "tract",
  variables = acs_vars,
  state = state.abb, 
  year = 2019,
  survey = "acs5",
  output = "wide", 
  key = Sys.getenv("CENSUS_API_KEY")
)

acs_clean <- acs_data %>%
  transmute(
    GEOID.Tract = GEOID,
    poverty_rate = poverty_rateE/100,
    median_rent = median_rentE,
    median_income = median_incomeE,
    race_ethnicity_group =   case_when(
      race.whiteE  == pmax(race.whiteE, race.blackE, race.latinE, na.rm = TRUE) ~ "White",
      race.blackE  == pmax(race.whiteE, race.blackE, race.latinE, na.rm = TRUE) ~ "Black",
      race.latinE  == pmax(race.whiteE, race.blackE, race.latinE, na.rm = TRUE) ~ "Hispanic/Latino",
      TRUE ~ "Other"
    )
  )

years <- 2011:2019

nyc_all <- map_dfr(
  years,
  ~ read_rds(paste0("data/clean/", .x, "_nyc.rds")) %>%
    mutate(year = .x)
)

nyc_all_with_acs <- nyc_all %>% 
  mutate(origin_tract = str_sub(origin, 1, 11)) %>% 
  left_join(acs_clean, by = c("origin_tract" = "GEOID.Tract"))

write_rds(nyc_all_with_acs, "data/clean/nyc_all_years.rds")


mih_developments_clean <- readRDS("data/clean/mih_developments_clean.rds")

#Join data set for analysis
nyc_full <- bind_rows(nyc_all_with_acs, mih_developments_clean)

saveRDS(nyc_full,"data/clean/nyc_full.rds")
