#### LOAD IN AND CLEAN MIGRATE DATA ####

# Jacob & Fabian
# December 2025

library(tidyverse)
library(janitor)
library(tigris)
library(sf)

#### ---- SETUP AND LOAD DATA ---- ####

# change your wd
setwd("C:/Users/Jacob/Documents/Wagner/RM/research_methods_final")

# which years do you want to load and save? 
years <- 2011:2019

#remove scientific notation
options(scipen = 999)

options(tigris_use_cache = TRUE)

#### ---- LOAD AND FILTER DATA TO NYC ---- ####

# Function to filter and clean NYC migration data for a given year
clean_nyc_migration <- function(year) {
  # NYC FIPS codes: Bronx (36005), Kings/Brooklyn (36047), New York/Manhattan (36061), 
  # Queens (36081), Richmond/Staten Island (36085)
  
  # Read the data
  file_path <- paste0("data/raw/", year, ".csv/", year, ".csv")
  migrate_data <- read_csv(file_path)
  
  # Filter and clean
  migrate_nyc <- migrate_data %>% 
    clean_names() %>% 
    mutate(origin = str_pad(origin, width = 12, side = "left", pad = "0")) %>% 
    filter(
      str_detect(origin, "^(36005|36047|36061|36081|36085)") | 
        str_detect(destination, "^(36005|36047|36061|36085|36081)"))
  
  # Save to data/clean folder
  output_path <- paste0("data/clean/", year, "_nyc.rds")
  write_rds(migrate_nyc, output_path)
  
  gc()
  message("Saved cleaned data to: ", output_path)
}

for (yr in years){
  clean_nyc_migration(yr)
}