library(tidyverse)
library(tidycensus)
library(tigris)
library(sf)
library(lubridate)
library(dplyr)
library(janitor)

setwd("C://Users//fl1351//Desktop//RM Project Local//research_methods_final")

#### ---- LOAD AND FILTER DATA TO NYC ---- ####

years <- 2011:2019

#remove scientific notation
options(scipen = 999)

options(tigris_use_cache = TRUE)


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
    mutate(origin = str_pad(origin, width = 12, side = "left", pad = "0"), 
           destination = str_pad(destination, width = 12, side = "left", pad = "0")) %>% 
    filter(str_detect(origin, "^(36005|36047|36061|36081|36085)") | 
             str_detect(destination, "^(36005|36047|36061|36081|36085)")) %>% 
    filter(origin != destination)
  
  # Save to data/clean folder
  output_path <- paste0("data/clean/", year, "_nyc.rds")
  write_rds(migrate_nyc, output_path)
  
  gc()
  message("Saved cleaned data to: ", output_path)
}

for(yr in years){
  clean_nyc_migration(yr)
}

