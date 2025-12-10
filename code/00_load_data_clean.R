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
    mutate(origin = str_pad(origin, width = 12, side = "left", pad = "0")) %>% 
    filter(str_detect(origin, "^(36005|36047|36061|36081|36085)") | 
             str_detect(destination, "^(36005|36047|36061|36081|36085)"))
  
  # Save to data/clean folder
  output_path <- paste0("data/clean/", year, "_nyc.rds")
  write_rds(migrate_nyc, output_path)
  
  gc()
  message("Saved cleaned data to: ", output_path)
}

for(yr in years){
  clean_nyc_migration(yr)
}

#### LOAD and CLEAN Inclusionary Zoning data ####

inclusionary_zoning_data <- read.csv("data/raw/All HPD Inclusionary Housing Sites _Query result.csv")
MIH_projects <- inclusionary_zoning_data %>%
  filter(Label %in% c("MIH Site", "MIH Development"))
#write.csv(mandatory_zoning_projects, "MIH_projects")

# Only keep MIH developments, and collapse by project (one or multiple adjacent addresses)
mih_developments_clean <- MIH_projects %>%
  filter(Label == "MIH Development") %>%
  # Arrange so:
  # Completed projects come first (not needed), newest Transfer_Date first, objectid for stable ordering. Basically just making sure  row first represents project well
  arrange(
    Project_Name,
    desc(Project_Status == "Completed"),
    desc(!is.na(Transfer_Date)),
    desc(Transfer_Date),
    OBJECTID 
  ) %>%
  # Collapse by project
  group_by(Project_Name) %>%
  mutate(
    Total_IH_Floor_Area = sum(Bldg_IH_Floor_Area, na.rm = TRUE)
  ) %>%
  slice_head(n = 1) %>% # Picking the first address to represent project
  ungroup()

#Drop extra variables:
mih_developments_clean <- mih_developments_clean%>% 
  select(-OBJECTID, -IH_Floor_Area_Transferred, -IH_Floor_Area_Balance,
         -Comp_IH_Floor_Area)

mih_sf <- mih_developments_clean %>%
  st_as_sf(
    coords = c("x", "y"), 
    crs = 2263  # New York State Plane Long Island Zone coordinate system
  )

# Transform to WGS84 (standard lat/long)
mih_sf_wgs84 <- st_transform(mih_sf, crs = 4326)

# Add longitude and latitude columns
mih_developments_clean <- mih_sf_wgs84 %>%
  mutate(
    Longitude = st_coordinates(.)[,1],
    Latitude = st_coordinates(.)[,2]
  )

saveRDS(mih_developments_clean, "data/clean/mih_developments_clean.rds")
