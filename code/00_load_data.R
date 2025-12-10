library(tidyverse)
library(tidycensus)
library(janitor)
library(tigris)
library(sf)
library(lubridate)
library(dplyr)

# Jacob & Fabian
# December 2025


migrate_2019 <- read_csv("data/2019.csv/2019.csv")

head(migrate)

#### LOAD IN AND CLEAN MIGRATE DATA ####

setwd("C://Users//fl1351//Desktop//RM Project Local//research_methods_final")

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


#Load and clean Inclusionary Zoning data! ####

inclusionary_zoning_data <- read.csv("data/All HPD Inclusionary Housing Sites _Query result.csv")
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

# Save as geopackage
#st_write(mih_developments_clean, "data/mih_developments_clean.gpkg")
#write.csv(mih_developments_clean, "data/mih_developments_clean.csv", row.names = FALSE)

saveRDS(mih_developments_clean, "data/mih_developments_clean.rds")

