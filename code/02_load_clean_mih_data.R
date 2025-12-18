library(tidyverse)
library(tidycensus)
library(tigris)
library(sf)
library(lubridate)
library(dplyr)
library(janitor)
library(tidylog)

setwd("C:/Users/Jacob/Documents/Wagner/RM/research_methods_final")

#### LOAD and CLEAN Inclusionary Zoning data ####

inclusionary_zoning_data <- read.csv("data/raw/All HPD Inclusionary Housing Sites _Query result.csv")

# Only keep MIH developments, and collapse by project (one or multiple adjacent addresses)
mih_developments_clean <- inclusionary_zoning_data %>%
  #replace missing dates with NAs
  mutate(Transfer_Date = na_if(Transfer_Date, "")) %>% 
  #remove VIH Compensated Development
  filter(Label %in% c("MIH Development", "MIH Site", "VIH Generating Site")) %>%
  # Arrange so:
  # Oldest Transfer_Date first, "MIH Site" before "MIH Development", objectid for stable ordering.
  # Basically just making sure first row first represents project well
  arrange(
    Project_Name,
    Transfer_Date,
    desc(Label),
    OBJECTID
  ) %>%
  # Collapse by project ID
  group_by(Project_ID) %>%
  mutate(
    Total_IH_Floor_Area = sum(Bldg_IH_Floor_Area, na.rm = TRUE)
  ) %>%
  slice_head(n = 1) %>% # Picking the first address to represent project
  ungroup()

#Drop extra variables:
mih_developments_clean <- mih_developments_clean %>% 
  select(-OBJECTID, -IH_Floor_Area_Transferred, -IH_Floor_Area_Balance,
         -Comp_IH_Floor_Area)

#Clean geographic info and make into sf object ####

mih_sf <- mih_developments_clean %>%
  st_as_sf(
    coords = c("x", "y"), 
    crs = 3857)  # WGS83 Web mecator

# Transform to WGS84 (standard lat/long)
mih_sf_wgs84 <- st_transform(mih_sf, crs = 4326)

# Block groups
bg_nyc <- block_groups(
  state  = "NY",
  county = c("Bronx", "Kings", "New York", "Queens", "Richmond"),
  year   = 2010,
  class  = "sf"
) %>%
  st_transform(4326) %>% 
  select(GEOID10)


# Join tracts, then block groups
mih_sf_geo <- mih_sf_wgs84 %>%
  st_join(bg_nyc, join = st_intersects)     

### ---- Final mutate: lon/lat + IDs ---- ####

mih_developments_clean <- mih_sf_geo %>%
  mutate(
    Longitude = st_coordinates(.)[, 1],
    Latitude  = st_coordinates(.)[, 2]
  ) 


#### ---- Clean Furman Center Inclusionary Zoning Data

fc_subsidized_housing <- read_excel("data/raw/FC_Subsidized_Housing_Database_2025-05-13/FC_Subsidized_Housing_Database_2025-05-13.xlsx", 
                                    sheet = "SubsidizedHousingDatabase")

fc_developments <- fc_subsidized_housing #%>% 
  #filter(program_name == "Inclusionary Housing")

fc_developments_sf <- fc_developments %>% 
  st_as_sf(
    coords = c("longitude", "latitude"), 
    crs = st_crs(mih_developments_clean))


fc_developments_w_bg <- fc_developments_sf %>% 
  st_join(bg_nyc %>% 
            select(GEOID10), 
          join = st_intersects)  

fc_developments_clean <- fc_developments_w_bg %>% 
  distinct() %>% 
  #agency supplied ID is equivalent to the project ID in the other MIH dataset
  group_by(agency_supplied_id_1) %>% 
  arrange(
    project_name,
    desc(start_date)) %>%
  slice(1) %>% 
  as_tibble() %>% 
  select(-geometry)
  
ih_developments <- mih_developments_clean %>% 
  as_tibble() %>% 
  select(-geometry) %>% 
  mutate(Project_ID = as.character(Project_ID)) %>% 
  full_join(fc_developments_clean, by = c("Project_ID" = "agency_supplied_id_1")) %>% 
  #filter to either projects in the HPD data or those labeled as Inclusionary Zoning in the Furman Data
  filter(!is.na(Label) | program_name == "Inclusionary Housing")

#only one development with mismatched census block groups - looks like the CBG next door. Can filter out. 
ih_developments %>% 
  filter(GEOID10.x != GEOID10.y) %>% 
  nrow()

ih_developments_clean <- ih_developments %>% 
  filter(is.na(GEOID10.y) | GEOID10.x == GEOID10.y) %>% 
  #convert to YMD format
  mutate(Transfer_Date = mdy(Transfer_Date)) %>% 
  rename(GEOID10 = GEOID10.x) %>% 
  select(Project_ID:Total_IH_Floor_Area,
         Transfer_Date:Latitude,
         project_name, 
         start_date:end_date, 
         assessed_value:res_units, 
         buildings) %>% 
  filter(is.na(end_date)) %>% 
  mutate(consolidated_year = if_else(!is.na(start_date), 
                                     year(start_date), 
                                     year(Transfer_Date)))

write_rds(ih_developments_clean, "data/clean/ih_developments_clean.rds")

  
#compare locations of two MIH datasets
ggplot(fc_developments_w_bg %>% 
         filter(program_name == "Inclusionary Housing")) +
  geom_sf() +
  geom_sf(
    data = mih_developments_clean,
    color = "red",
    alpha = 0.4,
    size = 2
  )

