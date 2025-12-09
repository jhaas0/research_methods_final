library(tidyverse)
library(tidycensus)
library(tigris)
library(sf)
library(lubridate)
library(dplyr)

setwd("C://Users//fl1351//Desktop//RM Project Local//research_methods_final")

# Migrate data clean up ####
migrate_2019 <- read_csv("data/2019.csv/2019.csv")

#fix leading zeros
migrate_2019$Origin <- str_pad(migrate_2019$Origin, 
                               width = 12, pad = 0, side = "left")
migrate_2019$Destination <- str_pad(migrate_2019$Destination, 
                               width = 12, pad = 0, side = "left")

# New York City FIPS codes
# Bronx (36005), Brooklyn(Kings) (36047),
# Manhattan (New York) (36061),
# Queens (36081), Staten Islands (Richmond): (36085)

nyc_counties <- c("36005", "36047", "36061", "36081", "36085")

migrate_2019 <- migrate_2019 %>%
  filter(
    substr(Origin, 1, 5) %in% nyc_counties |
      substr(Destination, 1, 5) %in% nyc_counties
  )


# Inclusionary housing polygons ####
#inc_housing_polygons<- read_csv("data/inclusionaryhousing_areas.csv")

#inclusionary_zoning_projects <- inc+housing_polygons$ProjectNam

#print(inclusionary_zoning_projects)

# Affordable housing Production by Building ####
#buildings_data <- read_csv("data/Affordable_Housing_Production_by_Building.csv")

#See if any projects are not represented in buildings data. None found directly
#setdiff(inclusionary_zoning_projects, buildings_data$`Project Name`)

#unique(buildings_data$`Project Name`)

# Affordable housing production by Project ####
#projects_data <- read_csv("data/Affordable_Housing_Production_by_Project.csv")

#Inclusionary Zoning data! ####

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

write.csv(mih_developments_clean, "mih_developments_clean.csv", row.names = FALSE)

