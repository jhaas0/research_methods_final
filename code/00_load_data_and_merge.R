library(tidyverse)
library(tidycensus)
library(tigris)
library(sf)

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


# Inclusionary housing data ####
inc_housing_areas <- read_csv("data/inclusionaryhousing_areas.csv")

inclusionary_zoning_projects <- inchousing_areas$ProjectNam

print(inclusionary_zoning_projects)

# Affordable housing Production by Building ####
buildings_data <- read_csv("data/Affordable_Housing_Production_by_Building.csv")

#See if any projects are not represented in buildings data. None found directly
setdiff(inclusionary_zoning_projects, buildings_data$`Project Name`)

unique(buildings_data$`Project Name`)

# Affordable housing production by Project ####
projects_data <- read_csv("data/Affordable_Housing_Production_by_Project.csv")





