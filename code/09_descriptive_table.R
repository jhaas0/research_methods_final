library(tidyverse)
library(janitor)
library(sf)
library(tidycensus)

#### ---- SETUP AND LOAD DATA ---- ####

setwd("C:/Users/Jacob/Documents/Wagner/RM/research_methods_final")

#remove scientific notation
options(scipen = 999)

dataset_for_regressions <- read_rds("data/clean/ih_dataset_for_regressions.rds") %>% 
  mutate(treatment_year_did = if_else(is.na(treatment_year), 0, treatment_year), 
         destination_numeric = as.numeric(destination))

acs_vars <- c(
  total_pop = "B01003_001", 
  median_rent = "B25064_001",
  median_income = "B19013_001"
)

#load acs 2011 data
acs_2013 <- get_acs(
  geography = "block group",
  variables = acs_vars,
  state = "NY",  # Adjust if you need other states
  year = 2013,
  survey = "acs5",
  output = "wide"
) %>% 
  select(-ends_with("E"))

# NYC FIPS codes and county names
nyc_counties <- c(
  "36005" = "Bronx",
  "36047" = "Brooklyn",
  "36061" = "Manhattan",
  "36081" = "Queens",
  "36085" = "Staten Island"
)

# Join to your dataset
dataset_for_tests <- dataset_for_regressions %>%
  left_join(acs_2013, by = c("destination" = "GEOID")) %>% 
  group_by(destination) %>% 
  mutate(ever_treated = max(ih), 
         county = str_sub(destination, 1, 5),
         borough = nyc_counties[county]) %>% 
  filter(year == 2011) %>% 
  ungroup()


# Create table with means by group
table_1 <- dataset_for_tests %>% 
  group_by(ever_treated) %>% 
  summarize(n = n(), 
            avg_pov_rate_2011 = mean(avg_pov_rate), 
            avg_median_income_2011 = mean(avg_median_income), 
            num_inflows = mean(num_inflows), 
            num_inflows_high_pov_rate = mean(num_inflows_high_pov_rate), 
            pct_bronx = round(sum(borough == "Bronx", na.rm = TRUE) / n(), 3),
            pct_brooklyn = round(sum(borough == "Brooklyn", na.rm = TRUE) / n(), 3),
            pct_manhattan = round(sum(borough == "Manhattan", na.rm = TRUE) / n(), 3),
            pct_queens = round(sum(borough == "Queens", na.rm = TRUE) / n(), 3),
            pct_staten_island = round(sum(borough == "Staten Island", na.rm = TRUE) / n(), 3), 
            total_pop_destination = mean(total_popM, na.rm = TRUE), 
            median_income_destination = mean(median_incomeM, na.rm = TRUE), 
            median_rent_destination = mean(median_rentM, na.rm = TRUE))

# Add p-values as a new row or column
p_values <- tibble(
  ever_treated = 999,
  n = NA,
  avg_pov_rate_2011 = t.test(avg_pov_rate ~ ever_treated, data = dataset_for_tests)$p.value,
  avg_median_income_2011 = t.test(avg_median_income ~ ever_treated, data = dataset_for_tests)$p.value,
  num_inflows = t.test(num_inflows ~ ever_treated, data = dataset_for_tests)$p.value,
  num_inflows_high_pov_rate = t.test(num_inflows_high_pov_rate ~ ever_treated, data = dataset_for_tests)$p.value,
  pct_bronx = NA,
  pct_brooklyn = NA,
  pct_manhattan = NA,
  pct_queens = NA,
  pct_staten_island = NA,
  total_pop_destination = t.test(total_popM ~ ever_treated, data = dataset_for_tests)$p.value,
  median_income_destination = t.test(median_incomeM ~ ever_treated, data = dataset_for_tests)$p.value,
  median_rent_destination = t.test(median_rentM ~ ever_treated, data = dataset_for_tests)$p.value
)

table_1_with_pvals <- bind_rows(table_1, p_values)

write_csv(table_1_with_pvals, "output/table_1.csv")
