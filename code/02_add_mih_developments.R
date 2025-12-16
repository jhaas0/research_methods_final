#### ADD MIZ DATA ####

# Jacob & Fabian
# December 2025


library(tidyverse)
library(janitor)
library(tigris)
library(sf)

#### ---- SETUP AND LOAD DATA ---- ####

setwd("C:/Users/Jacob/Documents/Wagner/RM/research_methods_final")

#remove scientific notation
options(scipen = 999)

options(tigris_use_cache = TRUE)

nyc_all <- read_rds("data/clean/nyc_all_years.rds")

mih_developments <- read_rds("data/clean/mih_developments_clean.rds")

block_groups_ny <- block_groups(state = "NY", year = "2010") %>% 
  st_transform(st_crs(mih_developments))

#### --- MERGE MIH DATA WITH MiGRATE DATA BY CBG ---- ####

nyc_all_clean <- nyc_all %>% 
  mutate(destination = as.character(str_pad(destination, width = 12, side = "left", pad = "0")))

mih_developments_by_bg <- mih_developments %>% 
  as_tibble() %>% 
  mutate(
    Transfer_Date = mdy(Transfer_Date),  
    transfer_year = year(Transfer_Date) 
  ) %>% 
  filter(!is.na(Transfer_Date)) %>% 
  group_by(BG) %>% 
  summarize(first_year = min(transfer_year)) %>% 
  mutate(mih = 1)

nyc_all_w_mih <- nyc_all_clean %>% 
  left_join(mih_developments_by_bg,  
            by = c("destination" = "BG", 
                   "year" = "first_year")) %>% 
  arrange(destination, year) %>%   # ensure correct order
  group_by(destination) %>%
  fill(mih, .direction = "down") %>%  # fill downward
  ungroup() %>% 
  mutate(mih = if_else(is.na(mih), 0, 1))

dataset_for_regressions <- nyc_all_w_mih %>% 
  mutate(destination_tract = str_sub(destination, 1, 11)) %>% 
  group_by(destination_tract) %>% 
  mutate(mih_in_tract = max(mih, na.rm = TRUE)) %>% 
  ungroup() %>% 
  filter(mih_in_tract == 1) %>% 
  group_by(year, destination, mih) %>% 
  summarize(
    avg_median_income = sum(median_income * flow, na.rm = TRUE) / sum(flow, na.rm = TRUE),  
    avg_pov_rate = sum(poverty_rate * flow, na.rm = TRUE) / sum(flow, na.rm = TRUE), 
    pct_high_pov_rate = sum((poverty_rate > 0.2) * flow, na.rm = TRUE) / sum(flow, na.rm = TRUE), 
    num_inflows = sum(flow, na.rm = TRUE), 
    num_inflows_out_of_state = sum(flow[str_sub(origin, 1, 2) != "36"], na.rm = TRUE), 
    pct_inflows_out_of_state = num_inflows_out_of_state/num_inflows
  ) %>% 
  ungroup()

write_rds(dataset_for_regressions, "data/clean/dataset_for_regressions.rds")  



