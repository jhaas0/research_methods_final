#### ADD MIZ DATA TO MIGRATE DATASET ####

# Jacob & Fabian
# December 2025


library(tidyverse)
library(janitor)
library(tigris)
library(sf)
library(readxl)
library(tidylog)

#### ---- SETUP AND LOAD DATA ---- ####

setwd("C:/Users/Jacob/Documents/Wagner/RM/research_methods_final")

#remove scientific notation
options(scipen = 999)

options(tigris_use_cache = TRUE)

nyc_all <- read_rds("data/clean/nyc_all_years.rds")

ih_developments_clean <- read_rds("data/clean/ih_developments_clean.rds")


#### --- MERGE MIH DATA WITH MIGRATE DATA BY CBG ---- ####

nyc_all_clean <- nyc_all %>% 
  mutate(destination = as.character(str_pad(destination, width = 12, side = "left", pad = "0"))) %>% 
  filter(origin != destination)

#get indicator variable for when a block group has an MIH property open
ih_developments_by_bg <- ih_developments_clean %>% 
  as_tibble() %>% 
  filter(!is.na(consolidated_year)) %>% 
  group_by(GEOID10) %>% 
  mutate(transfer_year = year(Transfer_Date), 
         mih = if_else(Program_Type == "MIH", 1, 0), 
         mih_year = case_when(
           mih == 1 & !is.na(consolidated_year) ~ consolidated_year,
           mih == 1 & is.na(consolidated_year) ~ transfer_year,
           TRUE ~ NA_real_
           )  
         ) %>% 
  #set the treatment year for a given CBG as the first year any MIH project opens
  summarize(first_year = min(consolidated_year, na.rm = TRUE), 
            first_mih_year = na_if(min(mih_year, na.rm = TRUE), Inf), 
            Total_IH_Floor_Area = sum(Total_IH_Floor_Area, na.rm = TRUE), 
            ih = 1,
            mih = max(mih)) %>% 
  filter(first_year > 2011, 
         first_year < 2020)


#merge migrate data with MIH data by block group
nyc_all_w_ih <- nyc_all_clean %>% 
  left_join(ih_developments_by_bg,  
            by = c("destination" = "GEOID10", 
                   "year" = "first_year")) %>% 
  #get binary MIH variable for if an MIH has opened by a given year
  arrange(destination, year) %>%   
  group_by(destination) %>%
  fill(ih, .direction = "down") %>% 
  fill(Total_IH_Floor_Area, .direction = "down") %>% 
  ungroup() %>% 
  mutate(ih = if_else(is.na(ih), 0, 1),
         Total_IH_Floor_Area = if_else(is.na(Total_IH_Floor_Area), 0, Total_IH_Floor_Area))


#collapse/summarize each block group by the inflow demographics
ih_dataset_for_regressions <- nyc_all_w_ih %>% 
  mutate(destination_tract = str_sub(destination, 1, 11)) %>% 
  group_by(destination_tract) %>% 
  mutate(ih_in_tract = max(ih, na.rm = TRUE)) %>% 
  ungroup() %>% 
  filter(ih_in_tract == 1) %>% 
  group_by(year, destination, ih, Total_IH_Floor_Area) %>% 
  summarize(
    avg_median_income = sum(median_income * flow, na.rm = TRUE) / sum(flow, na.rm = TRUE),  
    avg_pov_rate = sum(poverty_rate * flow, na.rm = TRUE) / sum(flow, na.rm = TRUE), 
    pct_high_pov_rate = sum((poverty_rate > 0.2) * flow, na.rm = TRUE) / sum(flow, na.rm = TRUE), 
    num_inflows = sum(flow, na.rm = TRUE), 
    num_inflows_high_pov_rate = sum((poverty_rate > 0.2) * flow, na.rm = TRUE), 
    num_inflows_out_of_state = sum(flow[str_sub(origin, 1, 2) != "36"], na.rm = TRUE), 
    pct_inflows_out_of_state = num_inflows_out_of_state/num_inflows
  ) %>% 
  ungroup() %>% 
  mutate(ln_ih_floor_area = log(Total_IH_Floor_Area + 1)) %>% 
  group_by(destination) %>%
  mutate(
    treatment_year = if(any(ih == 1)) min(year[ih == 1]) else NA_real_
  ) %>%
  ungroup() %>% 
  mutate(rel_year = if_else(is.na(treatment_year), -999, year - treatment_year), 
         rel_year_bucket = case_when(
           rel_year == -999 ~ "-999",
           rel_year >= 4  ~ ">=4",
           rel_year <= -5 ~ "<=-5",
           TRUE ~ as.character(rel_year)
         ),
         rel_year_bucket = factor(
           rel_year_bucket,
           levels = c("-999", "<=-5", "-4",
                      "-3", "-2", "-1", 
                      "0", "1", "2",
                      "3", ">=4")
         )
  )

write_rds(ih_dataset_for_regressions, "data/clean/ih_dataset_for_regressions.rds")  


# #### ---- CREATE MIH DATASET ---- ####
# 
# mih_developments_by_bg <- ih_developments_by_bg %>% 
#   filter(first_mih_year > 2011, 
#          first_mih_year < 2020,
#          !is.na(first_mih_year))
# 
# 
# #merge migrate data with MIH data by block group
# nyc_all_w_mih <- nyc_all_clean %>% 
#   left_join(mih_developments_by_bg,  
#             by = c("destination" = "GEOID10", 
#                    "year" = "first_year")) %>% 
#   #get binary MIH variable for if an MIH has opened by a given year
#   arrange(destination, year) %>%   
#   group_by(destination) %>%
#   fill(mih, .direction = "down") %>% 
#   fill(Total_IH_Floor_Area, .direction = "down") %>% 
#   ungroup() %>% 
#   mutate(mih = if_else(is.na(mih), 0, 1), 
#          Total_IH_Floor_Area = if_else(is.na(Total_IH_Floor_Area), 0, Total_IH_Floor_Area))
# 
# 
# #collapse/summarize each block group by the inflow demographics
# mih_dataset_for_regressions <- nyc_all_w_mih %>% 
#   mutate(destination_tract = str_sub(destination, 1, 11)) %>% 
#   group_by(destination_tract) %>% 
#   mutate(mih_in_tract = max(mih, na.rm = TRUE)) %>% 
#   ungroup() %>% 
#   filter(mih_in_tract == 1) %>% 
#   group_by(year, destination, mih, Total_IH_Floor_Area) %>% 
#   summarize(
#     avg_median_income = sum(median_income * flow, na.rm = TRUE) / sum(flow, na.rm = TRUE),  
#     avg_pov_rate = sum(poverty_rate * flow, na.rm = TRUE) / sum(flow, na.rm = TRUE), 
#     pct_high_pov_rate = sum((poverty_rate > 0.2) * flow, na.rm = TRUE) / sum(flow, na.rm = TRUE), 
#     num_inflows = sum(flow, na.rm = TRUE), 
#     num_inflows_high_pov_rate = sum((poverty_rate > 0.2) * flow, na.rm = TRUE), 
#     num_inflows_out_of_state = sum(flow[str_sub(origin, 1, 2) != "36"], na.rm = TRUE), 
#     pct_inflows_out_of_state = num_inflows_out_of_state/num_inflows
#   ) %>% 
#   ungroup()
# 
# write_rds(mih_dataset_for_regressions, "data/clean/mih_dataset_for_regressions.rds")  


