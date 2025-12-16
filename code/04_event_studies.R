#### RUN EVENT STUDIES ####

# Jacob & Fabian
# December 2025


library(tidyverse)
library(janitor)
library(tigris)
library(sf)
library(fixest)


#### ---- SETUP AND LOAD DATA ---- ####

setwd("C:/Users/Jacob/Documents/Wagner/RM/research_methods_final")

#remove scientific notation
options(scipen = 999)

dataset_for_regressions <- read_rds("data/clean/dataset_for_regressions.rds") %>% 
  group_by(destination) %>%
  mutate(
    treatment_year = if(any(mih == 1)) min(year[mih == 1]) else NA_real_
  ) %>%
  ungroup() %>% 
  mutate(rel_year = if_else(is.na(treatment_year), -999, year - treatment_year))



event_study_high_pov_rate <- feols(
  pct_high_pov_rate ~ i(rel_year, ref = c(-1, -999)) | destination + year,
  data = dataset_for_regressions,
  cluster = "destination"
)

png("output/event_study_high_pov_rate.png", width = 2000, height = 1500, res = 300)  # high-res

iplot(event_study_high_pov_rate,  
              xlab = "Years Relative to Treatment",
              ylab = "Change in Percent of High-Poverty Inflows",
              main = "Change in Percent of High-Poverty Inflows Before/After MIZ",
              ref.line = -1
)

dev.off()

event_study_avg_pov_rate <- feols(
  avg_pov_rate ~ i(rel_year, ref = c(-1, -999)) | destination + year,
  data = dataset_for_regressions,
  cluster = "destination"
)

png("output/event_study_avg_pov_rate.png", width = 2000, height = 1500, res = 300)  # high-res

iplot(event_study_avg_pov_rate,  
      xlab = "Years Relative to Treatment",
      ylab = "Change in Average Poverty Rate",
      main = "Change in Average Poverty Rate of Inflows Before/After MIZ",
      ref.line = -1
)

dev.off()

event_study_median_income <- feols(
  avg_median_income ~ i(rel_year, ref = c(-1, -999)) | destination + year,
  data = dataset_for_regressions,
  cluster = "destination"
)

png("output/event_study_median_income.png", width = 2000, height = 1500, res = 300)  # high-res

iplot(event_study_median_income,  
              xlab = "Years Relative to Treatment",
              ylab = "Change in Median Income",
              main = "Change in Average Median Income of Inflows Before/After MIZ",
              ref.line = -1
)

dev.off()

event_study_total_flows <- feols(
  num_inflows ~ i(rel_year, ref = c(-1, -999)) | destination + year,
  data = dataset_for_regressions,
  cluster = "destination"
)

png("output/event_study_total_flows.png", width = 2000, height = 1500, res = 300)  # high-res

iplot(event_study_total_flows,  
      xlab = "Years Relative to Treatment",
      ylab = "Change in Number of Inflows",
      main = "Change in Number of Inflows Before/After MIZ",
      ref.line = -1
)

dev.off()


event_study_out_of_state_flows <- feols(
  pct_inflows_out_of_state ~ i(rel_year, ref = c(-1, -999)) | destination + year,
  data = dataset_for_regressions,
  cluster = "destination"
)

png("output/event_study_out_of_state_flows.png", width = 2000, height = 1500, res = 300)  # high-res

iplot(event_study_out_of_state_flows,  
      xlab = "Years Relative to Treatment",
      ylab = "Change in Pct of OOS of Inflows",
      main = "Change in Percent of OOS Inflows Before/After MIZ",
      ref.line = -1
)

dev.off()






