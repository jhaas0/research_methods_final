#### RUN CALLAWAY-SANT'ANNA EVENT STUDIES - CONTINUOUS TREATMENT ####

# Jacob & Fabian
# December 2025


library(tidyverse)
library(janitor)
library(tigris)
library(sf)
library(fixest)
library(did)

# install.packages("devtools")
devtools::install_github("bcallaway11/contdid")

library(contdid)

set.seed(5662)

#### ---- SETUP AND LOAD DATA ---- ####

setwd("C:/Users/Jacob/Documents/Wagner/RM/research_methods_final")

#remove scientific notation
options(scipen = 999)

dataset_for_regressions <- read_rds("data/clean/ih_dataset_for_regressions.rds") %>% 
  mutate(treatment_year_did = if_else(is.na(treatment_year), 0, treatment_year), 
         destination_numeric = as.numeric(destination))





