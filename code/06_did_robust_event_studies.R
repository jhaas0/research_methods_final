#### RUN CALLAWAY-SANT'ANNA EVENT STUDIES ####

# Jacob & Fabian
# December 2025


library(tidyverse)
library(janitor)
library(tigris)
library(sf)
library(fixest)
library(did)

set.seed(5662)

#### ---- SETUP AND LOAD DATA ---- ####

setwd("C:/Users/Jacob/Documents/Wagner/RM/research_methods_final")

#remove scientific notation
options(scipen = 999)

dataset_for_regressions <- read_rds("data/clean/ih_dataset_for_regressions.rds") %>% 
  mutate(treatment_year_did = if_else(is.na(treatment_year), 0, treatment_year), 
         destination_numeric = as.numeric(destination))


# Note: claude was used to help functionalize Callaway Sant'anna event study code
run_cs_event_study <- function(dep_var,
                               y_label = NULL,
                               title = NULL,
                               output_dir = "output",
                               data = dataset_for_regressions,
                               save_plot = TRUE) {
  
  # Load required package
  if (!require("did")) {
    stop("Package 'did' is required. Install with: install.packages('did')")
  }
  
  # Create default labels if not provided
  if (is.null(y_label)) {
    y_label <- paste("ATT for", dep_var)
  }
  if (is.null(title)) {
    title <- paste("Callaway & Sant'Anna Event Study:", dep_var)
  }
  
  # Prepare data
  cs_data <- data %>%
    mutate(
      treatment_year_did = if_else(is.na(treatment_year), 0, treatment_year),
      destination_numeric = as.numeric(destination)
    )
  
  # Run Callaway & Sant'Anna estimator
  out <- att_gt(
    yname = dep_var,
    gname = "treatment_year_did",
    idname = "destination_numeric",
    tname = "year",
    xformla = ~1,
    data = cs_data
  )
  
  # Aggregate to event study
  es <- aggte(out, type = "dynamic")
  
  # Generate and save plot if requested
  if (save_plot) {
    output_file <- file.path(output_dir, paste0("cs_event_study_", dep_var, ".png"))
    
    png(output_file, width = 2000, height = 1500, res = 300)
    print(ggdid(es, 
                xlab = "Years Relative to Treatment",
                ylab = y_label,
                title = title))
    dev.off()
    
    cat("Callaway & Sant'Anna event study saved to:", output_file, "\n")
  }
  
  # Print summary
  cat("\nSummary:\n")
  print(summary(es))
  
  write_rds(es, paste0("output/regression_models/cs_", dep_var, ".rds"))
  
  # Return results
  return(list(
    att_gt = out,
    event_study = es
  ))
}

cs_total_flows <- run_cs_event_study("num_inflows",
                                     y_label = "Change in Number of Inflows",
                                     title = "Change in Number of Inflows Before/After IH")

cs_high_pov <- run_cs_event_study("pct_high_pov_rate",
                                  y_label = "Change in Percent of High-Poverty Inflows",
                                  title = "Change in Percent of High-Poverty Inflows Before/After IH")

cs_poverty_flows <- run_cs_event_study("num_inflows_high_pov_rate",
                                   y_label = "Change in Number of High Poverty Inflows",
                                   title = "Change in High-Poverty Inflows Before/After IH")

cs_avg_pov <- run_cs_event_study("avg_pov_rate", 
                                 y_label = "Change in Average Poverty Rate",
                                 title = "Change in Average Poverty Rate of Inflows Before/After IH")

cs_median_income <- run_cs_event_study("avg_median_income", 
                                    y_label = "Change in Median Income",
                                    title = "Change in Average Median Income of Inflows Before/After IH")

cs_oos_flows <- run_cs_event_study("pct_inflows_out_of_state",
                                   y_label = "Change in Pct of OOS of Inflows",
                                   title = "Change in Percent of OOS Inflows Before/After IH")

