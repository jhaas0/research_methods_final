#### RUN EVENT STUDIES ####

# Jacob & Fabian
# December 2025


library(tidyverse)
library(janitor)
library(tigris)
library(sf)
library(fixest)
library(did)
library(scales)


#### ---- SETUP AND LOAD DATA ---- ####

setwd("C:/Users/Jacob/Documents/Wagner/RM/research_methods_final")

#remove scientific notation
options(scipen = 999)

dataset_for_regressions <- read_rds("data/clean/ih_dataset_for_regressions.rds") %>% 
  group_by(destination) %>%
  mutate(
    # Forward-fill: all periods get the eventual treatment intensity
    eventual_ln_ih_floor_area = max(ln_ih_floor_area, na.rm = TRUE)
  ) %>%
  ungroup() %>% 
  filter((rel_year >= -5 & rel_year <= 4) | rel_year == -999)


#Note: claude was used to help functionalize our user-written event study code
run_event_study <- function(dep_var, 
                            y_label = NULL, 
                            title = NULL, 
                            ref_line = -1,
                            output_dir = "output",
                            continuous = FALSE,
                            data = dataset_for_regressions) {
  
  # Create default labels if not provided
  if (is.null(y_label)) {
    y_label <- paste("Change in", dep_var)
  }
  if (is.null(title)) {
    title <- paste("Change in", dep_var, "Before/After IH")
  }
  
  # Build formula dynamically
  if(continuous == TRUE){
    formula <- as.formula(paste0(dep_var, " ~ i(rel_year, eventual_ln_ih_floor_area, ref = c(-1, -999)) | destination + year"))
  }else{
    formula <- as.formula(paste0(dep_var, " ~ i(rel_year, ref = c(-1, -999)) | destination + year"))
  }
  
  # Run event study regression
  model <- feols(
    formula,
    data = data,
    cluster = "destination"
  )
  
  # Create output filename
  if(continuous == TRUE){
    output_file <- file.path(output_dir, paste0("event_study_", dep_var, "_continuous.png"))
  }else{
    output_file <- file.path(output_dir, paste0("event_study_", dep_var, ".png"))
  }
  
  # Generate plot
  png(output_file, width = 2000, height = 1500, res = 300)
  
  iplot(model,  
        xlab = "Years Relative to Treatment",
        ylab = y_label,
        main = title,
        ref.line = ref_line)
  
  dev.off()
  
  if(continuous == TRUE){
    write_rds(model, paste0("output/regression_models/es_", dep_var, "_continuous.rds"))
  }else{
    write_rds(model, paste0("output/regression_models/es_", dep_var, ".rds"))
  }
  
  # Print confirmation and return model
  cat("Event study saved to:", output_file, "\n")
  return(model)
}

# Run all your event studies
es_high_pov <- run_event_study("pct_high_pov_rate", 
                               y_label = "Change in Percent of High-Poverty Inflows",
                               title = "Change in Percent of High-Poverty Inflows Before/After IH",
                               ref_line = -1)

es_avg_pov <- run_event_study("avg_pov_rate",
                              y_label = "Change in Average Poverty Rate",
                              title = "Change in Average Poverty Rate of Inflows Before/After IH",
                              ref_line = -1)

es_median_income <- run_event_study("avg_median_income",
                                    y_label = "Change in Median Income",
                                    title = "Change in Average Median Income of Inflows Before/After IH",
                                    ref_line = -1)

es_total_flows <- run_event_study("num_inflows",
                                  y_label = "Change in Number of Inflows",
                                  title = "Change in Number of Inflows Before/After IH",
                                  ref_line = -1)

es_poverty_flows <- run_event_study("num_inflows_high_pov_rate",
                                    y_label = "Change in Number of High Poverty Inflows",
                                    title = "Change in High-Poverty Inflows Before/After IH",
                                    ref_line = -1)

es_oos_flows <- run_event_study("pct_inflows_out_of_state",
                                y_label = "Change in Pct of OOS of Inflows",
                                title = "Change in Percent of OOS Inflows Before/After IH",
                                ref_line = 5)

#### ---- CONTINUOUS ---- #### 
es_high_pov_continuous <- run_event_study("pct_high_pov_rate", 
                                          y_label = "Change in Percent of High-Poverty Inflows",
                                          title = "Effect of More Affordable Sqft on High-Poverty Inflows",
                                          ref_line = -1, 
                                          continuous = TRUE)

es_avg_pov_continuous <- run_event_study("avg_pov_rate",
                              y_label = "Change in Average Poverty Rate",
                              title = "Effect of More Affordable Sqft on Average Poverty Rate",
                              ref_line = -1,
                              continuous = TRUE)

es_median_income_continuous <- run_event_study("avg_median_income",
                                    y_label = "Change in Median Income",
                                    title = "Effect of More Affordable Sqft on Median Income",
                                    ref_line = -1,
                                    continuous = TRUE)

es_total_flows_continuous <- run_event_study("num_inflows",
                                  y_label = "Change in Number of Inflows",
                                  title = "Effect of More Affordable Sqft on Number of Inflows",
                                  ref_line = -1,
                                  continuous = TRUE)

es_poverty_flows_continuous <- run_event_study("num_inflows_high_pov_rate",
                                    y_label = "Change in Number of High-Poverty Inflows",
                                    title = "Effect of More Affordable Sqft on High-Poverty Inflows",
                                    ref_line = -1,
                                    continuous = TRUE)
