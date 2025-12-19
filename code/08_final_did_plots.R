library(ggplot2)
library(dplyr)
library(stringr)
library(scales)

plot_combined_event_study <- function(
    result1, 
    result2,
    result1_structure = "twfe",  # "twfe" or "custom"
    result2_structure = "cs",     # "cs" or "custom"
    model1_name = "TWFE",
    model2_name = "C&SA (2021)",
    time_range = c(-5, 4),
    ref_time = -1,
    y_label = "Estimate",
    plot_title = "Event Study",
    color_palette = "Set1",
    ci_level = 0.95, 
    output_file_name
) {
  
  # Calculate z-score for confidence intervals
  z_score <- qnorm(1 - (1 - ci_level) / 2)
  
  # Create df1 based on structure
  if (result1_structure == "twfe") {
    time_names_es <- as.integer(str_extract(names(result1$coefficients), "-?\\d+"))
    df1 <- data.frame(
      time = time_names_es,
      estimate = result1$coefficients,
      se = result1$se,
      ci_lower = result1$coefficients - z_score * result1$se,
      ci_upper = result1$coefficients + z_score * result1$se,
      model = model1_name
    )
  } else if (result1_structure == "custom") {
    # Assumes result1 is a list with time, estimates, se
    df1 <- data.frame(
      time = result1$time,
      estimate = result1$estimates,
      se = result1$se,
      ci_lower = result1$estimates - z_score * result1$se,
      ci_upper = result1$estimates + z_score * result1$se,
      model = model1_name
    )
  }
  
  # Create df2 based on structure
  if (result2_structure == "cs") {
    df2 <- data.frame(
      time = result2$event_study$egt,
      estimate = result2$event_study$att.egt,
      se = result2$event_study$se.egt,
      z_score = result2$event_study$crit.val.egt,
      ci_lower = result2$event_study$att.egt - result2$event_study$crit.val.egt * result2$event_study$se.egt,
      ci_upper = result2$event_study$att.egt + result2$event_study$crit.val.egt * result2$event_study$se.egt,
      model = model2_name
    )
  } else if (result2_structure == "custom") {
    # Assumes result2 is a list with time, estimates, se
    df2 <- data.frame(
      time = result2$time,
      estimate = result2$estimates,
      se = result2$se,
      ci_lower = result2$estimates - z_score * result2$se,
      ci_upper = result2$estimates + z_score * result2$se,
      model = model2_name
    )
  }
  
  # Add reference point
  ref_point1 <- data.frame(
    time = ref_time,
    estimate = 0,
    se = 0,
    ci_lower = 0,
    ci_upper = 0,
    model = model1_name
  )
  
  # Combine and filter
  combined_df <- bind_rows(df1, df2, ref_point1) %>%
    filter(time <= time_range[2] & time >= time_range[1]) %>%
    arrange(model, time)
  
  # Create plot
  p <- ggplot(combined_df, aes(x = time, y = estimate, color = model)) +
    geom_point(position = position_dodge(width = 0.3), size = 2) +
    geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper),
                  width = 0.2,
                  position = position_dodge(width = 0.3)) +
    geom_hline(yintercept = 0, linetype = "solid", color = "black") +
    geom_vline(xintercept = ref_time, linetype = "dashed", color = "black") +
    scale_color_brewer(palette = color_palette) +
    scale_x_continuous(breaks = breaks_pretty()) +
    labs(x = "Years Relative to Treatment",
         y = y_label,
         title = plot_title,
         color = "Specification") +
    theme_minimal() +
    theme(legend.position = "bottom")
  
  ggsave(paste0("output/", output_file_name, "_combined.png"), 
         last_plot(),
         height = 6, 
         width = 8, 
         units = "in")
  
  return(p)
}

plot_combined_event_study(
  result1 = es_total_flows,
  result2 = cs_total_flows,
  result1_structure = "twfe",
  result2_structure = "cs",
  model1_name = " TWFE",
  model2_name = "C&SA (2021)",
  time_range = c(-5, 4),
  y_label = "Change in Number of Inflows",
  plot_title = "",
  output_file_name = "total_flows"
)

plot_combined_event_study(
  result1 = es_poverty_flows,
  result2 = cs_poverty_flows,
  result1_structure = "twfe",
  result2_structure = "cs",
  model1_name = " TWFE",
  model2_name = "C&SA (2021)",
  time_range = c(-5, 4),
  y_label = "Change in Number of Inflows from High-Poverty Tracts",
  plot_title = "",
  output_file_name = "poverty_flows"
)

plot_combined_event_study(
  result1 = es_avg_pov,
  result2 = cs_avg_pov,
  result1_structure = "twfe",
  result2_structure = "cs",
  model1_name = " TWFE",
  model2_name = "C&SA (2021)",
  time_range = c(-5, 4),
  y_label = "Change in Average Inflow Poverty Rate",
  plot_title = "", 
  output_file_name = "avg_pov"
)

plot_combined_event_study(
  result1 = es_high_pov,
  result2 = cs_high_pov,
  result1_structure = "twfe",
  result2_structure = "cs",
  model1_name = " TWFE",
  model2_name = "C&SA (2021)",
  time_range = c(-5, 4),
  y_label = "Change in Average Inflow High Poverty Rate",
  plot_title = "", 
  output_file_name = "high_pov"
)

plot_combined_event_study(
  result1 = es_median_income,
  result2 = cs_median_income,
  result1_structure = "twfe",
  result2_structure = "cs",
  model1_name = " TWFE",
  model2_name = "C&SA (2021)",
  time_range = c(-5, 4),
  y_label = "Change in Average Inflow Median Income",
  plot_title = "",
  output_file_name = "median_income"
)



