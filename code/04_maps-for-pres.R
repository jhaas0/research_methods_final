#### LOAD IN AND CLEAN MIGRATE DATA ####

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

years <- 2011:2019

nyc_all <- read_rds(paste0("data/clean/nyc_all_years.rds"))

neighborhood_boundaries <- st_read("data/nynta2010_25d/nynta2010.shp")


ny_tracts <- tracts("NY", year = "2010")

bushwick <- neighborhood_boundaries %>%
  filter(NTAName %in% c("Bushwick South", "Bushwick North")) %>% 
  summarize(geometry = st_union(geometry)) %>% 
  mutate(neighborhood = "Bushwick")

ny_tracts <- st_transform(ny_tracts, st_crs(bushwick))

bushwick_tracts <- st_join(
  ny_tracts,
  bushwick,
  join = st_intersects
) %>% 
  filter(neighborhood == "Bushwick") %>% 
  pull(GEOID10)



#### ---- EXPLORE DATA ---- ####

nyc_moves <- nyc_all %>% 
  filter(origin != destination)


#get brooklyn flows by county
brooklyn_flows <- nyc_moves %>% 
  filter(str_sub(destination, 1, 5) == "36047") %>%
  mutate(same_cbg = origin == destination,
         same_tract = str_sub(origin, 1, 11) == str_sub(destination, 1, 11),
         same_county = str_sub(origin, 1, 5) == str_sub(destination, 1, 5), 
         same_state = str_sub(origin, 1, 2) == str_sub(destination, 1, 2)) %>% 
  group_by(year) %>% 
  summarize(pct_same_county = sum(flow[same_county], na.rm = TRUE) /
              sum(flow, na.rm = TRUE),
            pct_same_state  = sum(flow[same_state],  na.rm = TRUE) /
              sum(flow, na.rm = TRUE))


#get bushwick's flows
bushwick_flows <- nyc_moves %>% 
  filter(str_sub(destination, 1, 11) %in% bushwick_tracts) %>%
  mutate(same_neighborhood = str_sub(origin, 1, 11) %in% bushwick_tracts,
         same_county = str_sub(origin, 1, 5) == str_sub(destination, 1, 5), 
         same_state = str_sub(origin, 1, 2) == str_sub(destination, 1, 2)) %>% 
  group_by(year) %>% 
  summarize(pct_same_neighborhood = sum(flow[same_neighborhood], na.rm = TRUE) /
              sum(flow, na.rm = TRUE), 
            pct_same_county = sum(flow[same_county], na.rm = TRUE) /
              sum(flow, na.rm = TRUE),
            pct_same_state  = sum(flow[same_state],  na.rm = TRUE) /
              sum(flow, na.rm = TRUE))

write_csv(bushwick_flows, "output/bushwick_flows.csv")


# 
# #create flow map for brooklyn
# nyc_tracts <- ny_tracts %>% 
#   left_join(fabian_flow, by = c("GEOID" = "origin_tract")) %>% 
#   #filter to just nyc for now
#   filter(str_detect(GEOID, "^(36005|36047|36061|36081|36085)")) %>% 
#   mutate(tract_flow_groups = )


#### Create Map ####

# Get US counties shapefile
us_counties <- counties(cb = TRUE, year = 2019) %>% 
  st_transform(4326)


##### 2019 map - outside state ####

#get bushwick flows by county
bushwick_flows_for_map <- nyc_moves %>% 
  filter(str_sub(destination, 1, 11) %in% bushwick_tracts) %>%
  mutate(origin_county = str_sub(origin, 1, 5), 
         destination_county = str_sub(destination, 1, 5), 
         same_county = str_sub(origin, 1, 5) == str_sub(destination, 1, 5), 
         same_state = str_sub(origin, 1, 2) == str_sub(destination, 1, 2)) %>% 
  filter(!same_state) %>% 
  group_by(year, origin_county) %>% 
  summarize(total_flow = sum(flow)) %>% 
  group_by(year) %>% 
  arrange(desc(total_flow)) %>% 
  slice(1:20) %>% 
  ungroup()

#2019 map - outside state
bushwick_flows_for_map_2019 <- bushwick_flows_for_map %>% 
  filter(year == 2019)

bushwick_flow_map <- us_counties %>% 
  mutate(GEOID = paste0(STATEFP, COUNTYFP)) %>% 
  filter(GEOID %in% bushwick_flows_for_map_2019$origin_county) %>% 
  left_join(bushwick_flows_for_map_2019, by = c("GEOID" = "origin_county")) %>% 
  st_centroid()

bushwick_centroid <- bushwick %>% 
  st_centroid() %>% 
  st_transform(st_crs(bushwick_flow_map))

# Create flow lines
bushwick_lines <- bushwick_flow_map %>% 
  st_drop_geometry() %>% 
  bind_cols(st_coordinates(bushwick_flow_map) %>% as_tibble()) %>% 
  mutate(dest_X = st_coordinates(bushwick_centroid)[1],
         dest_Y = st_coordinates(bushwick_centroid)[2])

# Plot Bushwick flows
ggplot() +
  geom_sf(data = us_counties, fill = "gray95", color = "white", size = 0.1) +
  #geom_sf(data = brooklyn, fill = "gray80", color = "white", size = 0.3) +
  geom_sf(data = bushwick, fill = "#2C5F8D", color = "white", size = 0.5) +
  geom_curve(
    data = bushwick_lines,
    aes(
      x = X, y = Y,
      xend = dest_X, yend = dest_Y,
      size = total_flow,
      alpha = total_flow
    ),
    color = "#D55E00",
    curvature = -0.2,
    arrow = arrow(length = unit(0.2, "cm"))
  ) + 
  geom_sf(data = bushwick_flow_map, aes(size = total_flow), 
          color = "#D55E00", alpha = 0.6) +
  scale_size_continuous(range = c(0.5, 3), name = "Migration Flow") +
  scale_alpha_continuous(range = c(0.4, 0.8), guide = "none") +
  coord_sf(xlim = c(-125, -65), ylim = c(25, 50)) +
  labs(title = "Top Migration Inflows to Bushwick",
       subtitle = "By origin county (2019)") +
  theme_minimal() +
  theme(legend.position = "bottom", 
        axis.text = element_blank(), 
        axis.title = element_blank(), 
        plot.title = element_text(size = 20, face = "bold"),
        plot.subtitle = element_text(size = 14))

ggsave("output/bushwick_map_1.png", 
       last_plot(), 
       height = 6, 
       width = 12, 
       units = "in")


##### get map just for ny ####

#get bushwick flows by county
bushwick_flows_for_map_within_state <- nyc_moves %>% 
  filter(str_sub(destination, 1, 11) %in% bushwick_tracts) %>%
  mutate(origin_county = str_sub(origin, 1, 5), 
         destination_county = str_sub(destination, 1, 5), 
         same_neighborhood = str_sub(origin, 1, 11) %in% bushwick_tracts,
         same_county = str_sub(origin, 1, 5) == str_sub(destination, 1, 5), 
         same_state = str_sub(origin, 1, 2) == str_sub(destination, 1, 2)) %>% 
  filter(same_state, 
         !same_neighborhood) %>% 
  group_by(year, origin_county) %>% 
  summarize(total_flow = sum(flow)) %>% 
  group_by(year) %>% 
  arrange(desc(total_flow)) %>% 
  ungroup() %>% 
  filter(str_detect(origin_county, "^(36005|36047|36061|36081|36085)"))

#2019 map
bushwick_ny_flows_for_map_2019 <- bushwick_flows_for_map_within_state %>% 
  filter(year == 2019)

bushwick_flow_map_within <- us_counties %>%
  filter(STATEFP == "36") %>% 
  mutate(GEOID = paste0(STATEFP, COUNTYFP)) %>% 
  filter(GEOID %in% bushwick_ny_flows_for_map_2019$origin_county) %>% 
  left_join(bushwick_ny_flows_for_map_2019, by = c("GEOID" = "origin_county")) %>% 
  st_centroid()

bushwick_centroid <- bushwick %>% 
  st_centroid() %>% 
  st_transform(st_crs(bushwick_flow_map_within))


# Create flow lines
bushwick_lines <- bushwick_flow_map_within %>% 
  st_drop_geometry() %>% 
  bind_cols(st_coordinates(bushwick_flow_map_within) %>% as_tibble()) %>% 
  mutate(dest_X = st_coordinates(bushwick_centroid)[1],
         dest_Y = st_coordinates(bushwick_centroid)[2])

# Plot Bushwick flows
ggplot() +
  geom_sf(data = us_counties %>% filter(STATEFP == "36" & str_detect(COUNTYFP, "^(005|047|061|081|085)")) ,
          fill = "gray95", color = "white", size = 0.1) +
  #geom_sf(data = brooklyn, fill = "gray80", color = "white", size = 0.3) +
  geom_sf(data = bushwick, fill = "#2C5F8D", color = "white", size = 0.5) +
  geom_curve(
    data = bushwick_lines,
    aes(
      x = X, y = Y,
      xend = dest_X, yend = dest_Y,
      size = total_flow,
      alpha = total_flow
    ),
    color = "#D55E00",
    curvature = -0.2,
    arrow = arrow(length = unit(0.2, "cm"))
  ) + 
  geom_sf(data = bushwick_flow_map_within, aes(size = total_flow), 
          color = "#D55E00", alpha = 0.6) +
  scale_size_continuous(range = c(0.5, 3), name = "Migration Flow") +
  scale_alpha_continuous(range = c(0.4, 0.8), guide = "none") +
  #coord_sf(xlim = c(-125, -65), ylim = c(25, 50)) +
  labs(title = "Top Migration Inflows to Bushwick: NYC",
       subtitle = "By origin county (2019)") +
  theme_minimal() +
  theme(legend.position = "bottom", 
        axis.text = element_blank(), 
        axis.title = element_blank())


ggsave("output/bushwick_map_2.png", 
       last_plot(), 
       height = 6, 
       width = 12, 
       units = "in")


#### ----- Bushwick NYC map by tract ---- ####

nyc_tracts <- ny_tracts %>% 
  filter(str_detect(GEOID10, "^36(005|047|061|081|085)")) %>% 
  mutate(
    pct_land = ALAND10 / (ALAND10 + AWATER10)
  ) %>% 
  filter(pct_land > 0.05)

# Bushwick inflows by TRACT (within NYC)
bushwick_flows_for_map_within_state <- nyc_moves %>% 
  filter(str_sub(destination, 1, 11) %in% bushwick_tracts) %>%
  mutate(
    origin_tract = str_sub(origin, 1, 11),
    destination_tract = str_sub(destination, 1, 11),
    same_neighborhood = origin_tract %in% bushwick_tracts,
    same_county = str_sub(origin, 1, 5) == str_sub(destination, 1, 5),
    same_state = str_sub(origin, 1, 2) == str_sub(destination, 1, 2)
  ) %>% 
  filter(
    same_state,
    !same_neighborhood
  ) %>% 
  group_by(year, origin_tract) %>% 
  summarize(total_flow = sum(flow), .groups = "drop") %>% 
  filter(str_detect(origin_tract, "^36(005|047|061|081|085)"))


bushwick_ny_flows_for_map_2019 <- bushwick_flows_for_map_within_state %>% 
  filter(year == 2019) %>% 
  mutate(
    flow_bucket = cut(
      total_flow,
      breaks = seq(0, max(total_flow, na.rm = TRUE) + 5, by = 5),
      right = FALSE,
      include.lowest = TRUE
    )
  )


bushwick_flow_map_within <- nyc_tracts %>%
  filter(GEOID10 %in% bushwick_ny_flows_for_map_2019$origin_tract) %>% 
  left_join(
    bushwick_ny_flows_for_map_2019,
    by = c("GEOID10" = "origin_tract")
  ) %>% 
  st_centroid()

bushwick_centroid <- bushwick %>% 
  st_centroid() %>% 
  st_transform(st_crs(bushwick_flow_map_within))


bushwick_lines <- bushwick_flow_map_within %>% 
  st_drop_geometry() %>% 
  bind_cols(
    st_coordinates(bushwick_flow_map_within) %>% as_tibble()
  ) %>% 
  mutate(
    dest_X = st_coordinates(bushwick_centroid)[1],
    dest_Y = st_coordinates(bushwick_centroid)[2]
  )


ggplot() +
  geom_sf(
    data = nyc_tracts,
    fill = "gray95",
    color = "darkgrey",
    size = 0.05
  ) +
  geom_sf(
    data = bushwick,
    fill = "#2C5F8D",
    color = "white",
    size = 0.5
  ) +
  # geom_curve(
  #   data = bushwick_lines,
  #   aes(
  #     x = X, y = Y,
  #     xend = dest_X, yend = dest_Y,
  #     size = total_flow,
  #     alpha = total_flow
  #   ),
  #   color = "#D55E00",
  #   curvature = -0.2,
  #   arrow = arrow(length = unit(0.2, "cm"))
  # ) +
  geom_sf(
    data = bushwick_flow_map_within,
    aes(size = total_flow),
    color = "#D55E00",
    alpha = 0.6
  ) +
  scale_size_continuous(range = c(0.3, 2.5), name = "Migration Flow") +
  scale_alpha_continuous(range = c(0.4, 0.8), guide = "none") +
  labs(
    title = "Top Migration Inflows to Bushwick: NYC",
    subtitle = "By origin census tract (2019)"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom", 
    axis.text = element_blank(), 
    axis.title = element_blank()
  )

ggsave("output/bushwick_map_3.png", 
       last_plot(), 
       height = 6, 
       width = 12, 
       units = "in")
