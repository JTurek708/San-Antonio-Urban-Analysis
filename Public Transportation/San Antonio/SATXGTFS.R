### San Antonio GTFS
install.packages("gtfsio")
install.packages('tidytransit')
library(tidytransit)
library(tidyverse)
library(sf)
library(gtfsio)
library(ggmap)
library(lubridate)
library(tmap)
tmap_mode("view")
library(leaflet)
library(gt)
###############################################################################
gtfs_data <- read_gtfs("google_transit(6).zip")
summary(gtfs_data)
head(gtfs_data$stops)
names(gtfs_data)
validation_result <- attr(gtfs_data, "validation_result")
head(validation_result)

# Set service patterns
gtfs_data <- set_servicepattern(gtfs_data)

# Summarise each service by number of trips and stops; also summarise total 
# distance covered by all trips in service; check against total distance 
# covered by average route
# First - calculate distance of each part of the route shapes
gtfs_data <- gtfs_as_sf(gtfs_data)
gtfs_data$shapes$length <- st_length(gtfs_data$shapes)

shape_lengths <- gtfs_data$shapes %>%
  as.data.frame() %>%
  select(shape_id, length, -geometry)

# Roll stats up to services
service_pattern_summary <- gtfs_data$trips %>%
  left_join(gtfs_data$.$servicepatterns, by='service_id') %>%
  left_join(shape_lengths, by='shape_id') %>%
  left_join(gtfs_data$stop_times, by='trip_id') %>%
  group_by(servicepattern_id) %>%
  summarise(
    trips = n(),
    routes = n_distinct(route_id),
    total_dist_per_day_km = sum(as.numeric(length), na.rm = TRUE)/1e3,
    route_avg_dist_km = (sum(as.numeric(length), na.rm = TRUE)/1e3)/(trips*routes),
    stops = (n_distinct(stop_id)/2)
  )
View(service_pattern_summary)
# Add num of days each service is in operation
service_pattern_summary <- gtfs_data$.$dates_servicepatterns %>%
  group_by(servicepattern_id) %>%
  summarise(days_in_service = n()) %>%
  left_join(service_pattern_summary, by='servicepattern_id')

# Use s_d7b836e to pull out service_ids to ID trips in GTFS feed
service_ids <- gtfs_data$.$servicepattern %>%
  filter(servicepattern_id == 's_d7b836e') %>%
  pull(service_id)
head(service_ids) %>%
  knitr::kable()
View(service_ids)

# How many trips fall under the service id
gtfs_data$trips %>%
  filter(service_id %in% service_ids) %>%
  group_by(service_id, route_id) %>%
  summarise(count = n()) %>%
  head() %>%
  knitr::kable()

# Calculate service between 0600 & 1000
am_stop_freq <- get_stop_frequency(gtfs_data, start_time = 6*3600, end_time = 10*3600,
                                   service_ids = service_ids, by_route = TRUE)
knitr::kable(head(am_stop_freq))
View(am_stop_freq)

# Find headways for 552, 20, 2, 3, and 4 bus routes
five_five_two <- get_stop_frequency(gtfs_data, start_time = 6*3600, end_time = 18*3600,
                                    service_ids = service_ids, by_route = 552)
five_five_two <- five_five_two %>%
  filter(route_id == 552)
five_five_two <- five_five_two %>%
  mutate(mean_headway_min = mean_headway/60)
mean(five_five_two$mean_headway_min)

twenty <- get_stop_frequency(gtfs_data, start_time = 6*3600, end_time = 18*3600,
                                    service_ids = service_ids, by_route = TRUE)
twenty <- twenty %>%
  filter(route_id == 20) %>%
  mutate(mean_headway_min = mean_headway/60)
mean(twenty$mean_headway_min)
# 2, 3, 4 bus
two_three_four <- get_stop_frequency(gtfs_data, start_time = 16*3600, 
                                     end_time = 19*3600, 
                                     service_ids = service_ids, by_route = TRUE)
two_three_four <- two_three_four %>%
  filter(route_id == 2 | route_id == 3 | route_id == 4) %>%
  mutate(mean_headway_min = mean_headway/60)
View(two_three_four)
# Total average
avg_headway <- two_three_four %>%
  group_by(route_id) %>%
  summarise(mean_headway_mins = mean(mean_headway_min, na.rm = TRUE)) %>%
  ungroup()
avg_headway %>%
  gt() %>%
  tab_header(
    title = "Mean Headways for Routes 2, 3, 4",
    subtitle = "VIA Metropolitan Transit (1600-1900)"
  ) %>%
  cols_label(
    route_id = "Route Number",
    mean_headway_mins = "Mean Headway (min)"
  ) %>%
  fmt_number(
    columns = vars(mean_headway_mins),
    decimals = 0
  ) %>%
  tab_options(
    table.width = px(400),
    table.font.size = px(16)
  )

# Find headways for 2, 3, 4 bus 
four_bus_stops <- am_stop_freq %>%
  filter(route_id == 4 & direction_id == 0) %>%
  left_join(gtfs_data$stops, by='stop_id') %>%
  mutate(mean_headway_minutes = mean_headway/60)
View(two_bus_stops)
# Higher headways
four_bus_stops %>%
  arrange(desc(mean_headway)) %>%
  select(stop_name, n_departures, mean_headway) %>%
  head() %>%
  knitr::kable()
# Lower headways
four_bus_stops %>%
  arrange(desc(mean_headway)) %>%
  select(stop_name, n_departures, mean_headway) %>%
  tail() %>%
  knitr::kable()

# Plot 
four_bus_stops_sf <- gtfs_data$stops %>%
  right_join(four_bus_stops, by='stop_id')
View(four_bus_stops_sf)

four_bus_stops_sf %>%
  ggplot() +
  geom_sf(aes(color=mean_headway_minutes)) +
  theme_minimal() +
  labs(title = "#4 - San Pedro Frequent Headways",
       subtitle = "6:00 AM - 10:00 AM",
       color = "Average Headway (min)")+
  theme(axis.title.x=element_blank(),    # Removes x-axis title
        axis.text.x=element_blank(),    # Removes x-axis text
        axis.title.y=element_blank(),   # Removes y-axis title
        axis.text.y=element_blank(),    # Removes y-axis text
        axis.ticks.x=element_blank(),   # Removes x-axis ticks
        axis.ticks.y=element_blank()) 

# Summary
summary(two_bus_stops$mean_headway)
################################################################################
# Map Headways by Route
am_route_freq <- get_route_frequency(gtfs_data, service_ids = service_ids,
                                     start_time = 6*3600, end_time = 10*3600)
head(am_route_freq)%>%
  knitr::kable()
# Join geometries to calculated frequencies
routes_sf <- get_route_geometry(gtfs_data, service_ids = service_ids)
routes_sf <- routes_sf %>%
  inner_join(am_route_freq, by='route_id')
# Plot routes with median headways of 15 minutes or less
routes_sf_crs <- sf::st_transform(routes_sf, 26919)
routes_sf_crs %>%
  filter(median_headways < 15*60) %>%
  ggplot() +
  geom_sf(aes(color=as.factor(median_headways))) +
  labs(color="Headways") +
  geom_sf_text(aes(label=route_id)) +
  theme_bw()
