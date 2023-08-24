### City Health Dasbhoard Supplement
library(tidyverse)
library(tidycensus)
library(sf)
library(viridis)
library(ggthemes)
library(leaflet)
library(htmltools)
library(RColorBrewer)

# Create vector of tracts
tracts <- c(110500, 110600, 110700, 150800, 160501,
            190800, 191412, 191504, 191505, 191806)

# Download medinc data
san_antonio_medinc <- get_acs(
  geography = "tract",
  variables = "B19013_001",
  state = "TX",
  county = "Bexar",
  geometry = TRUE
)
View(san_antonio_medinc)
# Extract last 6 digits of GEOID
san_antonio_medinc <- san_antonio_medinc %>%
  mutate(tract_num = as.numeric(substr(GEOID, nchar(GEOID) - 5, nchar(GEOID))))

# New df filtered
filtered_satx <- san_antonio_medinc %>%
  filter(tract_num %in% tracts)
View(filtered_satx)

# Color palette
num_colors <- length(unique(filtered_satx$estimate))
color_pal <- colorQuantile(brewer.pal(7, "RdPu"),
                           domain = filtered_satx$estimate,
                           n=num_colors)
# Calculate centroids
centroids <- st_centroid(filtered_satx)
View(centroids)

# Plot
leaflet(data = filtered_satx) %>%
  addProviderTiles(providers$CartoDB) %>%  # this provides the basemap
  addPolygons(color = "#444444", weight = 1, smoothFactor = 0.5,
              opacity = 1.0, fillOpacity = 0.5,
              fillColor = ~color_pal(estimate),
              highlightOptions = highlightOptions(color = "white", weight = 2,
                                                  bringToFront = TRUE)) %>%
  addLabelOnlyMarkers(data = centroids, label = ~as.character(tract_num),
                      labelOptions = labelOptions(noHide = TRUE, textOnly = TRUE, direction = "center",
                                                  offset = c(0,1.5),
                                                  style = list("font-weight" = "bold",
                                                               "font-size" = "16px",
                                                               "color" = "black")))

                
              

