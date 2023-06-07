### San Antonio Scratch Work
library(tidycensus)
library(tidyverse)
library(tigris)
library(leaflet)
install.packages("tmap")
library(tmap)
library(viridis)
library(ggthemes)
library(lubridate)
library(ggrepel)
library(sf)

bexar_medhomevalue_21 <- get_acs(
  geography = "tract",
  variables = "B25077_001",
  state = "TX",
  county = "Bexar",
  geometry = TRUE,
  output = "wide", 
  year = 2021
)
# Add a year column
bexar_medhomevalue_10$Year <- 2010
bexar_medhomevalue_11$Year <- 2011
bexar_medhomevalue_12$Year <- 2012
bexar_medhomevalue_13$Year <- 2013
bexar_medhomevalue_14$Year <- 2014
bexar_medhomevalue_15$Year <- 2015
bexar_medhomevalue_16$Year <- 2016
bexar_medhomevalue_17$Year <- 2017
bexar_medhomevalue_18$Year <- 2018
bexar_medhomevalue_19$Year <- 2019
bexar_medhomevalue_20$Year <- 2020
bexar_medhomevalue_21$Year <- 2021
# Combine dataframes
bexar_medhomevalue <- rbind(bexar_medhomevalue_10, bexar_medhomevalue_11, bexar_medhomevalue_12,
                            bexar_medhomevalue_13, bexar_medhomevalue_14, bexar_medhomevalue_15,
                            bexar_medhomevalue_16, bexar_medhomevalue_17, bexar_medhomevalue_18,
                            bexar_medhomevalue_19, bexar_medhomevalue_20, bexar_medhomevalue_21)

# Separate into non OZ and OZ dataframes
bexar_medhomevalue_oz <- bexar_medhomevalue %>%
  filter(GEOID %in% satx_oz$GEOID10)
bexar_medhomevalue_nonoz <- bexar_medhomevalue %>%
  filter(!GEOID %in% satx_oz$GEOID10)
View(bexar_medhomevalue_oz)
View(bexar_medhomevalue_nonoz)
# Group and aggregate
bexar_medhomevalue_nonoz <- bexar_medhomevalue_nonoz %>%
  group_by(Year) %>%
  summarise(Median_Estimate = median(B25077_001E, na.rm=TRUE))
bexar_medhomevalue_oz <- bexar_medhomevalue_oz %>%
  group_by(Year) %>%
  summarise(Median_Estimate = median(B25077_001E, na.rm=TRUE))

# Plot
ggplot(bexar_medhomevalue_oz, aes(Year, Median_Estimate)) +
  geom_line() +
  geom_point() +
  geom_line(data = bexar_medhomevalue_nonoz, aes(Year, Median_Estimate, color = "red")) +
  geom_point(data = bexar_medhomevalue_nonoz, aes(Year, Median_Estimate, color='red')) +
  geom_vline(aes(xintercept = 2017), color = 'black', linetype = "dotdash") +
  annotate("text", x = 2017, y = 175000, label = "Tax Cuts &\nJobs Act Passed (2017)", hjust = -0.1) +
  annotate("text", x = 2012, y = 124000, label = "Non-Opportunity Zone Tracts", color='red') +
  annotate("text", x = 2012, y = 77000, label = "Opportunity Zone Tracts") +
  labs(title = "Median Home Value\nSan Antonio Census Tracts",
       subtitle = "U.S. Census Bureau ACS 1-Yr Survey, 2010-2021",
       caption = "Created with Kyle Walker's tidycensus R package") +
  scale_y_continuous(labels = scales::label_dollar()) +
  guides(col = "none") +
  theme_fivethirtyeight()



