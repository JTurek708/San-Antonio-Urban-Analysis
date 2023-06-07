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

options(tigris_use_cache = TRUE)
# Try loading variables
v2020acs5 <- load_variables(2020, "acs5")
View(v2020acs5)

# Median Household Income by Tract
satx_income <- get_acs(
  geography = "tract",
  variables = "B19013_001",
  county = "Bexar",
  state = "TX",
  year = 2020,
  geometry = TRUE
)
View(satx_income)
ggplot(satx_income) +
  geom_sf(aes(fill = estimate)) +
  scale_fill_viridis_c(labels = scales::label_dollar()) +
  theme_void() +
  labs(fill = "Median Household\nIncome")

# Read in Opportunity Zone csv
getwd()
satx_oz <- read.csv('opportunity_zone.csv', header = TRUE, 
                    stringsAsFactors = FALSE)
View(satx_oz)

# Visualize median household income in OZ tracts over time Bexar County
years <- 2006:2019
names(years) <- years
oz_medhhinc_2022 <- get_acs(
  geography = "tract",
  variables = "B19013_001",
  county = "Bexar",
  state = "TX",
  year = 2022,
  geometry = TRUE
)
View(oz_medhhinc_2020)

# Add year columsn to each data frame
oz_medhhinc_2010$Year <- 2010
oz_medhhinc_2011$Year <- 2011
oz_medhhinc_2012$Year <- 2012
oz_medhhinc_2013$Year <- 2013
oz_medhhinc_2014$Year <- 2014
oz_medhhinc_2015$Year <- 2015
oz_medhhinc_2016$Year <- 2016
oz_medhhinc_2017$Year <- 2017
oz_medhhinc_2018$Year <- 2018
oz_medhhinc_2019$Year <- 2019
oz_medhhinc_2020$Year <- 2020
oz_medhhinc_2021$Year <- 2021

# Bind together
oz_medhhinc_total <- rbind(oz_medhhinc_2010, oz_medhhinc_2011, oz_medhhinc_2012, oz_medhhinc_2013,
                     oz_medhhinc_2014, oz_medhhinc_2015, oz_medhhinc_2016, oz_medhhinc_2017,
                     oz_medhhinc_2018, oz_medhhinc_2019, oz_medhhinc_2020, oz_medhhinc_2021)

View(oz_medhhinc_total)

# Filter for Opportunity Zones
oz_medhhinc <- oz_medhhinc %>%
  filter(GEOID %in% satx_oz$GEOID10)

bexar_medhhinc_no_oz <- oz_medhhinc_total %>%
  filter(!GEOID %in% satx_oz$GEOID10)
View(bexar_medhhinc_no_oz)

bexar_medhhinc_agg <- bexar_medhhinc_no_oz %>%
  group_by(Year) %>%
  summarise(Median_Estimate = median(estimate, na.rm=TRUE))
View(bexar_medhhinc_agg)

oz_yoy_filtered <- oz_medhhinc %>%
  arrange(NAME, Year) %>%
  group_by(NAME) %>%
  mutate(YoY_Change = (estimate - lag(estimate)) / lag(estimate)*100)
View(oz_yoy_filtered)

oz_yoy_agg <- oz_yoy_filtered %>%
  group_by(Year) %>%
  summarise(Median_Estimate = median(estimate, na.rm=TRUE))
View(oz_yoy_agg)

# Add labels column to data frames
bexar_medhhinc_agg$lable = "Non-Opportunity Zone Tracts"
oz_yoy_agg$label = "Opportunity Zone Tract"

ggplot(oz_yoy_agg, aes(Year, Median_Estimate)) +
  geom_line() +
  geom_point() +
  geom_line(data = bexar_medhhinc_agg, aes(Year, Median_Estimate, color = "red")) +
  geom_point(data = bexar_medhhinc_agg, aes(Year, Median_Estimate, color='red')) +
  geom_vline(aes(xintercept = 2017), color = 'black', linetype = "dotdash") +
  annotate("text", x = 2017, y = min(bexar_medhhinc_agg$Median_Estimate), label = "Tax Cuts &\nJobs Act Passed (2017)", hjust = -0.1) +
  annotate("text", x = 2012, y = 51250, label = "Non-Opportunity Zone Tracts", color='red') +
  annotate("text", x = 2012, y = 39000, label = "Opportunity Zone Tracts") +
  labs(title = "Median Household Income\nSan Antonio Census Tracts",
       subtitle = "U.S. Census Bureau ACS 1-Yr Survey, 2010-2021",
       caption = "Created with Kyle Walker's tidycensus R package") +
  guides(col = "none") +
  scale_y_continuous(labels = scales::label_dollar()) +
  theme_fivethirtyeight()

########################################
# Med Income by Racial Group
install.packages("ggbeeswarm")
library(ggbeeswarm)

# Download data
satx_race_income <- get_acs(
  geography = "tract",
  state = "TX",
  county = "Bexar",
  variables = c(White = "B03002_003",
                Black = "B03002_004",
                Asian = "B03002_006",
                Hispanic = "B03002_012"),
  summary_var = "B19013_001", 
  year = 2021
) %>%
  group_by(GEOID) %>%
  filter(estimate == max(estimate, na.rm = TRUE)) %>%
  ungroup() %>%
  filter(estimate != 0)
# Split into OZ and non-OZ dfs
satx_race_income_oz <- satx_race_income %>%
  filter(GEOID %in% satx_oz$GEOID)
satx_race_income_non_oz <- satx_race_income %>%
  filter(!GEOID %in% satx_oz$GEOID)

# Group by GEOID and get max estimate
satx_race_income_oz <- satx_race_income_oz %>%
  group_by(GEOID) %>%
  filter(estimate == max(estimate, na.rm=TRUE)) %>%
  ungroup() %>%
  filter(estimate!=0)
satx_race_income_non_oz <- satx_race_income_non_oz %>%
  group_by(GEOID) %>%
  filter(estimate == max(estimate, na.rm=TRUE)) %>%
  ungroup() %>%
  filter(estimate!=0)

# Plot swarm plots
ggplot(satx_race_income, aes(x = variable, y = summary_est, color = summary_est)) +
  geom_quasirandom(alpha = 0.5) +
  coord_flip() +
  theme_fivethirtyeight() +
  scale_color_viridis_c(guide = "none") +
  scale_y_continuous(labels = scales::label_dollar()) +
  labs(x = "Largest Group in Census Tract",
       y = "Median Household Income",
       title = "Household Income Distribution by\nLargest Racial/Ethnic Group",
       subtitle = "Census Tracts, San Antonio, TX",
       caption = "Data source: 2017-2021 ACS")

# Dot plot of race in Bexar Co
bexar_race <- get_decennial(
  geography = "tract",
  state = "TX",
  county = "Bexar",
  variables = c(
    Hispanic = "P2_002N",
    White = "P2_005N",
    Black = "P2_006N",
    Native = "P2_007N",
    Asian = "P2_008N"
  ),
  summary_var = "P2_001N",
  year = 2020, 
  geometry = TRUE
) %>%
  mutate(Percent = 100 * (value/summary_value))
View(bexar_race)
bexar_dots <- bexar_race %>%
  as_dot_density(
    value = "value",
    values_per_dot = 100,
    group = "variable"
  )
View(bexar_dots)
bexar_dots_oz <- bexar_dots %>%
  filter(GEOID %in% satx_oz$GEOID10)
bexar_dots_non_oz <- bexar_dots %>%
  filter(!GEOID %in% satx_oz$GEOID10)
background_tracts <- filter(bexar_race, variable == "White")

tm_shape(background_tracts) +
  tm_polygons(col = "white",
              border.col = "grey") +
  tm_shape(bexar_dots_non_oz) +
  tm_dots(col = "variable",
          palette = "Set3",
          size = 0.02,
          title = "1 dot = 100 people") +
  tm_layout(legend.outside = TRUE,
            title = "Race/Ethnicity,\n2020 US Census",
            title.size = 1)


