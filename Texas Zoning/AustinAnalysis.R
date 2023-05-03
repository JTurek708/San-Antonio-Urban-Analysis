### Austin Zoning & Permit Analysis ###
library(tidyverse)
install.packages("ggthemes")
library(ggthemes)
library(gt)
library(gtExtras)
library(lubridate)
install.packages("tidycensus")
library(tidycensus)
options(scipen = 999)

# Load zoning data for Austin
atx_zoning_by_address <- read.csv("atx_zoning_by_address.csv", header = TRUE,
                                  stringsAsFactors = FALSE)
atx_neighborhood_zoning <- read.csv("atx_neighborhood_zoning_summary.csv", header = TRUE,
                                    stringsAsFactors = FALSE)
View(atx_zoning_by_address) # Key col - Base Zoning Category
unique(atx_zoning_by_address$BASE_ZONE_CATEGORY)
View(atx_neighborhood_zoning)# Key cols - General Zoning, Acres
unique(atx_neighborhood_zoning$GENERAL_ZONING)

# Find proportions
df1_address_zoning <- atx_zoning_by_address %>%
  group_by(BASE_ZONE_CATEGORY, BASE_ZONE) %>%
  summarise(Total = n()) %>%
  arrange(desc(Total))
sum(df1_address_zoning$Total)
df1_address_zoning <- df1_address_zoning %>%
  mutate(Percent = Total/258973)
df1_address_zoning <- df1_address_zoning %>%
  select(-c(`2`))
df1_address_zoning
df1_address_zoning$Percent <- round(df1_address_zoning$Percent*100,2)
df1_address_zoning

# Top 5 zoning in ATX
df1_address_zoning <- head(df1_address_zoning, 5)
df1_address_zoning

# Create table for top 5 zoning by address
df1_address_zoning %>% gt() %>%
  tab_header(
    title = md("**Zoning Percentages - Austin, TX**")
  ) %>%
  tab_source_note(md("Source: https://data.austintexas.gov/dataset/Zoning-by-Address/nbzi-qabm")) %>%
  gt_theme_538()

# Find proportion of address zoning for residential
df2_address_zoning <- atx_zoning_by_address %>%
  group_by(BASE_ZONE_CATEGORY, BASE_ZONE) %>%
  summarise(Total = n()) %>%
  arrange(desc(Total))

res_zoning <- df2_address_zoning %>%
  filter(grepl("Mixed|Family|Oriented|Residence", BASE_ZONE_CATEGORY))
sum(res_zoning$Total)
res_zoning <- res_zoning %>%
  mutate(Percent = Total/194451)  
res_zoning$Percent <- round(res_zoning$Percent*100,2)
res_zoning
res_zoning %>% gt() %>%
  tab_header(
    title = md("**Residential Zoning Percentages - Austin, TX**")
  ) %>%
  tab_source_note(md("Source: https://data.austintexas.gov/dataset/Zoning-by-Address/nbzi-qabm")) %>%
  gt_theme_538()

# Planned Unit Development
pud <- atx_zoning_by_address %>%
  filter(BASE_ZONE_CATEGORY == "Planned Unit Development")
View(pud)

# Neighborhood Zoning
atx_neighborhood_zoning %>%
  group_by(NEIGHNAME) %>%
  summarise(Acres = sum(ACRES)) %>%
  arrange(desc(Acres))
atx_neighborhood_zoning %>%
  group_by(GENERAL_ZONING) %>%
  summarise(Acres = sum(ACRES)) %>%
  arrange(desc(Acres))
atx_neighborhood_zoning %>%
  group_by(GENERAL_ZONING) %>%
  summarise(Acres = sum(ACRES)) %>%
  arrange(desc(Acres))

## Austin Issued building permits 
issued_permits <- read.csv("atx_issued_building_permits.csv", header = TRUE,
                           stringsAsFactors = FALSE)
head(issued_permits)
View(issued_permits)
issued_permits$CALENDAR_YEAR_ISSUED <- year(issued_permits$CALENDAR_YEAR_ISSUED)
issued_permits %>%
  filter(WORK_TYPE == "New") %>%
  group_by(SUB_TYPE) %>%
  summarize(Total = sum(CALENDAR_YEAR_ISSUED)) %>%
  arrange(desc(Total))

unique(issued_permits$SUB_TYPE)

atx_housing_permits <- issued_permits %>%
  select(PERMIT_TYPE, SUB_TYPE, WORK_TYPE, CALENDAR_YEAR_ISSUED, STATUS, CONDOMINIUM,
         NUMBER_OF_UNITS) %>%
  filter(str_detect(SUB_TYPE, "Family|Residential|Houses|Mixed|Apartment|Home"))
View(atx_housing_permits)
atx_housing_permits <- atx_housing_permits %>%
  filter(WORK_TYPE == "New")
atx_housing_permits %>%
  group_by(SUB_TYPE) %>%
  summarise(n = n()) %>%
  mutate(Total = sum(n),
         Percentage = n / Total) %>%
  arrange(desc(n))
# Yearly grouping
final_atx_housing_permits = atx_housing_permits %>%
  filter(SUB_TYPE == "R- 101 Single Family Houses", 
         STATUS == "Final") %>%
  group_by(CALENDAR_YEAR_ISSUED) %>%
  summarise(Single_Family_Home_Permits=n()) 
View(final_atx_housing_permits)
final_atx_housing_permits = final_atx_housing_permits %>%
  filter(CALENDAR_YEAR_ISSUED != '2023')
class(final_atx_housing_permits$CALENDAR_YEAR_ISSUED)
final_atx_housing_permits$CALENDAR_YEAR_ISSUED <- as.Date(
  paste0(final_atx_housing_permits$CALENDAR_YEAR_ISSUED, "-01-01")
)
final_atx_housing_permits$CALENDAR_YEAR_ISSUED <- year(final_atx_housing_permits$CALENDAR_YEAR_ISSUED)
class(final_atx_housing_permits$CALENDAR_YEAR_ISSUED)
ggplot(data = final_atx_housing_permits, aes(x = CALENDAR_YEAR_ISSUED, y = Single_Family_Home_Permits)) +
  geom_bar(stat = "identity", width = .75) +
  scale_x_continuous(breaks = final_atx_housing_permits$CALENDAR_YEAR_ISSUED) +
  theme_economist_white() +
  labs(title = "Single Family Home Permits Issued - Austin, TX",
       subtitle = "2006-2022",
       x = "",
       y = "Number of Permits",
       caption = "Source: City of Austin Open Data Portal") +
  theme(axis.text.x = element_text(angle = 35)) +
  geom_text(aes(label = Single_Family_Home_Permits), vjust = -0.5, size = 3.1)

# Median home value from Census
years <- 2005:2019
names(years) <- years
atx_value <- map_dfr(years, ~{
  get_acs(
    geography = "county",
    variables = "B25077_001",
    state = "TX",
    county = "Travis",
    year = .x,
    survey = "acs1"
  )
}, .id = "year")
ggplot(atx_value, aes(x = year, y = estimate, group = 1)) +
  geom_ribbon(aes(ymax = estimate + moe, ymin = estimate - moe),
              fill = "navy",
              alpha = 0.4) +
  geom_line(color = "navy") +
  geom_point(color = "navy", size = 2) +
  theme_fivethirtyeight() +
  theme(axis.text.x = element_text(angle = 35)) +
  scale_y_continuous(labels = scales::label_dollar(scale = 0.001, suffix = "k")) +
  labs(title = "Median Home Value - Travis County, TX",
       subtitle = "Source: ACS 1-Yr Estimates, tidycensus r package",
       x = "Year", y = "ACS Estimate",
       caption = "Shaded area represents margin of error around the ACS estimate.")
