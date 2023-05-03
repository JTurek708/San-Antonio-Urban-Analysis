### San Antonio Zoning & Permit Analysis ###
library(tidyverse)
install.packages("ggthemes")
library(ggthemes)
library(gt)
library(gtExtras)
library(lubridate)
install.packages("tidycensus")
library(tidycensus)
options(scipen = 999)

# Load zoning data
cosa_zoning <- read.csv("COSA_Zoning.csv", header = TRUE,
                        stringsAsFactors = FALSE)
View(cosa_zoning)
unique(cosa_zoning$Zoning)
unique(cosa_zoning$BaseDescription)

# Filter out outside city limites
cosa_zoning <- cosa_zoning %>%
  filter(BaseDescription != "Outside City Limits")

# Check for dupes
which(duplicated(cosa_zoning$SHAPE_Area))

df1 <- cosa_zoning %>%
  group_by(BaseDescription) %>%
  summarise(n = n()) %>%
  arrange(desc(n))
df1 <- df1 %>%
  mutate(Percent_Zone = (n) / 459833)
df1$Percent_Zone <- round((df1$Percent_Zone*100),2)

top_five_zone <- head(df1, 5)
View(top_five_zone)
top_five_zone %>% gt() %>%
  tab_header(
    title = md("**Zoning Percentages - San Antonio, TX**")
  ) %>%
  tab_source_note(md("Source: https://data.sanantonio.gov/dataset/cosa-zoning")) %>%
  cols_label(
    BaseDescription = "Zoning Descriptor",
    n = "Number of Parcels",
    Percent_Zone = "Zoned Percentage"
  ) %>%
  gt_theme_538()

# Residential proportion
res_zoning <- df1 %>%
  filter(grepl("Residential|Family", BaseDescription))
View(res_zoning)
res_zoning <- res_zoning %>%
  mutate(Percent_Zone = (n) / sum(n))
res_zoning$Percent_Zone <- round((res_zoning$Percent_Zone*100), 2)
  
res_zoning %>% gt() %>%
  tab_header(
    title = md("**Residential Zoning Percentages - San Antonio, TX**")
  ) %>%
  tab_source_note(md("Source: https://data.sanantonio.gov/dataset/cosa-zoning")) %>%
  cols_label(
    BaseDescription = "Zoning Descriptor",
    n = "Number of Parcels",
    Percent_Zone = "Zoned Percentage"
  ) %>%
  gt_theme_538()

## Read in permits data
satx_permits_issued <- read.csv("satx_permits_issued.csv", header = TRUE,
                                stringsAsFactors = FALSE)
View(satx_permits_issued)
# Adjust dates
str(satx_permits_issued$DATE_ISSUED)
satx_permits_issued$DATE_ISSUED <- mdy(satx_permits_issued$DATE_ISSUED)
satx_permits_issued$DATE_SUBMITTED <- mdy(satx_permits_issued$DATE_SUBMITTED)

# Add Year column
satx_permits_issued$Issue_Year <- year(satx_permits_issued$DATE_ISSUED)
satx_permits_issued$Submitted_Year <- year(satx_permits_issued$DATE_SUBMITTED)

unique(satx_permits_issued$PERMIT_TYPE)

res_permits <- satx_permits_issued %>%
  filter(str_detect(PERMIT_TYPE, "Res|Residential|Building|Home"))
unique(res_permits$PERMIT_TYPE)
res_permits %>%
  group_by(PERMIT_TYPE) %>%
  summarise(n = n()) %>%
  arrange(desc(n))

# read texas a&m permits
texas_am_permits <- read.csv("permit_data_texasAM.csv", header = TRUE,
                             stringsAsFactors = FALSE)
View(texas_am_permits)
# Filter for San Antonio
satx_permits <- texas_am_permits %>%
  filter(str_detect(area, "Antonio"))
View(satx_permits)

# Convert satx_permits to long format
satx_permits_long <- satx_permits %>%
  select(year, f1units, f24units, f5units)
satx_permits_long <- satx_permits_long %>%
  pivot_longer(cols = c(f1units, f24units, f5units),
               names_to = "factor", values_to = "units")

View(satx_permits_long)
# Graph units and time
ggplot(satx_permits_long, aes(x = year, y = units, color = factor)) +
  geom_line() +
  scale_color_discrete(labels = c("1 Unit Homes", "2-4 Unit Homes", "5+ Unit Homes")) +
  labs(title = "Housing Units Built",
       subtitle = "1980-2021, San Antonio-New Braunfels MSA",
       x = "",
       y = "Housing Units Built",
       caption = "Source: Texas A&M Texas Real Estate Research Center",
       color = NULL) +
  theme_fivethirtyeight()

# Rent from ZORI
satx_zori_rent <- read.csv("satx_zori_index.csv", header = TRUE,
                           stringsAsFactors = FALSE)
View(satx_zori_rent)
# Adjust Month
satx_zori_rent$Month <- mdy(satx_zori_rent$Month)
satx_zori_rent$Year <- year(satx_zori_rent$Month)
satx_zori_rent_yearly <- satx_zori_rent %>%
  group_by(Year) %>%
  mutate(Yearly_Rent = mean(Rental_Index)) %>%
  select(Year, Yearly_Rent) %>%
  group_by(Year)
satx_zori_rent_yearly <- aggregate(
  Yearly_Rent ~ Year, data = satx_zori_rent_yearly,
  FUN = mean
)
satx_zori_rent_yearly
ggplot(satx_zori_rent_yearly, aes(x = Year, y = Yearly_Rent)) +
  geom_line() +
  labs(title = "Zillow Observed Rent Index - All Properties",
       subtitle = "2015-2022, San Antonio, Texas",
       x = "",
       y = "Rental Index Price",
       caption = "Source: Zillow") +
  theme_fivethirtyeight()

# Median home value from Census
years <- 2005:2019
names(years) <- years
satx_value <- map_dfr(years, ~{
  get_acs(
    geography = "county",
    variables = "B25077_001",
    state = "TX",
    county = "Bexar",
    year = .x,
    survey = "acs1"
  )
}, .id = "year")
ggplot(satx_value, aes(x = year, y = estimate, group = 1)) +
  geom_ribbon(aes(ymax = estimate + moe, ymin = estimate - moe),
              fill = "navy",
              alpha = 0.4) +
  geom_line(color = "navy") +
  geom_point(color = "navy", size = 2) +
  theme_fivethirtyeight() +
  scale_y_continuous(labels = scales::label_dollar(scale = 0.001, suffix = "k")) +
  labs(title = "Median Home Value - Bexar County, TX",
       subtitle = "Source: ACS 1-Yr Estimates, tidycensus r package",
       x = "Year", y = "ACS Estimate",
       caption = "Shaded area represents margin of error around the ACS estimate.")
