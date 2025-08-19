# Clear environment
rm(list = ls())

# Load packages
library(lubridate)
library(tidyverse)
library(palmerpenguins)

# Data wrangling refresher
# 1. Only include penguins at Briscoe and Dream Islands
# 2. Remove the year and sex variables
# 3. Add a new column called body_mass_kg with penguin mass converted from grams to kilograms
# 4. Rename the island variable to location

penguins_data <- penguins %>%
  filter(island == c("Biscoe", "Dream")) %>% # %in% is also good to use because even when they arent equal length we can filter
  select(-c(year, sex)) %>%
  mutate(body_mass_kg = body_mass_g/1000) %>%
  rename(location = island)


# 1. Limit to only Adelie penguin
# 2. Remove any observation where flipper_length_mm is NA
# 3. Group the data by sex
# 4. Find the mean, standard deviation, and sample size (n()) of flipper lengths for make and females.

penguins_adelie <- penguins %>%
  filter(species == "Adelie") %>%
  na.omit() %>%
  group_by(sex) %>%
  summarise(flipper_length_mean = mean(flipper_length_mm),
            standard_dev = sd(flipper_length_mm),
            sample_size = n() )

# Practice with joins
animals <- data.frame(
  stringsAsFactors = FALSE,
  location = c("lagoon", "bluff", "creek", "oaks", "bluff"),
  species = c("bobcat", "coyote", "fox", "squirrel", "bobcat"),
  maturity = c("adult", "juvenile", "adult", "juvenile", "adult")
)


sites <- data.frame(
  stringsAsFactors = FALSE,
  location = c("beach", "lagoon", "bluff", "oaks"),
  full_site_name = c("Goleta Beach","UCSB Lagoon",
                     "Ellwood Mesa","Fremont Campground"),
  jurisdiction = c("SB City", "UCSB", "SB City", "USFS")
)

# Practice with full_join
# keeps all rows and adds all columns
full_join(animals, sites)

# Practice with left_join
left_join(animals, sites)


right_join(animals, sites)

inner_join(animals, sites)

# Filtering joins
semi_join(animals, sites)

animals %>%
  filter(location %in% sites$location)

anti_join(animals, sites) # filter first data frame based on whats in the second df

animals %>%
  filter(!location %in% sites$location)


anti_join(sites, animals)

library(lubridate)
# Practice with lubridate
my_date <- "03-13-1998"
lubridate::mdy(my_date) # fixed date to ISO 8601

my_date <- "08-Jun-1974"
lubridate::dmy(my_date)

# another example of different format
my_date <- "19160518"
lubridate::ymd(my_date)

# what happens if we give lubridate a date that doesnt make sense
# lubridate::mdy("1942-08-30") could not do this because there is no month that is 1942

lubridate::dmy("09/12/84")

# Working with date times

time <- "2020-08-12 11:18"

lubridate::ymd_hm(time)

# Convert to PDT
with_tz(time, "America/Los_Angeles")

# extract info from dates
lubridate::week(time)
lubridate::day(time)
lubridate::year(time)

start_time <- Sys.time()
end_time <- Sys.time()
start_time - end_time

