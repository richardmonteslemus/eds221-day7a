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

# Practice lubdridate wihin a data frame

urchin_counts <- tribble(
  ~date, ~species, ~size_mm,
  "10/3/2020", "purple", 55,
  "10/4/2020", "red", 48,
  "11/17/2020", "red", 67
)

urchin_counts %>%
  mutate(date = lubridate::mdy(date)) %>%
  mutate(year = year(date),
         month = month(date),
         day = day(date))

day_1 <- lubridate::ymd("2020-01-06")
day_2 <- lubridate::ymd("2020-05-18")
day_3 <- lubridate::ymd("2020-05-19")

# Create time interval
time_interval <- interval(day_1, day_2) # stores both these dates

time_length(time_interval, "week") # estimates number of weeks between days
time_length(time_interval, "year") # estimates number of years between dates


# Practice with stringr

# str_detect() to detect string patterns
# returns true or false depending on wheather the pattern is detected or not

my_string <- "Teddy loves eating salmon and socks."

# does the pattern "love" exsist within the string?
my_string %>%
  str_detect("love") # gives back true


my_string %>%
  str_detect("pup") # gives back false

my_string <- c("burrito", "fish taco", "Taco salad")

# does the vector element contain the pattern "fish"?

my_string %>%
  str_detect("fish") # goes through every vector and checks to see if condition met

# powerful when combined with dplyr functions
starwars %>%
  filter(str_detect(name, "Skywalker"))

firewalker <- starwars %>%
  mutate(name = str_replace(name, pattern = "Sky", replacement = "Fire")) # anywhere there was a sky walker now we have firewalker

# cleaning up white space
feedback <- c(" I ate  some nachos", "Wednesday morning      ")

feedback

# remove the leading, trialing, and duplicate spaces
str_squish(feedback)

# remove just the leading and trailing spaces, keep duplicate
str_trim(feedback)

# convert cases
str_to_lower(feedback)
str_to_upper(feedback)
str_to_sentence(feedback)
str_to_title(feedback)

# count number of matches in a string
str_count(feedback, pattern = "nachos") # first one had nachos mentioned once the second did not

