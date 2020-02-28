# Time series analysis in data science
library(dplyr)
library(readr)
library(anytime)
library(lubridate)
library(ggplot2)

# Get information on R version releases and downloads
url <- "https://assets.datacamp.com/production/repositories/1435/datasets/603b5835e87ce55c22221491406854cecf213898/rversions.csv"
releases <- read_csv(url)

head(releases)
str(releases)

# combine into a version number
releases <- releases %>% mutate(version = paste(major, minor, na.fill(patch, fill = 0), sep = "."))

release_time <- releases$datetime

# Examine the months, years, etc
month(release_time) %>% table()

month(release_time) %>% data_frame() %>% 
  ggplot(aes(x = .)) +
  geom_histogram(binwidth = 1)

year(release_time) %>% table() %>% prop.table()

# How often is the release in am?
mean(am(release_time))

# Use wday() to tabulate release by day of the week
wday(releases$datetime) %>% table()

# Add label = TRUE to make table more readable
wday(releases$datetime, label = TRUE) %>% table()

# Create column wday to hold labelled week days
releases$wday <- wday(releases$datetime, label = TRUE)

# Plot barchart of weekday by type of release
ggplot(releases, aes(x = wday)) +
  geom_bar() +
  facet_wrap(~ type, ncol = 1, scale = "free_y")

r_3_4_1 <- releases$datetime[releases$version == "3.4.1"]

# Round down to day
round_date(r_3_4_1, unit = "day")

# Round to nearest 5 minutes
round_date(r_3_4_1, unit = "5 minutes")

# Round up to week 
ceiling_date(r_3_4_1, unit = "week")

# Subtract r_3_4_1 rounded down to day
r_3_4_1 - floor_date(r_3_4_1, unit = "day")