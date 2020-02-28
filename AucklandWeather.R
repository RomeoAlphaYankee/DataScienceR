# Time series analysis in data science
library(dplyr)
library(readr)
library(anytime)
library(lubridate)
library(ggplot2)

# Retreive some time series data
url <- "https://assets.datacamp.com/production/repositories/1435/datasets/f6590278193112325a874cb69cb94d7fbca5732f/akl_weather_daily.csv"

akl_daily_raw <- read_csv(url)

# Check the data on Auckland daily temperatures
str(akl_daily_raw)

# Parse date 
akl_daily <- akl_daily_raw %>%
  mutate(date = ymd(date))

# Print akl_daily
akl_daily

# Plot to check work
ggplot(akl_daily, aes(x = date, y = max_temp)) +
  geom_line(color = "red") +
  geom_line(data = akl_daily, aes(y = min_temp), color = "blue") +
  scale_x_date(limits = c(as.Date("2010-01-01"), as.Date("2015-01-30")))

# Add regression to check for global warming
melt(akl_daily, id.vars = "date", measure.vars = c("max_temp", "min_temp")) %>%
  ggplot(aes(x = date, y = value, color = variable)) +
  geom_line() +
  geom_smooth(method = "lm", se = FALSE) +
  scale_color_manual(values = c("red", "blue"))

# Get the hourly data
url <- "https://assets.datacamp.com/production/repositories/1435/datasets/0b0ef636e2a69936ef8236108bfd2261647f5e9e/akl_weather_hourly_2016.csv"

akl_hourly_raw <- read_csv(url)

str(akl_hourly_raw)

# Parse the month, day into a date vector
akl_hourly <- akl_hourly_raw %>%
  mutate(date = make_date(year = year, month = month, day = mday))


# Parse datetime_string 
akl_hourly <- akl_hourly  %>% 
  mutate(
    datetime_string = paste(date, time, sep = "T"),
    datetime = ymd_hms(datetime_string)
  )

# Print date, time and datetime columns of akl_hourly
akl_hourly %>% select(date, time, datetime)

# Plot to check work
ggplot(akl_hourly, aes(x = datetime, y = temperature)) +
  geom_line()

# Other lubridate functions
hour(akl_hourly$datetime)
am(akl_hourly$datetime)

# Check for leap years
yday(akl_daily$date) == 366
akl_daily$date[which(month(akl_daily$date) == 2 & day(akl_daily$date) == 29)]

# Lets look at the Auckland temperatures again
library(ggridges)

# Add columns for year, yday and month
akl_daily <- akl_daily %>%
  mutate(
    year = year(date),
    yday = yday(date),
    month = month(date, label = TRUE))

# Plot max_temp by yday for all years
ggplot(akl_daily, aes(x = yday, y = max_temp)) +
  geom_line(aes(group = year), alpha = 0.5)

# Examine distribution of max_temp by month
ggplot(akl_daily, aes(x = max_temp, y = month, height = ..density..)) +
  geom_density_ridges(stat = "density") +
  ggtitle("Auckland, New Zeland", subtitle = "Maximum Temperatures 2007 - 2017")

# Extracting for filtering and summarizing
# Create new columns hour, month and rainy
akl_hourly <- akl_hourly %>%
  mutate(
    hour = hour(datetime),
    month = month(datetime, label = TRUE),
    rainy = weather == "Precipitation"
  )

# Filter for hours between 8am and 10pm (inclusive)
akl_day <- akl_hourly %>% 
  filter(hour >= 8 & hour <= 22)

# Summarise for each date if there is any rain
rainy_days <- akl_day %>% 
  group_by(month, date) %>%
  summarise(
    any_rain = any(rainy)
  )

# Summarise for each month, the number of days with rain
rainy_days %>% 
  summarise(
    days_rainy = sum(any_rain)
  )

# Percent of rainy days 
rainy_days %>%
  summarize(days_rainy = mean(any_rain))

