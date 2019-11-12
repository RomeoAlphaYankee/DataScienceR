# Data manipultaion with dplyr
# Babynames data exploration
library(dplyr)
library(ggplot2)

# Load the dataset
babynames <- read_rds('../data/babynames.rds')
dim(babynames)
glimpse(babynames)

# Plot the popularity of my name
babynames %>%
  filter(name == 'Raymond') %>%
  ggplot(aes(x = year, y = number)) +
  geom_line()

# Find the top baby names for the last year on record
babynames %>% 
  filter(year == max(year)) %>%
  top_n(5, number)

# Find the top baby names for 1990
babynames %>%
  filter(year == 1990) %>%
  arrange(desc(number))

# Find the most common name in each year
babynames %>%
  group_by(year) %>%
  top_n(1)

# Filter for the names Steven, Thomas, and Matthew
selected_names <- babynames %>%
  filter(name %in% c('Steven', 'Thomas', 'Matthew'))

# Plot the names using different color for eachc name
ggplot(selected_names, aes(x = year, y = number, color = name)) +
  geom_line

# Group mutate to calculate and plot popularity of names as a percentage
babynames %>%
  group_by(year) %>%
  mutate(year_total = sum(number)) %>%
  ungroup() %>%
  mutate(fraction = number / year_total) %>%
  filter(name %in% c('Steven', 'Thomas', 'Matthew')) %>%
  ggplot(aes(x = year, y = fraction, color = name)) +
  geom_line()

# Find the year each baby name is most common
babynames %>%
  group_by(year) %>%
  mutate(year_total = sum(number)) %>%
  ungroup() %>%
  mutate(fraction = number / year_total) %>%
  group_by(name) %>%
  top_n(1, wt = fraction)

# Normalize by scaling each name count from 0 to 1
# Then filter for certain names and plot peaks
babynames %>%
  group_by(name) %>%
  mutate(name_total = sum(number), name_max = max(number)) %>%
  ungroup() %>%
  mutate(fraction_max = number / name_max) %>%
  filter(name %in% c('Steven', 'Thomas', 'Matthew', 'Raymond')) %>%
  ggplot(aes(x = year, y = fraction_max, color = name)) + 
  geom_line()

# Calculate the rates of change in name popularity
# Then calculate the ratio of each year's percentage relative to prior year
# Then sort looking for sudden surges in name popularity
babynames %>%
  group_by(name) %>%
  mutate(name_total = sum(number), 
         fraction = number / name_total,
         change = fraction - lag(fraction),
         ratio = fraction / lag(fraction)) %>%
  filter(ratio >= 0.00001) %>%
  top_n(1) %>%
  arrange(desc(ratio)) %>%
  filter(ratio >= 1)

# Redo the above search but check for surges in overall popularity
# not just rate of change year to year
babynames %>%
  group_by(year) %>%
  mutate(year_total = sum(number), fraction = number / year_total) %>%
  ungroup() %>% 
  arrange(name, year) %>%
  group_by(name) %>%
  mutate(ratio = fraction / lag(fraction)) %>%
  filter(fraction >= 0.00001) %>%
  top_n(1) %>%
  arrange(desc(ratio)) %>%
  filter(fraction >= 0.001)

