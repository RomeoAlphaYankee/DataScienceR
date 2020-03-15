# Exploratory data analysis, modeling, and visualization of United Nations vote data
library(dplyr)
library(ggplot2)
library(readr)

# download and read data
# https://assets.datacamp.com/production/repositories/420/datasets/ddfa750d993c73026f621376f3c187f276bf0e2a/votes.rds
votes <- read_rds("../data/votes.rds")

# Look at the data
head(votes)
glimpse(votes)
dim(votes)

# Filter the data for countries that voted yes or no, and add years
votes %>% 
  filter(vote <= 3) %>%
  mutate(year = session + 1945)

# Need to add country names
library(countrycode)
countrycode::countrycode(100, 'cown', 'country.name')

# Apply above techniques to process data
votes_processed <- votes %>%
  filter(vote <= 3) %>%
  mutate(year = session + 1945,
         country = countrycode(ccode, "cown", "country.name"))

# Check the processed data
head(votes_processed)

# Find total and fraction of "yes" votes
votes_processed %>%
  summarize(total = n(), percent_yes = mean(vote == 1))

# Summarize by year
votes_processed %>%
  group_by(year) %>%
  summarize(total = n(),
            percent_yes = mean(vote == 1)) 

# Summarize by country: by_country
by_country <- votes_processed %>%
  group_by(country) %>%
  summarize(total = n(),
            percent_yes = mean(vote == 1))

# Examine the countries who voeted yes the lease, and the most
by_country %>%
  arrange(percent_yes)

by_country %>%
  arrange(desc(percent_yes))

# Filter out countries with fewer than 100 votes
by_country %>%
  arrange(percent_yes) %>%
  filter(total > 100)

# Visualizing the by-year data
by_year <- votes_processed %>%
    group_by(year) %>%
    summarize(total = n(), 
              percent_yes = mean(vote == 1))

by_year %>% ggplot(aes(x = year, y = percent_yes)) +
  geom_line()

# Points with regression
by_year %>%
  ggplot(aes(x = year, y = percent_yes)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

# Calculate the percentage of yes votes per country each year
by_year_country <- votes_processed %>%
  group_by(year, country) %>%
  summarize(percent_yes = mean(vote == 1)) 

# Filter for desired countries and plot time series
by_year_country %>%
  filter(country %in% c("United States", "France", "United Kingdom", "India")) %>%
  ggplot(aes(x = year, y = percent_yes, color = country)) +
  geom_line()

# Facet wrap plot with more countries
countries <- c("United States", "United Kingdom", "France", "Japan", "Brazil", "India")

by_year_country %>%
  filter(country %in% countries) %>%
  ggplot(aes(x = year, y = percent_yes)) +
  geom_line() +
  facet_wrap(~country)

# Add more countries
countries <- c("United States", "United Kingdom",
               "France", "Japan", "Brazil", "India",
               "Poland", "Russia", "China")

by_year_country %>%
  filter(country %in% countries) %>%
  ggplot(aes(x = year, y = percent_yes)) +
  geom_line(lwd = 1.25) +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~country, scale = "free_y")

# Fit a regression model to trend in countries voting
afghanistan <- by_year_country %>%
  filter(country == "Afghanistan") 

afghan.model <- lm(formula = percent_yes ~ year, data = afghanistan)

summary(afghan.model)

# Model for the US
usa <- by_year_country %>%
  filter(country == "United States")

usa.model <- lm(percent_yes ~ year, usa)

summary(usa.model)

uk <- by_year_country %>%
  filter(country == "United Kingdom") 

uk.model <- lm(formula = percent_yes ~ year, data = uk)

summary(uk.model)

# Tidy the models
library(broom)

usa.tidy <- tidy(usa.model)
afghan.tidy <- tidy(afghan.model)
uk.tidy <- tidy(uk.model)

# Compare models
bind_rows(afghan.tidy, uk.tidy, usa.tidy)

# Nest the data to create multiple models
library(tidyr)

nested <- by_year_country %>%
  nest(-country)

# Check
nested$data[nested$country == "Brazil"]

# Fit the models for each country using map
library(purrr)

country_coefficients <- by_year_country %>%
  nest(-country) %>%
  mutate(models = map(data, ~ lm(percent_yes ~ year, data = .)),
         tidied = map(models, tidy)) %>%
  unnest(tidied)

head(country_coefficients)

# Filter for statistically significant p-values and select coefficients
filtered_countries <- country_coefficients %>%
  filter(term == "year") %>%
  mutate(p.adjusted = p.adjust(p.value)) %>%
  filter(p.adjusted < 0.05)

# Sort for the countries increasing most quickly
filtered_countries %>%
  arrange(desc(estimate))


# Sort for the countries decreasing most quickly
filtered_countries %>%
  arrange(estimate)

# Join descriptions dataset
# https://assets.datacamp.com/production/repositories/420/datasets/a438432333a31a6f4aba2d5507df9a44e513b518/descriptions.rds
descriptions <- read_rds("../data/descriptions.rds")

head(descriptions)

# Find a key-value on which to join
head(votes_processed)

names(descriptions) %in% names(votes_processed)

names(descriptions)[names(descriptions) %in% names(votes_processed)]

# Join the description of topic to the country votes
votes_joined <- votes_processed %>%
  inner_join(descriptions, by = c("rcid", "session"))

# Filter for votes related to colonialism ("co")
votes_joined %>%
  filter(co == 1)

# Filter for US votes on colonialism an summarize by year
US_co_by_year <- votes_joined %>%
  filter(country == "United States", co == 1) %>%
  group_by(year) %>%
  summarize(percent_yes = mean(vote == 1))

# Plot the results
US_co_by_year %>%
  ggplot(aes(x = year, y = percent_yes)) + 
  geom_line() + 
  geom_smooth(method = "lm", se = F)

# Combine the topics into a single column
votes_gathered <- votes_joined %>%
  gather(topic, has_topic, me:ec) %>%
  filter(has_topic != 0)

glimpse(votes_gathered)

# Rename the topic column with descriptions
votes_tidied <- votes_gathered %>%
  mutate(topic = recode(topic,
                        me = "Palestinian conflict",
                        nu = "Nuclear weapons",
                        di = "Arms control and disarmament",
                        hr = "Human rights",
                        co = "Colonialism",
                        ec = "Economic development"))

glimpse(votes_tidied)

# Summarize the percentage "yes" per country-year-topic
country_year_topic <- votes_tidied %>%
  group_by(country, year, topic) %>%
  summarize(total = n(), percent_yes = mean(vote == 1)) %>%
  ungroup()

# view country_year_topic
country_year_topic

country_year_topic %>%
  filter(country == "United States") %>%
  ggplot(aes(x = year, y = percent_yes)) +
  geom_line() +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~topic)

# Model voting trends for each country by topic
country_coefficients <- country_year_topic %>%
  nest(-country, - topic) %>%
  mutate(model = map(data, ~lm(percent_yes ~ year, data = .)),
         tidied = map(model, tidy)) %>%
  unnest(tidied)

# Filter for slope, and adjusted p-values that are statistically significant
country_topic_filtered <- country_coefficients %>%
  filter(term == "year") %>%
  mutate(p.adjusted = p.adjust(p.value)) %>%
  filter(p.adjusted < 0.05)

# Examine trends
country_topic_filtered %>%
  arrange(estimate)

country_topic_filtered %>%
  arrange(desc(estimate))

# Investigate the and visualize rapid change in Vanuatu's attitude on certain topics 
country_topic_filtered %>%
  filter(country == "Vanuatu")

country_year_topic %>%
  filter(country == "Vanuatu") %>%
  ggplot(aes(x = year, y = percent_yes)) +
  geom_line() +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~topic)

# Which topics have exhiibted the most meaningful changes in attitude
country_topic_filtered %>%
  group_by(topic) %>%
  summarize(change = mean(estimate))

# It appears that over time more countries are voting in favor of economic development resolutions
# Investigate if that holds true for the world's largest economies
countries <- c("United States", "China", "Japan", "Germany", "India", "United Kingdom")

country_year_topic %>%
  filter(country %in% countries, topic == "Economic development") %>%
  ggplot(aes(x = year, y = percent_yes)) + 
  geom_line() +
  geom_smooth(method = "lm", se = F) +
  facet_wrap(~country)
