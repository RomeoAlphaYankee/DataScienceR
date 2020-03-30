library(readr)
library(dplyr)
library(ggplot2)
library(lubridate)
library(tidyr)
library(ggthemes)

# Url for Johns Hopkins University COVID-19 raw data
url <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv"

confirmed <- read_csv(url)

head(confirmed)
colnames(confirmed)

# Create a working dataset to clean instead of reloading
confirmed.tidy <- confirmed

colnames(confirmed.tidy)[1:4] <- c("State", "Country", "Lat", "Lon")

confirmed.tidy <- confirmed.tidy %>%
  filter(Country == "US") %>%
  gather(key = "Date", value = "Value", 5:ncol(confirmed.tidy)) %>%
  select(State, Country, Date, Value)

confirmed.tidy$Date <- mdy(confirmed.tidy$Date)

confirmed.tidy %>% filter(State == "New York", Date > "2020-03-07")

NY <- confirmed.tidy %>%
  filter(State == "New York", Value > 0) 

NY$Value <- as.numeric(NY$Value)

NY %>%
  mutate(Diff = c(Value[1], diff(Value)))  %>%
  ggplot(aes(x = Date, y = Diff)) +
  geom_col() +
  ggtitle("New COVID-19 Cases in New York") +
  ylab("New Cases") +
  ggthemes::theme_economist()

NY %>%
  mutate(Diff = c(Value[1], diff(Value)))  %>%
  ggplot(aes(x = Date, y = Diff)) +
  geom_line() +
  geom_smooth(method = "lm") +
  scale_y_continuous(trans = "log10") +
  ggtitle("New COVID-19 Cases in New York") +
  ylab("New Cases (log-10 scale)") +
  ggthemes::theme_economist()

confirmed.tidy %>%
  filter(State %in% c("New York", "Washington", "California", "Florida", "New Jersey")) %>% 
  filter(Date >= "2020-03-09") %>%
  ggplot(aes(x = Date, y = Value, color = State)) +
  labs(title = "Confirmed COVID-19 Cases by State", 
       subtitle = "Source: Johns Hopkins University") +
  ylab("Total Cases") +
  geom_line(lwd = 1.25)

confirmed.tidy %>%
  filter(State %in% c("New York", "Georgia", "Connecticut", "Florida")) %>% 
  filter(Date >= "2020-03-09") %>%
  ggplot(aes(x = Date, y = Value, color = State)) +
  labs(title = "Confirmed COVID-19 Cases by State", 
       subtitle = "Source: Johns Hopkins University") +
  ylab("Number of cases") +
  geom_line(lwd = 1.25) +
  scale_color_stata() +
  theme_stata()

confirmed.tidy %>%
  filter(State %in% c("New York", "California", "Washington", "Michigan", "New Jersey", "Louisiana", "Florida", "Illinois")) %>% 
  filter(Date >= "2020-03-09", Date < "2020-03-23") %>%
  ggplot(aes(x = Date, y = Value, color = State)) +
  scale_y_continuous(trans = "log10") +
  labs(title = "Confirmed COVID-19 Cases by State", 
       subtitle = "Source: Johns Hopkins University") +
  ylab("Number of cases (log10 scale)") +
  geom_line(lwd = 1.25) +
  scale_color_stata() +
  theme_stata()

confirmed.tidy %>% 
  filter(State == "New York") %>% 
  mutate(rate = c(0, diff(Value)) / lag(Value)) %>% 
  filter(Date > "2020-03-10", Date < '2020-03-23') %>%
  ggplot(aes(x = Date, y = rate)) + 
  geom_line() +
  geom_smooth(method = "lm") +
  ggtitle("Rate of Change in New Cases") +
  ylab("Growth Rate")

model <- confirmed.tidy %>% 
  filter(State == "New York") %>% 
  mutate(rate = c(0, diff(Value)) / lag(Value)) %>% 
  filter(Date > "2020-03-10") %>%
  lm(formula = rate ~ Date, data = .)

model

new.dates <- data.frame(Date = as.Date(c("2020-03-22", "2020-03-23")))

predict(model, newdata = new.dates)

summary(model)

confirmed %>% 
  rename("State" = "Province/State", "Country" = "Country/Region") %>%
  filter(Country %in% opec) %>% 
  gather(key = "Date", value = "Value", 5:ncol(confirmed)) %>%
  tail(10)

tail(NY)
