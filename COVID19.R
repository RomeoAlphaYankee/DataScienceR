library(readr)
library(dplyr)
library(ggplot2)
library(lubridate)
library(tidyr)

url <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv"
  
confirmed <- read_csv(url)

head(confirmed)
colnames(confirmed)

confirmed.clean <- confirmed

colnames(confirmed.clean)[1:4] <- c("State", "Country", "Lat", "Lon")

colnames(confirmed.clean)[5:ncol(confirmed.clean)] <- mdy(colnames(confirmed.clean)[5:ncol(confirmed.clean)])

confirmed.tidy <- confirmed.clean %>%
  filter(Country == "US") %>%
  gather(key = "Date", value = "Value", 5:ncol(confirmed.clean)) %>%
  select(State, Country, Date, Value)

confirmed.tidy$Date <- mdy(confirmed.tidy$Date)

confirmed.tidy %>%
  filter(State %in% c("New York", "Georgia", "Connecticut", "Florida")) %>% 
  filter(Date >= "2020-03-09") %>%
  ggplot(aes(x = Date, y = Value, color = State)) +
  labs(title = "Confirmed COVID-19 Cases by State") +
  ylab("Number of cases") +
  geom_line(lwd = 1.25) 
