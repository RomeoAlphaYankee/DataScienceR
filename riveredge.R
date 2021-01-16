library(dplyr)
library(readr)
library(lubridate)
library(ggplot2)

ti <- read.csv("C:/Users/angel/Downloads/multiTimeline.csv", skip = 2)
head(ti)

names(ti) <- c("week", "data")

ti %>% mutate(date = ymd(week)) %>% 
  mutate(year = year(date), month = month(date)) %>%
  filter(month >= 6, month <= 9) %>%
  group_by(month, year) %>% 
  summarise(mean = mean(data), median = median(data))

ti %>% mutate(date = ymd(week)) %>% 
  mutate(year = year(date), month = month(date)) %>%
  filter(month >= 6, month <= 10) %>%
  group_by(month, year) %>% 
  summarise(mean = mean(data), median = median(data)) %>%
  ggplot(aes(x = month, y = mean, fill = factor(year))) +
  geom_col(position = "dodge") +
  ggtitle("Google Searches for Thousand Islands") +
  labs(fill = "Year")

ti %>% mutate(date = ymd(week)) %>% 
  mutate(year = year(date)) %>%
  ggplot(aes(x = date, y = data)) + 
  geom_line() +
  ggtitle("Google Searches for Thousand Islands")


rr <- read_csv("C:/Users/angel/Downloads/Analytics Riveredge Resort Pages 20171205-20201211.csv", skip = 19)
head(rr)

rr$`Month Index` <- as.numeric(rr$`Month Index`)

tail(rr)

rr$month <- 12
rr$month[-1] <- 1:12

rr <- rr[-nrow(rr), ]

tail(rr)

rr$year <- NA

within(rr, rr$year <- ifelse(rr$Month.Index == 0, 2017,
                             ifelse(rr$Month.Index >= 1 & <= 12, 2018, 
                                    ifelse(rr$Month.Index >= 13 & <= 24, 2019, 2020))))
 

year <- c(2017, rep(2018, 12), rep(2019, 12), rep(2020, 12))

rr$year <- year

head(rr)
str(rr)

rr %>% filter(month >= 6, month <= 10) %>%
  ggplot(aes(x = month, y = Pageviews, fill = factor(year))) +
  geom_col(position = "dodge") + 
  ggtitle("Web Traffic to Riveredge Resort") +
  labs(fill = "Year")
