# Google analytics using googleAnalyticsR
# install.packages('googleAnalyticsR')
library(googleAnalyticsR)

# Authorize Google Analytics
ga_auth()

# Insert Google Analytics API key
ga_id <- #"Your Key Here"

# Establish time frame
start_date <- "2017-01-01"
end_date <- "2020-02-28"
metrics <- "sessions"

# Extract data
df_sessions <- google_analytics(ga_id,
                               date_range = c(start_date, end_date),
                               metrics = "sessions",
                               dimensions = "date")
plot(df_sessions, type = "l")
head(df_sessions)
str(df_sessions)

library(lubridate)
library(dplyr)
library(ggplot2)
library(ggthemes)

df_sessions$date <- as.POSIXct(df_sessions$date)  

df_sessions %>%
  mutate(year = year(date), month = month(date, label = TRUE), day = day(date)) %>%
  filter(date >= "2017-01-01") %>%
  #  filter(month %in% c("May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")) %>%
  filter(month %in% c("Jun","Jul", "Aug", "Sep")) %>%
  group_by(month, year) %>%
  summarize(total = sum(sessions)) %>% 
  ungroup() %>%
  ggplot(aes(x = month, y = total, fill = factor(year))) +
  geom_col(position = "dodge") +
  scale_fill_manual(values = c("black", "blue", "red"), name = "Year") +
  labs(x = "Month", y = "Visitors") +
  ggtitle("Riveredge Resort", subtitle = "2017 - 2019 seasons")

df_sessions %>%
  mutate(year = year(date), month = month(date, label = TRUE), day = day(date)) %>%
  filter(date >= "2018-01-01") %>%
  filter(month %in% c("Jun","Jul", "Aug", "Sep")) %>%
  group_by(month, year) %>%
  summarize(total = sum(sessions)) %>% 
  ungroup() %>% tidyr::spread(key = year, value = total) %>% 
  rename(Y2018 = '2018', Y2019 = '2019') %>%
  ggplot(aes(x = Y2018, y = Y2019)) +
  geom_point() +
  geom_abline()
