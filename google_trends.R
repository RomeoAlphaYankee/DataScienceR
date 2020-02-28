# install.packages('gtrendsR')
library(gtrendsR)

# Run the google trends querry
gdata <- gtrends(c("Riveredge Resort"), time = "2016-01-01 2018-12-31")

# Check the names of dataframes in the list
names(gdata)

# Check related queries
head(gdata$related_topics, 20)

# Drill down on a related querry
head(gtrends("Boldt Castle")$related_queries, 20)

gdata$related_topics[1:10, ]
gdata$interest_by_dma[1:5, ]
gdata$interest_by_country[1:5, ]

# Specify category
categories[grepl("^Hotel", categories$name), ]

gdata <- gtrends(keyword = "Riveredge Resort", time = "today+5-y", category = 179)

# Check interest by country and MSA
gdata$interest_by_dma[1:5, ]
gdata$interest_by_country[1:5, ]

# Write a function to querry Google Trends and plot interest over time
google_trends <- function(keyword, geo = ""){
  pres_data <- gtrends(keyword = keyword, geo = geo, time = "today+5-y", onlyInterest = TRUE)
  plot(pres_data, lwd = 5)
  hits <- pres_data$interest_over_time$hits
  last <- length(hits)
  round((mean(hits[(last-10):last]) / mean(hits[1:10]) - 1) * 100)
}

# Some sample searches
google_trends('NYC')

google_trends('Riveredge Resort')
google_trends('Riveredge Resort', geo = "US-NY")
google_trends('Riveredge Resort', geo = "CA-ON")

google_trends("1000 Islands")
google_trends('1000 Islands', geo = "CA-ON")
google_trends('1000 Islands', geo = "US-NY")

google_trends("Thousand Islands")
google_trends('Thousand Islands', geo = "CA-ON")
google_trends('Thousand Islands', geo = "US-NY")
