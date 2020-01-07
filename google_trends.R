# install.packages('gtrendsR')

library(gtrendsR)

google_trends <- function(keyword, geo = ""){
  pres_data <- gtrends(keyword = keyword, geo = geo, time = "all", onlyInterest = TRUE)
  plot(pres_data)
  hits <- pres_data$interest_over_time$hits
  last <- length(hits)
  round((mean(hits[(last-10):last]) / mean(hits[1:10]) - 1) * 100)
}

google_trends('NYC')

google_trends('Thousand Islands', geo = "US-NY")
