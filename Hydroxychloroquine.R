library(dplyr)
library(tidyr)
library(tabulizer)
library(ggplot2)

location <- "https://www.mediterranee-infection.com/wp-content/uploads/2020/03/Hydroxychloroquine_final_DOI_IJAA.pdf"

out <- tabulizer::extract_tables(location)

data <- out[[4]]
head(data)

data <- data.frame(data, stringsAsFactors = FALSE)

colnames(data) <- c("Patient", "Age", "Sex", "Status", "Symptom_days", "Hdrxclqn", "Dosage", "Azthrm","D0", "D1", "D2", "D3", "D4", "D5", "D6")
data <- data[-1, ]

head(data)

data$Age <- as.numeric(data$Age)

data$Patient <- as.numeric(data$Patient)

data <- data[!is.na(data$Patient), ]

data$Sex <- as.factor(data$Sex)

head(data)

tail(data)

str(data)

data <- data %>% gather(key = Day, value = Value, 9:15) 

View(data)

data$treatment <- NA

for(i in 1:nrow(data)) {
  if(data$Hdrxclqn[i] == "Yes" & data$Azthrm[i] == "Yes") {
    data$treatment[i] <- "Azth"
  } else if(data$Hdrxclqn[i] == "Yes" & data$Azthrm[i] != "Yes") {
    data$treatment[i] <- "Clqn"
  } else {
    data$treatment[i] <- "None"
  }
}

data$treatment <- as.factor(data$treatment)

data$Value[data$Age == 87 & data$Day == "D6"] <- "NEG"

data %>%
  group_by(treatment, Day) %>%
  summarize(infected = 1 - mean(Value == "NEG")) %>%
  ggplot(aes(x = Day, y = infected, fill = treatment)) +
  geom_col(position = "dodge") +
  ggtitle("French Study of COVID-19 Treatments") +
  ylab("Proportion Infected")

day6 <- data %>% filter(Day == "D6")

prop.table(table(day6$treatment, day6$Value == "NEG"), 1)

data %>%
  group_by(treatment) %>%
  summarise(age = mean(Age))

data$Value == "NEG"

data %>% group_by(treatment) %>%
  summarize(age = mean(Age))

data %>%
  group_by(treatment, Status) %>%
  summarize(count = n() / 7) 


prop.table(table(data$treatment, data$Status), 1)