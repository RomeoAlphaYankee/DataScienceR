# Zagat reviews, multiple regression models

library(dplyr)
library(ggplot2)
library(broom)
library(readr)

# Read the data for Zagat survey results
nyc <- read_csv("https://assets.datacamp.com/production/repositories/845/datasets/639a7a3f9020edb51bcbc4bfdb7b71cbd8b9a70e/nyc.csv")

dim(nyc)
str(nyc)

glimpse(nyc)

# Change name to a factor
nyc$Restaurant <- factor(nyc$Restaurant)

pairs(nyc)

# Plot Food quality vs price and fit lm
nyc %>% 
  ggplot(aes(x = Food, y = Price)) +
  geom_point() +
  geom_smooth(method = "lm")

# The model
lm(Price ~ Food, data = nyc)

# Check if restaurants charge more on the East side

nyc %>%
  group_by(East) %>%
  summarize(mean_price = mean(Price))

# Create a model that includes East
mod <- lm(formula = Price ~ Food + East, data = nyc)

summary(mod)

predict(mod)

coef(mod)

# Base R plot
plot(nyc$Food, nyc$Price, type = "p", pch = 20, col = "blue",
     xlab = "Food Quality", ylab = "Price")
abline(coef(mod)[1], coef(mod)[2], col = "red")
abline(coef(mod)[1] + coef(mod)[3], coef(mod)[2], col = "black")
legend(x = "bottomright", legend = c("East", "West"), col = c("black", "red"), lty = 1, cex = 1)

# Build an independent slopes model
mod2 <- lm(Price ~ Food + factor(East) + Food:factor(East), data = nyc)
mod2

plot(nyc$Food, nyc$Price, type = "p", pch = 16, col = "blue",
     xlab = "Food Quality", ylab = "Price")
abline(a = -9.0875, b = 2.4603, col = "red")
abline(a = -9.0875 - 10.8280, b = 2.4603 + 0.65035, col = "black")
legend(x = "bottomright", legend = c("East", "West"), col = c("black", "red"), lty = 1, cex = 1)

# Multiple regression models with dplyr and ggplot
# Use the augment function on model
augmented_mod <- augment(mod)
glimpse(augmented_mod)

augmented_mod %>%
  ggplot(aes(x = Food, y = Price, color = as.logical(East))) +
  geom_point() +
  geom_line(aes(y = .fitted)) +
  labs(color = "East Side") +
  ggtitle("Price of NYC Restaurants by Location, Controlling For Food Quality")

# Create a model for price given food and service
lm(formula = Price ~ Food + Service, data = nyc)

# Plot the relationship
p <- plotly(data = nyc, z = ~)

