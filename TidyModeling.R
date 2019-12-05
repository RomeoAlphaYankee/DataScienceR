# Tidy Modeling
library(dplyr)
library(ggplot2)
library(broom)
library(moderndive)

# Teacher evaluation dataset
data("evals")

glimpse(evals)

# Exploritory data analysis of teaching score as a function of age
evals %>%
  ggplot(aes(x = score)) +
  geom_histogram(binwidth = 0.25) +
  labs(x = "teaching score", y = "count")

evals %>%
  summarize(mean_score = mean(score), 
            medidan_score = median(score), 
            sd_score = sd(score))

evals %>%
  ggplot(aes(x = age)) +
  geom_histogram(binwidth = 5) +
  labs(x = "age", y = "count")

evals %>%
  summarize(mean_age = mean(age),
            median_age = median(age),
            sd_age = sd(age))

evals %>%
  ggplot(aes(x = age, y = score)) +
  geom_point(position = "jitter") +
  geom_smooth(method = "lm", se = FALSE)

evals %>% summarize(correlation = cor(age, score))

# Create a model based on the visual 
model_score_1 <- lm(score ~ age, data = evals)

model_score_1

get_regression_table(model_score_1)

# Forecast scores
get_regression_points(model_score_1)


# Investigate teaching score as a function of beauty score
evals %>%
  ggplot(aes(x = bty_avg)) +
  geom_histogram(binwidth = 0.5) +
  labs(x = "beauty score", y = "count")

evals %>%
  ggplot(aes(x = bty_avg, y = score)) +
  geom_point(position = "jitter") +
  labs(x = "beauty score", y = "teaching score")

evals %>%
  summarize(correlation = cor(bty_avg, score))

ggplot(evals, aes(x = bty_avg, y = score)) +
  geom_point(poitsition = "jitter") +
  labs(x = "beauty score", y = "score") +
  geom_smooth(method = "lm", se = FALSE)

# Fit a model to teaching score as a function of beauty score
model_score_2 <- lm(score ~ bty_avg, data = evals)

model_score_2

get_regression_table(model_score_2)

# Compare fitted and observed values
get_regression_table(model_score_2)
get_regression_points(model_score_2)

# The long way
evals$score - (coef(model_score_2)[1] + coef(model_score_2)[2] * evals$bty_avg)

# Explore teaching score as a function of gender

ggplot(evals, aes(x = gender, y = score)) +
  geom_boxplot()

evals %>%
  ggplot(aes(x = score)) +
  geom_histogram(binwidth = 0.25) +
  facet_wrap(~gender)

# Fit a regression model
model_score_3 <- lm(score ~ gender, data = evals)

get_regression_table(model_score_3)

# summary table
evals %>%
  group_by(gender) %>%
  summarize(number = n(), avg_score = mean(score), std_score = sd(score))

model_score_3_points <- get_regression_points(model_score_3)

model_score_3_points %>%
  ggplot(aes(x = residual)) +
  geom_histogram(binwidth = 0.25) +
  labs(title = "Residuals from score ~ gender model")

mod <- lm(score ~ bty_avg + gender, data = evals)

ggplot(mod, aes(x = bty_avg, y = score, color = gender)) +
  geom_point() +
  geom_line(data = augment(mod), aes(y = .fitted))

predict(mod, newdata = data.frame(bty_avg = c(4, 4), gender = c("female", "male")))


# Explore score as a function of categorical variable rank
evals %>% count(rank)

evals %>%
  ggplot(aes(x = rank, y = score)) +
  geom_boxplot()

evals %>%
  group_by(rank) %>%
  summarize(total = n(), mean_score = mean(score), sd_score = sd(score))

# Fit a model to score as a function of rank
model_score_4 <- lm(score ~ rank, data = evals)
get_regression_table(model_score_4)

model_score_4_points <- get_regression_points(model_score_4)

model_score_4_points %>%
  ggplot(aes(x = residual)) +
  geom_histogram() +
  labs(title = "Residuals from score ~ rank model")




# Home sales dataset
data("house_prices")
glimpse(house_prices)

house_prices %>%
  ggplot(aes(x = price)) +
  geom_histogram() +
  scale_x_continuous(trans = "log10") +
  labs(x = "house price", y = "count")

house_prices %>%
  summarize(mean_price = mean(price),
            median_price = median(price),
            sd_price = sd(price))

house_prices %>%
  ggplot(aes(x = sqft_living)) +
  geom_histogram() +
  labs(x = "size (sq. feet)", y = "count")

house_prices %>%
  mutate(log10_size = log10(sqft_living)) %>%
  ggplot(aes(x = log10_size)) +
  geom_histogram() +
  labs(x = "log10 size", y = "count")

house_prices %>%
  mutate(price = log10(price)) %>%
  select(price, condition) %>%
  glimpse()

house_prices %>%
  mutate(price = log10(price)) %>%
  ggplot(aes(x = condition, y = price)) +
  geom_boxplot() +
  labs(x = "house condition", y = "log10 price")

house_prices %>%
  mutate(price = log10(price)) %>%
  group_by(condition) %>%
  summarize(mean = mean(price), median = median(price), sd = sd(price), n = n())


# Model price as a function of number of bedrooms
house_prices %>%
  filter(bedrooms <= 15) %>%
  ggplot(aes(x = bedrooms, y = log10(price))) +
  geom_point(position = "jitter", alpha = 0.4) +
  labs(x = "Number of Bedrooms", y = "log10 Price") +
  geom_smooth(method = "lm", se = FALSE)

# Trim the outlier with 33 bedrooms, and scale the price and size
house_prices <- house_prices %>%
  filter(bedrooms <= 15) %>%
  mutate(log10_price = log10(price), log10_size = log10(sqft_living))

# Model log10 price as a function of log10 size and bedrooms
model_price_2 <- lm(log10_price ~ log10_size + bedrooms, data = house_prices)

get_regression_table(model_price_2)

# Model price as a function of year and size
model_price_1 <- lm(log10_price ~ log10_size + yr_built, data = house_prices)

get_regression_table(model_price_1)

newdata = data.frame(log10_size = 3.07, yr_built = 1980)

# Predict price manually
5.39 + 0.913 * 3.07 + -0.001 * 1980

# Predict price using predict and augment functions
hpp <- predict(model_price_1, newdata = newdata)

augment(model_price_1, newdata = newdata)

# Convert back to original units
10^hpp

# Get regression estimates for all 21612 data points and
# Check the sum of squared residuals
get_regression_points(model_price_1) %>%
  mutate(sq_residuals = residual^2) %>%
  summarise(sum_sq_residuals = sum(sq_residuals))

# Parallel slopes model, first model, then plot
model_price_3 <- lm(log10_price ~ log10_size + condition, data = house_prices)

get_regression_table(model_price_3)

house_prices %>%
  ggplot(aes(x = log10_size, y = log10_price, color = condition)) + 
  geom_point(alpha = 0.25) +
  geom_smooth(aes(y = augment(model_price_3)$.fitted), method = "lm", se = FALSE)

# Faceted plot of price as a function of size and condition
house_prices %>%
  ggplot(aes(x = log10_size, y = log10_price, color = condition)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~condition)

# Predict price as a function of size and condition
new_houses_1 <- data_frame(log10_size = c(2.9, 3.6), condition = factor(c(3, 4)))

# Check different functions to predict price
10^predict(model_price_3, newdata = new_houses_1)

get_regression_points(model_price_3, newdata = new_houses) %>%
  mutate(price_hat = 10^log10_price_hat)

augment(model_price_3, newdata = new_houses) %>%
  mutate(.prediction = 10^.fitted)


# Investigate the impact of water frontage on price
house_prices %>%
  mutate(price = log10(price)) %>%
  select(price, waterfront) %>%
  ggplot(aes(x = waterfront, y = price)) +
  geom_boxplot() +
  labs(x = "waterfront", y = "log10 price")

house_prices %>%
  group_by(waterfront) %>%
  summarise(mean_price = mean(price), n = n())

# Predict prices of two new houses based on size and waterfrontage
# Model and plot price as a function of water frontage controlling for size
model_price_4 <- lm(log10_price ~ log10_size + waterfront, data = house_prices)

get_regression_table(model_price_4)

house_prices %>%
  ggplot(aes(x = log10_size, y = log10_price, color = waterfront)) +
  geom_point(alpha = 0.4) +
  geom_smooth(aes(y = augment(model_price_4)$.fitted), method = "lm", se = FALSE) +
  labs( x = "Log10 Size", y = "Log10 Price", title = "Price as a function of waterfrontage, controlling for size")

# Facet on waterfrontage
house_prices %>%
  ggplot(aes(x = log10_size, y = log10_price, color = waterfront)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~waterfront)

# Predict new house prices using the model
new_houses_2 <- data_frame(log10_size = c(2.9, 3.6), waterfront = c(TRUE, FALSE))

get_regression_points(model_price_4, newdata = new_houses_2) %>%
  mutate(price_hat = 10^log10_price_hat) %>%
  select(log10_size, waterfront, price_hat)

# Model assesment and selection 
# Calculate sum of square residuals
get_residuals <- function(x){
  get_regression_points(x) %>% 
    mutate(sq_residuals = residual^2) %>%
    summarize(sum_sq_residuals = sum(sq_residuals))
}

models <- list(model_price_1, model_price_2, model_price_3, model_price_4)

lapply(models, FUN = get_residuals)

# Calculate R-squared for all four models
get_r_sq <- function(x){
  get_regression_points(x) %>%
    summarize(r_sq = 1 - (var(residual) / var(log10_price)))
}

data.frame(lapply(models, FUN = get_r_sq))

# Assess predictions with root mean square error
get_rmse <- function(x){
  get_regression_points(x) %>%
    mutate(sq_residuals = residual^2) %>%
    summarize(mse = mean(sq_residuals), rmse = sqrt(mse))
}


data.frame(lapply(models, FUN = get_rmse))

# Training set / test set validation
# Randomly shuffle order of rows:
house_prices_shuffled <- house_prices %>%
  sample_frac(size = 1, replace = FALSE)

# Split into train and test sets:
train <- house_prices_shuffled %>%
  slice(1:(nrow(house_prices_shuffled) * .8))

test <- house_prices_shuffled %>%
  slice((nrow(train) + 1):nrow(house_prices_shuffled))

# Check that all rows are in train and test, but no overlap
nrow(train) + nrow(test) == nrow(house_prices)

# Train model on training set
train_model_price_1 <- lm(log10_price ~ log10_size + yr_built, data = train)

get_regression_table(train_model_price_1)

get_regression_points(train_model_price_1, newdata = test)

# Check the root mean square error
get_regression_points(train_model_price_1) %>%
  mutate(sq_error = residual^2) %>%
  summarize(mse = mean(sq_error), rmse = sqrt(mse))

# Train a model on the test data using the remaining models
train_model_price_2 <- lm(log10_price ~ log10_size + bedrooms, data = train)
train_model_price_3 <- lm(log10_price ~ log10_size + condition, data = train)
train_model_price_4 <- lm(log10_price ~ log10_size + waterfront, data = test)

# Test the models on the test data
train_models <- list(train_model_price_1, train_model_price_2, train_model_price_3, train_model_price_4)

# Rewrite the function to get root mean square error for the test data
get_rmse <- function(x){
  get_regression_points(x, newdata = test) %>%
    mutate(sq_error = residual^2) %>%
    summarize(mse = mean(sq_error), rmse = sqrt(mse))
}

# Apply the function to the test data
lapply(train_models, FUN = get_rmse)

# Rewrite the r-squared function to take the test data
get_r_sq <- function(x){
  get_regression_points(x, newdata = test) %>%
    summarize(r_sq = 1 - var(residual) / var(log10_price))
}

# Apply the new function to the test data
lapply(models, FUN = get_r_sq)


train_model_price_5 <- lm(log10_price ~ log10_size + yr_built + waterfront, data = train)

get_r_sq(train_model_price_5)
summary(train_model_price_5)

get_regression_points(train_model_price_5, newdata = test)

test %>%
  ggplot(aes(x = log10_size, y = log10_price, color = waterfront)) +
  geom_point(alpha = 0.5) +
  geom_line(aes(y = get_regression_points(train_model_price_5, newdata = test)$log10_price_hat), lwd = 2)
