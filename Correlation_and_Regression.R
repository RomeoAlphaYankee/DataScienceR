# Correlation and regression in R
library(openintro)
library(dplyr)
library(ggplot2)
data("ncbirths")


head(ncbirths)
glimpse(ncbirths)

# Look at pairs relationships
pairs(ncbirths[ , c(1, 2, 4, 6, 8, 9)])

# Scatterplot of weight vs. weeks
ncbirths %>%
  ggplot(aes(x = weeks, y = weight)) +
  geom_point()

# Boxplot of weight vs. weeks
ncbirths %>%
  ggplot(aes(x = cut(weeks, breaks = 5), y = weight)) +
  geom_boxplot()

# Compute correlation
ncbirths %>%
  summarize(N = n(), r = cor(weight, mage))

# Compute correlation for all non-missing pairs
ncbirths %>%
  summarize(N = n(), r = cor(weight, weeks, use = 'pairwise.complete.obs'))

# Load Mammals data
data("mammals")

# Mammals scatterplot
mammals %>%
  ggplot(aes(x = BodyWt, y = BrainWt)) +
  geom_point()

# Scatterplot with log transformation
mammals %>%
  ggplot(aes(x = BodyWt, y = BrainWt)) +
  geom_point() +
  coord_trans(x = 'log10', y = 'log10')

# Done another way
mammals %>%
  ggplot(aes(x = BodyWt, y = BrainWt)) +
  geom_point() +
  scale_x_log10() +
  scale_y_log10()

mammals %>%
  summarize(N = n(),
            r = cor(BodyWt, BrainWt),
            r_log = cor(log(BrainWt), log(BodyWt)))

# Load mlbBat10 data
data("mlbBat10")

# Baseball player scatterplot comparing on base percentage to slugging
mlbBat10 %>%
  ggplot(aes(x = OBP, y = SLG)) +
  geom_point()

# Remove outliers by requiring minimun of 200 at bats
mlbBat10 %>%
  filter(AB >= 200) %>%
  ggplot(aes(x = OBP, y = SLG)) +
  geom_point()

# Identify outliers with OBP below .200
mlbBat10 %>%
  filter(AB >= 200 & OBP < .200)

# Comparing home runs to stolen bases using alpha to uncover overlapping points
mlbBat10 %>%
  filter(SB > 0 & HR > 0) %>%
  ggplot(aes(x = SB, y = HR)) +
  geom_point(alpha = 0.5)

# Similar plot using jitter to uncover overlapping data points
mlbBat10 %>%
  ggplot(aes(x = SB, y = HR)) +
  geom_point(alpha = 0.5, position = "jitter")

# Identify outliers
mlbBat10 %>%
  filter(SB > 60 | HR > 50) %>%
  select(name, team, position, SB, HR)

# Explore more closely the relationship between OBP and SLG
mlbBat10 %>% 
  ggplot(aes(x = OBP, y = SLG)) + 
  geom_point()

# Correlation for all baseball players between OBP and SLG
mlbBat10 %>%
  summarize(N = n(), r = cor(OBP, SLG))

# Filter first for 200 AB
mlbBat10 %>% 
  filter(AB >= 200) %>%
  ggplot(aes(x = OBP, y = SLG)) + 
  geom_point()

mlbBat10%>%
  filter(AB >= 200) %>%
  summarize(N = n(), r = cor(OBP, SLG))



# Load body dimenstions data
data("bdims")

# Body dimensions scatterplot
bdims %>%
  ggplot(aes(x = hgt, y = wgt, color = as.factor(sex))) +
  geom_point()

bdims %>%
  group_by(sex) %>%
  summarize(N = n(), r = cor(hgt, wgt))

# Load the smoking data set
data("smoking")

smoking %>%
  ggplot(aes(x = age, y = amtWeekdays)) + 
  geom_point()

# Explore the Anscombe data
data("anscombe")

glimpse(anscombe)

# Reshape into sets
anscombe %>%
  gather(var, val, everything()) %>%
  extract(var, into = c("variable", "set"), "(.)(.)") %>%
  group_by(variable, set) %>%
  mutate(ind = sequence(n())) %>%
  spread(variable, val) %>% 
  ggplot(aes(x = x, y = y)) +
  geom_point() +
  facet_wrap(~set)

# Done another way using the melt command
library(data.table)
anscombe <- melt(as.data.table(anscombe), 
     measure.vars = patterns(c("x", "y")), 
     value.name=c('x', 'y'), 
     variable.name = "set")

anscombe %>%
  ggplot(aes(x = x, y = y)) +
  geom_point() +
  facet_wrap(~set)

anscombe %>%
  group_by(set) %>%
  summarize(N = n(),
            mean_x = mean(x),
            std_dev_x = sd(x),
            mean_y = mean(y),
            std_dev_y = sd(y),
            correlation = cor(x, y))

# Plotting Linear Regressions
bdims %>%
  ggplot(aes(x = hgt, y = wgt)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE)

# Calculate slope and intercept
bdims_summary <- bdims %>%
  select(hgt, wgt) %>%
  summarize(N = n(), r = cor(wgt, hgt), mean_hgt = mean(hgt), sd_hgt = sd(hgt), mean_wgt = mean(wgt), sd_wgt = sd(wgt))

bdims_summary %>%
  mutate(slope = r * sd_wgt / sd_hgt,
         intercept = mean_wgt - slope * mean_hgt)

# Check the calculation
lm(formula = wgt ~ hgt, data = bdims)

# Linear model for weight as a function of height
lm(wgt ~ hgt, data = bdims)

# Linear model for SLG as a function of OBP
lm(SLG ~ OBP, data = mlbBat10)

# Log-linear model for body weight as a function of brain weight
mod <- lm(log10(BrainWt) ~ log10(BodyWt), data = mammals)

# Inspect the model and its elements
mod
coef(mod)
summary(mod)
fitted.values(mod)
residuals(mod)
sum(residuals(mod))
mean(residuals(mod))
hist(residuals(mod), breaks = 10)

# Load broom library and tidy up the model
library(broom)
augment(mod)

# Build another model using the body dimenstions dataset
mod <- lm(formula = wgt ~ hgt, data = bdims)
mod
coef(mod)
summary(mod)

# Mean of weights equal to mean of fitted values?
mean(bdims$wgt) == mean(fitted(mod))

# Mean of the residuals
mean(residuals(mod))

# Predict out of sample 
new_data <- data.frame(hgt = c(195, 200))
predict(mod, new_data)

pred_wgt <- broom::augment(mod, newdata = new_data)

# Plot data and new predictions
ggplot(data = bdims, aes(x = hgt, y = wgt)) +
  geom_point() + geom_smooth(method = 'lm') +
  geom_point(data = pred_wgt, aes(y = .fitted), size = 3, color = 'red')

# View summary of model
summary(mod)

# Compute the mean of the residuals
mean(residuals(mod))

# Compute RMSE
sqrt(sum(residuals(mod)^2) / df.residual(mod))

augment(mod) %>%
  arrange(desc(.cooksd))

# Model baseball batting statistics for HR on SB
mod <- lm(HR ~ SB, data = mlbBat10[mlbBat10$AB >= 400, ])

# Plot the regression line
mlbBat10 %>%
  filter(AB >= 400) %>%
  ggplot(aes(x = SB, y = HR)) +
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE)

# Rank points of high leverage
mod %>%
  augment() %>%
  arrange(desc(.hat)) %>%
  select(HR, SB, .fitted, .resid, .hat, .cooksd)

# Model baseball SLG on OBP
mod <- lm(SLG ~ OBP, data = mlbBat10[mlbBat10$AB >= 400, ])

# Plot SLG on OBP
mlbBat10 %>%
  filter(AB >=400) %>%
  ggplot(aes(x = OBP, y = SLG)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

# Rank points of high leverage
mod %>%
  augment() %>%
  arrange(desc(.hat)) %>%
  head(5)

# Rank influential points
mod %>%
  augment() %>%
  arrange(desc(.cooksd)) %>%
  head(5)

# Rank high leverage points
augment(mod) %>%
  arrange(desc(.hat), .cooksd) %>%
  head()

# Check slope, intercept, and R^2
coef(mod)
summary(mod)
