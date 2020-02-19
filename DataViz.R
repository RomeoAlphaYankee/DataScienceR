# Data visualization with ggplot2

library(dplyr)
library(ggplot2)

# Explore the mtcars data set
data("mtcars")

str(mtcars)

ggplot(mtcars, aes(x = mpg, fill = factor(cyl))) + geom_histogram()

ggplot(mtcars, aes(x = factor(cyl), fill = factor(am))) + geom_bar()

# A scatter plot with LOESS smooth
ggplot(mtcars, aes(x = wt, y = mpg)) +
  geom_point() +
  geom_smooth(method = "loess")

# A scatter plot with a Least Squares linear model
ggplot(mtcars, aes(x = wt, y = mpg)) +
  geom_point() + 
  geom_smooth(method = "lm")

# The previous plot, without CI ribbon
ggplot(mtcars, aes(x = wt, y = mpg)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

# The previous plot, without points
ggplot(mtcars, aes(x = wt, y = mpg)) +
  geom_smooth(method = "lm", se = FALSE)

# Define cyl as a factor variable and plot
ggplot(mtcars, aes(x = wt, y = mpg, color = factor(cyl))) +
  geom_point() +
  stat_smooth(method = "lm", se = FALSE)

# Define cyl as a factor variable, but group as one
ggplot(mtcars, aes(x = wt, y = mpg, color = factor(cyl))) +
  geom_point() +
  stat_smooth(method = "lm", se = FALSE, aes(group = 1))

# Plot with a different LOESS span

ggplot(mtcars, aes(x = wt, y = mpg)) +
  geom_point() +
  geom_smooth(se = FALSE, span = 0.7)


# Get the R Color Brewer
myColors <- c(RColorBrewer::brewer.pal(3, "Dark2"), "black")

# Plot with colors broken out by cylinders
ggplot(mtcars, aes(x = wt, y = mpg, col = factor(cyl))) +
  geom_point() +
  stat_smooth(method = "lm", se = FALSE) +
  stat_smooth(method = "loess",
              aes(group = 1, color = "All"),
              se = FALSE, span = 0.7) +
  scale_color_manual("Cylinders", values = myColors)

# All regressions on a single plot
mtcars %>%
  ggplot(aes(x = wt, y = mpg, color = factor(cyl))) +
  geom_point() +
  stat_smooth(method = "lm", se = FALSE) +
  stat_smooth(method = "lm", se = FALSE, aes(group = 1, color = "All")) +
  labs(title = "Miles Per Gallon as a Function of Weight and Cylinders", x = "Weight", y = "MPG") +
  scale_color_manual("Cylinders", values = myColors)

library(RColorBrewer)
library(car)
data('Vocab')
head(Vocab)

# Plot vocabulary against education with year as color
ggplot(Vocab, aes(x = education, y = vocabulary, color = year)) +
  geom_jitter(alpha = 0.2)

# Plot with a regression line
ggplot(Vocab, aes(x = education, y = vocabulary, color = factor(year))) +
  stat_smooth(method = "lm")

# Use stat_quantile instead of stat_smooth
ggplot(Vocab, aes(x = education, y = vocabulary, color = year, group = factor(year))) +
  stat_quantile(alpha = 0.6, size = 2) +
  scale_color_gradientn(colors = brewer.pal(9,"YlOrRd"))

# Too busy, just use the median
ggplot(Vocab, aes(x = education, y = vocabulary, color = year, group = factor(year))) +
  stat_quantile(alpha = 0.6, size = 2, quantiles = 0.5) +
  scale_color_gradientn(colors = brewer.pal(9,"YlOrRd"))

# Plot the data using jitter
p <- ggplot(Vocab, aes(x = education, y = vocabulary)) +
  geom_jitter(alpha = 0.2) +
  stat_smooth(method = "lm")

p

# Jitter plot with stat_sum
p + stat_sum()

# JItter with stat_sum set to a specific range
p + stat_sum() +
  scale_size(range = c(1, 10))

# Getting back to the mtcars dataset
mtcars$cyl <- as.factor(mtcars$cyl)
mtcars$am <- as.factor(mtcars$am)

# Base layers
wt.cyl.am <- ggplot(mtcars, aes(x = cyl, y = wt, col = am, fill = am, group = am))

# Define positions
posn.j <- position_jitter(width = 0.2)
posn.d <- position_dodge(width = 0.1)
posn.jd <- position_jitterdodge(jitter.width = 0.1, dodge.width = 0.2)

# Plot the base layer with a jitter dodge
wt.cyl.am +
  geom_point(position = posn.jd, alpha = 0.6)

# Use a stat summary layer to show the mean and sd
wt.cyl.am +
  geom_point(position = posn.jd, alpha = 0.6) +
  stat_summary(fun.data = mean_sdl, fun.args = list(mult = 1), position = posn.d)

# Same plot but show the 95% CI
wt.cyl.am + 
  geom_point(position = posn.jd, alpha = 0.6) +
  stat_summary(fun.data = mean_cl_normal, position = posn.d)

# Mean and SD - with T-tipped error bars
wt.cyl.am +
  stat_summary(geom = "point", fun.y = mean,
               position = posn.d) +
  stat_summary(geom = "errorbar", fun.data = mean_sdl,
               position = posn.d, fun.args = list(mult = 1), width = 0.1)

# A more standard boxplot
ggplot(mtcars, aes(x = cyl, y = wt, fill = am)) +
  geom_boxplot()

# Creat a plot using the 5 number summary
# Write custom functions to get the range and interquartile range for ggplot
gg_range <- function(x){
  data.frame(ymin = min(x),
             ymax = max(x))
}

med_IQR <- function(x){
  data.frame(y = median(x),
             ymin = quantile(x)[2],
             ymax = quantile(x)[4])
}

xx <- seq(1, 100)

# Use the base plot and add stat-summary calls
wt.cyl.am +
  stat_summary(geom = "linerange", fun.data = med_IQR,
               position = posn.d, size = 3) +
  stat_summary(geom = "linerange", fun.data = gg_range,
               position = posn.d, size = 3,
               alpha = 0.4) +
  stat_summary(geom = "point", fun.y = median,
               position = posn.d, size = 3,
               col = "black", shape = "X")

# Zoom a plot without distorting or dropping linear models or confidence intervals
iris.smooth <- iris %>% ggplot(aes(x = Sepal.Length, y = Sepal.Width, color = Species)) +
  geom_point(alpha = 0.7) +
  geom_smooth()

#Base plot
iris.smooth

# Normal use of x limits
iris.smooth +
  xlim(c(4.5, 5.5))

# Zoom to prevent distortion
iris.smooth +
  coord_cartesian(xlim = c(4.5, 5.5))

# Using scale x continuous
p <- ggplot(mtcars, aes(x = wt, y = hp, col = am)) +
  geom_point() + geom_smooth()

p + scale_x_continuous(limits = c(3, 6), expand = c(0, 0))

p + coord_cartesian(xlim = c(3, 6))

# Adjust the aspect ratio
base.plot <- ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width, col = Species)) +
  geom_jitter() +
  geom_smooth(method = "lm", se = FALSE)

# Base plot default aspect ratio
base.plot

# Use coord equal to force a 1:1 aspect ratio
base.plot +
  coord_equal()

# Create a bar plot
wide.bar <- ggplot(mtcars, aes(x = 1, fill = cyl)) +
  geom_bar()

# base plot
wide.bar

# Convert to polar chart
wide.bar +
  coord_polar()

# Convert to pie chart
wide.bar + 
  coord_polar(theta = "y")

# Thin bar plot
thin.bar <- ggplot(mtcars, aes(x = 1, fill = cyl)) +
  geom_bar(width = 0.1) +
  scale_x_continuous(limits = c(0.5, 1.5))

thin.bar

# Convert to donut chart
thin.bar +
  coord_polar(theta = "y")

# Faceting the mtcars base plot using facet grid
p <- ggplot(mtcars, aes(x = wt, y = mpg)) +
  geom_point()

# By row am
p + facet_grid(am~.)

# By column cyl
p + facet_grid(.~cyl)

# rows and cols using am and cyl
p + facet_grid(am~cyl)

# Use faceting and color to create a multi variable chart
mtcars$cyl_am <- paste(mtcars$cyl, mtcars$am, sep = "_")

# Generage colors
myCol <- rbind(brewer.pal(9, "Blues")[c(3, 6, 8)],
               brewer.pal(9, "Reds")[c(3, 6, 8)])

# Map cyl_am onto col
ggplot(mtcars, aes(x = wt, y = mpg, col = cyl_am)) +
  geom_point() +
  scale_color_manual(values = myCol)

# Grid facet on gear vs. vs
ggplot(mtcars, aes(x = wt, y = mpg, col = cyl_am)) +
  geom_point() +
  scale_color_manual(values = myCol) +
facet_grid(gear ~ vs)

# Map disp to size
ggplot(mtcars, aes(x = wt, y = mpg, col = cyl_am, size = disp)) +
  geom_point() +
  scale_color_manual(values = myCol) + 
  facet_grid(gear ~ vs)
 
