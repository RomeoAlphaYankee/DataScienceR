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

# Best practices: Bar Plots

# Set factors
mtcars$cyl <- as.factor(mtcars$cyl)
mtcars$am <- as.factor(mtcars$am)

# Base plot
m <- ggplot(mtcars, aes(x = cyl, y = wt))

# Dynamite plot
m + stat_summary(fun.y = mean, geom = "bar", fill = "skyblue") +
  stat_summary(fun.data = mean_sdl, fun.args = list(mult = 1), geom = "errorbar", width = 0.1)

# Add transmission type to base plot
m <- ggplot(mtcars, aes(x = cyl, y = wt, col = am, fill = am))

# Dynamite plot
m + 
  stat_summary(fun.y = mean, geom = "bar") +
  stat_summary(fun.data = mean_sdl, fun.args = list(mult = 1), geom = "errorbar", width = 0.1)

# Dynamite plot w/ position dodge
m + 
  stat_summary(fun.y = mean, geom = "bar", position = "dodge") +
  stat_summary(fun.data = mean_sdl, fun.args = list(mult = 1), geom = "errorbar", width = 0.1, position = "dodge")

# Set the dodge pssn manually
posn.d <- position_dodge(0.9)

# Rederaw the plot
m + 
  stat_summary(fun.y = mean, geom = "bar", position = posn.d) +
  stat_summary(fun.data = mean_sdl, fun.args = list(mult = 1), geom = "errorbar", width = 0.1, position = posn.d, color = "black")

# Create a summary dataframe for easier and more descriptive plotting
mtcars.cyl <- mtcars %>%
  group_by(cyl) %>%
  summarise(wt.avg = mean(wt), sd = sd(wt), n = n(), prop = n / nrow(mtcars))

# Establish a new base plot using summary data
m <- ggplot(mtcars.cyl, aes(x = cyl, y = wt.avg))

# Add a geom bar layer
m + geom_bar(stat = "identity", fill = "skyblue")

# Same plot using geom_col
m + geom_col(fill = "skyblue", width = mtcars.cyl$prop) +
  geom_errorbar(aes(ymin = wt.avg - sd, ymax = wt.avg + sd), width = 0.1)

# Bar chart as a replacement for proportional pie charts
# First the bar chart
ggplot(mtcars, aes(x = factor(cyl), fill = am)) +
  geom_bar(position = "fill")

# Convert to pie using geom_polar
ggplot(mtcars, aes(x = factor(cyl), fill = am)) +
  geom_bar(position = "fill") +
  facet_grid(. ~ cyl) + # Facets
  coord_polar(theta = "y") + # Coordinates
  theme_void() 

# Parallel coordinates plot using GGally
library(GGally)

# All columns except am
group_by_am <- 9
my_names_am <- (1:11)[-group_by_am]

# Basic parallel plot - each variable plotted as a z-score transformation
ggparcoord(mtcars, my_names_am, groupColumn = group_by_am, alpha = 0.8)

# SPLOM plot matrix using ggpairs
mtcars2 <- mtcars[ , c(1, 3, 5, 6, 7)]
ggpairs(mtcars2)

# Anothor pairs plot using ggpairs
mtcars3 <- mtcars[ , 1:5]

ggpairs(mtcars3)

# Heat maps: best practices
myColors <- brewer.pal(9, "Reds")

# Build the heat map
library(lattice)
data('barley')

str(barley)

ggplot(barley, aes(x = year,, y = variety, fill = yield)) + 
  geom_tile() +
  facet_wrap(~site, ncol = 1) +
  scale_fill_gradientn(colors = myColors)

# Heat map alternatives, line plot
ggplot(barley, aes(x = year, y = yield, color = variety, group = variety)) +
  geom_line() +
  facet_grid(.~site)

# Heat map alternaties, ribbon plot
# Create overlapping ribbon plot from scratch
ggplot(barley, aes(x = year, y = yield, color = site, group = site, fill = site)) + 
  stat_summary(fun.y = mean, geom = "line") +
  stat_summary(fun.data = mean_sdl, fun.args = list(mult = 1), geom = "ribbon", col = NA, alpha = 0.1)
