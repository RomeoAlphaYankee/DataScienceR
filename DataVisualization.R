library(ggplot2)
library(dplyr)

data("mtcars")

# Base R plots of mtcars dataset
plot(mtcars$wt, mtcars$mpg, col = mtcars$cyl, pch = 19, cex = 0.7)
plot(mtcars$wt, mtcars$mpg, col = factor(mtcars$cyl), pch = 19, cex = 1.5)

# Use lm() to calculate a linear model and save it as carModel
carModel <- lm(mpg ~ wt, data = mtcars)

# Basic plot
mtcars$cyl <- as.factor(mtcars$cyl)
plot(mtcars$wt, mtcars$mpg, col = mtcars$cyl, pch = 19, cex = 0.8)
abline(carModel, lty = 2, cex = 2)

# Plot each subset efficiently with lapply
plot(mtcars$wt, mtcars$mpg, col = mtcars$cyl, pch = 19)
lapply(mtcars$cyl, function(x) {
  abline(lm(mpg ~ wt, mtcars, subset = (cyl == x)), col = x, lty = 3)
})
legend(x = 5, y = 33, legend = levels(mtcars$cyl), col = 1:3, pch = 19)

# Recreate the base plot utilizing ggplot2

# Plot over cylinder as a factor
mtcars %>% ggplot(aes(x = wt, y = mpg, col = factor(cyl))) +
  geom_point(shape = 1, size = 4)


ggplot(mtcars, aes(x = wt, y = mpg, col = factor(am), fill = cyl)) +
  geom_point(shape = 21, size = 4, alpha = 0.6)

# Set the fill aesthetic; color, size and shape attributes with hexidecimal color
my_color <- "#4ABEFF"

ggplot(mtcars, aes(x = wt, y = mpg, fill = cyl)) +
  geom_point(color = my_color, size = 10, shape = 23)

# Fit mpg, qsec, a/m, hp/wt, and cyl data all into one plot
ggplot(mtcars, aes(x = mpg, y = qsec, 
                   col = factor(cyl), 
                   shape = factor(am), 
                   size = (hp / wt))) +
  geom_point() + 
  labs(x = "Miles per gallon", y = "Quarter mile in seconds",
       color = "Cylinder", size = "HP/Weight") +
  scale_shape_discrete("Trans", labels = c("Auto", "Manual"))

# More data plus a regression line
mtcars %>% ggplot(aes(x = wt, y = mpg)) +
  geom_point(aes(size = disp, color = factor(cyl))) +
  geom_smooth(method = "lm", se = FALSE)

# Plot with individual regression lines for each cylinder factor and for all data
mtcars %>%
  ggplot(aes(x = wt, y = mpg, col = cyl)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  geom_smooth(aes(group = 1), method = "lm", se = FALSE, linetype = 2)

# Barplot cylinders vs. transmission
val = c("#E41A1C", "#377EB8")
lab = c("Manual", "Automatic")
ggplot(mtcars, aes(x = factor(cyl), fill = factor(am))) +
  geom_bar(position = "dodge") +
  scale_x_discrete("Cylinders") + 
  scale_y_continuous("Numbers") +
  scale_fill_manual("Transmission", 
                    values = val,
                    labels = lab)

# Jitter plot ot avoid overplotting
mtcars %>%
  ggplot(aes(x = cyl, y = wt)) +
  geom_point(position = position_jitter(0.1))

# Plot a histogram of MPG
mtcars %>%
  ggplot(aes(x = mpg)) +
  geom_histogram(aes(y = ..density..), binwidth = 1, fill = "#377EB8")

# Create a barplot of cylinders by transmission which overlap
mtcars %>% 
  ggplot(aes(x = factor(cyl), fill = factor(am))) +
  geom_bar(position = position_dodge(width = 0.5), alpha = 0.6)

# Use qplot to make a quick plot
qplot(wt, mpg, data = mtcars)

qplot(x = factor(cyl), data = mtcars, xlab = "Cylinders")

qplot(x = factor(cyl), y = factor(vs), data = mtcars, geom = "jitter")

qplot(factor(cyl), wt, data = mtcars, fill = factor(am), geom = "dotplot",
      binaxis = "y", stackdir = "center")

# Explore the diamonds dataset
data("diamonds")

str(diamonds)
levels(diamonds$color)

diamonds %>%
  ggplot(aes(x = carat, y = price)) +
  geom_point(aes(color = color, alpha = 0.4)) + 
  geom_smooth()

dia_plot <- diamonds %>% ggplot(aes(x = carat, y = price))

dia_plot + geom_point()

dia_plot + geom_point(aes(color = clarity)) +
  facet_wrap(~clarity)

dia_plot <- dia_plot + geom_point(alpha = 0.2)

dia_plot + geom_smooth(aes(col = clarity), se = FALSE)


# Explore the iris dataset
data("iris")
str(iris)

plot(iris$Sepal.Length, iris$Sepal.Width, xlab = "Length", ylab = "Length", type = "p")
points(iris$Petal.Length, iris$Petal.Width, col = "red")

ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width)) +
  geom_point(aes(col = Species))

iris %>%
  ggplot(aes(x = Sepal.Length, y = Sepal.Width)) + 
  geom_jitter(alpha = 0.6) +
  facet_wrap(~Species) +
  stat_smooth(method = "lm", se = F, col = "blue")

# Transform the iris dataset into tidy format for visualization
library(tidyr)

# Gather the iris data into a longer dataset and split the names of the part and the dimension
iris.long <- gather(iris, "part", "data",  -Species) %>% 
  separate(part, into = c("part", "dimension")) 

colnames(iris.long) <- c("Species", "Part", "Measure", "Value")

head(iris.long)

# Now create a wide dataset with Width and Length as the variables
# Separate the length from the width
iris.length <- iris.long %>%
  filter(dimension == "Length")

iris.width <- iris.long %>%
  filter(dimension == "Width")

# Merge the length and width data in a tidy format
iris.length <- iris.length[ , c(1, 2, 4)]

iris.wide <- cbind(iris.length, iris.width$data)

colnames(iris.wide) <- c("Species", "Part", "Length", "Width")

head(iris.wide)

# Another way, more tidy
iris$flower <- 1:nrow(iris)

iris.wide <- iris %>% 
  gather("key", "value", -Species, -flower) %>%
  separate(key, into = c("Part", "Measure")) %>% 
  spread(Measure, value) %>% 
  select(Species, Part, Length, Width)

# Plot the wide tidy iris data
ggplot(iris.wide, aes(x = Length, y = Width, col = Part)) +
  geom_point() +
  facet_wrap(~Species)

# Plot the long tidy iris data
ggplot(iris.long, aes(x = Species, y = Value, col = Part)) +
  geom_jitter() +
  facet_grid(. ~ Measure)

# Use qplot for basic plots
qplot(Sepal.Length, Sepal.Width, data = iris, color = Species, alpha = I(0.5), geom = "jitter")

# Explore the economic data in the ggplot2 packcage
data("economics")

head(economics)

# Calculate and plot unemployment percentage rate
economics %>%
  ggplot(aes(x = date, y = unemploy / pop)) +
  geom_line()

# Add recession data
recess <- data.frame(begin = as.Date(c('1969-12-01', '1973-11-01', '1980-01-01', '1981-07-01', '1990-07-01', '2001-03-01', '2008-07-01'), format = "%Y-%m-%d"),
                     end = as.Date(c('1970-11-01', '1975-03-01', '1980-07-01', '1982-11-01', '1991-03-01', '2001-11-01', '2009-07-01'), format = "%Y-%m-%d"))

# Replot with recessions in shaded areas
economics %>%
  ggplot(aes(x = date, y = unemploy / pop)) + 
  geom_rect(data = recess, aes(xmin = begin, xmax = end, ymin = -Inf, ymax = +Inf),
            inherit.aes = FALSE, color = "red", alpha = 0.2) +
  geom_line()

# A plot to explore the weight of chickens, and determine which diet is best
data("ChickWeight")

ggplot(ChickWeight, aes(x = Time, y = weight, color = Diet)) +
  geom_line(aes(group = Chick), alpha = 0.3) + 
  geom_smooth(lwd = 2, se = FALSE)