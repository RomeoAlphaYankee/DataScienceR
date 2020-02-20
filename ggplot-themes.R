library(dplyr)
library(ggplot2)
library(ggthemes)

data("iris")
data("mtcars")

# Work with themes in ggplot 2 altering different elements including line, rect, and text
# Plot different backgrounds
z <- mtcars %>%
  ggplot(aes(x = wt, y = mpg, color = cyl)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  scale_color_brewer() +
  labs(title = "Car Efficiency Given Weight", color = "Cylinders") +
  xlab("Weight (lb/1000)") + ylab("Miles/(US) gallon)") 

z +
  theme(rect = element_blank()) +
  theme(plot.background = element_rect(fill = '#FEE0D2', color = "black", size = 3)) +
  facet_grid(.~cyl)

# Adjust the lines, add a border, and remove the grid
z +
  theme(rect = element_blank()) +
  theme(plot.background = element_rect(fill = '#FEE0D2', color = 'black', size = 3)) +
  theme(panel.grid = element_blank(), axis.line = element_line(color = "red"), axis.ticks = element_line(color = "red")) +
  facet_grid(.~cyl)

# Adjust the text
z2 <- z +
  theme(rect = element_blank()) +
  theme(plot.background = element_rect(fill = '#FEE0D2', color = 'black', size = 3)) +
  theme(panel.grid = element_blank(), axis.line = element_line(color = "red"), axis.ticks = element_line(color = "red")) +
  facet_grid(.~cyl) +
  theme(strip.text = element_text(size = 16, color = "red"),
        axis.title = element_text(color = "red", hjust = 0, face = "italic"),
        axis.text = element_text(color = "black")) 

# Move the legend to different positions
z2 +    theme(legend.position = c(0.85, 0.85))

z2 + theme(legend.direction = "horizontal")

z2 + theme(legend.position = 'none')

z3 <- z2 + theme(legend.position = "bottom") 

# Adjust the spacing and margins
library(grid)

z3 + theme(panel.spacing.x = unit(2, "cm"))


z3 + theme(panel.spacing.x = unit(2, "cm"),
           plot.margin = unit(c(1, 2, 1, 1), "cm"))

# Set themes with the iris dataset
z <- ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width, col = Species)) +
  geom_jitter(alpha = 0.7) +
  scale_color_brewer("Species", 
                     palette = "Dark2",
                     labels = c("Setosa", "Versicolor", "Verginica")) +
  scale_y_continuous("Width (cm)", limits = c(2, 4.5), expand = c(0, 0)) +
  scale_x_continuous("Length (cm)", limits = c(4, 8), expand = c(0, 0)) +
  ggtitle("Sepals") +
  coord_fixed(1)

# Create a theme for the iris plot
theme_iris <- theme(panel.background = element_blank(),
                    legend.background = element_blank(),
                    legend.key = element_blank(),
                    panel.grid = element_blank(),
                    axis.text = element_text(color = "black"),
                    axis.line = element_line(color = "black"))

# Try it out
z + theme_iris

# Use the theme on a plot from before
iris.smooth + theme_iris

# New plot
iris.smooth + theme_tufte()

base.plot + 
  scale_color_brewer(palette = "Dark2") +
  facet_grid(.~Species) + theme_tufte()

# Update themes for repeated use
z2 <- ggplot(mtcars, aes(x = wt, y = mpg, col = cyl)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  scale_color_brewer(palette = "Blues") +
  facet_grid(.~cyl)

z2

# Recreate the pink theme
theme_pink <- theme(panel.background = element_blank(),
                    legend.key = element_blank(),
                    legend.background = element_blank(),
                    strip.background = element_blank(),
                    plot.background = element_rect(fill = "#FEE0D2", color = "black", size = 3),
                    panel.grid = element_blank(),
                    axis.line = element_line(color = "red"),
                    axis.ticks = element_line(color = "red"),
                    strip.text = element_text(size = 16, color = "#99000D"),
                    axis.title.y = element_text(color = "#99000D", hjust = 0, face = "italic"),
                    axis.title.x = element_text(color = "#99000D", hjust = 0, face = "italic"),
                    axis.text = element_text(color = "black"),
                    legend.position = "none")

# Apply pink theme to plot
z2 + theme_pink

# Update the default theme and save the default in a new object
old <- theme_update(
  panel.background = element_blank(),
  legend.key = element_blank(),
  legend.background = element_blank(),
  strip.background = element_blank(),
  plot.background = element_rect(fill = "#FEE0D2", color = "black", size = 3),
  panel.grid = element_blank(),
  axis.line = element_line(color = "red"),
  axis.ticks = element_line(color = "red"),
  strip.text = element_text(size = 16, color = "#99000D"),
  axis.title.y = element_text(color = "#99000D", hjust = 0, face = "italic"),
  axis.title.x = element_text(color = "#99000D", hjust = 0, face = "italic"),
  axis.text = element_text(color = "black"),
  legend.position = "none")

# Display the plot with updated default theme
z2

# Restore the old default theme
theme_set(old)

# or theme_set(original)
# or theme_set(theme_gray())

# Check to see if the default theme is restored
z2

# Try ggthemes plots
z2 + theme_bw()
z2 + theme_wsj()
z2 + theme_fivethirtyeight()
z2 + theme_economist()

# Set the default to bw and test
theme_set(theme_bw())
z2

# Extend existing themes
z2 + theme_tufte()

# Add aditional elements
custom_theme <- theme_tufte() +
  theme(legend.position = c(0.9, 0.9),
        legend.title = element_text(size = 12, face = "italic"),
        axis.title = element_text(face = "bold", size = 14))

# Draw the custom plot
z + custom_theme

# Use theme set to set custom theme as default
theme_set(custom_theme)

# Plot z and z2 again
z
z2