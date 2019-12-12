# Pitch Analysis of Zach Greinke's 2015 season
# This analysis will cover pitch types, velocity over time, pitch selection for given counts,
# pitch selection early vs. late in games, locations, etc..
library(dplyr)
library(ggplot2)
library(tidyr)
library(lubridate)

# Load the data on Zack Greinke 2015 season
greinke <- read_csv("https://assets.datacamp.com/production/course_943/datasets/greinke2015.csv")

head(greinke)
dim(greinke)

# Check for missing data
colSums(apply(greinke, 2, FUN = is.na))
summary(greinke)

greinke[is.na(greinke$break_angle), ]
greinke[is.na(greinke$start_speed), ]

# Clean up the three pitches with NA data
greinke <- subset(greinke, subset = !is.na(greinke$start_speed))

# Check structure
str(greinke)

# Correct dates
greinke$game_date <- mdy(greinke$game_date)

class(greinke$game_date)

# Separate the months
greinke <- separate(data = greinke, col = game_date, into = c("year", "month", "day"), sep = "-", remove = FALSE)
greinke$month <- as.numeric(greinke$month)

# Isolate the month of July
greinke$july <- ifelse(greinke$month == 7, "july", "other")

# Check results
head(greinke)
summary(factor(greinke$july))

# Plot pitch speeds
summary(greinke$start_speed)

greinke %>%
  ggplot(aes(x = start_speed)) +
  geom_histogram(binwidth = 1)

# Clearly multi-modal
# Plot by pitch type
greinke %>%
  ggplot(aes(x = start_speed)) +
  geom_histogram() +
  facet_wrap(~pitch_type)

# Plot speeds of main pitches in the same plot
greinke %>%
  filter(pitch_type %in% c("FF", "FT","SL", "CH", "CU")) %>%
  ggplot(aes(x = start_speed, fill = pitch_type)) +
  geom_bar(aes(color = pitch_type), position = "dodge", alpha = 0.4)

# Examine the mean pitch speed
greinke %>% 
  filter(pitch_type %in% c("FF", "FT","SL", "CH", "CU")) %>%
  group_by(pitch_type) %>%
  summarize(mean_speed = mean(start_speed))

# Examine the four seam fastball velocity in more detail
greinke %>%
  filter(pitch_type == "FF") %>%
  group_by(month) %>%
  summarize(mean_velocity = mean(start_speed))

# Create a boxplot by month
greinke %>%
  filter(pitch_type == "FF") %>%
  group_by(month) %>%
  ggplot(aes(x = factor(month), y = start_speed)) +
  geom_boxplot(aes(group = month)) +
  labs(x = "Month", y = "Velocity (MPH)", title = "Greinke four-seam fastball speed by month")

# It looks like fastball velocity improved from the beginning of the season onward
# Lets examine the month of July more closely when veolocity began to peak
july_ff <- subset(x = greinke, subset = pitch_type == "FF" & month == 7)
other_ff <- subset(x = greinke, subset = pitch_type == "FF" & month != 7)

# Make a fastball speed histogram for other months
hist(other_ff$start_speed,
     col = "#00009950", freq = FALSE,
     ylim = c(0, .35), xlab = "Velocity (mph)",
     main = "Greinke 4-Seam Fastball Velocity")

# Add a histogram for July
hist(july_ff$start_speed, add = TRUE, col = "#99000050", freq = FALSE)

# Draw vertical lines at the means of the two fastball histograms
abline(v = mean(other_ff$start_speed), col = "#00009950", lwd = 2)
abline(v = mean(july_ff$start_speed), col = "#99000050", lwd = 2)

# Table average four-seam fastball velocity by month
monthAvg <- data.frame(tapply(X = greinke$start_speed, INDEX = greinke$month, FUN = mean))
monthAvg[[2]] <- tapply(X = greinke$start_speed, INDEX = greinke$month, FUN = median)
names(monthAvg) <- c("mean", "median")
monthAvg

# Look at the four-seam fastball velocity by game
greinke_ff <- subset(greinke, subset = pitch_type == "FF")

ff_dt <- data.frame(tapply(greinke_ff$start_speed, greinke_ff$game_date, mean))
head(ff_dt)

ff_dt$game_date <- ymd(rownames(ff_dt))
colnames(ff_dt) <- c("start_speed", colnames(ff_dt)[-1])
row.names(ff_dt) <- NULL
head(ff_dt)

# Plot game-by-game 4-seam fastballs
plot(ff_dt$start_speed ~ ff_dt$game_date,
     lwd = 4, type = "l", ylim = c(88, 95),
     main = "Greinke 4-Seam Fastball Velocity",
     xlab = "Date", ylab = "Velocity (MPH)")

# Add the individual pitches
points(greinke_ff$start_speed ~ jitter(as.numeric(greinke_ff$game_date)), pch = 16, col = "#99004450")


### Explore pitch mix in greater detail
# Lets start by removing the one or two eephus pitches and intentional balls
greinke <- greinke[-c(which(greinke$pitch_type == "EP" | greinke$pitch_type == "IN")), ]

table(greinke$pitch_type, greinke$month)
round(prop.table(table(greinke$pitch_type, greinke$month), margin = 2), 3)

# Specifically look at the proportion of pitches in July vs. all other months combined
type_prop <- round(prop.table(table(greinke$pitch_type, greinke$july), margin = 2), 3)
type_prop <- as.data.frame(type_prop)
type_prop <- spread(type_prop, Var2, Freq)

type_prop$Difference <- (type_prop$july - type_prop$other) / type_prop$other

# Plot the change in pitch selection in the month of July
barplot(type_prop$Difference, names.arg = type_prop$Var1, 
        main = "Pitch Usage in July vs. Other Months", 
        ylab = "Percentage Change in July", 
        ylim = c(-0.3, 0.3))

# Explore the pitch usage across ball-strike counts
# Create a ball-strike count column
greinke$bs_count <- paste(greinke$balls, greinke$strikes, sep = "-")

# Create bs_count_tab
bs_count_tab <- table(greinke$bs_count, greinke$july)
bs_count_tab

# Create bs_month
bs_month <- round(prop.table(bs_count_tab, margin = 2),3)

# Print bs_month
bs_month
diff_bs <- round((bs_month[ , 1] - bs_month[ , 2]) / bs_month[ , 2], 3)

# Create a bar plot of the changes
barplot(diff_bs, main = "Ball-Strike Count Rate in July vs. Other Months", 
        ylab = "Percentage Change in July", ylim = c(-0.15, 0.15), las = 2)

# Clearly there were more batter friendly counts in July
# Examine pitch selection
type_bs <- table(greinke$pitch_type, greinke$bs_count)

round(prop.table(type_bs, margin = 2), 3)

# Investigate if pitch selection changes late in game
greinke$late <- ifelse(greinke$inning > 5, 1, 0)

late_table <- round(prop.table(table(greinke$pitch_type, factor(greinke$late)), margin = 2), 3)
late_table <- t(late_table)
rownames(late_table) <- c("Early", "Late")

# Plot early pitch selection against later pitch selection
barplot(late_table, beside = TRUE, col = c("red", "blue"), 
        main = "Early vs. Late In Game Pitch Selection",
        ylab = "Pitch Selection Proportion",
        legend = rownames(late_table))

# Investigate pitch location
greinke %>% group_by(batter_stand, pitch_type) %>%
  summarise(avg_pitch_height = mean(pz) * 12) %>%
  spread(batter_stand, avg_pitch_height) 

# Look at pitch height in July vs. other months
tapply(greinke$pz, greinke$july, mean) * 12

# Separate the data into left and right handed batters
greinke_lhb <- subset(greinke, batter_stand == "L")
greinke_rhb <- subset(greinke, batter_stand == "R")

# Compare the average horizontal position for RHB vs. LHB for the month of July and other months
tapply(greinke_lhb$px, greinke_lhb$july, mean) * 12
tapply(greinke_rhb$px, greinke_rhb$july, mean) * 12

# Plot pitch location window
plot(x = c(-2, 2), y = c(0, 5), type = "n",
     main = "Greinke Locational Zone Proportions",
     xlab = "Horizontal Location (ft.; Catcher's View)",
     ylab = "Vertical Location (ft.)")

# Add the grid lines
grid(lty = "solid", col = "black")

# Or we could do it with ggplot2
p <- greinke %>%
  filter(bs_count == "0-2") %>%
  ggplot(aes(x = px, y = pz, size = start_speed)) +
  geom_point(aes(color = pitch_type), alpha = 0.6) +
  annotate("rect", ymin = 1.5, ymax = 3.4, xmin = -0.83, xmax = 0.83, color = "blue", alpha = 0.2) +
  labs(title = "Greinke Pitch Location on 0-2 Count", 
       x = "Horizontal Location (ft. from plate)",
       y = "Vertical Location (ft.)",
       color = "Pitch") +
  facet_grid(~batter_stand)

# Use the plotly library to make the chart interactive
library(plotly)
ggplotly(p)


greinke %>%
  select(all) %>%
  ggplot(aes(x = pitch_type, y = start_speed)) +
  geom_boxplot()

# Examine at bat results to determine if increased fastball velocity resulted in lower contact rate
greinke_ff$bs_count <- paste(greinke_ff$balls, greinke_ff$strikes, sep = "-")

# Create a vector of no swing results
no_swing <- c("Ball", "Called Strike", "Ball In Dirt", "Hit By Pitch")

# Create a variable which is TRUE if the batter took a hack
greinke_ff$batter_swing <- ifelse(greinke_ff$pitch_result %in% no_swing, 0, 1)

# Create a subset of fastball pitches for batter swings
swing_ff <- subset(greinke_ff, greinke_ff$batter_swing == 1)

# Create a contact variable
no_contact <- c("Swinging Strike", "Missed Bunt")
swing_ff$contact <- ifelse(swing_ff$pitch_result %in% no_contact, 0, 1)

# find the mean 4-seam fastball velocity
mean(swing_ff$start_speed)

# Bin the velocities
swing_ff$velo_bin <- ifelse(swing_ff$start_speed < 90.5, "Slow", NA)
swing_ff$velo_bin <- ifelse(swing_ff$start_speed >= 90.5 & swing_ff$start_speed < 92.5, "Medium", swing_ff$velo_bin)
swing_ff$velo_bin <- ifelse(swing_ff$start_speed > 92.5, "Fast", swing_ff$velo_bin)

# Aggregate contact rate by velocity bin
tapply(X = swing_ff$contact, INDEX = swing_ff$velo_bin, FUN = mean)

# Examine the contact rate across pitch types
swing <- greinke[-which(greinke$pitch_result %in% no_swing), ]

table(swing$pitch_result)

# Create the contact column
swing$contact <- ifelse(swing$pitch_result %in% no_contact, 0, 1)

# contact rate by pitch type
swing %>%
  group_by(pitch_type) %>%
  summarize(contact_rate = mean(contact))
  
# Write a function to check the contact rate across quantiles
thirds = c(0, 1/3, 2/3, 1)

nrow(swing)

# Apply quantile function
lapply(split(swing$start_speed, as.factor(swing$pitch_type)), FUN = quantile, probs = thirds)

# Could have used tapply
tapply(swing$start_speed, INDEX = swing$pitch_type, FUN = quantile, probs = thirds)

# In order to have a dataframe instead of a list, write a for loop to function over the pitch types
types <- unique(swing$pitch_type)

pitch_quantiles <- NULL

for(type in types){
  pitch_quantiles <- cbind(pitch_quantiles, quantile(swing$start_speed[swing$pitch_type == type], probs = thirds))
}

# Clean up and print
colnames(pitch_quantiles) <- types
pitch_quantiles

# Trying a different way to bin pitch quantiles within the swing dataframe
bin_pitch_speed <- function(start_speed){
  as.integer(cut(start_speed, quantile(start_speed, probs = thirds), include.lowest = TRUE))
}

# Test it
mean(bin_pitch_speed(swing$start_speed[swing$pitch_type == "CU"]))

# Apply it to make sure it works for all pitches
tapply(swing$start_speed, INDEX = swing$pitch_type, FUN = bin_pitch_speed)

# Create a dummy variable
swing$velo_bin <- NA

# Loop over the pitch types and bin the velocities
for(type in types){
  swing$velo_bin[swing$pitch_type == type] <- bin_pitch_speed(swing$start_speed[swing$pitch_type == type])
}

# Maybe there was an easier way to do that with dplyr
swing <- swing %>%
  group_by(pitch_type) %>%
  mutate(velo_bin = bin_pitch_speed(start_speed))

# Check the results by binned velocity
swing %>%
  group_by(pitch_type, velo_bin) %>%
  summarize(contact_rate = mean(contact)) %>%
  spread(velo_bin, contact_rate)

# Check for differences for right vs. left batters
swing %>%
  group_by(batter_stand, pitch_type, velo_bin) %>%
  summarize(contact_rate = mean(contact)) %>%
  spread(velo_bin, contact_rate)

# How many pitches of each type were thrown with a 2 strike count
table(swing[swing$strikes == 2, "pitch_type"])

# Create a table detailing contact rate of each pitch type in a two strike count
swing %>%
  filter(strikes ==2) %>%
  group_by(pitch_type) %>%
  summarize(avg = mean(contact))

# Bin the pitch location data
pitch_bins <- greinke %>%
  filter(px > -2 & px < 2 & pz > 0 & pz < 5) %>%
  select(batter_stand, pitch_type, start_speed, px, pz) %>%
  mutate(x_bin = as.numeric(cut(px, seq(-2, 2, 1), include.lowest = TRUE)),
         y_bin = as.numeric(cut(pz, seq(0, 5, 1), include.lowest = TRUE))) 

head(pitch_bins, 10)

# Create a table of counts of pitch locations
bin_tab <- table(pitch_bins$y_bin, pitch_bins$x_bin)
bin_tab

# Convert to a proportion table
pitch_prop <- round(prop.table(bin_tab), 3)
as.data.frame(pitch_prop)

# Convert to a data frame and plot
data.frame(pitch_prop) %>%
  ggplot(aes(x = Var2, y = Var1, label = Freq)) +
  geom_text(size = 10) +
  annotate("rect", xmin = 1.5, xmax = 3.5, ymin = 1.5, ymax = 4.5, col = "blue", fill = 0) +
  labs(x = "Pitch location from center of plate", y = "Pitch height from plate")
  
# Complete the whole process in one step
# Select left batters
pitch_bins %>%
  filter(batter_stand == "L") %>%
  select(y_bin, x_bin) %>%
  table() %>%
  prop.table() %>%
  round(3) %>%
  as.data.frame() %>%
  ggplot(aes(x = x_bin, y = y_bin, label = Freq)) +
  geom_text(size = 10) +
  annotate("rect", xmin = 1.5, xmax = 3.5, ymin = 1.5, ymax = 4.5, col = "blue", fill = 0) +
  labs(x = "Pitch location from center of plate", y = "Pitch height from plate") +
  ggtitle("Left Batter View") +
  theme_classic() +
  scale_x_discrete( labels = c(-2, 1, 1, 2))

# Let's make it easier to plot and analyze pitch locations by creating a pitch location grid
# Create vector px
px <- rep(seq(-1.5, 1.5, 1), times = 5)

# Create vector pz
pz <- rep(seq(4.5, 0.5, -1), each = 4)

# Create vector of zone numbers
zone <- seq(1, 20, 1)

# Create locgrid for plotting
locgrid <- data.frame(zone = zone, px = px, pz = pz)

# Create a bin template to inner_join into our pitch bins
bin_template <- data.frame(zone = zone, 
                           x_bin = rep(seq(1, 4, 1), times = 5), 
                           y_bin = rep(seq(1, 5, 1), each = 4))

# Inner join to create a column with the pitch location zones
pitch_bins <- pitch_bins %>% left_join(bin_template, on = c(x_bin = x_bin, y_bin = y_bin))

head(pitch_bins)

# Load the gridExtra package
library(gridExtra)
library(RColorBrewer)

# Generate a clean data frame with contact data for left and right handed batters
# then assign a bin and replace the px and pz data with the grid coordinates
swings <- swing %>%
  filter(px > -2 & px < 2 & pz > 0 & pz < 5) %>%
  select(batter_stand, pitch_type, atbat_result, px, pz, balls, strikes, contact, batted_ball_velocity) %>%
  mutate(x_bin = as.numeric(cut(px, seq(-2, 2, 1), include.lowest = TRUE)),
         y_bin = as.numeric(cut(pz, seq(0, 5, 1), include.lowest = TRUE))) %>%
  left_join(bin_template, on = c(x_bin = x_bin, y_bon = y_bin)) %>%
  select(batter_stand, pitch_type, atbat_result, balls, strikes, contact, batted_ball_velocity, x_bin, y_bin, zone) %>%
  left_join(locgrid, on = c(zone = zone))

head(swings)

# Let's use our new swings data frame to plot some contact grids
swings %>%
  group_by(batter_stand, zone) %>%
  mutate(contact_rate = mean(contact)) %>%
  ungroup() %>%
  ggplot(aes(x = px, y = pz)) +
  geom_tile(aes(fill = contact_rate)) +
  scale_fill_gradientn(name = "contact_rate", 
                       limits = c(0.5, 1), 
                       breaks = seq(from = 0.5, to = 1, by = 0.1), 
                       colors = c(brewer.pal(n = 7, name = "Reds"))) +
  xlim(-2, 2) + ylim(0, 5) +
  ggtitle("Contact Rates") +
  labs(x = "Horizontal Location (ft.)", y = "Vertical Location (ft.)") +
  geom_text(aes(x = px, y = pz, label = round(contact_rate, 3))) +
  annotate("rect", xmin = -1, xmax = 1, ymin = 1, ymax = 4, col = "blue", fill = 0) +
  facet_grid(~batter_stand)

# Explore batted ball exit velocity
tapply(swings$batted_ball_velocity, INDEX = swings$atbat_result, FUN = mean, na.rm = TRUE)

subset(swings, subset = contact == 1 & !is.na(batted_ball_velocity))

# Lets build a plot of exit velocities
swings %>%
  filter(contact == 1 & !is.na(batted_ball_velocity)) %>%
  group_by(batter_stand, zone) %>%
  mutate(exit_speed = mean(batted_ball_velocity)) %>%
  ungroup() %>%
  ggplot(aes(x = px, y = pz)) +
  geom_tile(aes(fill = exit_speed)) +
  scale_fill_gradientn(name = "exit_speed", 
                       limits = c(50, 100), 
                       breaks = seq(from = 50, to = 100, by = 10), 
                       colors = c(brewer.pal(n = 5, name = "Reds"))) +
  facet_grid(~batter_stand) +
  geom_text(aes(x = px, y = pz, label = round(exit_speed))) +
  annotate("rect", xmin = -1, xmax = 1, ymin = 1, ymax = 4, col = "blue", fill = 0) +
  ggtitle("Batted Ball Exit Velocity") +
  labs(x = "Horizontal Position From Center of Plate", y = "Vertical Distance From Plate")
