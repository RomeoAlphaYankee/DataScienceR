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