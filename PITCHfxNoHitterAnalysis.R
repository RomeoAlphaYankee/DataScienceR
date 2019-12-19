# PITCHfx analysis on 2015 Max Scherzer near perfect game against the Pirates

# Download data, perform exploratory analysis, and data visualization
# Load the pitchRx library
library(pitchRx)

# Other libraries will be needed
library(dplyr)
library(ggplot2)
library(RColorBrewer)

# Scrape some game data
# dat <- scrape(start = "2016-06-01", end = "2016-06-01")
dat <- scrape(game.ids = "gid_2015_06_20_pitmlb_wasmlb_1")

# Inspect the data
names(dat)
class(dat)
str(dat)


lapply(dat, FUN = dim)
lapply(dat, FUN = head, n =2)


# Extract the individual data frames from the data list
atbat <- dat$atbat
names(atbat)

pitch <- dat$pitch
names(pitch)
pitch$tfs

head(atbat)
head(pitch)

# Join the tables
nh <- inner_join(atbat, pitch, by = "num", suffix = c("_bat", "_pitch"))

# Filter and select the data needed for pitch plot
nh <- nh %>%  
  filter(inning_side_pitch == "top") %>%
  select(num, tfs, stand, event, inning_pitch, batter_name, des, tfs, start_speed, px, pz, pitch_type, count)

dim(nh)
names(nh)
head(nh)

# Create coorinates of strike zone
sz <- data.frame(x = c(-0.95, 0.95, 0.95, -0.95, -0.95), z = c(1.6, 1.6, 3.5, 3.5, 1.6))
sz

table(nh$pitch_type)

# Create a plot so superimpose on pitch locations
nh %>%
  ggplot(aes(x = px, y = pz)) +
  geom_point(aes(size = start_speed, color = pitch_type)) +
  scale_size(range = c(0.5, 2.5)) +
  geom_path(data = sz, aes(x = x, y = z)) +
  coord_equal() +
  labs(x = "Horizontal Position (ft.)", y = "Vertical Position (ft.)",
       size = "Speed", color = "Pitch") +
  facet_grid(.~stand)

# Clean up the pitch names to make them more readable
temp <- nh$pitch_type
temp[which(temp == "FF")] <- "Fastball"
temp[which(temp == "CU")] <- "Curveball"
temp[which(temp == "CH")] <- "Changeup"
temp[which(temp == "SL")] <- "Slider"
temp[which(temp == "FC")] <- "Cutter"

# Marge into no-hitter data frame
nh$pitch_description <- factor(temp, levels = c("Fastball", "Slider", "Cutter", "Changeup", "Curveball"))

# Create a coordinate for the batter location
nh$stand_xcoord <- ifelse(nh$stand == "R", -2, 2)

# Convert batter stance to a factor
nh$stand <- factor(nh$stand, levels = c("L", "R"))

# Scherzer faced so few batters, we can plot individual at bats on one plot
# Create a plot so superimpose batter name and handedness each at bat
nh %>%
  ggplot(aes(x = px, y = pz)) +
  geom_point(aes(size = start_speed, color = pitch_description)) +
  scale_size(range = c(0.5, 2.5)) +
  scale_color_manual(values = c("red2", "orange", "yellow2", "green", "blue")) +
  geom_path(data = sz, aes(x = x, y = z), lty = 2) +
  coord_equal() +
  labs(x = "Horizontal Position (ft.)", y = "Vertical Position (ft.)",
       size = "Speed", color = "Pitch") +
  facet_wrap(.~num) +
  geom_text(aes(x = stand_xcoord, y = 2.5, label = stand), size = 10) + 
  geom_text(aes(label = batter_name, x = 0, y = 0.5), size = 3) +
  geom_text(aes(label = paste("Inning:", inning_pitch)), x = 0, y = 4.5)

# Check the number of pitches thrown per inning
nh %>%
  rename(Inning = inning_pitch) %>%
  group_by(Inning) %>%
  summarize(Pitches = n())

# Focus on one batter and one at bat
batter <- "Pedro Alvarez"

nh %>%
  filter(batter_name == batter) %>%
  ggplot(aes(x = px, y = pz)) +
  geom_point(aes(size = start_speed, color = pitch_description)) +
  scale_size(range = c(0.5, 2.5)) +
  scale_color_manual(values = c(Fastball = "red2", Slider = "orange", Cutter = "yellow2", Changeup = "green", Curveball = "blue")) +
  geom_path(data = sz, aes(x = x, y = z), lty = 2) +
  coord_equal() +
  labs(x = "Horizontal Position (ft.)", y = "Vertical Position (ft.)",
       size = "Speed", color = "Pitch") +
  facet_wrap(.~num) +
  geom_text(aes(x = stand_xcoord, y = 2.5, label = stand), size = 10) + 
  geom_text(aes(label = batter_name, x = 0, y = 0.5), size = 4) +
  geom_text(aes(label = paste("Inning:", inning_pitch)), x = 0, y = 4.5, size = 4) +
  xlim(-2, 2) + ylim(0, 5)

# Let's see how lead off hitter Josh Harrison did
batter = "Josh Harrison"

nh %>%
  filter(batter_name == batter) %>%
  ggplot(aes(x = px, y = pz)) +
  geom_point(aes(size = start_speed, color = pitch_description)) +
  scale_size(range = c(0.5, 2.5)) +
  scale_color_manual(values = c(Fastball = "red2", Slider = "orange", Cutter = "yellow2", Changeup = "green", Curveball = "blue")) +
  geom_path(data = sz, aes(x = x, y = z), lty = 2) +
  coord_equal() +
  labs(x = "Horizontal Position (ft.)", y = "Vertical Position (ft.)",
       size = "Speed", color = "Pitch") +
  facet_wrap(.~num) +
  geom_text(aes(x = stand_xcoord, y = 2.5, label = stand), size = 10) + 
  geom_text(aes(label = batter_name, x = 0, y = 0.5), size = 4) +
  geom_text(aes(label = paste("Inning:", inning_pitch)), x = 0, y = 4.5, size = 4) +
  xlim(-2, 2) + ylim(0, 5)

# Incorporate pitch description
nh$pitch_result <- nh$des
nh$pitch_result

# Change "In play" to reflect the result of the event
library(stringr)

i <- str_detect(nh$des, "In play*")
nh$pitch_result[i] <- nh$event[i]


# Focus on speciffically on Pedro Alvarez in the fifth inningt
batter <- "Pedro Alvarez"
inning <- 8

nh %>%
  filter(batter_name == batter, inning_pitch == inning) %>%
  ggplot(aes(x = px, y = pz)) +
  geom_point(aes(size = start_speed, color = pitch_description)) +
  scale_size(range = c(0.5, 2.5)) +
  scale_color_manual(values = c(Fastball = "red2", Slider = "orange", Cutter = "yellow2", Changeup = "green", Curveball = "blue")) +
  geom_path(data = sz, aes(x = x, y = z), lty = 2) +
  coord_equal() +
  labs(x = "Horizontal Position (ft.)", y = "Vertical Position (ft.)",
       size = "Speed", color = "Pitch") +
  geom_text(aes(x = stand_xcoord, y = 2.5, label = stand), size = 10) + 
  ggtitle(paste0("Inning ", inning, ": ", batter)) +
  geom_text(aes(label = pitch_result, x = px, y = pz), nudge_y = .15) +
  xlim(-2, 2) + ylim(0, 5)
  

# Focus on speciffically on Jose Tabata ih the ninth inning
batter <- "Jose Tabata"
inning <- 9

# Just need to fix one thing
i <- str_detect(nh$des, "Hit By Pitch")
nh$pitch_result[i] <- "Unlikely HBP (ball)"

nh %>%
  filter(batter_name == batter, inning_pitch == inning) %>%
  ggplot(aes(x = px, y = pz)) +
  geom_point(aes(size = start_speed, color = pitch_description)) +
  scale_size(range = c(0.5, 2.5)) +
  scale_color_manual(values = c(Fastball = "red2", Slider = "orange", Cutter = "yellow2", Changeup = "green", Curveball = "blue")) +
  geom_path(data = sz, aes(x = x, y = z), lty = 2) +
  coord_equal() +
  labs(x = "Horizontal Position (ft.)", y = "Vertical Position (ft.)",
       size = "Speed", color = "Pitch") +
  geom_text(aes(x = stand_xcoord, y = 2.5, label = stand), size = 10) + 
  ggtitle(paste0("Inning ", inning, ": ", batter)) +
  geom_text(aes(label = pitch_result, x = px, y = pz), nudge_y = .15) +
  xlim(-2, 2) + ylim(0, 5)

# Let's plot the inside pitches from the game
nh %>%
  filter((stand == "L" & px > 1.2) | (stand == "R" & px < -1.2)) %>%
  ggplot(aes(x = px, y = pz)) +
  geom_point(aes(color = pitch_type)) +
  geom_path(data = sz, aes(x = x, y = z), lty = 2) +
  ggrepel::geom_text_repel(aes(label = pitch_result)) +
  ggtitle("Five Most Inside Pitches")

# Pitch count by at-bat in descending order
nh <- nh %>%
  arrange(tfs) 

# Sequence the pitches
pitches <- nh %>% 
  arrange(num) %>%
  count(num) 

nh %>% select(num, batter_name, balls, strikes, pitch_seq)

nh$pitch_seq <- unlist(lapply(pitches$n, seq))

# Now we can plot and analyze pitch sequences
batter <- "Pedro Alvarez"
inning <- 5

nh %>%
  filter(batter_name == batter, inning_pitch == inning) %>%
  ggplot(aes(x = px, y = pz)) +
  geom_point(aes(size = start_speed, color = pitch_description)) +
  scale_size(range = c(0.5, 2.5)) +
  scale_color_manual(values = c(Fastball = "red2", Slider = "orange", Cutter = "yellow2", Changeup = "green", Curveball = "blue")) +
  geom_path(data = sz, aes(x = x, y = z), lty = 2) +
  coord_equal() +
  labs(x = "Horizontal Position (ft.)", y = "Vertical Position (ft.)",
       size = "Speed", color = "Pitch") +
  geom_text(aes(x = stand_xcoord, y = 2.5, label = stand), size = 10) + 
  ggtitle(paste0("Inning ", inning, ": ", batter)) +
  geom_text(aes(label = pitch_result, x = px, y = pz), nudge_y = .15) +
  xlim(-2, 2) + ylim(0, 5) +
  geom_text(aes(label = pitch_seq, x = px, y = pz), nudge_y = - 0.15)


# Plot pitch selection by sequence
nh %>%
  group_by(pitch_seq) %>%
  ggplot(aes(x = factor(pitch_seq), fill = pitch_description)) +
  geom_bar(position = "fill") +
  labs(x = "Pitch", y = "Percent Thrown", fill = "Pitch Type") +
  ggtitle("Pitch Sequence")

# Let's filter it by two strike counts
library(tidyr)
nh <- separate(data = nh, col = count, into = c("balls", "strikes"), sep = "-")

nh[1:20, c(1, 2, 12, 13, 14, 17)]

nh %>%
  filter(strikes == 2) %>% 
  group_by(pitch_seq) %>%
  droplevels() %>%
  ungroup() %>%
  ggplot(aes(x = factor(pitch_seq), fill = pitch_description)) +
    geom_bar(position = "fill") +
    labs(x = "Pitch", y = "Percent Thrown", fill = "Pitch Type") +
    ggtitle("Pitch Sequence")

# Plot the pitch selection for three ball counts
nh %>%
  filter(balls == 3) %>%
  group_by(pitch_seq) %>%
  ggplot(aes(x = factor(pitch_seq), fill = pitch_description)) +
  geom_bar(position = "fill") +
  ggtitle("Pitch Selection With Tree-Ball Count") +
  labs(x = "Pitch in Sequence", y = "Pitch Selection", fill = "Pitch Type")
         
# Loop over the at-bats to create a scouting report flip-book covering all batters
# Save each plot to a .png file

# First scale the pitch speeds for consistent sizes across ab plots
nh$speed_scale <- (nh$start_speed - min(nh$start_speed)) / (max(nh$start_speed) - min(nh$start_speed))

for(i in unique(nh$num)){
  ab <- nh %>%
    filter(num == i) 
  
  batter <- ab$batter_name[1]
  inning <- ab$inning_pitch[1]
  
  plot <- ab %>%
    ggplot(aes(x = px, y = pz)) +
    geom_point(aes(size = start_speed, color = pitch_description)) +
    scale_size(range = c(min(ab$speed_scale * 2 + 0.5), max(ab$speed_scale * 2 + 0.5))) +
    scale_color_manual(values = c(Fastball = "red2", Slider = "orange", Cutter = "yellow2", Changeup = "green", Curveball = "blue")) +
    geom_path(data = sz, aes(x = x, y = z), lty = 2) +
    coord_equal() +
    labs(x = "Horizontal Position (ft.)", y = "Vertical Position (ft.)",
         size = "Speed", color = "Pitch") +
    geom_text(aes(x = stand_xcoord, y = 2.5, label = stand), size = 10) + 
    ggtitle(paste0("Inning ", inning, ": ", batter)) +
    geom_text(aes(label = pitch_result, x = px, y = pz), nudge_y = .15) +
    xlim(-2, 2) + ylim(0, 5) +
    geom_text(aes(label = pitch_seq, x = px, y = pz), nudge_y = - 0.15)

# Uncomment print or save plots, or both
#  print(plot)
  ggsave(paste("atbat", i, ".png", sep = ""), plot)
}
