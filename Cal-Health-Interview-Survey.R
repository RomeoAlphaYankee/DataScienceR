# California Health Intervivew Survey Data Visualization
# Data available at chis.ucla.edu

library(dplyr)
library(ggplot2)

load('../data/CHIS2009_reduced_2.Rdata')

# Explore the dataset with summary and str
summary(adult)
str(adult)

# Age histogram
ggplot(adult, aes(x = SRAGE_P)) +
  geom_histogram()


# BMI value histogram
ggplot(adult, aes(x = BMI_P)) +
  geom_histogram()


# Age colored by BMI, binwidth = 1
ggplot(adult, aes(x = SRAGE_P, fill = factor(RBMI))) +
  geom_histogram(binwidth = 1)

# It would appear that the highest age category is 85 and above, causing a huge number of 85 responses
# Keep adults younger than or equal to 84
adult <- adult[adult$SRAGE_P <= 84, ] 

# Keep adults with BMI at least 16 and less than 52
adult <- adult[adult$BMI_P >= 16 & adult$BMI_P < 52, ]

# Relabel the race variable
adult$RACEHPR2 <- factor(adult$RACEHPR2, labels = c("Latino", "Asian", "African American", "White"))

# Relabel the BMI categories variable
adult$RBMI <- factor(adult$RBMI, labels = c("Under-weight", "Normal-weight", "Over-weight", "Obese"))

# Create a color scale used in the plot
BMI_fill <- scale_fill_brewer("BMI Category", palette = "Reds")

# Theme to fix category display in faceted plot
fix_strips <- theme(strip.text.y = element_text(angle = 0, hjust = 0, vjust = 0.1, size = 14),
                    strip.background = element_blank(),
                    legend.position = "none")

# Original histogram plus BMI_fill and customizations
ggplot(adult, aes (x = SRAGE_P, fill= RBMI)) + 
  geom_histogram(binwidth = 1) +
  fix_strips +
  BMI_fill +
  facet_grid(RBMI~.) +
  theme_classic()

# Density histogram
ggplot(adult, aes (x = SRAGE_P, fill= factor(RBMI))) + 
  geom_histogram(aes(y = ..density..), binwidth = 1) +
  BMI_fill

# Density fill plot
ggplot(adult, aes (x = SRAGE_P, fill= factor(RBMI))) + 
  geom_histogram(aes(y = ..count.. / sum(..count..)), binwidth = 1, position = "fill") +
  BMI_fill

# Facet the accurate frequency histogram by doing the calculations outside of ggplot
# Create a table of age and BMI
DF <- table(adult$RBMI, adult$SRAGE_P)

# Get the frequency of each group
DF_freq <- apply(DF, 2, function(x) x / sum(x))

# Reshape the frequency dataframe
library(reshape2)

DF_melted <- melt(DF_freq)

# Rename the variables
names(DF_melted) <- c("FILL", "X", "value")

# Now we can facet by BMI category
ggplot(DF_melted, aes(x = X, y = value, fill = FILL)) +
  geom_col(position = "stack") +
  BMI_fill +
  facet_grid(FILL ~ .)
  
# Marimekko / Mosaic Plot
# Create a contingency table
DF <- as.data.frame.matrix(table(adult$SRAGE_P, adult$RBMI))

# Create groupSum, xmax and xmin columns
DF$groupSum <- rowSums(DF)
DF$xmax <- cumsum(DF$groupSum)
DF$xmin <- DF$xmax - DF$groupSum

DF$groupSum <- NULL

# Copy row names to variable X
DF$X <- row.names(DF)

# Reshape the dataframe
DF_melted <- melt(DF, id.vars = c("X", "xmin", "xmax"), variable.name = "FILL")

# Calculate ymin and ymax
DF_melted <- DF_melted %>%
  group_by(X) %>%
  mutate(ymax = cumsum(value / sum(value)),
         ymin = ymax - value / sum(value))

head(DF_melted)
tail(DF_melted)

# Plot rectangles
library(ggthemes)
ggplot(DF_melted, aes(ymin = ymin,
                      ymax = ymax,
                      xmin = xmin,
                      xmax = xmax,
                      fill = FILL)) +
  geom_rect(colour = "white") +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  BMI_fill +
  theme_tufte()

# Create a plot that visualizes under and over representation
# Perform chi.sq test (RBMI and SRAGE_P)
results <- chisq.test(table(adult$RBMI, adult$SRAGE_P))

# Melt results$residuals and store as resid
resid <- melt(results$residuals)

# Change names of resid
colnames(resid) <- c("FILL", "X", "residual")

# merge the two datasets:
DF_all <- merge(DF_melted, resid)

# Update plot command
P <- ggplot(DF_all, aes(ymin = ymin,
                      ymax = ymax,
                      xmin = xmin,
                      xmax = xmax,
                      fill = residual)) +
  geom_rect() +
  scale_fill_gradient2() +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  theme_tufte()

P

# Position for labels on y axis
index <- DF_all$xmax == max(DF_all$xmax)
DF_all$yposn <- DF_all$ymin[index] + (DF_all$ymax[index] - DF_all$ymin[index])/2

# Geom_text for BMI 
p1 <- P %+% DF_all + 
  geom_text(aes(x = max(xmax), 
                y = yposn,
                label = FILL),
            size = 3, hjust = 1,
            show.legend  = FALSE)
p1

# Position for labels on x axis
DF_all$xposn <- DF_all$xmin + (DF_all$xmax - DF_all$xmin)/2

# geom_text for ages 
p1 %+% DF_all + 
  geom_text(aes(x = xposn, label = X),
            y = 1, angle = 90,
            size = 3, hjust = 1,
            show.legend = FALSE)