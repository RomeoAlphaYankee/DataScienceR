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
