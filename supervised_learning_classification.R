# Machine learning
# Supervised learning: Classification

# k-Nearest Neighbors
library(class)

signs <- read.csv("~/DataFile/R_Files/data/knn_traffic_signs.csv")
str(signs)

signs <- signs[ , -1]

# Create a training set and test set
train_signs <- signs[signs$sample == 'train', -1]
next_sign <- signs[signs$sample == 'example', -1]
test_signs <- signs[signs$sample == 'test', -1]

# Create a vector of labels
sign_types <- train_signs$sign_type

# Count the number of signs of each type
table(sign_types)

# Classify the next sign observed
knn(train = train_signs[ , -1], test = next_sign[-1], cl = sign_types)

# Check r10's average red level by sign type
aggregate(r10 ~ sign_type, data = train_signs, mean)

# Use the k nearest neighbors method to test remaining signs 
signs_pred <- knn(train = train_signs[-1], test = test_signs[-1], cl = sign_types)

# Create confusion matrix of the predicted versus actual values
signs_actual <- test_signs$sign_type
table(signs_pred, signs_actual)

# Compute the accuracy
mean(signs_pred == signs_actual)

# Test accuracy with different values of k
k_7 <- knn(train = train_signs[-1], test = test_signs[-1], cl = sign_types, k = 7)
mean(k_7 == signs_actual)

k_15 <- knn(train = train_signs[-1], test = test_signs[-1], cl = sign_types, k = 15)
mean(k_15 == signs_actual)

# Re-run with probabilities
signs_pred <- knn(train = train_signs[-1], test = test_signs[-1], cl = sign_types, k = 7, prob = TRUE)

# Use prob parameter to get the proportion of votes for the winning class
sign_prob <- attr(signs_pred, "prob")
head(signs_pred)
head(sign_prob)

# Examine probability by sign type
aggregate(attr(signs_pred, "prob") ~ signs_pred, data = signs_pred, mean)

# Naive Bayes

locations <- read.csv("~/Downloads/locations.csv")
head(locations)
str(locations)
