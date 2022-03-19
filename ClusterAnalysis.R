library(tidyverse)
library(dendextend)

# Load data of soccor play positions
lineup <- readRDS("https://assets.datacamp.com/production/repositories/1219/datasets/94af7037c5834527cc8799a9723ebf3b5af73015/lineup.rds")

# Calculate the Distance
dist_players <- dist(lineup, method = "euclidean")

# Perform the hierarchical clustering using the complete linkage
hc_players <- hclust(dist_players, method = "complete")

# Calculate the assignment vector with a k of 2
clusters_k2 <- cutree(hc_players, k = 2)

# Create a new data frame storing these results
lineup_k2_complete <- mutate(lineup, cluster = clusters_k2)

# Count the cluster assignments
count(lineup_k2_complete, cluster)

# Plot results
ggplot(lineup_k2_complete, aes(x = x, y = y, color = factor(cluster))) +
  geom_point()

# Create a dendrogram object from the hclust variable
dend_players <- as.dendrogram(hc_players)

# Plot the dendrogram
plot(dend_players)

# Color branches by cluster formed from the cut at a height of 20 & plot
dend_20 <- color_branches(dend_players, h = 20)

# Plot the dendrogram with clusters colored below height 20
plot(dend_20)

# Color branches by cluster formed from the cut at a height of 40 & plot
dend_40 <- color_branches(dend_players, h = 40)

# Plot the dendrogram with clusters colored below height 40
plot(dend_40)
