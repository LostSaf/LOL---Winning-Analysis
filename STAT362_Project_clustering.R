# k-nearest neighbors and k-means clustering using 
# red and blue total gold

library(tidyverse)
library(factoextra)
library(class)
library(ggplot2)
library(cluster)

# Load data
league <- read.csv("C:/Users/harry/OneDrive/Desktop/Classes/2nd Year/STAT 362/STAT 362 Project/high_diamond_ranked_10min.csv")
df <- select(league, blueWins, blueTotalGold, redTotalGold,
             blueGoldDiff, redGoldDiff)
#
# k-nearest neighbors clustering
#

# Create training data sets
set.seed(1)

# For selecting 
p <- 0.90
random_index <- sample(nrow(df), nrow(df) * p) 

# our "x"
df_train <- df[random_index, -(2:3)]
df_test <- df[-random_index, -(2:3)]

# our "y"
df_train_labels <- df[random_index, ]$blueWins
df_test_labels <- df[-random_index, ]$blueWins

# Normalize data (so the scale isn't so crazy)
df_train_n <- df_train
df_test_n <- df_test

train_min <- apply(df_train, 2, min)
train_max <- apply(df_train, 2, max)

for (i in 1:ncol(df_train)) {
  df_train_n[, i] <- (df_train[, i] - train_min[i]) / (train_max[i] - train_min[i]) 
  # use the min and max from training data to normalize the testing data
  df_test_n[, i] <- (df_test[, i] - train_min[i]) / (train_max[i] - train_min[i]) 
}

# Perform k-nn clustering
knn_predicted <- knn(train = df_train_n, test = df_test_n, 
                     cl = df_train_labels, k = 21)

# 100% accuracy with k-nn (wow!!!)
table(df_test_labels, knn_predicted)


#
# k-means clustering
#
df_kmeans <- kmeans(x = scale(df[, 2:3]), centers = 2, nstart = 500)

# View clusters
df_kmeans$cluster

# Plot clusters
fviz_cluster(df_kmeans,  data = df[, 2:3], geom = "point",
             ggtheme = theme_classic(), 
             xlab = "Total Blue Gold",
             ylab = "Total Red Gold",
             main = "k-Means Clustering: Total Gold",
             ellipse.alpha = 0.2, ellipse = FALSE)

