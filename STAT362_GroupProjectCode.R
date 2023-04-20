######################################
# League of Legends Winning Analysis #
#                                    #
# STAT 362 Winter 2023               #
######################################

# Techniques used:
  # k-nearest neigbours clustering
  # k-means clustering
  # logistic regression
  # classification trees

library(tidyverse)
library(factoextra)
library(class)
library(ggplot2)
library(cluster)
library(tree)
library(pROC)
library(caret)

# Load data, calculate needed values
league <- read.csv("C:/Users/harry/OneDrive/Desktop/Classes/2nd Year/STAT 362/STAT 362 Project/high_diamond_ranked_10min.csv")
df <- select(league, blueWins, blueTotalGold, redTotalGold,
             blueGoldDiff, redGoldDiff)

# Average red and blue gold
redGoldAvg <- filter(df, blueWins == 0)
blueGoldAvg <- filter(df, blueWins == 1)

# Winning gold differential
m_red_gold_diff_w <- mean(redGoldAvg$redGoldDiff)
m_blue_gold_diff_w <- mean(blueGoldAvg$blueGoldDiff)

# Losing gold differential
m_blue_gold_diff_l <- mean(redGoldAvg$blueGoldDiff)
m_blue_gold_diff_l <- mean(blueGoldAvg$redGoldDiff)

# Random indicies for training and testing sets of data
set.seed(1)
random_index <- sample(nrow(df), size = nrow(df) * 0.7)


#######################
# Logistic Regression #
#######################

# Blue wins compared against red gold differential
log_reg <- glm(blueWins ~ redGoldDiff, data = df, family = binomial)
summary(log_reg)

# Prediction
prob <- predict(log_reg, df, type = "response")
predicted_class <- ifelse(prob > 0.5, "pos", "neg")

# Confusion matrix
confmat_lr <- table(predicted_class, df$blueWins)
confmat_lr

# Accuracy
acc_lr <- (sum(diag(confmat_lr)) / sum(confmat_lr))
acc_lr

# ROC Curve for logistic regression classification
roc_curve <- roc(df$blueWins, prob)
plot(roc_curve, ain = "ROC Curve", col = "blue",print.auc = TRUE)


###################################
# k-Nearest Neighbours Clustering #
###################################

# Split into training and testing data, get labels (for all clustering)
df_train_cl <- df[random_index, 1:3]
df_test_cl <- df[-random_index, 1:3]
df_train_labels_cl <- df[random_index, ]$blueWins
df_test_labels_cl <- df[-random_index, ]$blueWins

# Normalize data
df_train_n_cl <- df_train_cl
df_test_n_cl <- df_test_cl

train_min_cl <- apply(df_train_cl, 2, min)
train_max_cl <- apply(df_train_cl, 2, max)

for (i in 1:ncol(df_train_cl)) {
  df_train_n_cl[, i] <- (df_train_cl[, i] - train_min_cl[i]) / (train_max_cl[i] - train_min_cl[i]) 
  
  # use the min and max from training data to normalize the testing data
  df_test_n_cl[, i] <- (df_test_cl[, i] - train_min_cl[i]) / (train_max_cl[i] - train_min_cl[i]) 
}

# Perform clustering
knn_predicted <- knn(train = df_train_cl, test = df_test_cl, 
                     cl = df_train_labels_cl, k = 94)

# Confusion matrix
confmat_knn <- table(df_test_labels_cl, knn_predicted)
confmat_knn

# Accuracy
acc_knn <- sum(diag(confmat_knn) / sum(confmat_knn))
acc_knn

######################
# k-Means Clustering #
######################
kmeans_predicted <- kmeans(x = df_train_n_cl[, 2:3], centers = 2, nstart = 500)

# Adjust clusters to 0, and 1 so that we can compute the accuracy correctly
kmeans_predicted$cluster - 1

# Confusion matrix
confmat_kmeans <- table(df_train_labels_cl, kmeans_predicted$cluster)
confmat_kmeans

# Clustering accuracy
acc_kmeans <- sum(diag(confmat_kmeans) / sum(confmat_kmeans))
acc_kmeans

# Visualize clusters
fviz_cluster(kmeans_predicted,  data = df_train_n_cl[, 2:3], geom = "point", 
             stand = FALSE, ggtheme = theme_classic(), xlab = "Total Blue Gold",
             ylab = "Total Red Gold", ellipse.alpha = 0.2, ellipse = FALSE,
             main = "k-Means Clustering: Total Gold with Predicted Labels")


###################
# Regression Tree #
###################

# Split into training and testing data, get labels
df_train_tree <- df[random_index, 1:3]
df_test_tree <- df[-random_index, 1:3]

df_train_labels_tree <- df[random_index, ]$blueWins
df_test_labels_tree <- df[-random_index, ]$blueWins

# Fit tree
prediction_tree <- tree(as.factor(df_test_labels_tree) ~ redTotalGold + blueTotalGold, data = df_test_tree)
summary(prediction_tree)

# Get predicted labels
predicted_labels <- predict(prediction_tree, type = "class")

# Confusion matrix
confmat_tree <- table(predicted_labels, df_test_labels_tree)
confmat_tree

# Accuracy
acc_tree <- sum(diag(confmat_tree)) / sum(confmat_tree)
acc_tree

# Display tree
plot(prediction_tree)
text(prediction_tree)

             


