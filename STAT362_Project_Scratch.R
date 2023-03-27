
# For random analysis
library(tidyverse)

# For ROC curve
library(pROC)

league <- read.csv("C:/Users/harry/OneDrive/Desktop/Classes/2nd Year/STAT 362/STAT 362 Project/high_diamond_ranked_10min.csv")

df <- select(league, blueWins, blueTotalGold, redTotalGold,
             blueGoldDiff, redGoldDiff)

redGoldAvg <- filter(df, blueWins == 0)
blueGoldAvg <- filter(df, blueWins == 1)

# Winning gold differential
m_red_gold_diff_w <- mean(redGoldAvg$redGoldDiff)
m_blue_gold_diff_w <- mean(blueGoldAvg$blueGoldDiff)

# Losing gold differential
m_blue_gold_diff_l <- mean(redGoldAvg$blueGoldDiff)
m_blue_gold_diff_l <- mean(blueGoldAvg$redGoldDiff)

# Logistic Regression
set.seed(1)
random_index <- sample(nrow(df), size = nrow(df) * 0.7)
train_data  <- df[random_index, ]
test_data <- df[-random_index, ]

# Blue wins compared against red gold differential
log_reg <- glm(blueWins ~ redGoldDiff, data = df, family = binomial)
summary(log_reg)

# Prediction
prob <- predict(log_reg, df, type = "response")
predicted_class <- ifelse(prob > 0.5, "pos", "neg")

# Confusion matrix
confmat <- table(predicted_class, df$blueWins)
confmat

# Accuracy
acc <- (3585 + 3564) / sum(t)
acc

# Sensitivity (true positive rate or hit rate)
sens <- 3564 / (3564 + 1366)
sens

# What could we do for classification?
  # Linear Discriminant Analysis (LDA) for supervised learning
  # k-means clustering to find "natural" clusters
  # Forests lol?

# ROC curve
roc_curve <- roc(df$blueWins, prob)

# Plot ROC curve (ver 1)
plot(
  roc_curve, print.thres = c(0.1, 0.5, 0.9), 
  ain = "ROC Curve", col = "blue",
  print.auc = TRUE,
  )

# Make specificity axis range from 1.0 to 0 (ver 2)
plot(
  test_roc, 
  print.auc = TRUE, 
  col = "red", 
  legacy.axes = TRUE, 
  specificities = rev(seq(0, 1, by = 0.1)), 
  main = "ROC Curve",
  xlim = c(1.0, 0.0)
)