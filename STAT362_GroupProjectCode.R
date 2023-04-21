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


#####################
# Linear Regression #
#####################

library(ggplot2)
library(ggpubr)
library(tidyverse)
library(ggpubr)
library(equatiomatic)

# Colour
my_palette <- c("firebrick1", "#0072B2")

# Simple Linear Regression Plots
kills <- ggplot(league, aes(x = redKills, y = redTotalGold)) +
  geom_point(size = 2, aes(colour = factor(blueWins))) +
  stat_smooth(method = lm, se = FALSE) +
  labs(x = "Red Team kills", y = "Red Team total gold", color = "Wins") + 
  scale_colour_manual(values = my_palette, labels = c("Red Team", "Blue Team")) +
  ggtitle("Kills vs. Total Gold") + theme_classic2()


first_blood <- ggplot(league, aes(x = redFirstBlood, y = redTotalGold)) +
  geom_point(size = 2, aes(colour = factor(blueWins))) +
  stat_smooth(method = lm, se = FALSE) +
  scale_x_continuous(breaks = round(seq(min(league$redFirstBlood), max(league$redFirstBlood), by = 1),1)) +
  labs(x = "Red team first bloods", y = "Red team total gold", color = "Blue team wins") +
  scale_colour_manual(values = my_palette, labels = c("Red Team", "Blue Team")) +
  ggtitle("First Blood vs. Total Gold") + theme_classic2()

wards_destroyed <- ggplot(league, aes(x = redWardsDestroyed, y = redTotalGold)) +
  geom_point(size = 2, aes(colour = factor(blueWins))) +
  stat_smooth(method = lm, se = FALSE) +
  scale_x_continuous(breaks = round(seq(min(league$redWardsDestroyed), max(league$redWardsDestroyed), by = 1),1)) +
  labs(x = "Red team wards destroyed", y = "Red team total gold", color = "Blue team wins") +
  scale_colour_manual(values = my_palette, labels = c("Red Team", "Blue Team")) +
  ggtitle("Wards Destroyed vs. Total Gold") + theme_classic2()

elite_monsters <- ggplot(league, aes(x = redEliteMonsters, y = redTotalGold)) +
  geom_point(size = 2, aes(colour = factor(blueWins))) +
  stat_smooth(method = lm, se = FALSE) +
  labs(x = "Red team elite monsters", y = "Red team total gold", color = "Blue team wins") +
  scale_x_continuous(breaks = round(seq(min(league$redEliteMonsters), max(league$redEliteMonsters), by = 1),1)) +
    scale_colour_manual(values = my_palette, labels = c("Red Team", "Blue Team")) +
    ggtitle("Elite Monsters vs. Total Gold") + theme_classic2()

dragons <- ggplot(league, aes(x = redDragons, y = redTotalGold)) +
  geom_point(size = 2, aes(colour = factor(blueWins))) +
  stat_smooth(method = lm, se = FALSE) +
  labs(x = "Red team dragons", y = "Red team total gold", color = "Blue team wins") +
  scale_x_continuous(breaks = round(seq(min(league$redDragons), max(league$redDragons), by = 1),1)) +
  scale_colour_manual(values = my_palette, labels = c("Red Team", "Blue Team")) +
  ggtitle("Dragons vs. Total Gold") + theme_classic2()

heralds <- ggplot(league, aes(x = redHeralds, y = redTotalGold)) +
  geom_point(size = 2, aes(colour = factor(blueWins))) +
  stat_smooth(method = lm, se = FALSE) +
  labs(x = "Red team heralds", y = "Red team total gold", color = "Blue team wins") +
  scale_x_continuous(breaks = round(seq(min(league$redHeralds), max(league$redHeralds), by = 1),1)) +
  scale_colour_manual(values = my_palette, labels = c("Red Team", "Blue Team")) +
  ggtitle("Heralds vs. Total Gold") + theme_classic2()

towers_destroyed <- ggplot(league, aes(x = redTowersDestroyed, y = redTotalGold)) +
geom_point(size = 2, aes(colour = factor(blueWins))) +
stat_smooth(method = lm, se = FALSE) +
labs(x = "Red team towers destroyed", y = "Red team total gold", color = "Blue team wins") + 
  scale_x_continuous(breaks = round(seq(min(league$redTowersDestroyed), max(league$redTowersDestroyed), by = 1),1)) +
  scale_colour_manual(values = my_palette, labels = c("Red Team", "Blue Team")) +
  ggtitle("Towers Destroyed vs. Total Gold") + theme_classic2()

CS_per_min <- ggplot(league, aes(x = redCSPerMin, y = redTotalGold)) +
  geom_point(size = 2, aes(colour = factor(blueWins))) +
  stat_smooth(method = lm, se = FALSE) +
  labs(x = "Red team CS", y = "Red team total gold", color = "Blue team wins") + 
  scale_x_continuous(breaks = round(seq(min(league$redTowersDestroyed), max(league$redTowersDestroyed), by = 1),1)) +
  scale_colour_manual(values = my_palette, labels = c("Red Team", "Blue Team")) +
  ggtitle("CS per min vs. Total Gold") + theme_classic2()

jungle_minions <- ggplot(league, aes(x = redTotalJungleMinionsKilled, y = redTotalGold)) +
  geom_point(size = 2, aes(colour = factor(blueWins))) +
  stat_smooth(method = lm, se = FALSE) +
  labs(x = "Red team jungle minions killed", y = "Red team total gold", color = "Blue team wins") + 
  scale_x_continuous(breaks = round(seq(min(league$redTowersDestroyed), max(league$redTowersDestroyed), by = 1),1)) +
  scale_colour_manual(values = my_palette, labels = c("Red Team", "Blue Team")) +
  ggtitle("Minions Killed vs. Total Gold") + theme_classic2()

ggarrange(kills,first_blood,wards_destroyed,elite_monsters,dragons,
          heralds,towers_destroyed, CS_per_min, jungle_minions, common.legend= TRUE)
          
#Simple Linear Regression
k <- lm(redTotalGold ~ redKills, data = high_diamond_ranked_10min)
f_b <- lm(redTotalGold ~ redFirstBlood, data = high_diamond_ranked_10min)
w <- lm(redTotalGold ~ redWardsDestroyed, data = high_diamond_ranked_10min)
e_m <- lm(redTotalGold ~ redEliteMonsters, data = high_diamond_ranked_10min)
d <- lm(redTotalGold ~ redDragons, data = high_diamond_ranked_10min)
h <- lm(redTotalGold ~ redHeralds, data = high_diamond_ranked_10min)
t_d <- lm(redTotalGold ~ redTowersDestroyed, data = high_diamond_ranked_10min)

summary(k)
summary(f_b)
summary(w)
summary(e_m)
summary(d)
summary(h)
summary(t_d)

#Multivariate Regression
model <- lm(redTotalGold ~ redKills + redFirstBlood + redWardsDestroyed + redEliteMonsters + redDragons + + redTowersDestroyed + redCSPerMin + redTotalJungleMinionsKilled, data = high_diamond_ranked_10min)
summary(model)

