######
# Title: Prediction of Red Wine Quality using linear discriminant analysis
# Author: Jungbin Ahn
######

# Required packages

# Data Manipulation
library(MASS) 
library(caTools) 
library(tidyverse) 

# Visualization tool
library(ggplot2)


# Calling dataset and preparation
wine <- read.csv("winequality-red.csv")
head(wine)
summary(wine)
summary(is.na(wine)) # Clarifying that there's no NA values in dataset

# Distribution of Wine Quality
barplot(table(wine$quality), main = "Barplot of Wine Quality", xlab = "Quality", col = "red")
# Scatter plot for red wine data
plot(wine) 

# Preparation of train and test dataset
set.seed(7175)
sample <- sample.split(wine$quality, SplitRatio = .75)
train.wine = subset( wine , sample == TRUE)
test.wine = subset( wine, sample == FALSE)


# LDA using train data
wine.model <- lda(quality~., data = train.wine)
wine.model # LD1 and LD2 explains 94% variation of data. 1 & 2 is enough to construct LDA. 

# Visualization
lda.data <- cbind(train.wine, predict(wine.model)$x)
ggplot(lda.data, aes(LD1, LD2)) + geom_point(aes(color = quality)) + scale_color_gradientn(colours = rainbow(6))


# Prediction using LDA model
wine.pred <- wine.model %>% predict(test.wine)
# Model Accuracy
mean( wine.pred$class == test.wine$quality )

