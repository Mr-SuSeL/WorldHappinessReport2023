#importing libraries
library(plyr)
library(dplyr)
library(tidyverse)
#library(caTools)
library(ggplot2)
library(ggthemes)
#library(reshape2)
#library(data.table)
#library(rwa)
library(ggpubr)
library(tidyr)
#library(corrgram)       
library(corrplot) # do not know if I do corr matrix... hmmm
library(zoo)
library(qape)

#Loading dataset
happy_data <- read.csv("WHR2023.csv")
str(happy_data)
names(happy_data)
head(happy_data)
dim(happy_data)
# seeing what is a garbage
happy_data[ , 13:18]
happy_data[66, ]
#dropping unnecessary columns
happy_data <- happy_data[, -c(3,4,5,13, 14, 15, 16, 17, 18)] 
# ------------------------------------------------------------------------------

table(happy_data$Ladder.score)
# Searching NA's:
summary(happy_data)
# dystopia(col:10):1, Healthy life(col:5): 1. -> imputation by mean
#happy_data[ , 10]
#happy_data[ , 5]

#happy_data[ ,c(5,10)] <- lapply(happy_data[ ,c(5,10)], as.character) #nie na razie

which(is.na(happy_data$Healthy.life.expectancy))
which(is.na(happy_data$Dystopia...residual))
# Imputation by mean values
happy_data$Healthy.life.expectancy[is.na(happy_data$Healthy.life.expectancy)] <- 
  mean(happy_data$Healthy.life.expectancy, na.rm = T)
happy_data$Dystopia...residual[is.na(happy_data$Dystopia...residual)] <- 
  mean(happy_data$Dystopia...residual, na.rm = T)

#Adding country ranks in the dataset according to their scores
happy_data <- happy_data %>% mutate(Rank = row_number()) # na razie NIE
names(happy_data) # 11. place is rank, 1st is name

# ------------- Prediction - regression ----------------------------------------
# https://rdocumentation.org/packages/caTools/versions/1.17.1
#install.packages('caTools')
library(caTools)
set.seed(123)
dataset <- happy_data[2:9] #relevant columns into training
names(dataset)
dataset$Score <- happy_data$Ladder.score
split = sample.split(dataset$Score, SplitRatio = 0.7) #splitting
training_set = subset(dataset, split == TRUE) #training set
test_set = subset(dataset, split == FALSE) #test set


regressor_lm = lm(formula = Score ~ .,
                  data = training_set)

summary(regressor_lm) 


y_pred_lm = predict(regressor_lm, newdata = test_set)
Pred_Actual_lm <- as.data.frame(cbind(Prediction = y_pred_lm, Actual = test_set$Score))

# --------------------------  Accuracy of model  -------------------------------
gg.lm <- ggplot(Pred_Actual_lm, aes(Actual, Prediction )) +
  geom_point() + theme_bw() + geom_abline() +
  labs(title = "Multiple Linear Regression", x = "Actual happiness score",
       y = "Predicted happiness score")

gg.lm
# metrics: mean square error
MSE.lm <- sum((test_set$Happiness.Score - y_pred_lm)^2)/nrow(test_set)
print(paste("Mean Squared Error (Multiple Linear Regression):", MSE.lm))
# Luckly: [1] "Mean Squared Error (Multiple Linear Regression): 0"

# Data science about level of happiness in that particular example data:
test1 <- data.frame(Ladder.score = 5.738,
                    Logged.GDP.per.capita = 9.51,
                    Social.support = 0.906,
                    Healthy.life.expectancy = 65.9,
                    Freedom.to.make.life.choices = 0.891,
                    Generosity = 0.021,
                    Perceptions.of.corruption = 0.843,
                    Ladder.score.in.Dystopia = 1.778,
                    Dystopia...residual = 1.604)
print(paste("Happiness Score: ", round(predict(regressor_lm, test1), 3)))
# [1] "Happiness Score:  5.738"
# 5.738 - Paraguy (66th place) - ok

# Plot first the most happy 10 countries:
gg1 <- ggplot(happy_data[1:10, ],
              aes(x=Country.name,
                  y=Rank, 
                  color=Country.name))+
  geom_point()
gg1
# ------------------------------------------------------------------------------

# 2nd model: SVM
library(e1071)
regressor_svr = svm(formula = Score ~ .,
                    data = dataset,
                    type = 'eps-regression',
                    kernel = 'radial')
summary(regressor_svr)
# Number of Support Vectors:  45

y_pred_svr = predict(regressor_svr,  newdata = test_set)


Pred_Actual_svr <- as.data.frame(cbind(Prediction = y_pred_svr, Actual = test_set$Score))
    
                                                              
gg.svr <- ggplot(Pred_Actual_svr, aes(Prediction, Actual)) +
        geom_point() + theme_bw() + geom_abline() +
        labs(title = "SVR", x = "Actual happiness score",
         y = "Predicted happiness score")
gg.svr
ggsave("svrmodel.png")
# ------------------------------------------------------------------------------
# 2nd Data science test about level of happiness in that particular example data
# with SVM this time:
test2 <- data.frame(Ladder.score = 5.738,
                    Logged.GDP.per.capita = 9.51,
                    Social.support = 0.906,
                    Healthy.life.expectancy = 65.9,
                    Freedom.to.make.life.choices = 0.891,
                    Generosity = 0.021,
                    Perceptions.of.corruption = 0.843,
                    Ladder.score.in.Dystopia = 1.778,
                    Dystopia...residual = 1.604)
print(paste("Happiness Score: ", round(predict(regressor_svr, test2), 3)))
# Output: [1] "Happiness Score:  5.803"
# Little worse than in linear regression

accuracy_svm <-  5.803 / 5.738 
# 100-1.011328 % = 98,9%





















































