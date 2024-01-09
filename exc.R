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
library(corrplot)
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
#dropping unnecessary columns
happy_data <- happy_data[, -c(3,4,5,13, 14, 15, 16, 17, 18)] 
# ------------------------------------------------------------------------------

table(happy_data$Ladder.score)
# Searching NA's:
summary(happy_data)
# dystopia(col:10):1, Healthy life(col:5): 1. -> imputation by mean
#happy_data[ , 10]
#happy_data[ , 5]
happy_data[ ,c(5,10)] <- lapply(happy_data[ ,c(5,10)], as.character) #nie na razie

which(is.na(happy_data$Healthy.life.expectancy))
which(is.na(happy_data$Dystopia...residual))
# Imputation by mean values
happy_data$Healthy.life.expectancy[is.na(happy_data$Healthy.life.expectancy)] <- 
  mean(happy_data$Healthy.life.expectancy, na.rm = T)
happy_data$Dystopia...residual[is.na(happy_data$Dystopia...residual)] <- 
  mean(happy_data$Dystopia...residual, na.rm = T)




































