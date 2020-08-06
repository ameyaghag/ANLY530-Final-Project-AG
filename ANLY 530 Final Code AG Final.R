libraries = c("dummies","caret","rpart.plot","plyr","dplyr", "ggplot2","rpart","dplyr","DMwR","randomForest","usdm","corrgram","DataCombine","xlsx")
lapply(X = libraries,require, character.only = TRUE)
rm(libraries)

#Set working directory
getwd()
setwd("/Users/ameyaghag/Documents/Harrisburg University/ANLY 530/Final Project/ANLY530-Final-Project-AG")

#reading the excel file into a dataset

data <- read.csv("/Users/ameyaghag/Documents/Harrisburg University/ANLY 530/Final Project/ANLY530-Final-Project-AG/Absenteeism_at_work_train.csv")
summary(data)

