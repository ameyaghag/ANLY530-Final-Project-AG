libraries = c("dummies","caret","rpart.plot","plyr","dplyr", "ggplot2","rpart","dplyr","DMwR","randomForest","usdm","corrgram","DataCombine","xlsx")
lapply(X = libraries,require, character.only = TRUE)
rm(libraries)

#Set working directory
getwd()
setwd("/Users/ameyaghag/Documents/Harrisburg University/ANLY 530/Final Project/ANLY530-Final-Project-AG")

#reading the excel file into a dataset

employee <- read.csv("/Users/ameyaghag/Documents/Harrisburg University/ANLY 530/Final Project/ANLY530-Final-Project-AG/Absenteeism_at_work_train.csv")
summary(employee)
na.omit(employee) #removing any NA values present in this dataset

dim(employee)
#factorizing each employee since they are unique IDs for each employee
employee$ID = as.factor(as.character(employee$ID))

range(employee$Month.of.absence)
employee$Month.of.absence = as.factor(as.character(employee$Month.of.absence))



