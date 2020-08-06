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
View(employee)

dim(employee)
#factorizing each employee since they are unique IDs for each employee
employee$ID = as.factor(as.character(employee$ID))

range(employee$Month.of.absence)
employee$Month.of.absence = as.factor(as.character(employee$Month.of.absence))

employee$Reason.for.absence = as.factor(as.character(employee$Reason.for.absence))

employee$Day.of.the.week = as.factor(as.character(employee$Day.of.the.week))
employee$Social.drinker = as.factor(as.character(employee$Social.drinker))
employee$Social.smoker = as.factor(as.character(employee$Social.smoker))
employee$Seasons = as.factor(as.character(employee$Seasons))
employee$Disciplinary.failure = as.factor(as.character(employee$Disciplinary.failure))
employee$Education = as.factor(as.character(employee$Education))
employee$Son = as.factor(as.character(employee$Son))
employee$Pet = as.factor(as.character(employee$Pet))




