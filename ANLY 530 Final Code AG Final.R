libraries = c("dummies","caret","rpart.plot","plyr","dplyr", "ggplot2","rpart","dplyr","DMwR","randomForest",
              "usdm","corrgram","DataCombine","xlsx")
lapply(X = libraries,require, character.only = TRUE)
rm(libraries)

#Set working directory
getwd()
setwd("/Users/ameyaghag/Documents/Harrisburg University/ANLY 530/Final Project/ANLY530-Final-Project-AG")

#reading the excel file into a dataset

employee <- read.csv("/Users/ameyaghag/Documents/Harrisburg University/ANLY 530/Final Project/
                     ANLY530-Final-Project-AG/Absenteeism_at_work_train.csv")
summary(employee)
dim(employee)
#missing value analysis - showed we have 3 cases of NA

as.matrix(colSums(is.na(employee)))

#removing any NA values present in this dataset
df<-na.omit(employee) 

as.matrix(colSums(is.na(df)))

dim(df)

#cleaning data and making factors of many categorical variables
df$ID = as.factor(as.character(df$ID))
df$Month.of.absence = as.factor(as.character(df$Month.of.absence))
df$Reason.for.absence = as.factor(as.character(df$Reason.for.absence))
df$Day.of.the.week = as.factor(as.character(df$Day.of.the.week))
df$Social.drinker = as.factor(as.character(df$Social.drinker))
df$Social.smoker = as.factor(as.character(df$Social.smoker))
df$Seasons = as.factor(as.character(df$Seasons))
df$Disciplinary.failure = as.factor(as.character(df$Disciplinary.failure))
df$Education = as.factor(as.character(df$Education))
df$Son = as.factor(as.character(df$Son))
df$Pet = as.factor(as.character(df$Pet))

str(df)

employee_f<-df

#EXPLORATORY ANALYSIS

ggplot(employee_f,
       aes_string(y=employee_f$Absenteeism.time.in.hours,x=as.factor(employee_f$Reason.for.absence))) +
  geom_boxplot() + 
  xlab('Reason Codes for Absence') +
  ylab('Absenteeism Time in Hours')

#In the plot below, we look for a distribution of absenteeism time in hours
#we notice that there is a very big skew to the right - likely due to outliers

hist(employee_f$Absenteeism.time.in.hours, breaks = 30,
     xlab = 'Absenteeism Time in hours', main = " Absenteeism Time in Hours - Distribution", col = "blue")

#now plotting some histograms below to see how various categorical factors affect absenteeism
library(gridExtra)
library(grid)



smoking <- ggplot(employee_f, aes(x =  Social.smoker, fill =  Social.smoker))+
                                   labs(x = 'Is a Social Smoker (1) or Not(0) ?', title = " Social Smoking Histogram") + geom_bar()

#very few smokers in this dataset, so not likely to have a significant bearing

drink <- ggplot(employee_f, aes(x =  Social.drinker, fill =  Social.drinker)) + geom_bar()+
  labs(x = 'Is a Social Drinker (1) or Not(0) ?', title = " Social Drinking Histogram")


#both are almost close. not a big surprise since many employees might enjoy a social drink or two
#this is a fairly balanced variable

kids <- ggplot(employee_f, aes(x = Son, fill = Son)) + geom_bar() +
  labs(x = 'Number of Children', title = "Distribution of # of Children")

#most employees have 1-2 or no kids at all

seasons <- ggplot(employee_f, aes(x =Seasons,fill = Seasons)) + geom_bar() + 
  labs(x = 'Season Code', title = "Count of Absenteeism Recorded by Season Code")

#even the seasons histogram is fairly balanced between 1 and 3

grid.arrange(smoking,drink,kids,seasons, nrow = 2)

#exploratory analysis doesnt give us a significant idea about reasons for absenteeism