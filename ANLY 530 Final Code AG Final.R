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
na.omit(employee) #removing any NA values present in this dataset
View(employee)

dim(employee)
#factorizing each employee since they are unique IDs for each employee
employee$ID = as.factor(as.character(employee$ID))

range(employee$Month.of.absence)

#cleaning data and making factors of many categorical variables

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

str(employee)

#missing value analysis

as.matrix(colSums(is.na(employee)))

#we can see one missing value in Hit.target and 2 in Weight

df<-na.omit(employee)
dim(df)

#now we have 663 rows after removing the 3 missing cases

sapply(df,function(x){sum(is.na(x))})

## Box plot of Absenteeism time in hours with Reason for absence. To verify the outliers in each reason codes.

bar1 = ggplot(data = df, aes(x = ID)) + geom_bar() + ggtitle("Count of ID") + theme_bw()

bar2 = ggplot(data = df, aes(x = Reason.for.absence)) + geom_bar() + 
  ggtitle("Count of Reason for absence") + theme_bw()

bar3 = ggplot(data = df, aes(x = Month.of.absence)) + geom_bar() + ggtitle("Count of Month") + theme_bw()
bar3

bar4 = ggplot(data = df, aes(x = Disciplinary.failure)) + geom_bar() + 
  ggtitle("Count of Disciplinary failure") + theme_bw()

#Check the distribution of numerical data using histogram
numeric_index = sapply(df, is.numeric)
numeric_data = df[,numeric_index]

(hist1 = ggplot(data = numeric_data, aes(x =Transportation.expense)) + 
  ggtitle("Transportation.expense") + geom_histogram(bins = 25))


(hist2 = ggplot(data = numeric_data, aes(x =Height)) + 
  ggtitle("Distribution of Height") + geom_histogram(bins = 25))

(hist3 = ggplot(data = numeric_data, aes(x =Body.mass.index)) + 
  ggtitle("Distribution of Body.mass.index") + geom_histogram(bins = 25))

(hist4 = ggplot(data = numeric_data, aes(x =Absenteeism.time.in.hours)) + 
  ggtitle("Distribution of Absenteeism.time.in.hours") + geom_histogram(bins = 25))
