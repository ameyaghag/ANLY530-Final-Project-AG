libraries = c("dummies","caret","rpart.plot","plyr","dplyr", "ggplot2","rpart","dplyr","DMwR","randomForest",
              "usdm","corrgram","DataCombine","xlsx")
lapply(X = libraries,require, character.only = TRUE)
rm(libraries)

#Set working directory
getwd()
setwd("/Users/ameyaghag/Documents/Harrisburg University/ANLY 530/Final Project/ANLY530-Final-Project-AG")

#reading the excel file into a dataset

employee <- 
read.csv("/Users/ameyaghag/Documents/Harrisburg University/ANLY 530/Final Project/ANLY530-Final-Project-AG/Absenteeism_at_work_train.csv")
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

#OUTLIER ANALYSIS


ggplot(employee_f,
       aes_string(y=employee_f$Absenteeism.time.in.hours,x=as.factor(employee_f$Reason.for.absence))) +
  geom_boxplot() + 
  xlab('Reason Codes for Absence') +
  ylab('Absenteeism Time in Hours')

#In the plot below, we look for a distribution of absenteeism time in hours
#we notice that there is a very big skew to the right - likely due to outliers

par(mfrow=c(2,2))

boxplot(employee_f$Service.time, main = "Service.time")

boxplot(employee_f$Hit.target, main = "Hit.target")

boxplot(employee_f$Transportation.expense, main = "Transportation.expense")

boxplot(employee_f$Height, main = "Height")

employee_f2<-employee_f

#FIXING THE OUTLIERS

for (i in c('Transportation.expense','Service.time','Hit.target','Height','Absenteeism.time.in.hours')){
  q = quantile(employee_f2[,i],c(0.25,0.75))
  iqr1 = q[2]-q[1]
  min1 = q[1]-1.5*iqr1
  max1 = q[2]+1.5*iqr1
  employee_f2[,i][employee_f2[,i]<min1] = min1
  employee_f2[,i][employee_f2[,i]>max1] = max1
}

par(mfrow=c(2,2))

boxplot(employee_f2$Service.time, main = "Service.time")

boxplot(employee_f2$Hit.target, main = "Hit.target")

boxplot(employee_f2$Transportation.expense, main = "Transportation.expense")

boxplot(employee_f2$Height, main = "Height")

ggplot(employee_f2,
       aes_string(y=employee_f2$Absenteeism.time.in.hours,x=as.factor(employee_f2$Reason.for.absence))) +
  geom_boxplot() + 
  xlab('Reason Codes for Absence') +
  ylab('Absenteeism Time in Hours')

dfx<-employee_f2

#SPLITTING THE DATA INTO TEST AND TRAINING DATASETS


set.seed(1)
train_index = sample(1:nrow(employee_f2), 0.8*nrow(employee_f2))        
train_data = employee_f2[train_index,]
test_data = employee_f2[-train_index,]
#install.packages("dmm")
library(dmm)


#Build decsion tree using rpart
decision_model = rpart(Absenteeism.time.in.hours ~ ., data = train_data, method = "anova")

#Plot the tree
rpart.plot(decision_model)
#Perdict for test cases
#Perdict for test cases
dt_predictions = predict(decision_model, test_data)

#Create data frame for actual and predicted values
df_pred = data.frame("actual"=test_data, "dt_pred"=dt_predictions)


#Calcuate MAE, RMSE, R-sqaured for testing data 

MAE(df_pred$actual.Absenteeism.time.in.hours,df_pred$dt_pred)
RMSE(df_pred$actual.Absenteeism.time.in.hours,df_pred$dt_pred)

print(postResample(pred = dt_predictions, obs = test_data$Absenteeism.time.in.hours))

plot(test_data$Absenteeism.time.in.hours,type="l",lty=2,col="green")
lines(dt_predictions,col="blue")


#NOW TRYING RANDOM FOREST

rf_model = randomForest(Absenteeism.time.in.hours ~ ., data = train_data, ntree=200)

#Predicting for test data
rf_predictions = predict(rf_model, test_data)

#Creating dataframe with actual and predicted values
df_pred = cbind(df_pred,rf_predictions)
head(df_pred)

print(postResample(pred = rf_predictions, obs = test_data$Absenteeism.time.in.hours))

plot(test_data$Absenteeism.time.in.hours,type="l",lty=2,col="green")
lines(rf_predictions,col="blue")


