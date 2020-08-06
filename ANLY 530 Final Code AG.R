

#Set working directory
getwd()
setwd("/Users/ameyaghag/Documents/Harrisburg University/ANLY 530/Final Project")

#reading the excel file into a dataset

emp_absent = read.xlsx(file = "Absenteeism_at_work.xls", header = T, sheetIndex = 1)