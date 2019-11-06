#Set working directory
#Basic Data Analysis
#Which is Exploratory Data Analysis (EDA)
#And Summarization
#So the Data Which i am going to use here is processed before its use.
#So lets start with clean data now.

library(MASS)  #For Statistics
library(dplyr) #For SQL
library(ISLR) # For Statistical Learning  
library(xlsx) #Read and write Excel File
library(class) #for classification
library(datasets) #R comes with several built-in data sets
library(ggplot2) # For plotting graphs
library(ggpubr) # For plotting graphs
library(gplots)# For plotting graphs
library(glmnet) # GLM for ridge regression and LASSO
library(boot) #for bootstrap inference
library(randomForest) #For classification and regression.
library(e1071) #for latent class analysis, short time Fourier transform, fuzzy clustering, 
#support vector machines, shortest path computation, bagged clustering, naive Bayes classifier
library(fpc)
library(tree)
library(ROCR)
library(quantmod)  #Quantitative Financial Modelling and Trading Framework for R
library(forecast)
library(fpp)
library(leaps)# For subset regression

setwd("C://Users//DELL//Documents//Attrition")
getwd()

#Create Data Frame and assign all the data to it First
mydata <- data.frame(read.csv("Attrition_Data.csv",na.strings = "."))
head(mydata) # head gives us info of first six rows only
str(mydata)
#But here we came to know that there are total 1470 observations and 35 variables in mydata
#and i found that some variable like attrition is integer so first convert all such variables
#to categorical data. 

mydata$Attrition <- as.factor(mydata$Attrition) #Whether left organization (1=Left, 0=Not left)
mydata$Attrition_label <- factor(mydata$Attrition, labels=c("Not Left", "Left"))
mydata$BusinessTravel_label <- factor(mydata$BusinessTravel, labels=c("NT","Trav. Freq","Travel Rarely"))
mydata$Department_label <- factor(mydata$Department, labels=c("HR","R&D","Sales"))
mydata$Education <- factor(mydata$Education)
mydata$Education_label <- factor(mydata$Education, labels=c("12th", "Diploma", "Graduation","PG", "PhD"))
mydata$EducationField_label <- factor(mydata$EducationField, labels=c("HR","Life Sci","Marketing","Medical","Other","Tech Degree"))
mydata$EnvironmentSatisfaction <- as.factor(mydata$EnvironmentSatisfaction)#(1=Lowest, 4=Highest)
mydata$JobInvolvement <- as.factor(mydata$JobInvolvement)#(1=Lowest, 4=Highest)
mydata$JobLevel <- as.factor(mydata$JobLevel)#(1=Lowest, 4=Highest)
mydata$JobSatisfaction <- as.factor(mydata$JobSatisfaction)#(1=Lowest, 4=Highest)
mydata$JobRole_label <- factor(mydata$JobRole, labels=c("Healthcare","HR","Lab-Tech","Manager","Man.Director","Res.Director","Scientist", "Sales Exe", "Sales Rep"))   
mydata$PerformanceRating <- as.factor(mydata$PerformanceRating) #(1=Lowest, 4=Highest)
mydata$PerformanceRating_label <- factor(mydata$PerformanceRating, labels=c("3","4"))
mydata$RelationshipSatisfaction <- as.factor(mydata$RelationshipSatisfaction) #(1=Lowest, 4=Highest)
mydata$StockOptionLevel <- as.factor(mydata$StockOptionLevel)#0=No option, 1 = Low, 2 = Medim, 3 = High
mydata$WorkLifeBalance <- as.factor(mydata$WorkLifeBalance)#1=Lowest, 4 =  Highest

str(mydata) # Again check type of each variable in mydata Data Frame 

# All variable are now ready to use 
# So now lets take summary first
print(summary(mydata)) #This gives us summary statistics for each variable
# unfortunately it doesn't give us any idea about Standard deviation or variance