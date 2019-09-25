#Set working directory
#Basic Data Analysis
#Which is Exploratory Data Analysis (EDA)
#And Summerization
#So the Data Which i am going to use here is processed before its use.
#So lets start with clean data now.

library(MASS)  #For Statistics
library(dplyr) #For SQL
library(ISLR)  
library(xlsx) #Read and write Excel File
library(class)
library(datasets)

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
mydata$EnvironmentSatisfaction <- as.factor(mydata$EnvironmentSatisfaction)#(1=Lowest, 4=Highest)
mydata$JobInvolvement <- as.factor(mydata$JobInvolvement)#(1=Lowest, 4=Highest)
mydata$JobLevel <- as.factor(mydata$JobLevel)#(1=Lowest, 4=Highest)
mydata$JobSatisfaction <- as.factor(mydata$JobSatisfaction)#(1=Lowest, 4=Highest)
mydata$PerformanceRating <- as.factor(mydata$PerformanceRating) #(1=Lowest, 4=Highest)
mydata$RelationshipSatisfaction <- as.factor(mydata$RelationshipSatisfaction) #(1=Lowest, 4=Highest)
mydata$StockOptionLevel <- as.factor(mydata$StockOptionLevel)#0=No option, 1 = Low, 2 = Medim, 3 = High
mydata$WorkLifeBalance <- as.factor(mydata$WorkLifeBalance)#1=Lowest, 4 =  Highest

str(mydata) # Again check type of each variable in mydata Data Frame 

#CatSal <- cut(myhit$Salary, breaks =3, labels = c("L","M","H"))
#myhit <- data.frame(myhit,CatSal)
# all variable are now ready to use 
#So now lets take summary first
summary(mydata) #This gives us summary statistics for each variable
# unfortunately it doesn't give us any idea about Standard deviation or variance

############# EDA, summarization and Test of Hypothesis ###################
################# Few important Univatate ################
# Attrition which is Categorical data
#---Summerizarion
#------count, proportion,%,Ratio, mode

print(paste("Count = ",length(mydata$Attrition))) # Count =1470
print(paste("Ratio (Company not left Vs Company Left) is = ",table(mydata$Attrition)[1]/table(mydata$Attrition)[2])) # Ratio
#### Another method to calculate Ratio
Attrition.matrix <- as.matrix(table(mydata$Attrition))
print(paste("Ratio (Company not left Vs Company Left) by matrix method is = ",Attrition.matrix[1] / Attrition.matrix[2])) # Ratio
print(prop.table(table(mydata$Attrition_label))) # Proportion
print(paste("Percentage"))
print(round(prop.table(table(mydata$Attrition_label)) * 100,2))#Percentage
print(max(table(mydata$Attrition_label)))
#Mode
print(paste("Mode is",names(which.max(table(mydata$Attrition_label))),"Which is equal to",max(table(mydata$Attrition_label))))

#---Visualization
#------Pie Chart, Bar Chart
str(mydata$Attrition_label)
levels(mydata$Attrition_label)
#Pie chart of Attrition
pie(Attrition.matrix, radius = 1,
    main = "Pie Chart of Attrition",
    col = c("Red", "Blue"),
    labels = levels(mydata$Attrition_label),
    clockwise = TRUE , init.angle = 90
)
#Bar chart of Attrition
barplot(Attrition.matrix, beside = TRUE, 
        space = 0.25, main = "Barplot of Attrition",
        xlab="Attrition", ylab="No of Employees", 
        #names.arg=c("A","N")
        names.arg = levels(mydata$Attrition_label),# Alternate Option
        #col = c("Red", "Orange")
        col = c("#FF0000FF",3)  # Alternate Option using palettes
)

