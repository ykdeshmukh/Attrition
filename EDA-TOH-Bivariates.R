setwd("C://Users//DELL//Documents//Attrition")
source("source.R")

#Here before starting EDA, we want to save all plots to Bivariate plot folder
#So set the working directory accordingly and then at the end of 
#Bivariate EDA and TOH, we will set it back to "C://Users//DELL//Documents//Attrition"

setwd("C://Users//DELL//Documents//Attrition//_plots//Bivariate//Vs_Attrition")

#Here we need to check the factors afecting Attrition.
#So 'Y' will be an Attrition and 'X' will be other variables.

############# EDA, summarization and Test of Hypothesis ###################

#1. Age Vs Attrition i.e. Quantitative data Vs Categorical Data
#---Summerizarion
print(paste("Range of Age is from",min(mydata$Age),"To",max(mydata$Age)))
#---Visualization
png(filename = "AgeVsAttrition.png",height=480, width= 960, unit ="px")
#---- Y- Attrition, X- Age ---Horizontal side by side box plot
boxplot(mydata$Age~mydata$Attrition_label, col ="Red",
        horizontal = TRUE, 
        main = "Boxplot of Age Vs Attrition",
        xlab = "Age", ylab="Attrition"
) 
dev.off()
#stats-having the position of the lower/upper extremes 
#of the whiskers and box along with the median
print("$stats")
print(boxplot(mydata$Age~mydata$Attrition_label, col ="Red",
              horizontal = TRUE,
              main = "Boxplot of Age Vs Attrition",
              xlab = "Age", ylab="Attrition"
)$stats)

#n-the number of observation
print("$n")
print(boxplot(mydata$Age~mydata$Attrition_label, col ="Red",
              horizontal = TRUE,
              main = "Boxplot of Age Vs Attrition",
              xlab = "Age", ylab="Attrition"
)$n)

#conf-upper/lower extremes of the notch
print("$conf")
print(boxplot(mydata$Age~mydata$Attrition_label, col ="Red",
              horizontal = TRUE,
              main = "Boxplot of Age Vs Attrition",
              xlab = "Age", ylab="Attrition"
)$conf)

#out-value of the outliers
print("$out")
print(boxplot(mydata$Age~mydata$Attrition_label, col ="Red",
              horizontal = TRUE,
              main = "Boxplot of Age Vs Attrition",
              xlab = "Age", ylab="Attrition"
)$out)

#group-a vector of the same length as out whose elements 
#indicate to which group the outlier belongs (e.g. 56  & 58)
print("$group")
print(boxplot(mydata$Age~mydata$Attrition_label, col ="Red",
              horizontal = TRUE,
              main = "Boxplot of Age Vs Attrition",
              xlab = "Age", ylab="Attrition"
)$group)

#names-a vector of names for the groups
print("$names")
print(boxplot(mydata$Age~mydata$Attrition_label, col ="Red",
              horizontal = TRUE,
              main = "Boxplot of Age Vs Attrition",
              xlab = "Age", ylab="Attrition"
)$names)
#---Test of Hypothesis - Logistic Regression -Wich we will do in next step(seperate file)
#--- There is no such test of hypothesis for Q -> C but we can use gostic regression


#2. BusinessTravel Vs Attrition i.e. Categorical Data Vs Categorical Data
# C -> C
#---Summerizarion
#----Comparative Proportion
#----Contingency Table
print(paste("Table of Business Travel Vs Attrition"))
print(table(mydata$BusinessTravel_label, mydata$Attrition_label))
print(paste("Proportion Table of Business Travel Vs Attrition"))
print(prop.table(table(mydata$BusinessTravel_label, mydata$Attrition_label),1))

#---Visualization
png(filename = "BusinessTravelvsAttrition.png",height=480, width= 960, unit ="px")
#---- Y- Attrition, X- BusinessTravel ---Bubble Plot
plot(mydata$Attrition_label~mydata$BusinessTravel_label,
         col =c("Green","Red"), 
         main = "Plot of Business Travel Vs Attrition",
         xlab = "Business Travel", ylab="Attrition")


dev.off()

#---Test of Hypothesis 
#-----Null Hypothesis, Ho: BusinessTravel and Attrition are independent.
#-----Alt. Hypothesis, Ha: BusinessTravel and Attrition are dependent on each other.
#----Chi-Square test
chi.tab_bt <- table(mydata$BusinessTravel, mydata$Attrition_label)
chi.test_bt <- chisq.test(chi.tab_bt,p = rep(1/length(mydata$BusinessTravel), length(mydata$BusinessTravel)))

print("Pearson's Chi-squared test Result is:")
print(chi.test_bt)
print("Oberved Values are:")
print(chi.test_bt$observed)
print("Expected values are:")
print(chi.test_bt$expected)
print("Residuals values are:") 
print(chi.test_bt$residuals) #This Gives us (Obsevered values - Expected Values)
print("Sum :")
print(sum((chi.test_bt$residuals)^2))
print("Here we can see that P-Value is very less than 0.05, so we can Reject the Null Hypothesis.")
print("And we can say with 95% Confidence that an Attrition is dependent on BusinessTravel.")


#3. DailyRate Vs Attrition i.e. Quantitative data Vs Categorical Data
#---Summerizarion
print(paste("Range of Daily Rate is from",min(mydata$DailyRate),"To",max(mydata$DailyRate)))
#---Visualization
png(filename = "DailyRateVsAttrition.png",height=480, width= 960, unit ="px")
#---- Y- Attrition, X- DailyRate ---Horizontal side by side box plot
boxplot(mydata$DailyRate~mydata$Attrition_label, col ="Red",
        horizontal = TRUE, 
        main = "Boxplot of Daily Rate Vs Attrition",
        xlab = "Daily Rate", ylab="Attrition"
) 
dev.off()

print("$stats")
print(boxplot(mydata$DailyRate~mydata$Attrition_label, col ="Red",
                         horizontal = TRUE,
                         main = "Boxplot of Daily Rate Vs Attrition",
                         xlab = "Daily Rate", ylab="Attrition"
)$stats)

#n-the number of observation
print("$n")
print(boxplot(mydata$DailyRate~mydata$Attrition_label, col ="Red",
              horizontal = TRUE,
              main = "Boxplot of Daily Rate Vs Attrition",
              xlab = "Daily Rate", ylab="Attrition"
)$n)

#conf-upper/lower extremes of the notch
print("$conf")
print(boxplot(mydata$DailyRate~mydata$Attrition_label, col ="Red",
              horizontal = TRUE,
              main = "Boxplot of Daily Rate Vs Attrition",
              xlab = "Daily Rate", ylab="Attrition"
)$conf)

#out-value of the outliers
print("$out")
print(boxplot(mydata$DailyRate~mydata$Attrition_label, col ="Red",
              horizontal = TRUE,
              main = "Boxplot of Daily Rate Vs Attrition",
              xlab = "Daily Rate", ylab="Attrition"
)$out)

#group-a vector of the same length as out whose elements 
#indicate to which group the outlier belongs
print("$group")
print(boxplot(mydata$DailyRate~mydata$Attrition_label, col ="Red",
              horizontal = TRUE,
              main = "Boxplot of Daily Rate Vs Attrition",
              xlab = "Daily Rate", ylab="Attrition"
)$group)

#names-a vector of names for the groups
print("$names")
print(boxplot(mydata$DailyRate~mydata$Attrition_label, col ="Red",
              horizontal = TRUE,
              main = "Boxplot of Daily Rate Vs Attrition",
              xlab = "Daily Rate", ylab="Attrition"
)$names)
#---Test of Hypothesis - Logistic Regression -Wich we will do in next step(seperate file)
#--- There is no such test of hypothesis for Q -> C but we can use gostic regression


#4. Department Vs Attrition i.e. Categorical Data Vs Categorical Data
# C -> C
#---Summerizarion
#----Comparative Proportion
#----Contingency Table
print(paste("Table of Department Vs Attrition"))
print(table(mydata$Department_label, mydata$Attrition_label))
print(paste("Proportion Table of Department Vs Attrition"))
print(prop.table(table(mydata$Department_label, mydata$Attrition_label),1))

#---Visualization
png(filename = "DepartmentvsAttrition.png",height=480, width= 960, unit ="px")
#---- Y- Attrition, X- Department ---Bubble Plot
plot(mydata$Attrition_label~mydata$Department_label,
     col =c("Green","Red"), 
     main = "Plot of Department Vs Attrition",
     xlab = "Department", ylab="Attrition")


dev.off()

#---Test of Hypothesis 
#-----Null Hypothesis, Ho: Department and Attrition are independent.
#-----Alt. Hypothesis, Ha: Department and Attrition are dependent on each other.
#----Chi-Square test
chi.tab_dep <- table(mydata$Department_label, mydata$Attrition_label)
chi.test_dep <- chisq.test(chi.tab_dep,p = rep(1/length(mydata$Department_label), length(mydata$Department_label)))

print("Pearson's Chi-squared test Result is:")
print(chi.test_dep)
print("Oberved Values are:")
print(chi.test_dep$observed)
print("Expected values are:")
print(chi.test_dep$expected)
print("Residuals values are:") 
print(chi.test_dep$residuals) #This Gives us (Obsevered values - Expected Values)
print("Sum :")
print(sum((chi.test_dep$residuals)^2))
print("Here we can see that P-Value is very less than 0.05, so we can Reject the Null Hypothesis.")
print("And we can say with 95% Confidence that an Attrition is dependent on Department.")

#5. DistanceFromHome Vs Attrition i.e. Quantitative data Vs Categorical Data
#---Summerizarion
print(paste("Range of Distance From Home is from",min(mydata$DistanceFromHome),"To",max(mydata$DistanceFromHome)))
#---Visualization
png(filename = "DistanceFromHomeVsAttrition.png",height=480, width= 960, unit ="px")
#---- Y- Attrition, X- DistanceFromHome ---Horizontal side by side box plot
boxplot(mydata$DistanceFromHome~mydata$Attrition_label, col ="Red",
        horizontal = TRUE, 
        main = "Boxplot of Distance From Home Vs Attrition",
        xlab = "Distance From Home", ylab="Attrition"
) 
dev.off()

#stats-having the position of the lower/upper extremes 
#of the whiskers and box along with the median
print("$stats")
print(boxplot(mydata$DistanceFromHome~mydata$Attrition_label, col ="Red",
                                horizontal = TRUE,
                                main = "Boxplot of Distance From Home Vs Attrition",
                                xlab = "Distance From Home", ylab="Attrition"
)$stats)


#n-the number of observations
print("$n")
print(boxplot(mydata$DistanceFromHome~mydata$Attrition_label, col ="Red",
              horizontal = TRUE,
              main = "Boxplot of Distance From Home Vs Attrition",
              xlab = "Distance From Home", ylab="Attrition"
)$n)

#conf-upper/lower extremes of the notch
print("$conf")
print(boxplot(mydata$DistanceFromHome~mydata$Attrition_label, col ="Red",
              horizontal = TRUE,
              main = "Boxplot of Distance From Home Vs Attrition",
              xlab = "Distance From Home", ylab="Attrition"
)$conf)

#out-value of the outliers
print("$out")
print(boxplot(mydata$DistanceFromHome~mydata$Attrition_label, col ="Red",
              horizontal = TRUE,
              main = "Boxplot of Distance From Home Vs Attrition",
              xlab = "Distance From Home", ylab="Attrition"
)$out)

#group-a vector of the same length as out whose elements 
#indicate to which group the outlier belongs (e.g. 56  & 58)
print("$group")
print(boxplot(mydata$DistanceFromHome~mydata$Attrition_label, col ="Red",
              horizontal = TRUE,
              main = "Boxplot of Distance From Home Vs Attrition",
              xlab = "Distance From Home", ylab="Attrition"
)$group)

#names-a vector of names for the groups
print("$names")
print(boxplot(mydata$DistanceFromHome~mydata$Attrition_label, col ="Red",
              horizontal = TRUE,
              main = "Boxplot of Distance From Home Vs Attrition",
              xlab = "Distance From Home", ylab="Attrition"
)$names)


#---Test of Hypothesis - Logistic Regression -Wich we will do in next step(seperate file)
#--- There is no such test of hypothesis for Q -> C but we can use gostic regression

#6. Education Vs Attrition i.e. Categorical Data Vs Categorical Data
# C -> C
#---Summerizarion
#----Comparative Proportion
#----Contingency Table
print(paste("Table of Education Vs Attrition"))
print(table(mydata$Education_label, mydata$Attrition_label))
print(paste("Proportion Table of Education Vs Attrition"))
print(prop.table(table(mydata$Education_label, mydata$Attrition_label),1))

#---Visualization
png(filename = "EducationvsAttrition.png",height=480, width= 960, unit ="px")
#---- Y- Attrition, X- Education ---Bubble Plot
plot(mydata$Attrition_label~mydata$Education_label,
     col =c("Green","Red"), 
     main = "Plot of Education Vs Attrition",
     xlab = "Education", ylab="Attrition")


dev.off()

#---Test of Hypothesis 
#-----Null Hypothesis, Ho: Education and Attrition are independent.
#-----Alt. Hypothesis, Ha: Education and Attrition are dependent on each other.
#----Chi-Square test
chi.tab_edu <- table(mydata$Education_label, mydata$Attrition_label)
chi.test_edu <- chisq.test(chi.tab_edu,p = rep(1/length(mydata$Education_label), length(mydata$Education_label)))

print("Pearson's Chi-squared test Result is:")
print(chi.test_edu)
print("Oberved Values are:")
print(chi.test_edu$observed)
print("Expected values are:")
print(chi.test_edu$expected)
print("Residuals values are:") 
print(chi.test_edu$residuals) #This Gives us (Obsevered values - Expected Values)
print("Sum :")
print(sum((chi.test_edu$residuals)^2))
print("Here we can see that P-Value is greater than 0.05, so we can not Reject the Null Hypothesis.")
print("And we can say with 95% Confidence that an Attrition and an Education are independent.")


#7. EducationField Vs Attrition i.e. Categorical Data Vs Categorical Data
# C -> C
#---Summerizarion
#----Comparative Proportion
#----Contingency Table
print(paste("Table of Education Field Vs Attrition"))
print(table(mydata$EducationField_label, mydata$Attrition_label))
print(paste("Proportion Table of Education Field Vs Attrition"))
print(prop.table(table(mydata$EducationField_label, mydata$Attrition_label),1))

#---Visualization
png(filename = "EducationFieldvsAttrition.png",height=480, width= 960, unit ="px")
#---- Y- Attrition, X- EducationField ---Bubble Plot
plot(mydata$Attrition_label~mydata$EducationField_label,
     col =c("Green","Red"), 
     main = "Plot of Education Field Vs Attrition",
     xlab = "Education Field", ylab="Attrition")


dev.off()

#---Test of Hypothesis 
#-----Null Hypothesis, Ho: EducationField and Attrition are independent.
#-----Alt. Hypothesis, Ha: EducationField and Attrition are dependent on each other.
#----Chi-Square test
chi.tab_ef <- table(mydata$EducationField_label, mydata$Attrition_label)
chi.test_ef <- chisq.test(chi.tab_ef,p = rep(1/length(mydata$EducationField_label), length(mydata$EducationField_label)))

print("Pearson's Chi-squared test Result is:")
print(chi.test_ef)
print("Oberved Values are:")
print(chi.test_ef$observed)
print("Expected values are:")
print(chi.test_ef$expected)
print("Residuals values are:") 
print(chi.test_ef$residuals) #This Gives us (Obsevered values - Expected Values)
print("Sum :")
print(sum((chi.test_ef$residuals)^2))
print("Here we can see that P-Value is very less than 0.05, so we can Reject the Null Hypothesis.")
print("And we can say with 95% Confidence that an Attrition is dependent on Education Field.")


# 8.As EmployeeCount is 1 for all employees and which is non relevant 
# here in case of Attrition so we can skip EDA of EmployeeCount Vs Attrition.
# We will only print the table for EmployeeCount
print(table(mydata$EmployeeCount))

# 9.As EmployeeNumber is unique for all employees and which is non relevant 
# here in case of Attrition so we can skip EDA of EmployeeNumber Vs Attrition
# but we will use this variable whenever it will require later in this project.

#10. EnvironmentSatisfaction Vs Attrition i.e. Categorical Data Vs Categorical Data
# C -> C
#---Summerizarion
#----Comparative Proportion
#----Contingency Table
print(paste("Table of Environment Satisfaction Vs Attrition"))
print(table(mydata$EnvironmentSatisfaction, mydata$Attrition_label))
print(paste("Proportion Table of Environment Satisfaction Vs Attrition"))
print(prop.table(table(mydata$EnvironmentSatisfaction, mydata$Attrition_label),1))

#---Visualization
png(filename = "EnvironmentSatisfactionvsAttrition.png",height=480, width= 960, unit ="px")
#---- Y- Attrition, X- EnvironmentSatisfaction ---Bubble Plot
plot(mydata$Attrition_label~mydata$EnvironmentSatisfaction,
     col =c("Green","Red"), 
     main = "Plot of Environment Satisfaction Vs Attrition",
     xlab = "Environment Satisfaction(1=Lowest, 4=Highest)", ylab="Attrition")


dev.off()

#---Test of Hypothesis 
#-----Null Hypothesis, Ho: EnvironmentSatisfaction and Attrition are independent.
#-----Alt. Hypothesis, Ha: EnvironmentSatisfaction and Attrition are dependent on each other.
#----Chi-Square test
chi.tab_es <- table(mydata$EnvironmentSatisfaction, mydata$Attrition_label)
chi.test_es <- chisq.test(chi.tab_es,p = rep(1/length(mydata$EnvironmentSatisfaction), length(mydata$EnvironmentSatisfaction)))

print("Pearson's Chi-squared test Result is:")
print(chi.test_es)
print("Oberved Values are:")
print(chi.test_es$observed)
print("Expected values are:")
print(chi.test_es$expected)
print("Residuals values are:") 
print(chi.test_es$residuals) #This Gives us (Obsevered values - Expected Values)
print("Sum :")
print(sum((chi.test_es$residuals)^2))
print("Here we can see that P-Value is very less than 0.05, so we can Reject the Null Hypothesis.")
print("And we can say with 95% Confidence that an Attrition is dependent on EnvironmentSatisfaction.")

#11. Gender Vs Attrition i.e. Categorical Data Vs Categorical Data
# C -> C
#---Summerizarion
#----Comparative Proportion
#----Contingency Table
print(paste("Table of Gender Vs Attrition"))
print(table(mydata$Gender, mydata$Attrition_label))
print(paste("Proportion Table of Gender Vs Attrition"))
print(prop.table(table(mydata$Gender, mydata$Attrition_label),1))

#---Visualization
png(filename = "GendervsAttrition.png",height=480, width= 960, unit ="px")
#---- Y- Attrition, X- Gender ---Bubble Plot
plot(mydata$Attrition_label~mydata$Gender,
     col =c("Green","Red"), 
     main = "Plot of Gender Vs Attrition",
     xlab = "Gender", ylab="Attrition")


dev.off()

#---Test of Hypothesis 
#-----Null Hypothesis, Ho: Gender and Attrition are independent.
#-----Alt. Hypothesis, Ha: Gender and Attrition are dependent on each other.
#----Chi-Square test
chi.tab_Gender <- table(mydata$Gender, mydata$Attrition_label)
chi.test_Gender <- chisq.test(chi.tab_Gender,p = rep(1/length(mydata$Gender), length(mydata$Gender)))

print("Pearson's Chi-squared test Result is:")
print(chi.test_Gender)
print("Oberved Values are:")
print(chi.test_Gender$observed)
print("Expected values are:")
print(chi.test_Gender$expected)
print("Residuals values are:") 
print(chi.test_Gender$residuals) #This Gives us (Obsevered values - Expected Values)
print("Sum :")
print(sum((chi.test_Gender$residuals)^2))
print("Here we can see that P-Value is greater than 0.05, so we can not reject the Null Hypothesis.")
print("And we can say with 95% Confidence that an Attrition and Gender are independent.")


#12. HourlyRate Vs Attrition i.e. Quantitative data Vs Categorical Data
#---Summerizarion
print(paste("Range of Hourly Rate is from",min(mydata$HourlyRate),"To",max(mydata$HourlyRate)))
#---Visualization
png(filename = "HourlyRateVsAttrition.png",height=480, width= 960, unit ="px")
#---- Y- Attrition, X- HourlyRate ---Horizontal side by side box plot
boxplot(mydata$HourlyRate~mydata$Attrition_label, col ="Red",
        horizontal = TRUE, 
        main = "Boxplot of Hourly Rate Vs Attrition",
        xlab = "Hourly Rate", ylab="Attrition") 
dev.off()


#stats-having the position of the lower/upper extremes 
#of the whiskers and box along with the median
print("$stats")
print(boxplot(mydata$HourlyRate~mydata$Attrition_label, col ="Red",
                          horizontal = TRUE,
                          main = "Boxplot of Hourly Rate Vs Attrition",
                          xlab = "Hourly Rate", ylab="Attrition"
)$stats)

#n-the number of observations
print("$n")
print(boxplot(mydata$HourlyRate~mydata$Attrition_label, col ="Red",
              horizontal = TRUE,
              main = "Boxplot of Hourly Rate Vs Attrition",
              xlab = "Hourly Rate", ylab="Attrition"
)$n)

#conf-upper/lower extremes of the notch
print("$conf")
print(boxplot(mydata$HourlyRate~mydata$Attrition_label, col ="Red",
              horizontal = TRUE,
              main = "Boxplot of Hourly Rate Vs Attrition",
              xlab = "Hourly Rate", ylab="Attrition"
)$conf)

#out-value of the outliers
print("$out")
print(boxplot(mydata$HourlyRate~mydata$Attrition_label, col ="Red",
              horizontal = TRUE,
              main = "Boxplot of Hourly Rate Vs Attrition",
              xlab = "Hourly Rate", ylab="Attrition"
)$out)

#group-a vector of the same length as out whose elements 
#indicate to which group the outlier belongs
print("$group")
print(boxplot(mydata$HourlyRate~mydata$Attrition_label, col ="Red",
              horizontal = TRUE,
              main = "Boxplot of Hourly Rate Vs Attrition",
              xlab = "Hourly Rate", ylab="Attrition"
)$group)

#names-a vector of names for the groups
print("$names")
print(boxplot(mydata$HourlyRate~mydata$Attrition_label, col ="Red",
              horizontal = TRUE,
              main = "Boxplot of Hourly Rate Vs Attrition",
              xlab = "Hourly Rate", ylab="Attrition"
)$names)


#---Test of Hypothesis - Logistic Regression -Which we will do in next step(seperate file)
#--- There is no such test of hypothesis for Q -> C but we can use gostic regression.


#13. JobInvolvement Vs Attrition i.e. Categorical Data Vs Categorical Data
# C -> C
#---Summerizarion
#----Comparative Proportion
#----Contingency Table
print(paste("Table of Job Involvement Vs Attrition"))
print(table(mydata$JobInvolvement, mydata$Attrition_label))
print(paste("Proportion Table of Job Involvement Vs Attrition"))
print(prop.table(table(mydata$JobInvolvement, mydata$Attrition_label),1))

#---Visualization
png(filename = "JobInvolvementvsAttrition.png",height=480, width= 960, unit ="px")
#---- Y- Attrition, X- JobInvolvement ---Bubble Plot
plot(mydata$Attrition_label~mydata$JobInvolvement,
     col =c("Green","Red"), 
     main = "Plot of Job Involvement Vs Attrition",
     xlab = "Job Involvement(1=Lowest, 4=Highest)", ylab="Attrition")


dev.off()

#---Test of Hypothesis 
#-----Null Hypothesis, Ho: JobInvolvement and Attrition are independent.
#-----Alt. Hypothesis, Ha: JobInvolvement and Attrition are dependent on each other.
#----Chi-Square test
chi.tab_JobInvolvement <- table(mydata$JobInvolvement, mydata$Attrition_label)
chi.test_JobInvolvement <- chisq.test(chi.tab_JobInvolvement,p = rep(1/length(mydata$JobInvolvement), length(mydata$JobInvolvement)))

print("Pearson's Chi-squared test Result is:")
print(chi.test_JobInvolvement)
print("Oberved Values are:")
print(chi.test_JobInvolvement$observed)
print("Expected values are:")
print(chi.test_JobInvolvement$expected)
print("Residuals values are:") 
print(chi.test_JobInvolvement$residuals) #This Gives us (Obsevered values - Expected Values)
print("Sum :")
print(sum((chi.test_JobInvolvement$residuals)^2))
print("Here we can see that P-Value is very less than 0.05, so we can Reject the Null Hypothesis.")
print("And we can say with 95% Confidence that an Attrition is dependent on Job Involvement.")


#14. JobLevel Vs Attrition i.e. Categorical Data Vs Categorical Data
# C -> C
#---Summerizarion
#----Comparative Proportion
#----Contingency Table
print(paste("Table of Job Level Vs Attrition"))
print(table(mydata$JobLevel, mydata$Attrition_label))
print(paste("Proportion Table of Job Level Vs Attrition"))
print(prop.table(table(mydata$JobLevel, mydata$Attrition_label),1))

#---Visualization
png(filename = "JobLevelvsAttrition.png",height=480, width= 960, unit ="px")
#---- Y- Attrition, X- JobLevel ---Bubble Plot
plot(mydata$Attrition_label~mydata$JobLevel,
     col =c("Green","Red"), 
     main = "Plot of Job Level Vs Attrition",
     xlab = "Job Level(1=Lowest, 5=Highest)", ylab="Attrition")


dev.off()

#---Test of Hypothesis 
#-----Null Hypothesis, Ho: JobLevel and Attrition are independent.
#-----Alt. Hypothesis, Ha: JobLevel and Attrition are dependent on each other.
#----Chi-Square test
chi.tab_JobLevel <- table(mydata$JobLevel, mydata$Attrition_label)
chi.test_JobLevel <- chisq.test(chi.tab_JobLevel,p = rep(1/length(mydata$JobLevel), length(mydata$JobLevel)))

print("Pearson's Chi-squared test Result is:")
print(chi.test_JobLevel)
print("Oberved Values are:")
print(chi.test_JobLevel$observed)
print("Expected values are:")
print(chi.test_JobLevel$expected)
print("Residuals values are:") 
print(chi.test_JobLevel$residuals) #This Gives us (Obsevered values - Expected Values)
print("Sum :")
print(sum((chi.test_JobLevel$residuals)^2))
print("Here we can see that P-Value is very less than 0.05, so we can Reject the Null Hypothesis.")
print("And we can say with 95% Confidence that an Attrition is dependent on Job Level.")


#15. JobRole Vs Attrition i.e. Categorical Data Vs Categorical Data
# C -> C
#---Summerizarion
#----Comparative Proportion
#----Contingency Table
print(paste("Table of Job Role Vs Attrition"))
print(table(mydata$JobRole_label, mydata$Attrition_label))
print(paste("Proportion Table of Job Role Vs Attrition"))
print(prop.table(table(mydata$JobRole_label, mydata$Attrition_label),1))

#---Visualization
png(filename = "JobRolevsAttrition.png",height=480, width= 960, unit ="px")
#---- Y- Attrition, X- JobRole---Bubble Plot
plot(mydata$Attrition_label~mydata$JobRole_label,
     col =c("Green","Red"), 
     main = "Plot of Job Role Vs Attrition",
     xlab = "Job Role", ylab="Attrition")


dev.off()

#---Test of Hypothesis 
#-----Null Hypothesis, Ho: JobRole and Attrition are independent.
#-----Alt. Hypothesis, Ha: JobRole and Attrition are dependent on each other.
#----Chi-Square test

chi.tab_JobRole<- table(mydata$JobRole, mydata$Attrition_label)
chi.test_JobRole<- chisq.test(chi.tab_JobRole,p = rep(1/length(mydata$JobRole), length(mydata$JobRole)))

print("Pearson's Chi-squared test Result is:")
print(chi.test_JobRole)
print("Oberved Values are:")
print(chi.test_JobRole$observed)
print("Expected values are:")
print(chi.test_JobRole$expected)
print("Residuals values are:") 
print(chi.test_JobRole$residuals) #This Gives us (Obsevered values - Expected Values)
print("Sum :")
print(sum((chi.test_JobRole$residuals)^2))
print("Here we can see that P-Value is very less than 0.05, so we can Reject the Null Hypothesis.")
print("And we can say with 95% Confidence that an Attrition is dependent on Job Role.")


#16. JobSatisfaction Vs Attrition i.e. Categorical Data Vs Categorical Data
# C -> C
#---Summerizarion
#----Comparative Proportion
#----Contingency Table
print(paste("Table of Job Satisfaction Vs Attrition"))
print(table(mydata$JobSatisfaction, mydata$Attrition_label))
print(paste("Proportion Table of Job Satisfaction Vs Attrition"))
print(prop.table(table(mydata$JobSatisfaction, mydata$Attrition_label),1))

#---Visualization
png(filename = "JobSatisfactionvsAttrition.png",height=480, width= 960, unit ="px")
#---- Y- Attrition, X- JobSatisfaction ---Bubble Plot
plot(mydata$Attrition_label~mydata$JobSatisfaction,
     col =c("Green","Red"), 
     main = "Plot of Job Satisfaction Vs Attrition",
     xlab = "Job Satisfaction(1=Lowest, 4=Highest)", ylab="Attrition")


dev.off()

#---Test of Hypothesis 
#-----Null Hypothesis, Ho: JobSatisfaction and Attrition are independent.
#-----Alt. Hypothesis, Ha: JobSatisfaction and Attrition are dependent on each other.
#----Chi-Square test
chi.tab_js <- table(mydata$JobSatisfaction, mydata$Attrition_label)
chi.test_js <- chisq.test(chi.tab_js,p = rep(1/length(mydata$JobSatisfaction), length(mydata$JobSatisfaction)))

print("Pearson's Chi-squared test Result is:")
print(chi.test_js)
print("Oberved Values are:")
print(chi.test_js$observed)
print("Expected values are:")
print(chi.test_js$expected)
print("Residuals values are:") 
print(chi.test_js$residuals) #This Gives us (Obsevered values - Expected Values)
print("Sum :")
print(sum((chi.test_js$residuals)^2))
print("Here we can see that P-Value is very less than 0.05, so we can Reject the Null Hypothesis.")
print("And we can say with 95% Confidence that an Attrition is dependent on Job Satisfaction.")


#17. MaritalStatus Vs Attrition i.e. Categorical Data Vs Categorical Data
# C -> C
#---Summerizarion
#----Comparative Proportion
#----Contingency Table
print(paste("Table of Marital Status Vs Attrition"))
print(table(mydata$MaritalStatus, mydata$Attrition_label))
print(paste("Proportion Table of Marital Status Vs Attrition"))
print(prop.table(table(mydata$MaritalStatus, mydata$Attrition_label),1))

#---Visualization
png(filename = "MaritalStatusvsAttrition.png",height=480, width= 960, unit ="px")
#---- Y- Attrition, X- MaritalStatus ---Bubble Plot
plot(mydata$Attrition_label~mydata$MaritalStatus,
     col =c("Green","Red"), 
     main = "Plot of Marital Status Vs Attrition",
     xlab = "Marital Status", ylab="Attrition")

dev.off()

#---Test of Hypothesis 
#-----Null Hypothesis, Ho: MaritalStatus and Attrition are independent.
#-----Alt. Hypothesis, Ha: MaritalStatus and Attrition are dependent on each other.
#----Chi-Square test
chi.tab_ms <- table(mydata$MaritalStatus, mydata$Attrition_label)
chi.test_ms <- chisq.test(chi.tab_ms,p = rep(1/length(mydata$MaritalStatus), length(mydata$MaritalStatus)))

print("Pearson's Chi-squared test Result is:")
print(chi.test_ms)
print("Oberved Values are:")
print(chi.test_ms$observed)
print("Expected values are:")
print(chi.test_ms$expected)
print("Residuals values are:") 
print(chi.test_ms$residuals) #This Gives us (Obsevered values - Expected Values)
print("Sum :")
print(sum((chi.test_ms$residuals)^2))
print("Here we can see that P-Value is very less than 0.05, so we can Reject the Null Hypothesis.")
print("And we can say with 95% Confidence that an Attrition is dependent on Marital Status.")


#18. MonthlyIncome Vs Attrition i.e. Quantitative data Vs Categorical Data
#---Summerizarion
print(paste("Range of Monthly Income is from",min(mydata$MonthlyIncome),"To",max(mydata$MonthlyIncome)))
#---Visualization
png(filename = "MonthlyIncomeVsAttrition.png",height=480, width= 960, unit ="px")
#---- Y- Attrition, X- MonthlyIncome ---Horizontal side by side box plot
boxplot(mydata$MonthlyIncome~mydata$Attrition_label, col ="Red",
        horizontal = TRUE, 
        main = "Boxplot of Monthly Income Vs Attrition",
        xlab = "Monthly Income", ylab="Attrition"
) 
dev.off()

#stats-having the position of the lower/upper extremes 
#of the whiskers and box along with the median
print("$stats")
print(boxplot(mydata$MonthlyIncome~mydata$Attrition_label, col ="Red",
                             horizontal = TRUE,
                             main = "Boxplot of Monthly Income Vs Attrition",
                             xlab = "Monthly Income", ylab="Attrition"
)$stats)

#n-the number of observations
print("$n")
print(boxplot(mydata$MonthlyIncome~mydata$Attrition_label, col ="Red",
              horizontal = TRUE,
              main = "Boxplot of Monthly Income Vs Attrition",
              xlab = "Monthly Income", ylab="Attrition"
)$n)

#conf-upper/lower extremes of the notch
print("$conf")
print(boxplot(mydata$MonthlyIncome~mydata$Attrition_label, col ="Red",
              horizontal = TRUE,
              main = "Boxplot of Monthly Income Vs Attrition",
              xlab = "Monthly Income", ylab="Attrition"
)$conf)

#out-value of the outliers
print("$out")
print(boxplot(mydata$MonthlyIncome~mydata$Attrition_label, col ="Red",
              horizontal = TRUE,
              main = "Boxplot of Monthly Income Vs Attrition",
              xlab = "Monthly Income", ylab="Attrition"
)$out)

#group-a vector of the same length as out whose elements 
#indicate to which group the outlier belongs (e.g. 56  & 58)
print("$group")
print(boxplot(mydata$MonthlyIncome~mydata$Attrition_label, col ="Red",
              horizontal = TRUE,
              main = "Boxplot of Monthly Income Vs Attrition",
              xlab = "Monthly Income", ylab="Attrition"
)$group)

#names-a vector of names for the groups
print("$names")
print(boxplot(mydata$MonthlyIncome~mydata$Attrition_label, col ="Red",
              horizontal = TRUE,
              main = "Boxplot of Monthly Income Vs Attrition",
              xlab = "Monthly Income", ylab="Attrition"
)$names)


#---Test of Hypothesis - Logistic Regression -Which we will do in next step(seperate file)
#--- There is no such test of hypothesis for Q -> C but we can use gostic regression.

#19. MonthlyRate Vs Attrition i.e. Quantitative data Vs Categorical Data
#---Summerizarion
print(paste("Range of Monthly Rate is from",min(mydata$MonthlyRate),"To",max(mydata$MonthlyRate)))

#---Visualization
png(filename = "MonthlyRateVsAttrition.png",height=480, width= 960, unit ="px")
#---- Y- Attrition, X- MonthlyRate ---Horizontal side by side box plot
boxplot(mydata$MonthlyRate~mydata$Attrition_label, col ="Red",
        horizontal = TRUE, 
        main = "Boxplot of Monthly Rate Vs Attrition",
        xlab = "Monthly Rate", ylab="Attrition"
) 
dev.off()

#stats-having the position of the lower/upper extremes 
#of the whiskers and box along with the median
print("$stats")
print(boxplot(mydata$MonthlyRate~mydata$Attrition_label, col ="Red",
                           horizontal = TRUE,
                           main = "Boxplot of Monthly Rate Vs Attrition",
                           xlab = "Monthly Rate", ylab="Attrition"
)$stats)

#n-the number of observations
print("$n")
print(boxplot(mydata$MonthlyRate~mydata$Attrition_label, col ="Red",
              horizontal = TRUE,
              main = "Boxplot of Monthly Rate Vs Attrition",
              xlab = "Monthly Rate", ylab="Attrition"
)$n)

#conf-upper/lower extremes of the notch
print("$conf")
print(boxplot(mydata$MonthlyRate~mydata$Attrition_label, col ="Red",
              horizontal = TRUE,
              main = "Boxplot of Monthly Rate Vs Attrition",
              xlab = "Monthly Rate", ylab="Attrition"
)$conf)

#out-value of the outliers
print("$out")
print(boxplot(mydata$MonthlyRate~mydata$Attrition_label, col ="Red",
              horizontal = TRUE,
              main = "Boxplot of Monthly Rate Vs Attrition",
              xlab = "Monthly Rate", ylab="Attrition"
)$out)

#group-a vector of the same length as out whose elements 
#indicate to which group the outlier belongs (e.g. 56  & 58)
print("$group")
print(boxplot(mydata$MonthlyRate~mydata$Attrition_label, col ="Red",
              horizontal = TRUE,
              main = "Boxplot of Monthly Rate Vs Attrition",
              xlab = "Monthly Rate", ylab="Attrition"
)$group)

#names-a vector of names for the groups
print("$names")
print(boxplot(mydata$MonthlyRate~mydata$Attrition_label, col ="Red",
              horizontal = TRUE,
              main = "Boxplot of Monthly Rate Vs Attrition",
              xlab = "Monthly Rate", ylab="Attrition"
)$names)


#---Test of Hypothesis - Logistic Regression -Which we will do in next step(seperate file)
#--- There is no such test of hypothesis for Q -> C but we can use gostic regression.


#20. NumCompaniesWorked Vs Attrition i.e. Quantitative data Vs Categorical Data
#---Summerizarion
print(paste("Range of No. of Companies Worked is from",min(mydata$NumCompaniesWorked),"To",max(mydata$NumCompaniesWorked)))
#---Visualization
png(filename = "NumCompaniesWorkedVsAttrition.png",height=480, width= 960, unit ="px")
#---- Y- Attrition, X- NumCompaniesWorked ---Horizontal side by side box plot
boxplot(mydata$NumCompaniesWorked~mydata$Attrition_label, col ="Red",
        horizontal = TRUE, 
        main = "Boxplot of No. of Companies Worked Vs Attrition",
        xlab = "No. of Companies Worked", ylab="Attrition"
) 
dev.off()

#stats-having the position of the lower/upper extremes 
#of the whiskers and box along with the median
print("$stats")
print(boxplot(mydata$NumCompaniesWorked~mydata$Attrition_label, col ="Red",
        horizontal = TRUE,
        main = "Boxplot of NumCompaniesWorked Vs Attrition",
        xlab = "NumCompaniesWorked", ylab="Attrition"
)$stats)

#n-the number of observations
print("$n")
print(boxplot(mydata$NumCompaniesWorked~mydata$Attrition_label, col ="Red",
              horizontal = TRUE,
              main = "Boxplot of NumCompaniesWorked Vs Attrition",
              xlab = "NumCompaniesWorked", ylab="Attrition"
)$n)

#conf-upper/lower extremes of the notch
print("$conf")
print(boxplot(mydata$NumCompaniesWorked~mydata$Attrition_label, col ="Red",
              horizontal = TRUE,
              main = "Boxplot of NumCompaniesWorked Vs Attrition",
              xlab = "NumCompaniesWorked", ylab="Attrition"
)$conf)

#out-value of the outliers
print("$out")
print(boxplot(mydata$NumCompaniesWorked~mydata$Attrition_label, col ="Red",
              horizontal = TRUE,
              main = "Boxplot of NumCompaniesWorked Vs Attrition",
              xlab = "NumCompaniesWorked", ylab="Attrition"
)$out)

#group-a vector of the same length as out whose elements 
#indicate to which group the outlier belongs (e.g. 56  & 58)
print("$group")
print(boxplot(mydata$NumCompaniesWorked~mydata$Attrition_label, col ="Red",
              horizontal = TRUE,
              main = "Boxplot of NumCompaniesWorked Vs Attrition",
              xlab = "NumCompaniesWorked", ylab="Attrition"
)$group)

#names-a vector of names for the groups
print("$names")
print(boxplot(mydata$NumCompaniesWorked~mydata$Attrition_label, col ="Red",
              horizontal = TRUE,
              main = "Boxplot of NumCompaniesWorked Vs Attrition",
              xlab = "NumCompaniesWorked", ylab="Attrition"
)$names)

#---Test of Hypothesis - Logistic Regression -Which we will do in next step(seperate file)
#--- There is no such test of hypothesis for Q -> C but we can use gostic regression.


#21. Here Over18 variable is not useful against Attrition because all 
# Employees are Over 18 years of Age.

#22. OverTime Vs Attrition i.e. Categorical Data Vs Categorical Data
# C -> C
#---Summerizarion
#----Comparative Proportion
#----Contingency Table
print(paste("Proportion Table of Over Time Vs Attrition"))
print(table(mydata$OverTime, mydata$Attrition_label))
print(paste("Proportion Table of Over Time Vs Attrition"))
print(prop.table(table(mydata$OverTime, mydata$Attrition_label),1))

#---Visualization
png(filename = "OverTimevsAttrition.png",height=480, width= 960, unit ="px")
#---- Y- Attrition, X- OverTime ---Bubble Plot
plot(mydata$Attrition_label~mydata$OverTime,
     col =c("Green","Red"), 
     main = "Plot of Over Time Vs Attrition",
     xlab = "Over Time", ylab="Attrition")


dev.off()

#---Test of Hypothesis 
#-----Null Hypothesis, Ho: OverTime and Attrition are independent.
#-----Alt. Hypothesis, Ha: OverTime and Attrition are dependent on each other.
#----Chi-Square test
chi.tab_ot <- table(mydata$OverTime, mydata$Attrition_label)
chi.test_ot <- chisq.test(chi.tab_ot,p = rep(1/length(mydata$OverTime), length(mydata$OverTime)))

print("Pearson's Chi-squared test Result is:")
print(chi.test_ot)
print("Oberved Values are:")
print(chi.test_ot$observed)
print("Expected values are:")
print(chi.test_ot$expected)
print("Residuals values are:") 
print(chi.test_ot$residuals) #This Gives us (Obsevered values - Expected Values)
print("Sum :")
print(sum((chi.test_ot$residuals)^2))
print("Here we can see that P-Value is very less than 0.05, so we can Reject the Null Hypothesis.")
print("And we can say with 95% Confidence that an Attrition is dependent on Over Time.")


#23. PercentSalaryHike Vs Attrition i.e. Quantitative data Vs Categorical Data
#---Summerizarion
print(paste("Range of Percent Salary Hike is from",min(mydata$PercentSalaryHike),"To",max(mydata$PercentSalaryHike)))
#---Visualization
png(filename = "PercentSalaryHikeVsAttrition.png",height=480, width= 960, unit ="px")
#---- Y- Attrition, X- PercentSalaryHike ---Horizontal side by side box plot
boxplot(mydata$PercentSalaryHike~mydata$Attrition_label, col ="Red",
        horizontal = TRUE, 
        main = "Boxplot of Percent Salary Hike Vs Attrition",
        xlab = "Percent Salary Hike", ylab="Attrition"
) 
dev.off()

#stats-having the position of the lower/upper extremes 
#of the whiskers and box along with the median
print("$stats")
print(boxplot(mydata$PercentSalaryHike~mydata$Attrition_label, col ="Red",
        horizontal = TRUE,
        main = "Boxplot of PercentSalaryHike Vs Attrition",
        xlab = "PercentSalaryHike", ylab="Attrition"
)$stats)

#n-the number of observations
print("$n")
print(boxplot(mydata$PercentSalaryHike~mydata$Attrition_label, col ="Red",
              horizontal = TRUE,
              main = "Boxplot of PercentSalaryHike Vs Attrition",
              xlab = "PercentSalaryHike", ylab="Attrition"
)$n)

#conf-upper/lower extremes of the notch
print("$conf")
print(boxplot(mydata$PercentSalaryHike~mydata$Attrition_label, col ="Red",
              horizontal = TRUE,
              main = "Boxplot of PercentSalaryHike Vs Attrition",
              xlab = "PercentSalaryHike", ylab="Attrition"
)$conf)

#out-value of the outliers
print("$out")
print(boxplot(mydata$PercentSalaryHike~mydata$Attrition_label, col ="Red",
              horizontal = TRUE,
              main = "Boxplot of PercentSalaryHike Vs Attrition",
              xlab = "PercentSalaryHike", ylab="Attrition"
)$out)

#group-a vector of the same length as out whose elements 
#indicate to which group the outlier belongs
print("$group")
print(boxplot(mydata$PercentSalaryHike~mydata$Attrition_label, col ="Red",
              horizontal = TRUE,
              main = "Boxplot of PercentSalaryHike Vs Attrition",
              xlab = "PercentSalaryHike", ylab="Attrition"
)$group)

#names-a vector of names for the groups
print("$names")
print(boxplot(mydata$PercentSalaryHike~mydata$Attrition_label, col ="Red",
              horizontal = TRUE,
              main = "Boxplot of PercentSalaryHike Vs Attrition",
              xlab = "PercentSalaryHike", ylab="Attrition"
)$names)

#---Test of Hypothesis - Logistic Regression -Which we will do in next step(seperate file)
#--- There is no such test of hypothesis for Q -> C but we can use gostic regression.


#24. PerformanceRating Vs Attrition i.e. Categorical Data Vs Categorical Data
# C -> C
#---Summerizarion
#----Comparative Proportion
#----Contingency Table
print(paste("Table of Performance Rating Vs Attrition"))
print(table(mydata$PerformanceRating, mydata$Attrition_label))
print(paste("Proportion Table of Performance Rating Vs Attrition"))
print(prop.table(table(mydata$PerformanceRating, mydata$Attrition_label),1))

#---Visualization
png(filename = "PerformanceRatingvsAttrition.png",height=480, width= 960, unit ="px")
#---- Y- Attrition, X- PerformanceRating ---Bubble Plot
plot(mydata$Attrition_label~mydata$PerformanceRating,
     col =c("Green","Red"), 
     main = "Plot of Performance Rating Vs Attrition",
     xlab = "Performance Rating (1=Lowest,5=Highest)", ylab="Attrition")


dev.off()

#---Test of Hypothesis 
#-----Null Hypothesis, Ho: PerformanceRating and Attrition are independent.
#-----Alt. Hypothesis, Ha: PerformanceRating and Attrition are dependent on each other.
#----Chi-Square test
chi.tab_pr <- table(mydata$PerformanceRating, mydata$Attrition_label)
chi.test_pr <- chisq.test(chi.tab_pr,p = rep(1/length(mydata$PerformanceRating), length(mydata$PerformanceRating)))

print("Pearson's Chi-squared test Result is:")
print(chi.test_pr)
print("Oberved Values are:")
print(chi.test_pr$observed)
print("Expected values are:")
print(chi.test_pr$expected)
print("Residuals values are:") 
print(chi.test_pr$residuals) #This Gives us (Obsevered values - Expected Values)
print("Sum :")
print(sum((chi.test_pr$residuals)^2))
print("Here we can see that P-Value is significantly greater than 0.05, so we can not Reject the Null Hypothesis.")
print("And we can say with 95% Confidence that an Attrition and Performance Rating is independent of each other.")


#25. RelationshipSatisfaction Vs Attrition i.e. Categorical Data Vs Categorical Data
# C -> C
#---Summerizarion
#----Comparative Proportion
#----Contingency Table
print(paste("Table of Relationship Satisfaction Vs Attrition"))
print(table(mydata$RelationshipSatisfaction, mydata$Attrition_label))
print(paste("Proportion Table of Relationship Satisfaction Vs Attrition"))
print(prop.table(table(mydata$RelationshipSatisfaction, mydata$Attrition_label),1))

#---Visualization
png(filename = "RelationshipSatisfactionvsAttrition.png",height=480, width= 960, unit ="px")
#---- Y- Attrition, X- RelationshipSatisfaction ---Bubble Plot
plot(mydata$Attrition_label~mydata$RelationshipSatisfaction,
     col =c("Green","Red"), 
     main = "Plot of Relationship Satisfaction Vs Attrition",
     xlab = "Relationship Satisfaction  (1=Lowest, 4=Highest)", ylab="Attrition")


dev.off()

#---Test of Hypothesis 
#-----Null Hypothesis, Ho: RelationshipSatisfaction and Attrition are independent.
#-----Alt. Hypothesis, Ha: RelationshipSatisfaction and Attrition are dependent on each other.
#----Chi-Square test
chi.tab_rs <- table(mydata$RelationshipSatisfaction, mydata$Attrition_label)
chi.test_rs <- chisq.test(chi.tab_rs,p = rep(1/length(mydata$RelationshipSatisfaction), length(mydata$RelationshipSatisfaction)))

print("Pearson's Chi-squared test Result is:")
print(chi.test_rs)
print("Oberved Values are:")
print(chi.test_rs$observed)
print("Expected values are:")
print(chi.test_rs$expected)
print("Residuals values are:") 
print(chi.test_rs$residuals) #This Gives us (Obsevered values - Expected Values)
print("Sum :")
print(sum((chi.test_rs$residuals)^2))
print("Here we can see that P-Value is significantly greater than 0.05, so we can not Reject the Null Hypothesis.")
print("And we can say with 95% Confidence that an Attrition and Relationship Satisfaction are independent of each other.")


#26. StandardHours Vs Attrition i.e. Quantitative data Vs Categorical Data
#---Summerizarion
print(paste("Range of Standard Hours is from",min(mydata$StandardHours),"To",max(mydata$StandardHours)))
#---Visualization
png(filename = "StandardHoursVsAttrition.png",height=480, width= 960, unit ="px")
#---- Y- Attrition, X- StandardHours ---Horizontal side by side box plot
boxplot(mydata$StandardHours~mydata$Attrition_label, col ="Red",
        horizontal = TRUE, 
        main = "Boxplot of Standard Hours Vs Attrition",
        xlab = "Standard Hours", ylab="Attrition"
) 
dev.off()
print("$stats")
print(boxplot(mydata$StandardHours~mydata$Attrition_label, col ="Red",
        horizontal = TRUE,
        main = "Boxplot of StandardHours Vs Attrition",
        xlab = "StandardHours", ylab="Attrition"
)$stats)
#---Test of Hypothesis - Logistic Regression -Which we will do in next step(seperate file)
#--- There is no such test of hypothesis for Q -> C but we can use gostic regression.

#27. StockOptionLevel Vs Attrition i.e. Categorical Data Vs Categorical Data
# C -> C
#---Summerizarion
#----Comparative Proportion
#----Contingency Table
print(paste("Table of Stock Option Level Vs Attrition"))
print(table(mydata$StockOptionLevel, mydata$Attrition_label))
print(paste("Proportion Table of Stock Option Level Vs Attrition"))
print(prop.table(table(mydata$StockOptionLevel, mydata$Attrition_label),1))

#---Visualization
png(filename = "StockOptionLevelvsAttrition.png",height=480, width= 960, unit ="px")
#---- Y- Attrition, X- StockOptionLevel ---Bubble Plot
plot(mydata$Attrition_label~mydata$StockOptionLevel,
     col =c("Green","Red"), 
     main = "Plot of Stock Option Level Vs Attrition",
     xlab = "Stock Option Level  (0=No option, 1 = Low, 2 = Medim, 3 = High)", ylab="Attrition")


dev.off()

#---Test of Hypothesis 
#-----Null Hypothesis, Ho: StockOptionLevel and Attrition are independent.
#-----Alt. Hypothesis, Ha: StockOptionLevel and Attrition are dependent on each other.
#----Chi-Square test
chi.tab_so <- table(mydata$StockOptionLevel, mydata$Attrition_label)
chi.test_so <- chisq.test(chi.tab_so,p = rep(1/length(mydata$StockOptionLevel), length(mydata$StockOptionLevel)))

print("Pearson's Chi-squared test Result is:")
print(chi.test_so)
print("Oberved Values are:")
print(chi.test_so$observed)
print("Expected values are:")
print(chi.test_so$expected)
print("Residuals values are:") 
print(chi.test_so$residuals) #This Gives us (Obsevered values - Expected Values)
print("Sum :")
print(sum((chi.test_so$residuals)^2))
print("Here we can see that P-Value is less than 0.05, so we can Reject the Null Hypothesis.")
print("And we can say with 95% Confidence that an Attrition is dependent on Stock Option Level.")


#28. TotalWorkingYears Vs Attrition i.e. Quantitative data Vs Categorical Data
#---Summerizarion
print(paste("Range of Total Working Years is from",min(mydata$TotalWorkingYears),"To",max(mydata$TotalWorkingYears)))
#---Visualization
png(filename = "TotalWorkingYearsVsAttrition.png",height=480, width= 960, unit ="px")
#---- Y- Attrition, X- TotalWorkingYears ---Horizontal side by side box plot
boxplot(mydata$TotalWorkingYears~mydata$Attrition_label, col ="Red",
        horizontal = TRUE, 
        main = "Boxplot of Total Working Years Vs Attrition",
        xlab = "Total Working Years", ylab="Attrition"
) 
dev.off()

#stats-having the position of the lower/upper extremes 
#of the whiskers and box along with the median
print("$stats")
print(boxplot(mydata$TotalWorkingYears~mydata$Attrition_label, col ="Red",
        horizontal = TRUE,
        main = "Boxplot of TotalWorkingYears Vs Attrition",
        xlab = "TotalWorkingYears", ylab="Attrition"
)$stats)

#n-the number of observations
print("$n")
print(boxplot(mydata$TotalWorkingYears~mydata$Attrition_label, col ="Red",
              horizontal = TRUE,
              main = "Boxplot of TotalWorkingYears Vs Attrition",
              xlab = "TotalWorkingYears", ylab="Attrition"
)$n)

#conf-upper/lower extremes of the notch
print("$conf")
print(boxplot(mydata$TotalWorkingYears~mydata$Attrition_label, col ="Red",
              horizontal = TRUE,
              main = "Boxplot of TotalWorkingYears Vs Attrition",
              xlab = "TotalWorkingYears", ylab="Attrition"
)$conf)

#out-value of the outliers
print("$out")
print(boxplot(mydata$TotalWorkingYears~mydata$Attrition_label, col ="Red",
              horizontal = TRUE,
              main = "Boxplot of TotalWorkingYears Vs Attrition",
              xlab = "TotalWorkingYears", ylab="Attrition"
)$out)

#group-a vector of the same length as out whose elements 
#indicate to which group the outlier belongs (e.g. 56  & 58)
print("$group")
print(boxplot(mydata$TotalWorkingYears~mydata$Attrition_label, col ="Red",
              horizontal = TRUE,
              main = "Boxplot of TotalWorkingYears Vs Attrition",
              xlab = "TotalWorkingYears", ylab="Attrition"
)$group)

#names-a vector of names for the groups
print("$names")
print(boxplot(mydata$TotalWorkingYears~mydata$Attrition_label, col ="Red",
              horizontal = TRUE,
              main = "Boxplot of TotalWorkingYears Vs Attrition",
              xlab = "TotalWorkingYears", ylab="Attrition"
)$names)

#---Test of Hypothesis - Logistic Regression -Which we will do in next step(seperate file)
#--- There is no such test of hypothesis for Q -> C but we can use gostic regression.


#29. TrainingTimesLastYear Vs Attrition i.e. Quantitative data Vs Categorical Data
#---Summerizarion
print(paste("Range of Training Times Last Year is from",min(mydata$TrainingTimesLastYear),"To",max(mydata$TrainingTimesLastYear)))
#---Visualization
png(filename = "TrainingTimesLastYearVsAttrition.png",height=480, width= 960, unit ="px")
#---- Y- Attrition, X- TrainingTimesLastYear ---Horizontal side by side box plot
boxplot(mydata$TrainingTimesLastYear~mydata$Attrition_label, col ="Red",
        horizontal = TRUE, 
        main = "Boxplot of Training Times Last Year Vs Attrition",
        xlab = "Training Times Last Year", ylab="Attrition"
) 
dev.off()

#stats-having the position of the lower/upper extremes 
#of the whiskers and box along with the median
print("$stats")
print(boxplot(mydata$TrainingTimesLastYear~mydata$Attrition_label, col ="Red",
        horizontal = TRUE,
        main = "Boxplot of TrainingTimesLastYear Vs Attrition",
        xlab = "TrainingTimesLastYear", ylab="Attrition"
)$stats)

#n-the number of observations
print("$n")
print(boxplot(mydata$TrainingTimesLastYear~mydata$Attrition_label, col ="Red",
              horizontal = TRUE,
              main = "Boxplot of TrainingTimesLastYear Vs Attrition",
              xlab = "TrainingTimesLastYear", ylab="Attrition"
)$n)

#conf-upper/lower extremes of the notch
print("$conf")
print(boxplot(mydata$TrainingTimesLastYear~mydata$Attrition_label, col ="Red",
              horizontal = TRUE,
              main = "Boxplot of TrainingTimesLastYear Vs Attrition",
              xlab = "TrainingTimesLastYear", ylab="Attrition"
)$conf)

#out-value of the outliers
print("$out")
print(boxplot(mydata$TrainingTimesLastYear~mydata$Attrition_label, col ="Red",
              horizontal = TRUE,
              main = "Boxplot of TrainingTimesLastYear Vs Attrition",
              xlab = "TrainingTimesLastYear", ylab="Attrition"
)$out)

#group-a vector of the same length as out whose elements 
#indicate to which group the outlier belongs (e.g. 56  & 58)
print("$group")
print(boxplot(mydata$TrainingTimesLastYear~mydata$Attrition_label, col ="Red",
              horizontal = TRUE,
              main = "Boxplot of TrainingTimesLastYear Vs Attrition",
              xlab = "TrainingTimesLastYear", ylab="Attrition"
)$group)

#names-a vector of names for the groups
print("$names")
print(boxplot(mydata$TrainingTimesLastYear~mydata$Attrition_label, col ="Red",
              horizontal = TRUE,
              main = "Boxplot of TrainingTimesLastYear Vs Attrition",
              xlab = "TrainingTimesLastYear", ylab="Attrition"
)$names)

#---Test of Hypothesis - Logistic Regression -Which we will do in next step(seperate file)
#--- There is no such test of hypothesis for Q -> C but we can use gostic regression.


#30. WorkLifeBalance Vs Attrition i.e. Categorical Data Vs Categorical Data
# C -> C
#---Summerizarion
#----Comparative Proportion
#----Contingency Table
print(paste("Table of Work Life Balance Vs Attrition"))
print(table(mydata$WorkLifeBalance, mydata$Attrition_label))
print(paste("Proportion Table of Work Life Balance Vs Attrition"))
print(prop.table(table(mydata$WorkLifeBalance, mydata$Attrition_label),1))

#---Visualization
png(filename = "WorkLifeBalancevsAttrition.png",height=480, width= 960, unit ="px")
#---- Y- Attrition, X- WorkLifeBalance ---Bubble Plot
plot(mydata$Attrition_label~mydata$WorkLifeBalance,
     col =c("Green","Red"), 
     main = "Plot of Work Life Balance Vs Attrition",
     xlab = "Work Life Balance  (1=Lowest, 4=Highest)", ylab="Attrition")


dev.off()

#---Test of Hypothesis 
#-----Null Hypothesis, Ho: WorkLifeBalance and Attrition are independent.
#-----Alt. Hypothesis, Ha: WorkLifeBalance and Attrition are dependent on each other.
#----Chi-Square test
chi.tab_wb <- table(mydata$WorkLifeBalance, mydata$Attrition_label)
chi.test_wb <- chisq.test(chi.tab_wb,p = rep(1/length(mydata$WorkLifeBalance), length(mydata$WorkLifeBalance)))

print("Pearson's Chi-squared test Result is:")
print(chi.test_wb)
print("Oberved Values are:")
print(chi.test_wb$observed)
print("Expected values are:")
print(chi.test_wb$expected)
print("Residuals values are:") 
print(chi.test_wb$residuals) #This Gives us (Obsevered values - Expected Values)
print("Sum :")
print(sum((chi.test_wb$residuals)^2))
print("Here we can see that P-Value is less than 0.05, so we can Reject the Null Hypothesis.")
print("And we can say with 95% Confidence that an Attrition is dependent on Work Life Balance.")


#31. YearsAtCompany Vs Attrition i.e. Quantitative data Vs Categorical Data
#---Summerizarion
print(paste("Range of Years At Company is from",min(mydata$YearsAtCompany),"To",max(mydata$YearsAtCompany)))
#---Visualization
png(filename = "YearsAtCompanyVsAttrition.png",height=480, width= 960, unit ="px")
#---- Y- Attrition, X- YearsAtCompany ---Horizontal side by side box plot
boxplot(mydata$YearsAtCompany~mydata$Attrition_label, col ="Red",
        horizontal = TRUE, 
        main = "Boxplot of Years At Company Vs Attrition",
        xlab = "Years At Company", ylab="Attrition"
) 
dev.off()

#stats-having the position of the lower/upper extremes 
#of the whiskers and box along with the median
print("$stats")
print(boxplot(mydata$YearsAtCompany~mydata$Attrition_label, col ="Red",
        horizontal = TRUE,
        main = "Boxplot of YearsAtCompany Vs Attrition",
        xlab = "YearsAtCompany", ylab="Attrition"
)$stats)

#n-the number of observations
print("$n")
print(boxplot(mydata$YearsAtCompany~mydata$Attrition_label, col ="Red",
              horizontal = TRUE,
              main = "Boxplot of YearsAtCompany Vs Attrition",
              xlab = "YearsAtCompany", ylab="Attrition"
)$n)

#conf-upper/lower extremes of the notch
print("$conf")
print(boxplot(mydata$YearsAtCompany~mydata$Attrition_label, col ="Red",
              horizontal = TRUE,
              main = "Boxplot of YearsAtCompany Vs Attrition",
              xlab = "YearsAtCompany", ylab="Attrition"
)$conf)

#out-value of the outliers
print("$out")
print(boxplot(mydata$YearsAtCompany~mydata$Attrition_label, col ="Red",
              horizontal = TRUE,
              main = "Boxplot of YearsAtCompany Vs Attrition",
              xlab = "YearsAtCompany", ylab="Attrition"
)$out)

#group-a vector of the same length as out whose elements 
#indicate to which group the outlier belongs (e.g. 56  & 58)
print("$group")
print(boxplot(mydata$YearsAtCompany~mydata$Attrition_label, col ="Red",
              horizontal = TRUE,
              main = "Boxplot of YearsAtCompany Vs Attrition",
              xlab = "YearsAtCompany", ylab="Attrition"
)$group)

#names-a vector of names for the groups
print("$names")
print(boxplot(mydata$YearsAtCompany~mydata$Attrition_label, col ="Red",
              horizontal = TRUE,
              main = "Boxplot of YearsAtCompany Vs Attrition",
              xlab = "YearsAtCompany", ylab="Attrition"
)$names)

#---Test of Hypothesis - Logistic Regression -Which we will do in next step(seperate file)
#--- There is no such test of hypothesis for Q -> C but we can use gostic regression.


#32. YearsInCurrentRole Vs Attrition i.e. Quantitative data Vs Categorical Data
#---Summerizarion
print(paste("Range of Years In Current Role is from",min(mydata$YearsInCurrentRole),"To",max(mydata$YearsInCurrentRole)))
#---Visualization
png(filename = "YearsInCurrentRoleVsAttrition.png",height=480, width= 960, unit ="px")
#---- Y- Attrition, X- YearsInCurrentRole---Horizontal side by side box plot
boxplot(mydata$YearsInCurrentRole~mydata$Attrition_label, col ="Red",
        horizontal = TRUE, 
        main = "Boxplot of Years In Current Role Vs Attrition",
        xlab = "Years In Current Role", ylab="Attrition"
) 
dev.off()

#stats-having the position of the lower/upper extremes 
#of the whiskers and box along with the median
print("$stats")
print(boxplot(mydata$YearsInCurrentRole~mydata$Attrition_label, col ="Red",
        horizontal = TRUE,
        main = "Boxplot of YearsInCurrentRoleVs Attrition",
        xlab = "YearsInCurrentRole", ylab="Attrition"
)$stats)

#n-the number of observations
print("$n")
print(boxplot(mydata$YearsInCurrentRole~mydata$Attrition_label, col ="Red",
              horizontal = TRUE,
              main = "Boxplot of YearsInCurrentRoleVs Attrition",
              xlab = "YearsInCurrentRole", ylab="Attrition"
)$n)

#conf-upper/lower extremes of the notch
print("$conf")
print(boxplot(mydata$YearsInCurrentRole~mydata$Attrition_label, col ="Red",
              horizontal = TRUE,
              main = "Boxplot of YearsInCurrentRoleVs Attrition",
              xlab = "YearsInCurrentRole", ylab="Attrition"
)$conf)

#out-value of the outliers
print("$out")
print(boxplot(mydata$YearsInCurrentRole~mydata$Attrition_label, col ="Red",
              horizontal = TRUE,
              main = "Boxplot of YearsInCurrentRoleVs Attrition",
              xlab = "YearsInCurrentRole", ylab="Attrition"
)$out)

#group-a vector of the same length as out whose elements 
#indicate to which group the outlier belongs (e.g. 56  & 58)
print("$group")
print(boxplot(mydata$YearsInCurrentRole~mydata$Attrition_label, col ="Red",
              horizontal = TRUE,
              main = "Boxplot of YearsInCurrentRoleVs Attrition",
              xlab = "YearsInCurrentRole", ylab="Attrition"
)$group)

#names-a vector of names for the groups
print("$names")
print(boxplot(mydata$YearsInCurrentRole~mydata$Attrition_label, col ="Red",
              horizontal = TRUE,
              main = "Boxplot of YearsInCurrentRoleVs Attrition",
              xlab = "YearsInCurrentRole", ylab="Attrition"
)$names)

#---Test of Hypothesis - Logistic Regression -Which we will do in next step(seperate file)
#--- There is no such test of hypothesis for Q -> C but we can use gostic regression.


#33. YearsSinceLastPromotion Vs Attrition i.e. Quantitative data Vs Categorical Data
#---Summerizarion
print(paste("Range of Years Since Last Promotion is from",min(mydata$YearsSinceLastPromotion),"To",max(mydata$YearsSinceLastPromotion)))
#---Visualization
png(filename = "YearsSinceLastPromotionVsAttrition.png",height=480, width= 960, unit ="px")
#---- Y- Attrition, X- YearsSinceLastPromotion---Horizontal side by side box plot
boxplot(mydata$YearsSinceLastPromotion~mydata$Attrition_label, col ="Red",
        horizontal = TRUE, 
        main = "Boxplot of Years Since Last Promotion Vs Attrition",
        xlab = "Years Since Last Promotion", ylab="Attrition"
) 
dev.off()

#stats-having the position of the lower/upper extremes 
#of the whiskers and box along with the median
print("$stats")
print(boxplot(mydata$YearsSinceLastPromotion~mydata$Attrition_label, col ="Red",
        horizontal = TRUE,
        main = "Boxplot of YearsSinceLastPromotionVs Attrition",
        xlab = "YearsSinceLastPromotion", ylab="Attrition"
)$stats)


#n-the number of observations
print("$n")
print(boxplot(mydata$YearsSinceLastPromotion~mydata$Attrition_label, col ="Red",
              horizontal = TRUE,
              main = "Boxplot of YearsSinceLastPromotionVs Attrition",
              xlab = "YearsSinceLastPromotion", ylab="Attrition"
)$n)

#conf-upper/lower extremes of the notch
print("$conf")
print(boxplot(mydata$YearsSinceLastPromotion~mydata$Attrition_label, col ="Red",
              horizontal = TRUE,
              main = "Boxplot of YearsSinceLastPromotionVs Attrition",
              xlab = "Years Since Last Promotion", ylab="Attrition"
)$conf)

#out-value of the outliers
print("$out")
print(boxplot(mydata$YearsSinceLastPromotion~mydata$Attrition_label, col ="Red",
              horizontal = TRUE,
              main = "Boxplot of YearsSinceLastPromotionVs Attrition",
              xlab = "YearsSinceLastPromotion", ylab="Attrition"
)$out)

#group-a vector of the same length as out whose elements 
#indicate to which group the outlier belongs (e.g. 56  & 58)
print("$group")
print(boxplot(mydata$YearsSinceLastPromotion~mydata$Attrition_label, col ="Red",
              horizontal = TRUE,
              main = "Boxplot of YearsSinceLastPromotionVs Attrition",
              xlab = "YearsSinceLastPromotion", ylab="Attrition"
)$group)

#names-a vector of names for the groups
print("$names")
print(boxplot(mydata$YearsSinceLastPromotion~mydata$Attrition_label, col ="Red",
              horizontal = TRUE,
              main = "Boxplot of YearsSinceLastPromotionVs Attrition",
              xlab = "YearsSinceLastPromotion", ylab="Attrition"
)$names)

#---Test of Hypothesis - Logistic Regression -Which we will do in next step(seperate file)
#--- There is no such test of hypothesis for Q -> C but we can use gostic regression.


#34. YearsWithCurrManager Vs Attrition i.e. Quantitative data Vs Categorical Data
#---Summerizarion
print(paste("Range of Years With Current Manager is from",min(mydata$YearsWithCurrManager),"To",max(mydata$YearsWithCurrManager)))
#---Visualization
png(filename = "YearsWithCurrManagerVsAttrition.png",height=480, width= 960, unit ="px")
#---- Y- Attrition, X- YearsWithCurrManager---Horizontal side by side box plot
boxplot(mydata$YearsWithCurrManager~mydata$Attrition_label, col ="Red",
        horizontal = TRUE, 
        main = "Boxplot of Years With Current Manager Vs Attrition",
        xlab = "Years With Current Manager", ylab="Attrition"
) 
dev.off()

#stats-having the position of the lower/upper extremes 
#of the whiskers and box along with the median
print("$stats")
print(boxplot(mydata$YearsWithCurrManager~mydata$Attrition_label, col ="Red",
        horizontal = TRUE,
        main = "Boxplot of YearsWithCurrManagerVs Attrition",
        xlab = "YearsWithCurrManager", ylab="Attrition"
)$stats)

#n-the number of observations
print("$n")
print(boxplot(mydata$YearsWithCurrManager~mydata$Attrition_label, col ="Red",
              horizontal = TRUE,
              main = "Boxplot of YearsWithCurrManagerVs Attrition",
              xlab = "YearsWithCurrManager", ylab="Attrition"
)$n)

#conf-upper/lower extremes of the notch
print("$conf")
print(boxplot(mydata$YearsWithCurrManager~mydata$Attrition_label, col ="Red",
              horizontal = TRUE,
              main = "Boxplot of YearsWithCurrManagerVs Attrition",
              xlab = "YearsWithCurrManager", ylab="Attrition"
)$conf)

#out-value of the outliers
print("$out")
print(boxplot(mydata$YearsWithCurrManager~mydata$Attrition_label, col ="Red",
              horizontal = TRUE,
              main = "Boxplot of YearsWithCurrManagerVs Attrition",
              xlab = "YearsWithCurrManager", ylab="Attrition"
)$out)

#group-a vector of the same length as out whose elements 
#indicate to which group the outlier belongs (e.g. 56  & 58)
print("$group")
print(boxplot(mydata$YearsWithCurrManager~mydata$Attrition_label, col ="Red",
              horizontal = TRUE,
              main = "Boxplot of YearsWithCurrManager Vs Attrition",
              xlab = "YearsWithCurrManager", ylab="Attrition"
)$group)

#names-a vector of names for the groups
print("$names")
print(boxplot(mydata$YearsWithCurrManager~mydata$Attrition_label, col ="Red",
              horizontal = TRUE,
              main = "Boxplot of YearsWithCurrManagerVs Attrition",
              xlab = "YearsWithCurrManager", ylab="Attrition"
)$names)

#---Test of Hypothesis - Logistic Regression -Which we will do in next step(seperate file)
#--- There is no such test of hypothesis for Q -> C but we can use gostic regression.

print("Here we are done with EDA and TOH of all variables against Attrition,
      And we found that some variables like Education, Gender, PerformanceRating 
      and RelationshipSatisfaction are independent of Attrition. We also
      found that some variables like EmployeeCount, EmployeeNumber, Over18
      and StandardHours are not useful and not affecting the Attrition in 
      the Orgnization. So now we will continue EDA and TOH with Y as a Monthly Salary.")



setwd("C://Users//DELL//Documents//Attrition//_plots//Bivariate//Vs_MonthlyIncome")
#Here we need to check the all variables against Monthly Income.
#So 'Y' will be an MonthlyIncome and 'X' will be other variables.

############# EDA, summarization and Test of Hypothesis ###################


#1. Attrition (X)  Vs MonthlyIncome (Y) i.e. Categorical Data Vs Quantitative data (C -> Q) 
 
#---Summerizarion
print(paste("Summary of Monthly Income Vs Attrition is as follows"))
print(group_by(mydata,mydata$Attrition_label) %>%
  summarise(
    count = n(),
    mean = mean(mydata$MonthlyIncome, na.rm = TRUE),
    sd = sd(mydata$MonthlyIncome, na.rm = TRUE)
  ))

#---Visualization
png(filename = "AttritionVsMonthlyIncome.png",height=480, width= 960, unit ="px")
par(mfrow = c(1,2))
#---- Y- MonthlyIncome, X- Attrition 
#-----Side by side Box Plot
boxplot(mydata$MonthlyIncome~mydata$Attrition_label, 
        main = "Boxplot of Attrition Vs Monthly Income",
        xlab = "Attrition", ylab="Monthly Income",
        col = c(2,4))
plotmeans(mydata$MonthlyIncome~mydata$Attrition_label, data = mydata, frame = FALSE,
          xlab = "Attrition", ylab = "Monthly Income",
          main="Mean Plot with 95% CI",col = c(2,4)) 
dev.off()

print("Statistics of Monthly Income Vs Attrition")
print(boxplot(mydata$MonthlyIncome~mydata$Attrition_label, 
              main = "Boxplot of Attrition Vs Monthly Income",
              xlab = "Attrition", ylab="Monthly Income",
              col = c(2,4))$stats)

#---Test of Hypothesis
#A.----- 2 Independent Samples Test / 2 Sample t-test
#--------Ho: Monthly Income and Attrition are not significantly Different.
#--------Ha: Monthly Income and Attrition are significantly Different.
print("Test of Hypothesis (2 Independent Samples Test / 2 Sample t-test)")
print(t.test(mydata$MonthlyIncome~mydata$Attrition_label, conf.level = 0.95, alternate = "two.sided"))
print("Here we can see that P-Value is very less than 0.05, so we can Reject the Null Hypothesis.")
print("And we can say with 95% Confidence that Monthly Income and Attrition are significantly Different.")

#B.----- 2 Dependent Samples Test / Paired t-Test
#------Ho: No difference in mean
#------Ha: The two means are different
print("Test of Hypothesis (2 Dependent Samples Test / Paired t-test)")
print(pairwise.t.test(mydata$MonthlyIncome,mydata$Attrition_label, conf.level =0.95,
                p.adjust.method = "BH", alternative = c("two.sided", "less", "greater")
               )) #BH-Benjamini & Hochberg (1995)

print("Here we can see that P-Value is very less than 0.05, so we can Reject the Null Hypothesis.")
print("And we can say with 95% Confidence that The two means are different.")

#C.----- More than 2 Independent Samples /Anova F test
#--------Ho: The means of the different groups are the same.
#--------Ha: At least one sample mean is not equal to the others.
print("Compute one-way ANOVA test: ")
print(summary(aov(mydata$MonthlyIncome~mydata$Attrition_label)))
print("The output includes the columns F value and Pr(>F) corresponding to the p-value of the test")
print("As the P-Value is less than the significance level 0.05")
print("At least one sample mean is not equal to the others.")

#2. Age (X) Vs MonthlyIncome (Y) i.e. Quantitative Data Vs Quantitative data (Q->Q) 

#---Summerizarion
#----Corelation Coefficient 
print(paste("Corelation Coefficient of Monthly Income and Age is:",cor(mydata$MonthlyIncome,mydata$Age)))

#---Visualization
png(filename = "AgeVsMonthlyIncome.png",height=480, width= 960, unit ="px")
par(mfrow = c(1,2))
#----Scatter Plot
plot(mydata$Age,mydata$MonthlyIncome,
     main = "Scattered plot of Age Vs Monthly Income",
     xlab = "Age", ylab="Monthly Income",
     col= c(2,4))

#---Test of Hypothesis
#---- t Test for population slope
#---- y = beta0 + beta1*X  #line equarion beta0 is intercept and beta1 is slope
#---- Ho: Two Variables are independent i.e. beta1=0 (i.e. No Relationship)
#---- Ha: Two Variables are dependent i.e. beta1 <> 0 (i.e. There is Relationship)
#---- Fit a simple linear regression
print("Test of Hypothesis for MonthlyIncome (Y) Vs Age (X) is: ")
reg_age_mi <- lm(mydata$MonthlyIncome~mydata$Age, data =mydata)
coeff=coefficients(reg_age_mi)
# equation of the line : Y=Beta0+Beta1X 
eq1 = paste("Y = ", c(coeff[1]), "+", c(coeff[2]),"*X")

#----Statified Scatter plot
plot(mydata$Age,mydata$MonthlyIncome, col = mydata$Attrition_label,
     xlab = "Age - Red Triangles are for (Left) and Black circles are for (Not Left)", 
     ylab="Monthly Income",
     main = eq1, 
     pch = as.numeric(mydata$Attrition_label)+15)
abline(reg_age_mi, col="green",lwd=3)
dev.off()
print(summary(reg_age_mi))
print("After TOH we can clearly see that P-Value is less than 0.05")
print("Hence we can reject the Null Hypothesis and we say with 95% confidence")
print("That Monthly Income and Age are dependent on each other.")


#3. Business Travel (X)  Vs MonthlyIncome (Y) i.e. Categorical Data Vs Quantitative data (C -> Q) 

#---Summerizarion
print(paste("Summary of Monthly Income Vs Business Travel is as follows"))
print(group_by(mydata,mydata$BusinessTravel_label) %>%
        summarise(
          count = n(),
          mean = mean(mydata$MonthlyIncome, na.rm = TRUE),
          sd = sd(mydata$MonthlyIncome, na.rm = TRUE)
        ))

#---Visualization
png(filename = "BusinessTravelVsMonthlyIncome.png",height=480, width= 960, unit ="px")
par(mfrow = c(1,2))
#---- Y- MonthlyIncome, X- Business Travel 
#-----Side by side Box Plot
boxplot(mydata$MonthlyIncome~mydata$BusinessTravel_label, 
        main = "Boxplot of Business Travel Vs Monthly Income",
        xlab = "Business Travel", ylab="Monthly Income",
        col = c(1:3))
plotmeans(mydata$MonthlyIncome~mydata$BusinessTravel_label, data = mydata, frame = FALSE,
          xlab = "Business Travel", ylab = "Monthly Income",
          main="Mean Plot with 95% CI",col = c(1:3)) 
dev.off()

print("Statistics of Monthly Income Vs Business Travel")
print(boxplot(mydata$MonthlyIncome~mydata$BusinessTravel_label, 
              main = "Boxplot of Business Travel Vs Monthly Income",
              xlab = "Business Travel", ylab="Monthly Income",
              col = c(1:3))$stats)

#---Test of Hypothesis
#----- More than 2 Independent Samples /Anova F test
#--------Ho: The means of the different groups are the same.
#--------Ha: At least one sample mean is not equal to the others.
print("Compute one-way ANOVA test: ")
print(summary(aov(mydata$MonthlyIncome~mydata$BusinessTravel_label)))
print("The output includes the columns F value and Pr(>F) corresponding to the p-value of the test")
print("As the P-Value is greater than the significance level 0.05")
print("We can not reject Null Hypothesis, The means of the different groups are the same.")

#4. DailyRate (X) Vs MonthlyIncome (Y) i.e. Quantitative Data Vs Quantitative data (Q->Q) 

#---Summerizarion
#----Corelation Coefficient 
print(paste("Corelation Coefficient of Monthly Income and DailyRate is:",cor(mydata$DailyRate,mydata$MonthlyIncome)))

#---Visualization
png(filename = "DailyRateVsMonthlyIncome.png",height=480, width= 960, unit ="px")
par(mfrow = c(1,2))
#----Scatter Plot
plot(mydata$DailyRate, mydata$MonthlyIncome,
     main = "Scattered plot of DailyRate Vs Monthly Income",
     xlab = "DailyRate", ylab="Monthly Income",
     col= c(2,4))

#---Test of Hypothesis
#---- t Test for population slope
#---- y = beta0 + beta1*X  #line equarion beta0 is intercept and beta1 is slope
#---- Ho: Two Variables are independent i.e. beta1=0 (i.e. No Relationship)
#---- Ha: Two Variables are dependent i.e. beta1 <> 0 (i.e. There is Relationship)
#---- Fit a simple linear regression
print("Test of Hypothesis for MonthlyIncome (Y) Vs DailyRate (X) is: ")
reg_DailyRate_mi <- lm(mydata$MonthlyIncome~mydata$DailyRate, data =mydata)
coeff=coefficients(reg_DailyRate_mi)
# equation of the line : Y=Beta0+Beta1X 
eq1 = paste("Y = ", c(coeff[1]), "+", round(coeff[2],1),"*X")

#----Statified Scatter plot
plot(mydata$DailyRate,mydata$MonthlyIncome, col = mydata$Attrition_label,
     xlab = "DailyRate - Red Triangles are for (Left) and Black circles are for (Not Left)", 
     ylab="Monthly Income",
     main = eq1, 
     pch = as.numeric(mydata$Attrition_label)+15)
abline(reg_DailyRate_mi, col="green",lwd=3)
dev.off()

print(summary(reg_DailyRate_mi))
print("After TOH we can clearly see that P-Value is greater than 0.05")
print("Hence we can not reject the Null Hypothesis and we say with 95% confidence")
print("That Monthly Income and DailyRate are independent of each other.")

#5. Department (X)  Vs MonthlyIncome (Y) i.e. Categorical Data Vs Quantitative data (C -> Q) 
#---Summerizarion
print(paste("Summary of Monthly Income Vs Department is as follows"))
print(group_by(mydata,mydata$Department_label) %>%
         summarise(
          count = n(),
          mean = mean(mydata$MonthlyIncome, na.rm = TRUE),
          sd = sd(mydata$MonthlyIncome, na.rm = TRUE)
        ))

#---Visualization
png(filename = "DepartmentVsMonthlyIncome.png",height=480, width= 960, unit ="px")
par(mfrow = c(1,2))
#---- Y- MonthlyIncome, X- Department 
#-----Side by side Box Plot
boxplot(mydata$MonthlyIncome~mydata$Department_label, 
        main = "Boxplot of Department_label Vs Monthly Income",
        xlab = "Department", ylab="Monthly Income",
        col = c(1:5))
plotmeans(mydata$MonthlyIncome~mydata$Department_label, data = mydata, frame = FALSE,
          xlab = "Department", ylab = "Monthly Income",
          main="Mean Plot with 95% CI",col = c(1:5)) 
dev.off()

print("Statistics of Monthly Income Vs Department")
print(boxplot(mydata$MonthlyIncome~mydata$Department_label, 
              main = "Boxplot of Department Vs Monthly Income",
              xlab = "Department_label", ylab="Monthly Income",
              col = c(1:5))$stats)

#---Test of Hypothesis
#C.----- More than 2 Independent Samples /Anova F test
#--------Ho: The means of the different groups are the same.
#--------Ha: At least one sample mean is not equal to the others.
print("Compute one-way ANOVA test: ")
print(summary(aov(mydata$MonthlyIncome~mydata$Department_label)))
print("The output includes the columns F value and Pr(>F) corresponding to the p-value of the test")
print("As the P-Value is less than the significance level 0.05")
print("At least one sample mean is not equal to the others.")


#6. DistanceFromHome (X) Vs MonthlyIncome (Y) i.e. Quantitative Data Vs Quantitative data (Q->Q) 

#---Summerizarion
#----Corelation Coefficient 
print(paste("Corelation Coefficient of Monthly Income and Distance From Home is:",cor(mydata$MonthlyIncome,mydata$DistanceFromHome)))

#---Visualization
png(filename = "DistanceFromHomeVsMonthlyIncome.png",height=480, width= 960, unit ="px")
par(mfrow = c(1,2))
#----Scatter Plot
plot(mydata$DistanceFromHome,mydata$MonthlyIncome,
     main = "Scattered plot of DistanceFromHome Vs Monthly Income",
     xlab = "Distance From Home", ylab="Monthly Income",
     col= c(2,4))

#---Test of Hypothesis
#---- t Test for population slope
#---- y = beta0 + beta1*X  #line equarion beta0 is intercept and beta1 is slope
#---- Ho: Two Variables are independent i.e. beta1=0 (i.e. No Relationship)
#---- Ha: Two Variables are dependent i.e. beta1 <> 0 (i.e. There is Relationship)
#---- Fit a simple linear regression
print("Test of Hypothesis for MonthlyIncome (Y) Vs DistanceFromHome (X) is: ")
reg_DistanceFromHome_mi <- lm(mydata$MonthlyIncome~mydata$DistanceFromHome, data =mydata)
coeff=coefficients(reg_DistanceFromHome_mi)
# equation of the line : Y=Beta0+Beta1X 
eq1 = paste("Y = ", c(coeff[1]), "+", round(coeff[2],1),"*X")

#----Statified Scatter plot
plot(mydata$DistanceFromHome,mydata$MonthlyIncome, col = mydata$Attrition_label,
     xlab = "Distance From Home - Red Triangles are for (Left) and Black circles are for (Not Left)", 
     ylab="Monthly Income",
     main = eq1, 
     pch = as.numeric(mydata$Attrition_label)+15)
abline(reg_DistanceFromHome_mi, col="green",lwd=3)
dev.off()
print(summary(reg_DistanceFromHome_mi))
print("After TOH we can clearly see that P-Value is greater than 0.05")
print("Hence we can not reject the Null Hypothesis and we say with 95% confidence")
print("That Monthly Income and Distance From Home are independent of each other.")


#7. EducationField (X)  Vs MonthlyIncome (Y) i.e. Categorical Data Vs Quantitative data (C -> Q) 

#---Summerizarion
print(paste("Summary of Monthly Income Vs Education Field is as follows"))
print(group_by(mydata,mydata$EducationField_label) %>%
        summarise(
          count = n(),
          mean = mean(mydata$MonthlyIncome, na.rm = TRUE),
          sd = sd(mydata$MonthlyIncome, na.rm = TRUE)
        ))

#---Visualization
png(filename = "EducationFieldVsMonthlyIncome.png",height=480, width= 960, unit ="px")
par(mfrow = c(1,2))
#---- Y- MonthlyIncome, X- EducationField 
#-----Side by side Box Plot
boxplot(mydata$MonthlyIncome~mydata$EducationField_label, 
        main = "Boxplot of EducationField Vs Monthly Income",
        xlab = "Education Field", ylab="Monthly Income",
        col = c(1:6))
plotmeans(mydata$MonthlyIncome~mydata$EducationField_label, data = mydata, frame = FALSE,
          xlab = "Education Field", ylab = "Monthly Income",
          main="Mean Plot with 95% CI",col = c(1:6)) 
dev.off()

print("Statistics of Monthly Income Vs Education Field")
print(boxplot(mydata$MonthlyIncome~mydata$EducationField_label, 
              main = "Boxplot of Education Field Vs Monthly Income",
              xlab = "Education Field", ylab="Monthly Income",
              col = c(1:6))$stats)

#---Test of Hypothesis
#C.----- More than 2 Independent Samples /Anova F test
#--------Ho: The means of the different groups are the same.
#--------Ha: At least one sample mean is not equal to the others.
print("Compute one-way ANOVA test: ")
print(summary(aov(mydata$MonthlyIncome~mydata$EducationField_label)))
print("The output includes the columns F value and Pr(>F) corresponding to the p-value of the test")
print("As the P-Value is greater than the significance level 0.05")
print("The means of the different groups are the same.")


#8. EnvironmentSatisfaction (X)  Vs MonthlyIncome (Y) i.e. Categorical Data Vs Quantitative data (C -> Q) 

#---Summerizarion
print(paste("Summary of Monthly Income Vs Environment Satisfaction is as follows"))
print(group_by(mydata,mydata$EnvironmentSatisfaction) %>%
        summarise(
          count = n(),
          mean = mean(mydata$MonthlyIncome, na.rm = TRUE),
          sd = sd(mydata$MonthlyIncome, na.rm = TRUE)
        ))

#---Visualization
png(filename = "EnvironmentSatisfactionVsMonthlyIncome.png",height=480, width= 960, unit ="px")
par(mfrow = c(1,2))
#---- Y- MonthlyIncome, X- EnvironmentSatisfaction 
#-----Side by side Box Plot
boxplot(mydata$MonthlyIncome~mydata$EnvironmentSatisfaction, 
        main = "Boxplot of EnvironmentSatisfaction Vs Monthly Income",
        xlab = "Environment Satisfaction", ylab="Monthly Income",
        col = c(1:5))
plotmeans(mydata$MonthlyIncome~mydata$EnvironmentSatisfaction, data = mydata, frame = FALSE,
          xlab = "Environment Satisfaction", ylab = "Monthly Income",
          main="Mean Plot with 95% CI",col = c(1:5)) 
dev.off()

print("Statistics of Monthly Income Vs Environment Satisfaction")
print(boxplot(mydata$MonthlyIncome~mydata$EnvironmentSatisfaction, 
              main = "Boxplot of Environment Satisfaction Vs Monthly Income",
              xlab = "Environment Satisfaction", ylab="Monthly Income",
              col = c(1:5))$stats)

#---Test of Hypothesis
#C.----- More than 2 Independent Samples /Anova F test
#--------Ho: The means of the different groups are the same.
#--------Ha: At least one sample mean is not equal to the others.
print("Compute one-way ANOVA test: ")
print(summary(aov(mydata$MonthlyIncome~mydata$EnvironmentSatisfaction)))
print("The output includes the columns F value and Pr(>F) corresponding to the p-value of the test")
print("As the P-Value is less than the significance level 0.05")
print("At least one sample mean is not equal to the others.")

#9. HourlyRate (X) Vs MonthlyIncome (Y) i.e. Quantitative Data Vs Quantitative data (Q->Q) 

#---Summerizarion
#----Corelation Coefficient 
print(paste("Corelation Coefficient of Monthly Income and HourlyRate is:",cor(mydata$MonthlyIncome,mydata$HourlyRate)))

#---Visualization
png(filename = "HourlyRateVsMonthlyIncome.png",height=480, width= 960, unit ="px")
par(mfrow = c(1,2))
#----Scatter Plot
plot(mydata$HourlyRate,mydata$MonthlyIncome,
     main = "Scattered plot of HourlyRate Vs Monthly Income",
     xlab = "HourlyRate", ylab="Monthly Income",
     col= c(2,4))

#---Test of Hypothesis
#---- t Test for population slope
#---- y = beta0 + beta1*X  #line equarion beta0 is intercept and beta1 is slope
#---- Ho: Two Variables are independent i.e. beta1=0 (i.e. No Relationship)
#---- Ha: Two Variables are dependent i.e. beta1 <> 0 (i.e. There is Relationship)
#---- Fit a simple linear regression
print("Test of Hypothesis for MonthlyIncome (Y) Vs HourlyRate (X) is: ")
reg_HourlyRate_mi <- lm(mydata$MonthlyIncome~mydata$HourlyRate, data =mydata)
coeff=coefficients(reg_HourlyRate_mi)
# equation of the line : Y=Beta0+Beta1X 
eq1 = paste("Y = ", c(coeff[1]), "+", c(coeff[2]),"*X")

#----Statified Scatter plot
plot(mydata$HourlyRate, mydata$MonthlyIncome, col = mydata$Attrition_label,
     xlab = "HourlyRate - Red Triangles are for (Left) and Black circles are for (Not Left)", 
     ylab="Monthly Income",
     main = eq1, 
     pch = as.numeric(mydata$Attrition_label)+15)
abline(reg_HourlyRate_mi, col="green",lwd=3)
dev.off()
print(summary(reg_HourlyRate_mi))
print("After TOH we can clearly see that P-Value is less than 0.05")
print("Hence we can not reject the Null Hypothesis and we say with 95% confidence")
print("That Monthly Income and HourlyRate are independent of each other.")

#10. JobInvolvement (X)  Vs MonthlyIncome (Y) i.e. Categorical Data Vs Quantitative data (C -> Q) 

#---Summerizarion
print(paste("Summary of Monthly Income Vs Job Involvement is as follows"))
print(group_by(mydata,mydata$JobInvolvement) %>%
        summarise(
          count = n(),
          mean = mean(mydata$MonthlyIncome, na.rm = TRUE),
          sd = sd(mydata$MonthlyIncome, na.rm = TRUE)
        ))

#---Visualization
png(filename = "JobInvolvementVsMonthlyIncome.png",height=480, width= 960, unit ="px")
par(mfrow = c(1,2))
#---- Y- MonthlyIncome, X- JobInvolvement 
#-----Side by side Box Plot
boxplot(mydata$MonthlyIncome~mydata$JobInvolvement, 
        main = "Boxplot of JobInvolvement Vs Monthly Income",
        xlab = "Job Involvement", ylab="Monthly Income",
        col = c(1:5))
plotmeans(mydata$MonthlyIncome~mydata$JobInvolvement, data = mydata, frame = FALSE,
          xlab = "Job Involvement", ylab = "Monthly Income",
          main="Mean Plot with 95% CI",col = c(1:5)) 
dev.off()

print("Statistics of Monthly Income Vs Job Involvement")
print(boxplot(mydata$MonthlyIncome~mydata$JobInvolvement, 
              main = "Boxplot of Job Involvement Vs Monthly Income",
              xlab = "Job Involvement", ylab="Monthly Income",
              col = c(1:5))$stats)

#---Test of Hypothesis
#C.----- More than 2 Independent Samples /Anova F test
#--------Ho: The means of the different groups are the same.
#--------Ha: At least one sample mean is not equal to the others.
print("Compute one-way ANOVA test: ")
print(summary(aov(mydata$MonthlyIncome~mydata$JobInvolvement)))
print("The output includes the columns F value and Pr(>F) corresponding to the p-value of the test")
print("As the P-Value is greater than the significance level 0.05")
print("The means of the different groups are the same.")


#11. Job Level (X)  Vs MonthlyIncome (Y) i.e. Categorical Data Vs Quantitative data (C -> Q) 

#---Summerizarion
print(paste("Summary of Monthly Income Vs Job Level is as follows"))
print(group_by(mydata,mydata$JobLevel) %>%
        summarise(
          count = n(),
          mean = mean(mydata$MonthlyIncome, na.rm = TRUE),
          sd = sd(mydata$MonthlyIncome, na.rm = TRUE)
        ))

#---Visualization
png(filename = "JobLevelVsMonthlyIncome.png",height=480, width= 960, unit ="px")
par(mfrow = c(1,2))
#---- Y- MonthlyIncome, X- Job Level 
#-----Side by side Box Plot
boxplot(mydata$MonthlyIncome~mydata$JobLevel, 
        main = "Boxplot of Job Level Vs Monthly Income",
        xlab = "Job Level", ylab="Monthly Income",
        col = c(1:5))
plotmeans(mydata$MonthlyIncome~mydata$JobLevel, data = mydata, frame = FALSE,
          xlab = "Job Level", ylab = "Monthly Income",
          main="Mean Plot with 95% CI",col = c(1:5)) 
dev.off()

print("Statistics of Monthly Income Vs Job Level")
print(boxplot(mydata$MonthlyIncome~mydata$JobLevel, 
              main = "Boxplot of Job Level Vs Monthly Income",
              xlab = "Job Level", ylab="Monthly Income",
              col = c(1:5))$stats)

#---Test of Hypothesis
#----- More than 2 Independent Samples /Anova F test
#--------Ho: The means of the different groups are the same.
#--------Ha: At least one sample mean is not equal to the others.
print("Compute one-way ANOVA test: ")
print(summary(aov(mydata$MonthlyIncome~mydata$JobLevel)))
print("The output includes the columns F value and Pr(>F) corresponding to the p-value of the test")
print("As the P-Value is less than the significance level 0.05")
print("We can reject Null Hypothesis, At least one sample mean is not equal to the others.")


#12. Job Role (X)  Vs MonthlyIncome (Y) i.e. Categorical Data Vs Quantitative data (C -> Q) 

#---Summerizarion
print(paste("Summary of Monthly Income Vs Job Role is as follows"))
print(group_by(mydata,mydata$JobRole_label) %>%
        summarise(
          count = n(),
          mean = mean(mydata$MonthlyIncome, na.rm = TRUE),
          sd = sd(mydata$MonthlyIncome, na.rm = TRUE)
        ))

#---Visualization
png(filename = "JobRoleVsMonthlyIncome.png",height=480, width= 960, unit ="px")
par(mfrow = c(1,2))
#---- Y- MonthlyIncome, X- Job Role 
#-----Side by side Box Plot
boxplot(mydata$MonthlyIncome~mydata$JobRole_label, 
        main = "Boxplot of Job Role Vs Monthly Income",
        xlab = "Job Role", ylab="Monthly Income",
        col = c(1:8))
plotmeans(mydata$MonthlyIncome~mydata$JobRole_label, data = mydata, frame = FALSE,
          xlab = "Job Role", ylab = "Monthly Income",
          main="Mean Plot with 95% CI",col = c(1:8)) 
dev.off()

print("Statistics of Monthly Income Vs Job Role")
print(boxplot(mydata$MonthlyIncome~mydata$JobRole_label, 
              main = "Boxplot of Job Role Vs Monthly Income",
              xlab = "Job Role", ylab="Monthly Income",
              col = c(1:8))$stats)

#---Test of Hypothesis
#----- More than 2 Independent Samples /Anova F test
#--------Ho: The means of the different groups are the same.
#--------Ha: At least one sample mean is not equal to the others.
print("Compute one-way ANOVA test: ")
print(summary(aov(mydata$MonthlyIncome~mydata$JobRole_label)))
print("The output includes the columns F value and Pr(>F) corresponding to the p-value of the test")
print("As the P-Value is less than the significance level 0.05")
print("We can reject Null Hypothesis, At least one sample mean is not equal to the others.")

#13. Job Satisfaction (X)  Vs MonthlyIncome (Y) i.e. Categorical Data Vs Quantitative data (C -> Q) 

#---Summerizarion
print(paste("Summary of Monthly Income Vs Job Satisfaction is as follows"))
print(group_by(mydata,mydata$JobSatisfaction) %>%
        summarise(
          count = n(),
          mean = mean(mydata$MonthlyIncome, na.rm = TRUE),
          sd = sd(mydata$MonthlyIncome, na.rm = TRUE)
        ))

#---Visualization
png(filename = "JobSatisfactionVsMonthlyIncome.png",height=480, width= 960, unit ="px")
par(mfrow = c(1,2))
#---- Y- MonthlyIncome, X- Job Satisfaction 
#-----Side by side Box Plot
boxplot(mydata$MonthlyIncome~mydata$JobSatisfaction, 
        main = "Boxplot of Job Satisfaction Vs Monthly Income",
        xlab = "Job Satisfaction", ylab="Monthly Income",
        col = c(1:8))
plotmeans(mydata$MonthlyIncome~mydata$JobSatisfaction, data = mydata, frame = FALSE,
          xlab = "Job Satisfaction", ylab = "Monthly Income",
          main="Mean Plot with 95% CI",col = c(1:8)) 
dev.off()

print("Statistics of Monthly Income Vs Job Satisfaction")
print(boxplot(mydata$MonthlyIncome~mydata$JobSatisfaction, 
              main = "Boxplot of Job Satisfaction Vs Monthly Income",
              xlab = "Job Satisfaction", ylab="Monthly Income",
              col = c(1:8))$stats)

#---Test of Hypothesis
#----- More than 2 Independent Samples /Anova F test
#--------Ho: The means of the different groups are the same.
#--------Ha: At least one sample mean is not equal to the others.
print("Compute one-way ANOVA test: ")
print(summary(aov(mydata$MonthlyIncome~mydata$JobSatisfaction)))
print("The output includes the columns F value and Pr(>F) corresponding to the p-value of the test")
print("As the P-Value is greater than the significance level 0.05")
print("We can not reject Null Hypothesis, The means of the different groups are the same.")

#14. Marital Status (X)  Vs MonthlyIncome (Y) i.e. Categorical Data Vs Quantitative data (C -> Q) 

#---Summerizarion
print(paste("Summary of Monthly Income Vs Marital Status is as follows"))
print(group_by(mydata,mydata$MaritalStatus) %>%
        summarise(
          count = n(),
          mean = mean(mydata$MonthlyIncome, na.rm = TRUE),
          sd = sd(mydata$MonthlyIncome, na.rm = TRUE)
        ))

#---Visualization
png(filename = "MaritalStatusVsMonthlyIncome.png",height=480, width= 960, unit ="px")
par(mfrow = c(1,2))
#---- Y- MonthlyIncome, X- Marital Status 
#-----Side by side Box Plot
boxplot(mydata$MonthlyIncome~mydata$MaritalStatus, 
        main = "Boxplot of Marital Status Vs Monthly Income",
        xlab = "Marital Status", ylab="Monthly Income",
        col = c(1:8))
plotmeans(mydata$MonthlyIncome~mydata$MaritalStatus, data = mydata, frame = FALSE,
          xlab = "Marital Status", ylab = "Monthly Income",
          main="Mean Plot with 95% CI",col = c(1:8)) 
dev.off()

print("Statistics of Monthly Income Vs Marital Status")
print(boxplot(mydata$MonthlyIncome~mydata$MaritalStatus, 
              main = "Boxplot of Marital Status Vs Monthly Income",
              xlab = "Marital Status", ylab="Monthly Income",
              col = c(1:8))$stats)

#---Test of Hypothesis
#----- More than 2 Independent Samples /Anova F test
#--------Ho: The means of the different groups are the same.
#--------Ha: At least one sample mean is not equal to the others.
print("Compute one-way ANOVA test: ")
print(summary(aov(mydata$MonthlyIncome~mydata$MaritalStatus)))
print("The output includes the columns F value and Pr(>F) corresponding to the p-value of the test")
print("As the P-Value is less than the significance level 0.05")
print("We can reject Null Hypothesis, At least one sample mean is not equal to the others.")

#15. MonthlyRate (X) Vs MonthlyIncome (Y) i.e. Quantitative Data Vs Quantitative data (Q->Q) 

#---Summerizarion
#----Corelation Coefficient 
print(paste("Corelation Coefficient of Monthly Income and MonthlyRate is:",cor(mydata$MonthlyIncome,mydata$MonthlyRate)))

#---Visualization
png(filename = "MonthlyRateVsMonthlyIncome.png",height=480, width= 960, unit ="px")
par(mfrow = c(1,2))
#----Scatter Plot
plot(mydata$MonthlyRate,mydata$MonthlyIncome,
     main = "Scattered plot of MonthlyRate Vs Monthly Income",
     xlab = "MonthlyRate", ylab="Monthly Income",
     col= c(2,4))

#---Test of Hypothesis
#---- t Test for population slope
#---- y = beta0 + beta1*X  #line equarion beta0 is intercept and beta1 is slope
#---- Ho: Two Variables are independent i.e. beta1=0 (i.e. No Relationship)
#---- Ha: Two Variables are dependent i.e. beta1 <> 0 (i.e. There is Relationship)
#---- Fit a simple linear regression
print("Test of Hypothesis for MonthlyIncome (Y) Vs MonthlyRate (X) is: ")
reg_MonthlyRate_mi <- lm(mydata$MonthlyIncome~mydata$MonthlyRate, data =mydata)
coeff=coefficients(reg_MonthlyRate_mi)
# equation of the line : Y=Beta0+Beta1X 
eq1 = paste("Y = ", c(coeff[1]), "+", c(coeff[2]),"*X")

#----Statified Scatter plot
plot(mydata$MonthlyRate, mydata$MonthlyIncome, col = mydata$Attrition_label,
     xlab = "MonthlyRate - Red Triangles are for (Left) and Black circles are for (Not Left)", 
     ylab="Monthly Income",
     main = eq1, 
     pch = as.numeric(mydata$Attrition_label)+15)
abline(reg_MonthlyRate_mi, col="green",lwd=3)
dev.off()
print(summary(reg_MonthlyRate_mi))
print("After TOH we can clearly see that P-Value is greater than 0.05")
print("Hence we can not reject the Null Hypothesis and we say with 95% confidence")
print("That Monthly Income and MonthlyRate are independent of each other.")

#16. NumCompaniesWorked (X) Vs MonthlyIncome (Y) i.e. Quantitative Data Vs Quantitative data (Q->Q) 

#---Summerizarion
#----Corelation Coefficient 
print(paste("Corelation Coefficient of Monthly Income and No. of Companies Worked is:",cor(mydata$MonthlyIncome,mydata$NumCompaniesWorked)))

#---Visualization
png(filename = "NumCompaniesWorkedVsMonthlyIncome.png",height=480, width= 960, unit ="px")
par(mfrow = c(1,2))
#----Scatter Plot
plot(mydata$NumCompaniesWorked,mydata$MonthlyIncome,
     main = "Scattered plot of NumCompaniesWorked Vs Monthly Income",
     xlab = "No. of Companies Worked", ylab="Monthly Income",
     col= c(2,4))

#---Test of Hypothesis
#---- t Test for population slope
#---- y = beta0 + beta1*X  #line equarion beta0 is intercept and beta1 is slope
#---- Ho: Two Variables are independent i.e. beta1=0 (i.e. No Relationship)
#---- Ha: Two Variables are dependent i.e. beta1 <> 0 (i.e. There is Relationship)
#---- Fit a simple linear regression
print("Test of Hypothesis for MonthlyIncome (Y) Vs NumCompaniesWorked (X) is: ")
reg_NumCompaniesWorked_mi <- lm(mydata$MonthlyIncome~mydata$NumCompaniesWorked, data =mydata)
coeff=coefficients(reg_NumCompaniesWorked_mi)
# equation of the line : Y=Beta0+Beta1X 
eq1 = paste("Y = ", c(coeff[1]), "+", c(coeff[2]),"*X")

#----Statified Scatter plot
plot(mydata$NumCompaniesWorked,mydata$MonthlyIncome, col = mydata$Attrition_label,
     xlab = "No. of Companies Worked - Red are for (Left) and Black are for (Not Left)", 
     ylab="Monthly Income",
     main = eq1, 
     pch = as.numeric(mydata$Attrition_label)+15)
abline(reg_NumCompaniesWorked_mi, col="green",lwd=3)
dev.off()
print(summary(reg_NumCompaniesWorked_mi))
print("After TOH we can clearly see that P-Value is less than 0.05")
print("Hence we can reject the Null Hypothesis and we say with 95% confidence")
print("That Monthly Income and NumCompaniesWorked are dependent on each other.")


#17. Over Time (X)  Vs MonthlyIncome (Y) i.e. Categorical Data Vs Quantitative data (C -> Q) 

#---Summerizarion
print(paste("Summary of Monthly Income Vs Over Time is as follows"))
print(group_by(mydata,mydata$OverTime) %>%
        summarise(
          count = n(),
          mean = mean(mydata$MonthlyIncome, na.rm = TRUE),
          sd = sd(mydata$MonthlyIncome, na.rm = TRUE)
        ))

#---Visualization
png(filename = "Over TimeVsMonthlyIncome.png",height=480, width= 960, unit ="px")
par(mfrow = c(1,2))
#---- Y- MonthlyIncome, X- Over Time 
#-----Side by side Box Plot
boxplot(mydata$MonthlyIncome~mydata$OverTime, 
        main = "Boxplot of Over Time Vs Monthly Income",
        xlab = "Over Time", ylab="Monthly Income",
        col = c(2,4))
plotmeans(mydata$MonthlyIncome~mydata$OverTime, data = mydata, frame = FALSE,
          xlab = "Over Time", ylab = "Monthly Income",
          main="Mean Plot with 95% CI",col = c(2,4)) 
dev.off()

print("Statistics of Monthly Income Vs Over Time")
print(boxplot(mydata$MonthlyIncome~mydata$OverTime, 
              main = "Boxplot of Over Time Vs Monthly Income",
              xlab = "Over Time", ylab="Monthly Income",
              col = c(2,4))$stats)

#---Test of Hypothesis
#A.----- 2 Independent Samples Test / 2 Sample t-test
#--------Ho: Monthly Income and Over Time are not significantly Different.
#--------Ha: Monthly Income and Over Time are significantly Different.
print("Test of Hypothesis (2 Independent Samples Test / 2 Sample t-test)")
print(t.test(mydata$MonthlyIncome~mydata$OverTime, conf.level = 0.95, alternate = "two.sided"))
print("Here we can see that P-Value is greater than 0.05, so we can not Reject the Null Hypothesis.")
print("And we can say with 95% Confidence that Monthly Income and Over Time are not significantly Different.")

#B.----- 2 Dependent Samples Test / Paired t-Test
#------Ho: No difference in mean
#------Ha: The two means are different
print("Test of Hypothesis (2 Dependent Samples Test / Paired t-test)")
print(pairwise.t.test(mydata$MonthlyIncome,mydata$OverTime, conf.level =0.95,
                      p.adjust.method = "BH", alternative = c("two.sided", "less", "greater")
)) #BH-Benjamini & Hochberg (1995)
print("As the P-Value is greater than the significance level 0.05")
print("Here we can see that there is No difference in mean.")

#C.----- More than 2 Independent Samples /Anova F test
#--------Ho: The means of the different groups are the same.
#--------Ha: At least one sample mean is not equal to the others.
print("Compute one-way ANOVA test: ")
print(summary(aov(mydata$MonthlyIncome~mydata$OverTime)))
print("The output includes the columns F value and Pr(>F) corresponding to the p-value of the test")
print("As the P-Value is greater than the significance level 0.05")
print("The means of the different groups are the same.")


#18. PercentSalaryHike (X) Vs MonthlyIncome (Y) i.e. Quantitative Data Vs Quantitative data (Q->Q) 

#---Summerizarion
#----Corelation Coefficient 
print(paste("Corelation Coefficient of Monthly Income and Percent Salary Hike is:",cor(mydata$MonthlyIncome,mydata$PercentSalaryHike)))

#---Visualization
png(filename = "PercentSalaryHikeVsMonthlyIncome.png",height=480, width= 960, unit ="px")
par(mfrow = c(1,2))
#----Scatter Plot
plot(mydata$PercentSalaryHike,mydata$MonthlyIncome,
     main = "Scattered plot of PercentSalaryHike Vs Monthly Income",
     xlab = "Percent Salary Hike", ylab="Monthly Income",
     col= c(2,4))

#---Test of Hypothesis
#---- t Test for population slope
#---- y = beta0 + beta1*X  #line equarion beta0 is intercept and beta1 is slope
#---- Ho: Two Variables are independent i.e. beta1=0 (i.e. No Relationship)
#---- Ha: Two Variables are dependent i.e. beta1 <> 0 (i.e. There is Relationship)
#---- Fit a simple linear regression
print("Test of Hypothesis for MonthlyIncome (Y) Vs PercentSalaryHike (X) is: ")
reg_PercentSalaryHike_mi <- lm(mydata$MonthlyIncome~mydata$PercentSalaryHike, data =mydata)
coeff=coefficients(reg_PercentSalaryHike_mi)
# equation of the line : Y=Beta0+Beta1X 
eq1 = paste("Y = ", c(coeff[1]), "+", c(coeff[2]),"*X")

#----Statified Scatter plot
plot(mydata$PercentSalaryHike,mydata$MonthlyIncome, col = mydata$Attrition_label,
     xlab = "Percent Salary Hike - Red are for (Left) and Black are for (Not Left)", 
     ylab="Monthly Income",
     main = eq1, 
     pch = as.numeric(mydata$Attrition_label)+15)
abline(reg_PercentSalaryHike_mi, col="green",lwd=3)
dev.off()
print(summary(reg_PercentSalaryHike_mi))
print("After TOH we can clearly see that P-Value is greater than 0.05")
print("Hence we can not reject the Null Hypothesis and we say with 95% confidence")
print("That Monthly Income and PercentSalaryHike are independent of each other.")


#19. Stock Option Level (X)  Vs MonthlyIncome (Y) i.e. Categorical Data Vs Quantitative data (C -> Q) 

#---Summerizarion
print(paste("Summary of Monthly Income Vs Stock Option Level is as follows"))
print(group_by(mydata,mydata$StockOptionLevel) %>%
        summarise(
          count = n(),
          mean = mean(mydata$MonthlyIncome, na.rm = TRUE),
          sd = sd(mydata$MonthlyIncome, na.rm = TRUE)
        ))

#---Visualization
png(filename = "StockOptionLevelVsMonthlyIncome.png",height=480, width= 960, unit ="px")
par(mfrow = c(1,2))
#---- Y- MonthlyIncome, X- Stock Option Level 
#-----Side by side Box Plot
boxplot(mydata$MonthlyIncome~mydata$StockOptionLevel, 
        main = "Boxplot of Stock Option Level Vs Monthly Income",
        xlab = "Stock Option Level", ylab="Monthly Income",
        col = c(1:4))
plotmeans(mydata$MonthlyIncome~mydata$StockOptionLevel, data = mydata, frame = FALSE,
          xlab = "Stock Option Level", ylab = "Monthly Income",
          main="Mean Plot with 95% CI",col = c(1:4)) 
dev.off()

print("Statistics of Monthly Income Vs Stock Option Level")
print(boxplot(mydata$MonthlyIncome~mydata$StockOptionLevel, 
              main = "Boxplot of Stock Option Level Vs Monthly Income",
              xlab = "Stock Option Level", ylab="Monthly Income",
              col = c(1:4))$stats)

#---Test of Hypothesis
#C.----- More than 2 Independent Samples /Anova F test
#--------Ho: The means of the different groups are the same.
#--------Ha: At least one sample mean is not equal to the others.
print("Compute one-way ANOVA test: ")
print(summary(aov(mydata$MonthlyIncome~mydata$StockOptionLevel)))
print("The output includes the columns F value and Pr(>F) corresponding to the p-value of the test")
print("As the P-Value is less than the significance level 0.05")
print("At least one sample mean is not equal to the others.")


#20. TotalWorkingYears (X) Vs MonthlyIncome (Y) i.e. Quantitative Data Vs Quantitative data (Q->Q) 

#---Summerizarion
#----Corelation Coefficient 
print(paste("Corelation Coefficient of Monthly Income and Total Working Years is:",cor(mydata$MonthlyIncome,mydata$TotalWorkingYears)))

#---Visualization
png(filename = "TotalWorkingYearsVsMonthlyIncome.png",height=480, width= 960, unit ="px")
par(mfrow = c(1,2))
#----Scatter Plot
plot(mydata$TotalWorkingYears,mydata$MonthlyIncome,
     main = "Scattered plot of TotalWorkingYears Vs Monthly Income",
     xlab = "Total Working Years", ylab="Monthly Income",
     col= c(2,4))

#---Test of Hypothesis
#---- t Test for population slope
#---- y = beta0 + beta1*X  #line equarion beta0 is intercept and beta1 is slope
#---- Ho: Two Variables are independent i.e. beta1=0 (i.e. No Relationship)
#---- Ha: Two Variables are dependent i.e. beta1 <> 0 (i.e. There is Relationship)
#---- Fit a simple linear regression
print("Test of Hypothesis for MonthlyIncome (Y) Vs TotalWorkingYears (X) is: ")
reg_TotalWorkingYears_mi <- lm(mydata$MonthlyIncome~mydata$TotalWorkingYears, data =mydata)
coeff=coefficients(reg_TotalWorkingYears_mi)
# equation of the line : Y=Beta0+Beta1X 
eq1 = paste("Y = ", c(coeff[1]), "+", c(coeff[2]),"*X")

#----Statified Scatter plot
plot(mydata$TotalWorkingYears,mydata$MonthlyIncome, col = mydata$Attrition_label,
     xlab = "Total Working Years - Red are for (Left) and Black are for (Not Left)", 
     ylab="Monthly Income",
     main = eq1, 
     pch = as.numeric(mydata$Attrition_label)+15)
abline(reg_TotalWorkingYears_mi, col="green",lwd=3)
dev.off()
print(summary(reg_TotalWorkingYears_mi))
print("After TOH we can clearly see that P-Value is less than 0.05")
print("Hence we can reject the Null Hypothesis and we say with 95% confidence")
print("That Monthly Income and TotalWorkingYears are dependent on each other.")


#21. TrainingTimesLastYear (X) Vs MonthlyIncome (Y) i.e. Quantitative Data Vs Quantitative data (Q->Q) 

#---Summerizarion
#----Corelation Coefficient 
print(paste("Corelation Coefficient of Monthly Income and Training Times Last Year is:",cor(mydata$MonthlyIncome,mydata$TrainingTimesLastYear)))

#---Visualization
png(filename = "TrainingTimesLastYearVsMonthlyIncome.png",height=480, width= 960, unit ="px")
par(mfrow = c(1,2))
#----Scatter Plot
plot(mydata$TrainingTimesLastYear,mydata$MonthlyIncome,
     main = "Scattered plot of TrainingTimesLastYear Vs Monthly Income",
     xlab = "Training Times Last Year", ylab="Monthly Income",
     col= c(2,4))

#---Test of Hypothesis
#---- t Test for population slope
#---- y = beta0 + beta1*X  #line equarion beta0 is intercept and beta1 is slope
#---- Ho: Two Variables are independent i.e. beta1=0 (i.e. No Relationship)
#---- Ha: Two Variables are dependent i.e. beta1 <> 0 (i.e. There is Relationship)
#---- Fit a simple linear regression
print("Test of Hypothesis for MonthlyIncome (Y) Vs TrainingTimesLastYear (X) is: ")
reg_TrainingTimesLastYear_mi <- lm(mydata$MonthlyIncome~mydata$TrainingTimesLastYear, data =mydata)
coeff=coefficients(reg_TrainingTimesLastYear_mi)
# equation of the line : Y=Beta0+Beta1X 
eq1 = paste("Y = ", c(coeff[1]), "+", c(coeff[2]),"*X")

#----Statified Scatter plot
plot(mydata$TrainingTimesLastYear,mydata$MonthlyIncome, col = mydata$Attrition_label,
     xlab = "Training Times Last Year - Red are for (Left) and Black are for (Not Left)", 
     ylab="Monthly Income",
     main = eq1, 
     pch = as.numeric(mydata$Attrition_label)+15)
abline(reg_TrainingTimesLastYear_mi, col="green",lwd=3)
dev.off()
print(summary(reg_TrainingTimesLastYear_mi))
print("After TOH we can clearly see that P-Value is greater than 0.05")
print("Hence we can not reject the Null Hypothesis and we say with 95% confidence")
print("That Monthly Income and TrainingTimesLastYear are independent of each other.")


#22. Work Life Balance (X)  Vs MonthlyIncome (Y) i.e. Categorical Data Vs Quantitative data (C -> Q) 

#---Summerizarion
print(paste("Summary of Monthly Income Vs Work Life Balance is as follows"))
print(group_by(mydata,mydata$WorkLifeBalance) %>%
        summarise(
          count = n(),
          mean = mean(mydata$MonthlyIncome, na.rm = TRUE),
          sd = sd(mydata$MonthlyIncome, na.rm = TRUE)
        ))

#---Visualization
png(filename = "Work Life BalanceVsMonthlyIncome.png",height=480, width= 960, unit ="px")
par(mfrow = c(1,2))
#---- Y- MonthlyIncome, X- Work Life Balance 
#-----Side by side Box Plot
boxplot(mydata$MonthlyIncome~mydata$WorkLifeBalance, 
        main = "Boxplot of Work Life Balance Vs Monthly Income",
        xlab = "Work Life Balance", ylab="Monthly Income",
        col = c(1:4))
plotmeans(mydata$MonthlyIncome~mydata$WorkLifeBalance, data = mydata, frame = FALSE,
          xlab = "Work Life Balance", ylab = "Monthly Income",
          main="Mean Plot with 95% CI",col = c(1:4)) 
dev.off()

print("Statistics of Monthly Income Vs Work Life Balance")
print(boxplot(mydata$MonthlyIncome~mydata$WorkLifeBalance, 
              main = "Boxplot of Work Life Balance Vs Monthly Income",
              xlab = "Work Life Balance", ylab="Monthly Income",
              col = c(1:4))$stats)

#---Test of Hypothesis
#C.----- More than 2 Independent Samples /Anova F test
#--------Ho: The means of the different groups are the same.
#--------Ha: At least one sample mean is not equal to the others.
print("Compute one-way ANOVA test: ")
print(summary(aov(mydata$MonthlyIncome~mydata$WorkLifeBalance)))
print("The output includes the columns F value and Pr(>F) corresponding to the p-value of the test")
print("As the P-Value is greater than the significance level 0.05")
print("The means of the different groups are the same.")

#23. YearsAtCompany (X) Vs MonthlyIncome (Y) i.e. Quantitative Data Vs Quantitative data (Q->Q) 

#---Summerizarion
#----Corelation Coefficient 
print(paste("Corelation Coefficient of Monthly Income and Years At Company is:",cor(mydata$MonthlyIncome,mydata$YearsAtCompany)))

#---Visualization
png(filename = "YearsAtCompanyVsMonthlyIncome.png",height=480, width= 960, unit ="px")
par(mfrow = c(1,2))
#----Scatter Plot
plot(mydata$YearsAtCompany,mydata$MonthlyIncome,
     main = "Scattered plot of YearsAtCompany Vs Monthly Income",
     xlab = "Years At Company", ylab="Monthly Income",
     col= c(2,4))

#---Test of Hypothesis
#---- t Test for population slope
#---- y = beta0 + beta1*X  #line equarion beta0 is intercept and beta1 is slope
#---- Ho: Two Variables are independent i.e. beta1=0 (i.e. No Relationship)
#---- Ha: Two Variables are dependent i.e. beta1 <> 0 (i.e. There is Relationship)
#---- Fit a simple linear regression
print("Test of Hypothesis for MonthlyIncome (Y) Vs YearsAtCompany (X) is: ")
reg_YearsAtCompany_mi <- lm(mydata$MonthlyIncome~mydata$YearsAtCompany, data =mydata)
coeff=coefficients(reg_YearsAtCompany_mi)
# equation of the line : Y=Beta0+Beta1X 
eq1 = paste("Y = ", c(coeff[1]), "+", c(coeff[2]),"*X")

#----Statified Scatter plot
plot(mydata$YearsAtCompany,mydata$MonthlyIncome, col = mydata$Attrition_label,
     xlab = "Years At Company - Red are for (Left) and Black are for (Not Left)", 
     ylab="Monthly Income",
     main = eq1, 
     pch = as.numeric(mydata$Attrition_label)+15)
abline(reg_YearsAtCompany_mi, col="green",lwd=3)
dev.off()
print(summary(reg_YearsAtCompany_mi))
print("After TOH we can clearly see that P-Value is less than 0.05")
print("Hence we can reject the Null Hypothesis and we say with 95% confidence")
print("That Monthly Income and YearsAtCompany are dependent on each other.")


#24. YearsInCurrentRole (X) Vs MonthlyIncome (Y) i.e. Quantitative Data Vs Quantitative data (Q->Q) 

#---Summerizarion
#----Corelation Coefficient 
print(paste("Corelation Coefficient of Monthly Income and Years In Current Role is:",cor(mydata$MonthlyIncome,mydata$YearsInCurrentRole)))

#---Visualization
png(filename = "YearsInCurrentRoleVsMonthlyIncome.png",height=480, width= 960, unit ="px")
par(mfrow = c(1,2))
#----Scatter Plot
plot(mydata$YearsInCurrentRole,mydata$MonthlyIncome,
     main = "Scattered plot of YearsInCurrentRole Vs Monthly Income",
     xlab = "Years In Current Role", ylab="Monthly Income",
     col= c(2,4))

#---Test of Hypothesis
#---- t Test for population slope
#---- y = beta0 + beta1*X  #line equarion beta0 is intercept and beta1 is slope
#---- Ho: Two Variables are independent i.e. beta1=0 (i.e. No Relationship)
#---- Ha: Two Variables are dependent i.e. beta1 <> 0 (i.e. There is Relationship)
#---- Fit a simple linear regression
print("Test of Hypothesis for MonthlyIncome (Y) Vs YearsInCurrentRole (X) is: ")
reg_YearsInCurrentRole_mi <- lm(mydata$MonthlyIncome~mydata$YearsInCurrentRole, data =mydata)
coeff=coefficients(reg_YearsInCurrentRole_mi)
# equation of the line : Y=Beta0+Beta1X 
eq1 = paste("Y = ", c(coeff[1]), "+", c(coeff[2]),"*X")

#----Statified Scatter plot
plot(mydata$YearsInCurrentRole,mydata$MonthlyIncome, col = mydata$Attrition_label,
     xlab = "Years In Current Role - Red are for (Left) and Black are for (Not Left)", 
     ylab="Monthly Income",
     main = eq1, 
     pch = as.numeric(mydata$Attrition_label)+15)
abline(reg_YearsInCurrentRole_mi, col="green",lwd=3)
dev.off()
print(summary(reg_YearsInCurrentRole_mi))
print("After TOH we can clearly see that P-Value is less than 0.05")
print("Hence we can reject the Null Hypothesis and we say with 95% confidence")
print("That Monthly Income and YearsInCurrentRole are dependent on each other.")

#25. YearsSinceLastPromotion (X) Vs MonthlyIncome (Y) i.e. Quantitative Data Vs Quantitative data (Q->Q) 

#---Summerizarion
#----Corelation Coefficient 
print(paste("Corelation Coefficient of Monthly Income and Years Since Last Promotion is:",cor(mydata$MonthlyIncome,mydata$YearsSinceLastPromotion)))

#---Visualization
png(filename = "YearsSinceLastPromotionVsMonthlyIncome.png",height=480, width= 960, unit ="px")
par(mfrow = c(1,2))
#----Scatter Plot
plot(mydata$YearsSinceLastPromotion,mydata$MonthlyIncome,
     main = "Scattered plot of YearsSinceLastPromotion Vs Monthly Income",
     xlab = "Years Since Last Promotion", ylab="Monthly Income",
     col= c(2,4))

#---Test of Hypothesis
#---- t Test for population slope
#---- y = beta0 + beta1*X  #line equarion beta0 is intercept and beta1 is slope
#---- Ho: Two Variables are independent i.e. beta1=0 (i.e. No Relationship)
#---- Ha: Two Variables are dependent i.e. beta1 <> 0 (i.e. There is Relationship)
#---- Fit a simple linear regression
print("Test of Hypothesis for MonthlyIncome (Y) Vs YearsSinceLastPromotion (X) is: ")
reg_YearsSinceLastPromotion_mi <- lm(mydata$MonthlyIncome~mydata$YearsSinceLastPromotion, data =mydata)
coeff=coefficients(reg_YearsSinceLastPromotion_mi)
# equation of the line : Y=Beta0+Beta1X 
eq1 = paste("Y = ", c(coeff[1]), "+", c(coeff[2]),"*X")

#----Statified Scatter plot
plot(mydata$YearsSinceLastPromotion,mydata$MonthlyIncome, col = mydata$Attrition_label,
     xlab = "Years Since Last Promotion - Red are for (Left) and Black are for (Not Left)", 
     ylab="Monthly Income",
     main = eq1, 
     pch = as.numeric(mydata$Attrition_label)+15)
abline(reg_YearsSinceLastPromotion_mi, col="green",lwd=3)
dev.off()
print(summary(reg_YearsSinceLastPromotion_mi))
print("After TOH we can clearly see that P-Value is less than 0.05")
print("Hence we can reject the Null Hypothesis and we say with 95% confidence")
print("That Monthly Income and YearsSinceLastPromotion are dependent on each other.")

#26. YearsWithCurrManager (X) Vs MonthlyIncome (Y) i.e. Quantitative Data Vs Quantitative data (Q->Q) 

#---Summerizarion
#----Corelation Coefficient 
print(paste("Corelation Coefficient of Monthly Income and Years With Current Manager is:",cor(mydata$MonthlyIncome,mydata$YearsWithCurrManager)))

#---Visualization
png(filename = "YearsWithCurrManagerVsMonthlyIncome.png",height=480, width= 960, unit ="px")
par(mfrow = c(1,2))
#----Scatter Plot
plot(mydata$YearsWithCurrManager,mydata$MonthlyIncome,
     main = "Scattered plot of YearsWithCurrManager Vs Monthly Income",
     xlab = "Years With Current Manager", ylab="Monthly Income",
     col= c(2,4))

#---Test of Hypothesis
#---- t Test for population slope
#---- y = beta0 + beta1*X  #line equarion beta0 is intercept and beta1 is slope
#---- Ho: Two Variables are independent i.e. beta1=0 (i.e. No Relationship)
#---- Ha: Two Variables are dependent i.e. beta1 <> 0 (i.e. There is Relationship)
#---- Fit a simple linear regression
print("Test of Hypothesis for MonthlyIncome (Y) Vs YearsWithCurrManager (X) is: ")
reg_YearsWithCurrManager_mi <- lm(mydata$MonthlyIncome~mydata$YearsWithCurrManager, data =mydata)
coeff=coefficients(reg_YearsWithCurrManager_mi)
# equation of the line : Y=Beta0+Beta1X 
eq1 = paste("Y = ", c(coeff[1]), "+", c(coeff[2]),"*X")

#----Statified Scatter plot
plot(mydata$YearsWithCurrManager,mydata$MonthlyIncome, col = mydata$Attrition_label,
     xlab = "Years With Current Manager - Red are for (Left) and Black are for (Not Left)", 
     ylab="Monthly Income",
     main = eq1, 
     pch = as.numeric(mydata$Attrition_label)+15)
abline(reg_YearsWithCurrManager_mi, col="green",lwd=3)
dev.off()
print(summary(reg_YearsWithCurrManager_mi))
print("After TOH we can clearly see that P-Value is less than 0.05")
print("Hence we can reject the Null Hypothesis and we say with 95% confidence")
print("That Monthly Income and YearsWithCurrManager are dependent on each other.")

setwd("C://Users//DELL//Documents//Attrition")
