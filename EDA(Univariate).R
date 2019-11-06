setwd("C://Users//DELL//Documents//Attrition")
source("source.R")

#Here before starting EDA, we want to save all plots to univariate plot folder
#So set the working directory accordingly and then at the end of 
#Univariate EDA we will set it back to "C://Users//DELL//Documents//Attrition"

setwd("C://Users//DELL//Documents//Attrition//_plots//Univariates")

############# EDA, summarization and Test of Hypothesis ###################
#1. Attrition which is Categorical data
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
png(filename = "Attrition.png",height=480, width= 960, unit ="px")
par(mfrow = c(1,2))
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
dev.off()

# Test of Hypothesis
#------ 1 Sample proportion test
#------ Above calculation shows 16.12% employees left the orgnization
#------ Domain claim is that the attrition problem is resolved and now less employees
#------ are leaving the orgnization, so lets check it now
#------ Ho(Null Hypothesis): p <= 0.1
#------ Ha(Alternate Hypothesis): p != and greater than 0.1
#------ New Data was collected and out of 400 employees 50 employees left orgnization
#------ Now Test the Hypothesis

print(prop.test(x=50, n = 400, p = 0.1, conf.level = 0.95))

# p-value = 0.1133 # taken from output of above test.
# Since p-value is greater than Alpha (0.05) hence alternate Hypothesis is accepted here.
# also the confidence interval is (0.09,0.16) which is very less and we are less confident.
# New proportion test shows that the attrition problem is not resolved.
# And we need to take some measures to stop it.

#2. Age which is quantitative data
#---Summerizarion
#----- mydata$Age

#---------Central Tendency - mean, median, mode
print(paste("Mean is",mean(mydata$Age)))
print(paste("Median is",median(mydata$Age)))


#---------Spread - min, max, range, sd, var, IQR
print(paste("Minium age is",min(mydata$Age)))
print(paste("Maxium age is",max(mydata$Age)))
print(paste("Range of Age is from",min(mydata$Age),"To",max(mydata$Age)))
print(paste("Standard deviation is",sd(mydata$Age)))
print(paste("Variance is",var(mydata$Age)))
print(paste("Inter Quartile Range is",IQR(mydata$Age)))

#---------Shape - Five number summary

print(paste("Five Number Summary is as follows"))
print(fivenum(mydata$Age))
print(paste("View summary"))
print(summary(mydata$Age))

#---Visualization
#?par  # Set or Query More Graphical Parameters
png(filename = "Age of an employees.png",height=480, width= 960, unit ="px") 
par(mfrow = c(2,2))
#-----Histogram
#-----Age wise breaks
hist(mydata$Age, breaks = c(0,10,20,30,40,50,60),
     main = "Histogram of an Age of employees",
     xlab = "Age", ylab="No of Employees",
     col = c(1:8))
abline(v= mean(mydata$Age), col = "Red")
abline(v= mean(mydata$Age)+2*sd(mydata$Age), col = "Orange")
text(37,550, "Mean")
text(55,550, "Mean+2SD", col = "Red")

#-----Boxplot
boxplot(mydata$Age, col ="Red",
        main = "Boxplot of an Age of employees",
        xlab = "Age", ylab="No of Emplyoees"
)  
text(1.3, fivenum(mydata$Age),c("Min","Q1","Med","Q3","Max")) 

# Dot Chart
dotchart(mydata$Age,col ="Red",
         main = "Dot Chart of an Age of employees",
         xlab = "Age", ylab="No of Emplyoees")

# Density Plot
plot(density(mydata$Age),col ="Red",
             main = "Density plot of an Age of employees",
             xlab = "Age", ylab="No of Emplyoees")

dev.off()

# Test of Hypothesis
#------ Employees mean age is 36.92
#------ 1 Sample t Test
#------ Management claimed that there is significant increase of mean age after new recruitment
#------ Ho: Mean Age (After new Recruitment) = 36.92381
#------ Ha: Mean Age (After new Recruitment) > 36.92381
#------ Now Test the Hypothesis

print(t.test(mydata$Age, mu = 36.92381, conf.level =0.95, alternate = "Greeater"))

# P-Value is 1, which is greater than Alpha=0.05
# Above test output shows that the alternate hypothesis is true
# and Management claim is right.
# But here again we will have to check more parameters.

# From here onwards we will only do EDA & Summarization of all remaining 33 univariates
# And then we will move towars Bivariates

#3. BusinessTravel which is Categorical data
#---Summerizarion
#------count, proportion,%,Ratio, mode

print(paste("Count = ",length(mydata$BusinessTravel))) # Count =1470
table(mydata$BusinessTravel)

BusinessTravel.matrix = as.matrix(table(mydata$BusinessTravel))
cat("Ratio of Non-Travel vs Travel_Frequently vs Travel_Rarely is: ",
    BusinessTravel.matrix[1]/length(mydata$BusinessTravel), # Ratio of Non-Travel
    BusinessTravel.matrix[2]/length(mydata$BusinessTravel), # Ratio of Travel_Frequently 
    BusinessTravel.matrix[3]/length(mydata$BusinessTravel),sep=" ") # Ratio of Travel_Rarely 

print(prop.table(table(mydata$BusinessTravel))) # Proportion
print(paste("Percentage"))
print(round(prop.table(table(mydata$BusinessTravel)) * 100,2))#Percentage
#print(max(table(mydata$BusinessTravel)))
#Mode
print(paste("Mode is",names(which.max(table(mydata$BusinessTravel))),"Which is equal to",max(table(mydata$BusinessTravel))))

#---Visualization
png(filename = "BusinessTravel.png",height=480, width= 960, unit ="px")
par(mfrow = c(1,2))
#------Pie Chart, Bar Chart
str(mydata$BusinessTravel)
levels(mydata$BusinessTravel)
#Pie chart of BusinessTravel
pie(BusinessTravel.matrix, radius = 1,
    main = "Pie Chart of BusinessTravel",
    col = c("Red", "Blue","Green"),
    labels = levels(mydata$BusinessTravel),
    clockwise = TRUE , init.angle = 90
)
#Bar chart of BusinessTravel
barplot(BusinessTravel.matrix, beside = TRUE, 
        space = 0.25, main = "Barplot of BusinessTravel",
        xlab="BusinessTravel", ylab="No of Employees", 
        names.arg = levels(mydata$BusinessTravel),# Alternate Option
        col = c("Red", "Blue","Green")
        #col = c("#FF0000FF",3)  # Alternate Option using palettes
)
dev.off()

#4. DailyRate which is quantitative data
#---Summerizarion
#----- mydata$DailyRate 

#---------Central Tendency - mean, median, mode
print(paste("Mean is",mean(mydata$DailyRate )))
print(paste("Median is",median(mydata$DailyRate )))


#---------Spread - min, max, range, sd, var, IQR
print(paste("Minium Daily rate is",min(mydata$DailyRate )))
print(paste("Maxium Daily Rate is",max(mydata$DailyRate )))
print(paste("Range of Daily Rate is from",min(mydata$DailyRate),"To",max(mydata$DailyRate)))
print(paste("Standard deviation is",sd(mydata$DailyRate )))
print(paste("Variance is",var(mydata$DailyRate )))
print(paste("Inter Quartile Range is",IQR(mydata$DailyRate )))

#---------Shape - Five number summary

print(paste("Five Number Summary is as follows"))
print(fivenum(mydata$DailyRate ))
print(paste("View summary"))
print(summary(mydata$DailyRate ))

#---Visualization
#?par  # Set or Query More Graphical Parameters
png(filename = "DailyRate of an employee.png",height=480, width= 960, unit ="px") 
par(mfrow = c(2,2))
#-----Histogram
#-----DailyRate wise breaks
hist(mydata$DailyRate , breaks = c(100,200,300,400,500,600,700,800,900,1000,1100,1200,1300,1400,1500),
     main = "Histogram of DailyRate of employees",
     xlab = "DailyRate", ylab="No of Employees",
     col = c(1:8))
abline(v= mean(mydata$DailyRate ), col = "Red")
abline(v= mean(mydata$DailyRate )+2*sd(mydata$DailyRate ), col = "Orange")

text(800,120, "Mean")


#-----Boxplot
boxplot(mydata$DailyRate , col ="Red",
        main = "Boxplot of DailyRate of employees",
        xlab = "DailyRate", ylab="No of Emplyoees"
)  
text(1.3, fivenum(mydata$DailyRate ),c("Min","Q1","Med","Q3","Max")) 

# Dot Chart
dotchart(mydata$DailyRate ,col ="Red",
         main = "Dot Chart of DailyRateof employees",
         xlab = "DailyRate", ylab="No of Emplyoees")

# Density Plot
plot(density(mydata$DailyRate ),col ="Red",
     main = "Density plot of DailyRate of employees",
     xlab = "DailyRate", ylab="No of Emplyoees")

dev.off()


#5. Department which is Categorical data
#---Summerizarion
#------count, proportion,%,Ratio, mode

print(paste("Count = ",length(mydata$Department))) # Count =1470
table(mydata$Department)

Department.matrix = as.matrix(table(mydata$Department))
cat("Ratio of Human Resources vs Research & Development  vs Sales is: ",Department.matrix[1]/length(mydata$Department), # Ratio of Non-Travel
    Department.matrix[2]/length(mydata$Department), # Ratio of Travel_Frequently 
    Department.matrix[3]/length(mydata$Department),sep=" ") # Ratio of Travel_Rarely

print(prop.table(table(mydata$Department))) # Proportion
print(paste("Percentage"))
print(round(prop.table(table(mydata$Department)) * 100,2))#Percentage
print(max(table(mydata$Department)))
#Mode
print(paste("Mode is",names(which.max(table(mydata$Department))),"Which is equal to",max(table(mydata$Department))))

#---Visualization
png(filename = "Department.png",height=480, width= 960, unit ="px")
par(mfrow = c(1,2))
#------Pie Chart, Bar Chart
str(mydata$Department)
levels(mydata$Department)
#Pie chart of Department
pie(Department.matrix, radius = 1,
    main = "Pie Chart of Department",
    col = c("Red", "Blue", "Green"),
    labels = levels(mydata$Department),
    clockwise = TRUE , init.angle = 90
)
#Bar chart of Department
barplot(Department.matrix, beside = TRUE, 
        space = 0.5, main = "Barplot of Department",
        xlab="Department", ylab="No of Employees", 
        names.arg = levels(mydata$Department),# Alternate Option
        col = c("Red", "Blue", "Green")
        #col = c("#FF0000FF",3)  # Alternate Option using palettes
)
dev.off()


#6. DistanceFromHome which is quantitative data
#---Summerizarion
#----- mydata$DistanceFromHome 

#---------Central Tendency - mean, median, mode
print(paste("Mean is",mean(mydata$DistanceFromHome )))
print(paste("Median is",median(mydata$DistanceFromHome )))


#---------Spread - min, max, range, sd, var, IQR
print(paste("Minium Distance From Home  is",min(mydata$DistanceFromHome )))
print(paste("Maxium Distance From Home  is",max(mydata$DistanceFromHome )))
print(paste("Range is",min(mydata$DistanceFromHome ),"To",max(mydata$DistanceFromHome )))
print(paste("Standard deviation is",sd(mydata$DistanceFromHome )))
print(paste("Variance is",var(mydata$DistanceFromHome )))
print(paste("Inter Quartile Range is",IQR(mydata$DistanceFromHome )))

#---------Shape - Five number summary

print(paste("Five Number Summary is as follows"))
print(fivenum(mydata$DistanceFromHome ))
print(paste("View summary"))
print(summary(mydata$DistanceFromHome ))

#---Visualization
#?par  # Set or Query More Graphical Parameters
png(filename = "DistanceFromHome.png",height=480, width= 960, unit ="px") 
par(mfrow = c(2,2))
#-----Histogram
#-----Distance From Home wise breaks
hist(mydata$DistanceFromHome , breaks = c(1:29),
     main = "Histogram of Distance From Home of employees",
     xlab = "Distance From Home (KMs)", ylab="No of Employees",
     col = c(1:8))
abline(v= mean(mydata$DistanceFromHome ), col = "Red")
abline(v= mean(mydata$DistanceFromHome )+2*sd(mydata$DistanceFromHome ), col = "Orange")


text(9,200, "Mean")
text(26,200, "Mean+2SD", col = "Red")

#-----Boxplot
boxplot(mydata$DistanceFromHome , col ="Red",
        main = "Boxplot of Distance From Home of an employees",
        xlab = "Distance From Home (KMs)", ylab="No of Emplyoees"
)  
text(1.3, fivenum(mydata$DistanceFromHome ),c("Min","Q1","Med","Q3","Max")) 

# Dot Chart
dotchart(mydata$DistanceFromHome ,col ="Red",
         main = "Dot Chart of Distance From Home of employees",
         xlab = "Distance From Home (KMs)", ylab="No of Emplyoees")

# Density Plot
plot(density(mydata$DistanceFromHome ),col ="Red",
     main = "Density plot of Distance From Home of employees",
     xlab = "Distance From Home (KMs)", ylab="No of Emplyoees")

dev.off()

#7. Education which is Categorical data
#---Summerizarion
#------count, proportion,%,Ratio, mode

print(paste("Count = ",length(mydata$Education_label))) # Count =1470
table(mydata$Education_label)

Education_label.matrix = as.matrix(table(mydata$Education_label))
cat("Ratio of 12th Vs Diploma Vs Graduation Vs PG Vs PhD is: ",Education_label.matrix[1]/length(mydata$Education_label), # Ratio of Non-Travel
    Education_label.matrix[2]/length(mydata$Education_label), # Ratio of Travel_Frequently 
    Education_label.matrix[3]/length(mydata$Education_label),
    Education_label.matrix[4]/length(mydata$Education_label),
    Education_label.matrix[5]/length(mydata$Education_label),sep=" ") # Ratio of Travel_Rarely

print(prop.table(table(mydata$Education_label))) # Proportion
print(paste("Percentage"))
print(round(prop.table(table(mydata$Education_label)) * 100,2))#Percentage
print(max(table(mydata$Education_label)))
#Mode
print(paste("Mode is",names(which.max(table(mydata$Education_label))),"Which is equal to",max(table(mydata$Education_label))))

#---Visualization
png(filename = "Education.png",height=480, width= 960, unit ="px")
par(mfrow = c(1,2))
#------Pie Chart, Bar Chart
str(mydata$Education_label)
levels(mydata$Education_label)
#Pie chart of Education_label
pie(Education_label.matrix, radius = 1,
    main = "Pie Chart of Education",
    col = c("Red", "Blue", "Green","Cyan","Yellow"),
    labels = levels(mydata$Education_label),
    clockwise = TRUE , init.angle = 90
)
#Bar chart of Education_label
barplot(Education_label.matrix, beside = TRUE, 
        space = 0.5, main = "Barplot of Education",
        xlab="Education", ylab="No of Employees", 
        names.arg = levels(mydata$Education_label),# Alternate Option
        col = c("Red", "Blue", "Green","Cyan","Yellow")
        #col = c("#FF0000FF",3)  # Alternate Option using palettes
)
dev.off()

#8. EducationField_label which is Categorical data
#---Summerizarion
#------count, proportion,%,Ratio, mode

print(paste("Count = ",length(mydata$EducationField_label))) # Count =1470
table(mydata$EducationField_label)

EducationField_label.matrix = as.matrix(table(mydata$EducationField_label))
cat("Ratio of  Human Resources Vs Life Sciences Vs Marketing  Vs Medical  Vs Other Vs Technical Degree is: ",
    EducationField_label.matrix[1]/length(mydata$EducationField_label), 
    EducationField_label.matrix[2]/length(mydata$EducationField_label),  
    EducationField_label.matrix[3]/length(mydata$EducationField_label),
    EducationField_label.matrix[4]/length(mydata$EducationField_label),
    EducationField_label.matrix[5]/length(mydata$EducationField_label),
    EducationField_label.matrix[6]/length(mydata$EducationField_label),sep=" ")

print(prop.table(table(mydata$EducationField_label))) # Proportion
print(paste("Percentage"))
print(round(prop.table(table(mydata$EducationField_label)) * 100,2))#Percentage
print(max(table(mydata$EducationField_label)))
#Mode
print(paste("Mode is",names(which.max(table(mydata$EducationField_label))),"Which is equal to",max(table(mydata$EducationField_label))))

#---Visualization
png(filename = "EducationField.png",height=480, width= 960, unit ="px")
par(mfrow = c(1,2))
#------Pie Chart, Bar Chart
str(mydata$EducationField_label)
levels(mydata$EducationField_label)
#Pie chart of EducationField_label
pie(EducationField_label.matrix, radius = 0.75,
    main = "Pie Chart of Education Field",
    col = c(1:8),
    labels = levels(mydata$EducationField_label),
    clockwise = TRUE , init.angle = 90
)
#Bar chart of EducationField_label
barplot(EducationField_label.matrix, beside = TRUE, 
        space = 1, main = "Barplot of Education Field",
        xlab="Education Field", ylab="No of Employees", 
        names.arg = levels(mydata$EducationField_label),# Alternate Option
        col = c(1:8)
        #col = c("#FF0000FF",3)  # Alternate Option using palettes
)
dev.off()

# 9.As EmployeeCount is 1 for all employees and which is non relevant 
# here in case of Attrition so we can skip EDA of EmployeeCount
# We will only print the table for EmployeeCount
print(table(mydata$EmployeeCount))

# 10.As EmployeeNumber is unique for all employees and which is non relevant 
# here in case of Attrition so we can skip EDA of EmployeeNumber
# but we will use this variable whenever it will require later in this project.

#11. EnvironmentSatisfaction which is Categorical data
#---Summerizarion
#------count, proportion,%,Ratio, mode

print(paste("Count = ",length(mydata$EnvironmentSatisfaction))) # Count =1470
table(mydata$EnvironmentSatisfaction)

EnvironmentSatisfaction.matrix = as.matrix(table(mydata$EnvironmentSatisfaction))
cat("Ratio of Environment Satisfacetion from (Low to High) is: ",
    EnvironmentSatisfaction.matrix[1]/length(mydata$EnvironmentSatisfaction), 
    EnvironmentSatisfaction.matrix[2]/length(mydata$EnvironmentSatisfaction),  
    EnvironmentSatisfaction.matrix[3]/length(mydata$EnvironmentSatisfaction),
    EnvironmentSatisfaction.matrix[4]/length(mydata$EnvironmentSatisfaction),sep=" ")

print(prop.table(table(mydata$EnvironmentSatisfaction))) # Proportion
print(paste("Percentage"))
print(round(prop.table(table(mydata$EnvironmentSatisfaction)) * 100,2))#Percentage
print(max(table(mydata$EnvironmentSatisfaction)))
#Mode
print(paste("Mode is",names(which.max(table(mydata$EnvironmentSatisfaction))),"Which is equal to",max(table(mydata$EnvironmentSatisfaction))))

#---Visualization
png(filename = "EnvironmentSatisfaction.png",height=480, width= 960, unit ="px")
par(mfrow = c(1,2))
#------Pie Chart, Bar Chart
str(mydata$EnvironmentSatisfaction)
levels(mydata$EnvironmentSatisfaction)
#Pie chart of EnvironmentSatisfaction
pie(EnvironmentSatisfaction.matrix, radius = 1,
    main = "Pie Chart of Environment Satisfaction",
    col = c("Red", "Blue", "Green", "Yellow"),
    labels = levels(mydata$EnvironmentSatisfaction),
    clockwise = TRUE , init.angle = 90
)
#Bar chart of EnvironmentSatisfaction
barplot(EnvironmentSatisfaction.matrix, beside = TRUE, 
        space = 0.25, main = "Barplot of Environment Satisfaction",
        xlab="Environment Satisfaction", ylab="No of Employees", 
        names.arg = levels(mydata$EnvironmentSatisfaction),# Alternate Option
        col = c("Red", "Blue", "Green", "Yellow")
        #col = c("#FF0000FF",3)  # Alternate Option using palettes
)
dev.off()


#12. Gender which is Categorical data
#---Summerizarion
#------count, proportion,%,Ratio, mode

print(paste("Count = ",length(mydata$Gender))) # Count =1470
table(mydata$Gender)

Gender.matrix = as.matrix(table(mydata$Gender))
cat("Ratio of Female Vs Male is: ",Gender.matrix[1]/length(mydata$Gender), 
    Gender.matrix[2]/length(mydata$Gender),sep=" ")

print(prop.table(table(mydata$Gender))) # Proportion
print(paste("Percentage"))
print(round(prop.table(table(mydata$Gender)) * 100,2))#Percentage
print(max(table(mydata$Gender)))
#Mode
print(paste("Mode is",names(which.max(table(mydata$Gender))),"Which is equal to",max(table(mydata$Gender))))

#---Visualization
png(filename = "Gender.png",height=480, width= 960, unit ="px")
par(mfrow = c(1,2))
#------Pie Chart, Bar Chart
str(mydata$Gender)
levels(mydata$Gender)
#Pie chart of Gender
pie(Gender.matrix, radius = 1,
    main = "Pie Chart of Gender",
    col = c("Blue", "Green"),
    labels = levels(mydata$Gender),
    clockwise = TRUE , init.angle = 90
)
#Bar chart of Gender
barplot(Gender.matrix, beside = TRUE, 
        space = 0.25, main = "Barplot of Gender",
        xlab="Gender", ylab="No of Employees", 
        names.arg = levels(mydata$Gender),# Alternate Option
        col = c("Blue", "Green")
        #col = c("#FF0000FF",3)  # Alternate Option using palettes
)
dev.off()


#13. HourlyRate which is quantitative data
#---Summerizarion
#----- mydata$HourlyRate 

#---------Central Tendency - mean, median, mode
print(paste("Mean is",mean(mydata$HourlyRate )))
print(paste("Median is",median(mydata$HourlyRate )))


#---------Spread - min, max, range, sd, var, IQR
print(paste("Minium Hourly Rate is",min(mydata$HourlyRate)))
print(paste("Maxium Hourly Rate is",max(mydata$HourlyRate)))
print(paste("Range is from",min(mydata$HourlyRate),"To",max(mydata$HourlyRate)))
print(paste("Standard deviation is",sd(mydata$HourlyRate )))
print(paste("Variance is",var(mydata$HourlyRate )))
print(paste("Inter Quartile Range is",IQR(mydata$HourlyRate )))

#---------Shape - Five number summary

print(paste("Five Number Summary is as follows"))
print(fivenum(mydata$HourlyRate ))
print(paste("View summary"))
print(summary(mydata$HourlyRate ))

#---Visualization
#?par  # Set or Query More Graphical Parameters
png(filename = "HourlyRate.png",height=480, width= 960, unit ="px") 
par(mfrow = c(2,2))
#-----Histogram
#-----Age wise breaks
hist(mydata$HourlyRate , breaks = c(30,40,50,60,70,80,90,100),
     main = "Histogram of Hourly Rate of employees",
     xlab = "Hourly Rate", ylab="No of Employees",
     col = c(1:8))
abline(v= mean(mydata$HourlyRate ), col = "Red")
abline(v= mean(mydata$HourlyRate )+2*sd(mydata$HourlyRate ), col = "Orange")

text(67,200, "Mean")

#-----Boxplot
boxplot(mydata$HourlyRate , col ="Red",
        main = "Boxplot of Hourly Rate of employees",
        xlab = "Hourly Rate", ylab="No of Emplyoees"
)  
text(1.3, fivenum(mydata$HourlyRate ),c("Min","Q1","Med","Q3","Max")) 

# Dot Chart
dotchart(mydata$HourlyRate ,col ="Red",
         main = "Dot Chart of Hourly Rate of employees",
         xlab = "Hourly Rate", ylab="No of Emplyoees")

# Density Plot
plot(density(mydata$HourlyRate ),col ="Red",
     main = "Density plot of Hourly Rate of employees",
     xlab = "Hourly Rate", ylab="No of Emplyoees")

dev.off()


#14. JobInvolvement which is Categorical data
#---Summerizarion
#------count, proportion,%,Ratio, mode

print(paste("Count = ",length(mydata$JobInvolvement))) # Count =1470
table(mydata$JobInvolvement)

JobInvolvement.matrix = as.matrix(table(mydata$JobInvolvement))
cat("Ratio of Job Involvement (from low to High) is: ",
    JobInvolvement.matrix[1]/length(mydata$JobInvolvement),
    JobInvolvement.matrix[2]/length(mydata$JobInvolvement),
    JobInvolvement.matrix[3]/length(mydata$JobInvolvement),
    JobInvolvement.matrix[3]/length(mydata$JobInvolvement),sep=" ")

print(prop.table(table(mydata$JobInvolvement))) # Proportion
print(paste("Percentage"))
print(round(prop.table(table(mydata$JobInvolvement)) * 100,2))#Percentage
print(max(table(mydata$JobInvolvement)))
#Mode
print(paste("Mode is",names(which.max(table(mydata$JobInvolvement))),"Which is equal to",max(table(mydata$JobInvolvement))))

#---Visualization
png(filename = "JobInvolvement.png",height=480, width= 960, unit ="px")
par(mfrow = c(1,2))
#------Pie Chart, Bar Chart
str(mydata$JobInvolvement)
levels(mydata$JobInvolvement)
#Pie chart of JobInvolvement
pie(JobInvolvement.matrix, radius = 1,
    main = "Pie Chart of Job Involvement",
    col = c("Red", "Blue", "Green", "Yellow"),
    labels = levels(mydata$JobInvolvement),
    clockwise = TRUE , init.angle = 90
)
#Bar chart of JobInvolvement
barplot(JobInvolvement.matrix, beside = TRUE, 
        space = 0.25, main = "Barplot of Job Involvement",
        xlab="Job Involvement", ylab="No of Employees", 
        names.arg = levels(mydata$JobInvolvement),# Alternate Option
        col = c("Red", "Blue", "Green","Yellow")
        #col = c("#FF0000FF",3)  # Alternate Option using palettes
)
dev.off()


#15. JobLevel which is Categorical data
#---Summerizarion
#------count, proportion,%,Ratio, mode

print(paste("Count = ",length(mydata$JobLevel))) # Count =1470
table(mydata$JobLevel)

JobLevel.matrix = as.matrix(table(mydata$JobLevel))
cat("Ratio of Job level (From low to high) is: ",
    JobLevel.matrix[1]/length(mydata$JobLevel),
    JobLevel.matrix[2]/length(mydata$JobLevel), 
    JobLevel.matrix[3]/length(mydata$JobLevel),
    JobLevel.matrix[4]/length(mydata$JobLevel), 
    JobLevel.matrix[5]/length(mydata$JobLevel),sep=" ") 

print(prop.table(table(mydata$JobLevel))) # Proportion
print(paste("Percentage"))
print(round(prop.table(table(mydata$JobLevel)) * 100,2))#Percentage
print(max(table(mydata$JobLevel)))
#Mode
print(paste("Mode is",names(which.max(table(mydata$JobLevel))),"Which is equal to",max(table(mydata$JobLevel))))

#---Visualization
png(filename = "JobLevel.png",height=480, width= 960, unit ="px")
par(mfrow = c(1,2))
#------Pie Chart, Bar Chart
str(mydata$JobLevel)
levels(mydata$JobLevel)
#Pie chart of JobLevel
pie(JobLevel.matrix, radius = 1,
    main = "Pie Chart of Job Level",
    col = c("Red", "Blue", "Green", "Yellow","Cyan"),
    labels = levels(mydata$JobLevel),
    clockwise = TRUE , init.angle = 90
)
#Bar chart of JobLevel
barplot(JobLevel.matrix, beside = TRUE, 
        space = 0.25, main = "Barplot of Job Level",
        xlab="Job Level", ylab="No of Employees", 
        names.arg = levels(mydata$JobLevel),# Alternate Option
        col = c("Red", "Blue", "Green","Yellow","Cyan")
        #col = c("#FF0000FF",3)  # Alternate Option using palettes
)
dev.off()


#16. JobRole_label which is Categorical data
#---Summerizarion
#------count, proportion,%,Ratio, mode

print(paste("Count = ",length(mydata$JobRole_label))) # Count =1470
table(mydata$JobRole_label)

JobRole_label.matrix = as.matrix(table(mydata$JobRole_label))
cat("Ratio of Job Role is: ",
    JobRole_label.matrix[1]/length(mydata$JobRole_label),
    JobRole_label.matrix[2]/length(mydata$JobRole_label), 
    JobRole_label.matrix[3]/length(mydata$JobRole_label),
    JobRole_label.matrix[4]/length(mydata$JobRole_label), 
    JobRole_label.matrix[5]/length(mydata$JobRole_label),
    JobRole_label.matrix[6]/length(mydata$JobRole_label), 
    JobRole_label.matrix[7]/length(mydata$JobRole_label),
    JobRole_label.matrix[8]/length(mydata$JobRole_label), 
    JobRole_label.matrix[9]/length(mydata$JobRole_label),sep=" ") 

print(prop.table(table(mydata$JobRole_label))) # Proportion
print(paste("Percentage"))
print(round(prop.table(table(mydata$JobRole_label)) * 100,2))#Percentage
print(max(table(mydata$JobRole_label)))
#Mode
print(paste("Mode is",names(which.max(table(mydata$JobRole_label))),"Which is equal to",max(table(mydata$JobRole_label))))

#---Visualization
png(filename = "JobRole_Pie.png",height=480, width= 960, unit ="px")
par(mfrow = c(1,1))
#------Pie Chart, Bar Chart
str(mydata$JobRole_label)
levels(mydata$JobRole_label)
#Pie chart of JobRole_label
pie(JobRole_label.matrix, radius = 1,
    main = "Pie Chart of Job Role",
    col = c(1:8),
    labels = levels(mydata$JobRole_label),
    clockwise = TRUE , init.angle = 90
)
dev.off()
#Bar chart of JobRole_label
png(filename = "JobRole_Barplot.png",height=480, width= 960, unit ="px")
barplot(JobRole_label.matrix, beside = TRUE, 
        space = 2, main = "Barplot of Job Role",
        xlab="Job Role", ylab="No of Employees", 
        names.arg = levels(mydata$JobRole_label),# Alternate Option
        col = c(1:8)
        #col = c("#FF0000FF",3)  # Alternate Option using palettes
)
dev.off()

#17. JobSatisfaction which is Categorical data
#---Summerizarion
#------count, proportion,%,Ratio, mode

print(paste("Count = ",length(mydata$JobSatisfaction))) # Count =1470
table(mydata$JobSatisfaction)

JobSatisfaction.matrix = as.matrix(table(mydata$JobSatisfaction))
cat("Ratio of Job level (From low to high) is: ",
    JobSatisfaction.matrix[1]/length(mydata$JobSatisfaction),
    JobSatisfaction.matrix[2]/length(mydata$JobSatisfaction), 
    JobSatisfaction.matrix[3]/length(mydata$JobSatisfaction),
    JobSatisfaction.matrix[4]/length(mydata$JobSatisfaction),sep=" ")  
    

print(prop.table(table(mydata$JobSatisfaction))) # Proportion
print(paste("Percentage"))
print(round(prop.table(table(mydata$JobSatisfaction)) * 100,2))#Percentage
print(max(table(mydata$JobSatisfaction)))
#Mode
print(paste("Mode is",names(which.max(table(mydata$JobSatisfaction))),"Which is equal to",max(table(mydata$JobSatisfaction))))

#---Visualization
png(filename = "JobSatisfaction.png",height=480, width= 960, unit ="px")
par(mfrow = c(1,2))
#------Pie Chart, Bar Chart
str(mydata$JobSatisfaction)
levels(mydata$JobSatisfaction)
#Pie chart of JobSatisfaction
pie(JobSatisfaction.matrix, radius = 1,
    main = "Pie Chart of Job Satisfaction",
    col = c("Red", "Blue","Yellow","Green"),
    labels = levels(mydata$JobSatisfaction),
    clockwise = TRUE , init.angle = 90
)
#Bar chart of JobSatisfaction
barplot(JobSatisfaction.matrix, beside = TRUE, 
        space = 0.25, main = "Barplot of Job Satisfaction",
        xlab="Job Satisfaction", ylab="No of Employees", 
        names.arg = levels(mydata$JobSatisfaction),# Alternate Option
        col = c("Red", "Blue","Yellow","Green")
        #col = c("#FF0000FF",3)  # Alternate Option using palettes
)
dev.off()

#18. MaritalStatus which is Categorical data
#---Summerizarion
#------count, proportion,%,Ratio, mode

print(paste("Count = ",length(mydata$MaritalStatus))) # Count =1470
table(mydata$MaritalStatus)

MaritalStatus.matrix = as.matrix(table(mydata$MaritalStatus))
cat("Ratio of Divorced Vs Married Vs Single ) is: ",
    MaritalStatus.matrix[1]/length(mydata$MaritalStatus),
    MaritalStatus.matrix[2]/length(mydata$MaritalStatus), 
    MaritalStatus.matrix[3]/length(mydata$MaritalStatus),sep=" ") 

print(prop.table(table(mydata$MaritalStatus))) # Proportion
print(paste("Percentage"))
print(round(prop.table(table(mydata$MaritalStatus)) * 100,2))#Percentage
print(max(table(mydata$MaritalStatus)))
#Mode
print(paste("Mode is",names(which.max(table(mydata$MaritalStatus))),"Which is equal to",max(table(mydata$MaritalStatus))))

#---Visualization
png(filename = "MaritalStatus.png",height=480, width= 960, unit ="px")
par(mfrow = c(1,2))
#------Pie Chart, Bar Chart
str(mydata$MaritalStatus)
levels(mydata$MaritalStatus)
#Pie chart of MaritalStatus
pie(MaritalStatus.matrix, radius = 1,
    main = "Pie Chart of Marital Status",
    col = c("Red", "Blue", "Green"),
    labels = levels(mydata$MaritalStatus),
    clockwise = TRUE , init.angle = 90
)
#Bar chart of MaritalStatus
barplot(MaritalStatus.matrix, beside = TRUE, 
        space = 0.25, main = "Barplot of MaritalStatus",
        xlab="Marital Status", ylab="No of Employees", 
        names.arg = levels(mydata$MaritalStatus),# Alternate Option
        col = c("Red", "Blue", "Green")
        #col = c("#FF0000FF",3)  # Alternate Option using palettes
)
dev.off()

#19. MonthlyIncome which is quantitative data
#---Summerizarion
#----- mydata$MonthlyIncome 

#---------Central Tendency - mean, median, mode
print(paste("Mean is",mean(mydata$MonthlyIncome )))
print(paste("Median is",median(mydata$MonthlyIncome )))


#---------Spread - min, max, range, sd, var, IQR
print(paste("Minimum Monthly Income is",min(mydata$MonthlyIncome)))
print(paste("Maximum Monthly Income is",max(mydata$MonthlyIncome )))
print(paste("Range is from",min(mydata$MonthlyIncome),"To",max(mydata$MonthlyIncome)))
print(paste("Standard deviation is",sd(mydata$MonthlyIncome )))
print(paste("Variance is",var(mydata$MonthlyIncome )))
print(paste("Inter Quartile Range is",IQR(mydata$MonthlyIncome )))

#---------Shape - Five number summary

print(paste("Five Number Summary is as follows"))
print(fivenum(mydata$MonthlyIncome ))
print(paste("View summary"))
print(summary(mydata$MonthlyIncome ))

#---Visualization
#?par  # Set or Query More Graphical Parameters
png(filename = "MonthlyIncome.png",height=480, width= 960, unit ="px") 
par(mfrow = c(2,2))
#-----Histogram
#-----MonthlyIncome wise breaks
hist(mydata$MonthlyIncome , breaks = c(1000,2000,3000,4000,5000,6000,7000,8000,9000,10000,
                                       11000,12000,13000,14000,15000,16000,17000,
                                       18000,19000,20000),
     include.lowest = TRUE,
     main = "Histogram of Monthly Income of employees",
     xlab = "Monthly Income", ylab="No of Employees",
     col = c(1:8))
abline(v= mean(mydata$MonthlyIncome ), col = "Red")
abline(v= mean(mydata$MonthlyIncome )+2*sd(mydata$MonthlyIncome ), col = "Orange")
   

text(7000,300, "Mean")
text(16999,300, "Mean+2SD", col = "Red")

#-----Boxplot
boxplot(mydata$MonthlyIncome , col ="Red",
        main = "Boxplot of Monthly Income of employees",
        xlab = "Monthly Income", ylab="No of Emplyoees"
)  
text(1.3, fivenum(mydata$MonthlyIncome ),c("Min","Q1","Med","Q3","Max")) 

# Dot Chart
dotchart(mydata$MonthlyIncome ,col ="Red",
         main = "Dot Chart of Monthly Income of employees",
         xlab = "Monthly Income", ylab="No of Emplyoees")

# Density Plot
plot(density(mydata$MonthlyIncome ),col ="Red",
     main = "Density plot of Monthly Income of employees",
     xlab = "Monthly Income", ylab="No of Emplyoees")

dev.off()


#20. MonthlyRate which is quantitative data
#---Summerizarion
#----- mydata$MonthlyRate 

#---------Central Tendency - mean, median, mode
print(paste("Mean is",mean(mydata$MonthlyRate )))
print(paste("Median is",median(mydata$MonthlyRate )))


#---------Spread - min, max, range, sd, var, IQR
print(paste("Minimum Monthly Rate is",min(mydata$MonthlyRate )))
print(paste("Maximum Monthly Rate is",max(mydata$MonthlyRate )))
print(paste("Range is from",min(mydata$MonthlyRate),"To",max(mydata$MonthlyRate)))
print(paste("Standard deviation is",sd(mydata$MonthlyRate )))
print(paste("Variance is",var(mydata$MonthlyRate )))
print(paste("Inter Quartile Range is",IQR(mydata$MonthlyRate )))

#---------Shape - Five number summary

print(paste("Five Number Summary is as follows"))
print(fivenum(mydata$MonthlyRate ))
print(paste("View summary"))
print(summary(mydata$MonthlyRate ))

#---Visualization
#?par  # Set or Query More Graphical Parameters
png(filename = "MonthlyRate.png",height=480, width= 960, unit ="px") 
par(mfrow = c(2,2))
#-----Histogram
#-----Monthly Rate wise breaks
hist(mydata$MonthlyRate , breaks = c(2000,4000,6000,8000,10000,
                                     12000,14000,16000,18000,20000,
                                     22000,24000,26000,28000),
     main = "Histogram of Monthly Rate of employees",
     xlab = "Monthly Rate", ylab="No of Employees",
     col = c(1:8))
abline(v= mean(mydata$MonthlyRate ), col = "Red")
abline(v= mean(mydata$MonthlyRate )+2*sd(mydata$MonthlyRate ), col = "Orange")
   

text(14200,125, "Mean")
text(28200,125, "Mean+2SD", col = "Red")

#-----Boxplot
boxplot(mydata$MonthlyRate , col ="Red",
        main = "Boxplot of Monthly Rate of employees",
        xlab = "Monthly Rate", ylab="No of Emplyoees"
)  
text(1.3, fivenum(mydata$MonthlyRate ),c("Min","Q1","Med","Q3","Max")) 

# Dot Chart
dotchart(mydata$MonthlyRate ,col ="Red",
         main = "Dot Chart of Monthly Rate of employees",
         xlab = "Monthly Rate", ylab="No of Emplyoees")

# Density Plot
plot(density(mydata$MonthlyRate ),col ="Red",
     main = "Density plot of Monthly Rate of employees",
     xlab = "Monthly Rate", ylab="No of Emplyoees")

dev.off()


#21. NumCompaniesWorked which is quantitative data
#---Summerizarion
#----- mydata$NumCompaniesWorked 

#---------Central Tendency - mean, median, mode
print(paste("Mean is",mean(mydata$NumCompaniesWorked )))
print(paste("Median is",median(mydata$NumCompaniesWorked )))


#---------Spread - min, max, range, sd, var, IQR
print(paste("Minimum No. of Companies Worked is",min(mydata$NumCompaniesWorked )))
print(paste("Maximum No. of Companies Worked is",max(mydata$NumCompaniesWorked )))
print(paste("Range is from",min(mydata$NumCompaniesWorked),"To",max(mydata$NumCompaniesWorked)))
print(paste("Standard deviation is",sd(mydata$NumCompaniesWorked )))
print(paste("Variance is",var(mydata$NumCompaniesWorked )))
print(paste("Inter Quartile Range is",IQR(mydata$NumCompaniesWorked )))

#---------Shape - Five number summary

print(paste("Five Number Summary is as follows"))
print(fivenum(mydata$NumCompaniesWorked ))
print(paste("View summary"))
print(summary(mydata$NumCompaniesWorked ))

#---Visualization
#?par  # Set or Query More Graphical Parameters
png(filename = "NumCompaniesWorked.png",height=480, width= 960, unit ="px") 
par(mfrow = c(2,2))
#-----Histogram
#-----No. of Companies Worked wise breaks
hist(mydata$NumCompaniesWorked , breaks = c(0,1,2,3,4,5,6,7,8,9),
     main = "Histogram of No. of Companies Worked of employees",
     xlab = "No. of Companies Worked", ylab="No of Employees",
     col = c(1:8))
abline(v= mean(mydata$NumCompaniesWorked ), col = "Red")
abline(v= mean(mydata$NumCompaniesWorked )+2*sd(mydata$NumCompaniesWorked ), col = "Orange")


text(2.7,600, "Mean")
text(7.7,600, "Mean+2SD", col = "Red")

#-----Boxplot
boxplot(mydata$NumCompaniesWorked , col ="Red",
        main = "Boxplot of No. of Companies Worked of employees",
        xlab = "No. of Companies Worked", ylab="No of Emplyoees"
)  
text(1.3, fivenum(mydata$NumCompaniesWorked ),c("Min","Q1","Med","Q3","Max")) 

# Dot Chart
dotchart(mydata$NumCompaniesWorked ,col ="Red",
         main = "Dot Chart of No. of Companies Worked of employees",
         xlab = "No. of Companies Worked", ylab="No of Emplyoees")

# Density Plot
plot(density(mydata$NumCompaniesWorked ),col ="Red",
     main = "Density plot of No. of Companies Worked of employees",
     xlab = "No. of Companies Worked", ylab="No of Emplyoees")

dev.off()

#22. Over18 which is Categorical data
#---Summerizarion
#------count, proportion,%,Ratio, mode

print(paste("Count = ",length(mydata$Over18))) # Count =1470
table(mydata$Over18)

Over18.matrix = as.matrix(table(mydata$Over18))
cat("Ratio of Yes Vs No is: ",
    Over18.matrix[1]/length(mydata$Over18),sep=" ") 

print(prop.table(table(mydata$Over18))) # Proportion
print(paste("Percentage"))
print(round(prop.table(table(mydata$Over18)) * 100,2))#Percentage
print(max(table(mydata$Over18)))
#Mode
print(paste("Mode is",names(which.max(table(mydata$Over18))),"Which is equal to",max(table(mydata$Over18))))

#---Visualization
png(filename = "Over18.png",height=480, width= 960, unit ="px")
par(mfrow = c(1,2))
#------Pie Chart, Bar Chart
str(mydata$Over18)
levels(mydata$Over18)
#Pie chart of Over18
pie(Over18.matrix, radius = 1,
    main = "Pie Chart of Over18",
    col = c("Red", "Blue"),
    labels = levels(mydata$Over18),
    clockwise = TRUE , init.angle = 90
)
#Bar chart of Over18
barplot(Over18.matrix, beside = TRUE, 
        space = 0.25, main = "Barplot of Over18",
        xlab="Over18", ylab="No of Employees", 
        names.arg = levels(mydata$Over18),# Alternate Option
        col = c("Red", "Blue")
        #col = c("#FF0000FF",3)  # Alternate Option using palettes
)
dev.off()


#23. OverTime which is Categorical data
#---Summerizarion
#------count, proportion,%,Ratio, mode

print(paste("Count = ",length(mydata$OverTime))) # Count =1470
table(mydata$OverTime)

OverTime.matrix = as.matrix(table(mydata$OverTime))
cat("Ratio of Over Time (No Vs Yes) is: ",
    OverTime.matrix[1]/length(mydata$OverTime),
    OverTime.matrix[2]/length(mydata$OverTime),sep=" ") 

print(prop.table(table(mydata$OverTime))) # Proportion
print(paste("Percentage"))
print(round(prop.table(table(mydata$OverTime)) * 100,2))#Percentage
print(max(table(mydata$OverTime)))
#Mode
print(paste("Mode is",names(which.max(table(mydata$OverTime))),"Which is equal to",max(table(mydata$OverTime))))

#---Visualization
png(filename = "OverTime.png",height=480, width= 960, unit ="px")
par(mfrow = c(1,2))
#------Pie Chart, Bar Chart
str(mydata$OverTime)
levels(mydata$OverTime)
#Pie chart of OverTime
pie(OverTime.matrix, radius = 1,
    main = "Pie Chart of Over Time",
    col = c("Red", "Green"),
    labels = levels(mydata$OverTime),
    clockwise = TRUE , init.angle = 90
)
#Bar chart of OverTime
barplot(OverTime.matrix, beside = TRUE, 
        space = 0.25, main = "Barplot of Over Time",
        xlab="Over Time", ylab="No of Employees", 
        names.arg = levels(mydata$OverTime),# Alternate Option
        col = c("Red","Green")
        #col = c("#FF0000FF",3)  # Alternate Option using palettes
)
dev.off()

#24. PercentSalaryHike which is quantitative data
#---Summerizarion
#----- mydata$PercentSalaryHike 

#---------Central Tendency - mean, median, mode
print(paste("Mean is",mean(mydata$PercentSalaryHike )))
print(paste("Median is",median(mydata$PercentSalaryHike )))


#---------Spread - min, max, range, sd, var, IQR
print(paste("Minimum Percent Salary Hike is",min(mydata$PercentSalaryHike )))
print(paste("Maximum Percent Salary Hike is",max(mydata$PercentSalaryHike )))
print(paste("Range is from",min(mydata$PercentSalaryHike),"To",max(mydata$PercentSalaryHike)))
print(paste("Standard deviation is",sd(mydata$PercentSalaryHike )))
print(paste("Variance is",var(mydata$PercentSalaryHike )))
print(paste("Inter Quartile Range is",IQR(mydata$PercentSalaryHike )))

#---------Shape - Five number summary

print(paste("Five Number Summary is as follows"))
print(fivenum(mydata$PercentSalaryHike ))
print(paste("View summary"))
print(summary(mydata$PercentSalaryHike ))

#---Visualization
#?par  # Set or Query More Graphical Parameters
png(filename = "PercentSalaryHike.png",height=480, width= 960, unit ="px") 
par(mfrow = c(2,2))
#-----Histogram
#-----Percent Salary Hike wise breaks
hist(mydata$PercentSalaryHike , breaks = c(11,12,13,14,15,16,17,18,19,20,
                                           21,22,23,24,25,26),
     main = "Histogram of Percent Salary Hike of employees",
     xlab = "Percent Salary Hike", ylab="No of Employees",
     col = c(1:8))
abline(v= mean(mydata$PercentSalaryHike ), col = "Red")
abline(v= mean(mydata$PercentSalaryHike )+2*sd(mydata$PercentSalaryHike ), col = "Orange")


text(15.1,300, "Mean")
text(23,300, "Mean+2SD", col = "Red")

#-----Boxplot
boxplot(mydata$PercentSalaryHike , col ="Red",
        main = "Boxplot of Percent Salary Hike of employees",
        xlab = "Percent Salary Hike", ylab="No of Emplyoees"
)  
text(1.3, fivenum(mydata$PercentSalaryHike ),c("Min","Q1","Med","Q3","Max")) 

# Dot Chart
dotchart(mydata$PercentSalaryHike ,col ="Red",
         main = "Dot Chart of Percent Salary Hike of employees",
         xlab = "Percent Salary Hike", ylab="No of Emplyoees")

# Density Plot
plot(density(mydata$PercentSalaryHike ),col ="Red",
     main = "Density plot of Percent Salary Hike of employees",
     xlab = "Percent Salary Hike", ylab="No of Emplyoees")

dev.off()

#25. PerformanceRating_label which is Categorical data
#---Summerizarion
#------count, proportion,%,Ratio, mode

print(paste("Count = ",length(mydata$PerformanceRating_label))) # Count =1470
table(mydata$PerformanceRating_label)

PerformanceRating_label.matrix = as.matrix(table(mydata$PerformanceRating_label))
cat("Ratio of Performance Rating (3 Vs 4) is: ",
    PerformanceRating_label.matrix[1]/length(mydata$PerformanceRating_label),
    PerformanceRating_label.matrix[2]/length(mydata$PerformanceRating_label),sep=" ") 

print(prop.table(table(mydata$PerformanceRating_label))) # Proportion
print(paste("Percentage"))
print(round(prop.table(table(mydata$PerformanceRating_label)) * 100,2))#Percentage
print(max(table(mydata$PerformanceRating_label)))
#Mode
print(paste("Mode is",names(which.max(table(mydata$PerformanceRating_label))),"Which is equal to",max(table(mydata$PerformanceRating_label))))

#---Visualization
png(filename = "PerformanceRating.png",height=480, width= 960, unit ="px")
par(mfrow = c(1,2))
#------Pie Chart, Bar Chart
str(mydata$PerformanceRating_label)
levels(mydata$PerformanceRating_label)
#Pie chart of PerformanceRating_label
pie(PerformanceRating_label.matrix, radius = 1,
    main = "Pie Chart of Performance Rating",
    col = c("Red", "Blue"),
    labels = levels(mydata$PerformanceRating_label),
    clockwise = TRUE , init.angle = 90
)
#Bar chart of PerformanceRating_label
barplot(PerformanceRating_label.matrix, beside = TRUE, 
        space = 0.25, main = "Barplot of Performance Rating",
        xlab="Performance Rating", ylab="No of Employees", 
        names.arg = levels(mydata$PerformanceRating_label),# Alternate Option
        col = c("Red", "Blue")
        #col = c("#FF0000FF",3)  # Alternate Option using palettes
)
dev.off()

#26. RelationshipSatisfaction which is Categorical data
#---Summerizarion
#------count, proportion,%,Ratio, mode

print(paste("Count = ",length(mydata$RelationshipSatisfaction))) # Count =1470
table(mydata$RelationshipSatisfaction)

RelationshipSatisfaction.matrix = as.matrix(table(mydata$RelationshipSatisfaction))
cat("Ratio of Relationship Satisfaction is: ",
    RelationshipSatisfaction.matrix[1]/length(mydata$RelationshipSatisfaction),
    RelationshipSatisfaction.matrix[2]/length(mydata$RelationshipSatisfaction),
    RelationshipSatisfaction.matrix[3]/length(mydata$RelationshipSatisfaction),
    RelationshipSatisfaction.matrix[4]/length(mydata$RelationshipSatisfaction),sep=" ") 

print(prop.table(table(mydata$RelationshipSatisfaction))) # Proportion
print(paste("Percentage"))
print(round(prop.table(table(mydata$RelationshipSatisfaction)) * 100,2))#Percentage
print(max(table(mydata$RelationshipSatisfaction)))
#Mode
print(paste("Mode is",names(which.max(table(mydata$RelationshipSatisfaction))),"Which is equal to",max(table(mydata$RelationshipSatisfaction))))

#---Visualization
png(filename = "RelationshipSatisfaction.png",height=480, width= 960, unit ="px")
par(mfrow = c(1,2))
#------Pie Chart, Bar Chart
str(mydata$RelationshipSatisfaction)
levels(mydata$RelationshipSatisfaction)
#Pie chart of RelationshipSatisfaction
pie(RelationshipSatisfaction.matrix, radius = 1,
    main = "Pie Chart of Relationship Satisfaction",
    col = c("Red", "Blue","Green","Yellow"),
    labels = levels(mydata$RelationshipSatisfaction),
    clockwise = TRUE , init.angle = 90
)
#Bar chart of RelationshipSatisfaction
barplot(RelationshipSatisfaction.matrix, beside = TRUE, 
        space = 0.25, main = "Barplot of Relationship Satisfaction",
        xlab="Relationship Satisfaction", ylab="No of Employees", 
        names.arg = levels(mydata$RelationshipSatisfaction),# Alternate Option
        col = c("Red", "Blue","Green","Yellow")
        #col = c("#FF0000FF",3)  # Alternate Option using palettes
)
dev.off()

#27. StandardHours which is quantitative data
#---Summerizarion
#----- mydata$StandardHours 

#---------Central Tendency - mean, median, mode
print(paste("Mean is",mean(mydata$StandardHours )))
print(paste("Median is",median(mydata$StandardHours )))


#---------Spread - min, max, range, sd, var, IQR
print(paste("Minimum Standard Hours is",min(mydata$StandardHours )))
print(paste("Maximum Standard Hours is",max(mydata$StandardHours )))
print(paste("Range is from",min(mydata$StandardHours),"To",max(mydata$StandardHours)))
print(paste("Standard deviation is",sd(mydata$StandardHours )))
print(paste("Variance is",var(mydata$StandardHours )))
print(paste("Inter Quartile Range is",IQR(mydata$StandardHours )))

#---------Shape - Five number summary

print(paste("Five Number Summary is as follows"))
print(fivenum(mydata$StandardHours ))
print(paste("View summary"))
print(summary(mydata$StandardHours ))

#---Visualization
#?par  # Set or Query More Graphical Parameters
png(filename = "StandardHours.png",height=480, width= 960, unit ="px") 
par(mfrow = c(2,2))
#-----Histogram
#-----Standard Hours wise breaks
hist(mydata$StandardHours , breaks = c(20,40,60,80,100),
     main = "Histogram of Standard Hours of employees",
     xlab = "Standard Hours", ylab="No of Employees",
     col = c(1:8))
abline(v= mean(mydata$StandardHours ), col = "Red")
abline(v= mean(mydata$StandardHours )+2*sd(mydata$StandardHours ), col = "Orange")


text(80,1000, "Mean")
text(80,1100, "Mean+2SD", col = "Red")

#-----Boxplot
boxplot(mydata$StandardHours , col ="Red",
        main = "Boxplot of Standard Hours of employees",
        xlab = "Standard Hours", ylab="No of Emplyoees"
)  
text(1.3, fivenum(mydata$StandardHours ),c("Min","Q1","Med","Q3","Max")) 

# Dot Chart
dotchart(mydata$StandardHours ,col ="Red",
         main = "Dot Chart of Standard Hours of employees",
         xlab = "Standard Hours", ylab="No of Emplyoees")

# Density Plot
plot(density(mydata$StandardHours ),col ="Red",
     main = "Density plot of Standard Hours of employees",
     xlab = "Standard Hours", ylab="No of Emplyoees")

dev.off()


#28. StockOptionLevel which is Categorical data
#---Summerizarion
#------count, proportion,%,Ratio, mode

print(paste("Count = ",length(mydata$StockOptionLevel))) # Count =1470
table(mydata$StockOptionLevel)

StockOptionLevel.matrix = as.matrix(table(mydata$StockOptionLevel))
cat("Ratio of Stock Option Level (0 Vs 1 Vs 2 Vs 3) is: ",
    StockOptionLevel.matrix[1]/length(mydata$StockOptionLevel),
    StockOptionLevel.matrix[2]/length(mydata$StockOptionLevel),
    StockOptionLevel.matrix[3]/length(mydata$StockOptionLevel),
    StockOptionLevel.matrix[4]/length(mydata$StockOptionLevel),sep=" ") 

print(prop.table(table(mydata$StockOptionLevel))) # Proportion
print(paste("Percentage"))
print(round(prop.table(table(mydata$StockOptionLevel)) * 100,2))#Percentage
print(max(table(mydata$StockOptionLevel)))
#Mode
print(paste("Mode is",names(which.max(table(mydata$StockOptionLevel))),"Which is equal to",max(table(mydata$StockOptionLevel))))

#---Visualization
png(filename = "StockOptionLevel.png",height=480, width= 960, unit ="px")
par(mfrow = c(1,2))
#------Pie Chart, Bar Chart
str(mydata$StockOptionLevel)
levels(mydata$StockOptionLevel)
#Pie chart of StockOptionLevel
pie(StockOptionLevel.matrix, radius = 1,
    main = "Pie Chart of Stock Option Level",
    col = c("Red", "Blue","Green","Yellow"),
    labels = levels(mydata$StockOptionLevel),
    clockwise = TRUE , init.angle = 90
)
#Bar chart of StockOptionLevel
barplot(StockOptionLevel.matrix, beside = TRUE, 
        space = 0.25, main = "Barplot of Stock Option Level",
        xlab="Stock Option Level", ylab="No of Employees", 
        names.arg = levels(mydata$StockOptionLevel),# Alternate Option
        col = c("Red", "Blue","Green","Yellow")
        #col = c("#FF0000FF",3)  # Alternate Option using palettes
)
dev.off()


#29. TotalWorkingYears which is quantitative data
#---Summerizarion
#----- mydata$TotalWorkingYears 

#---------Central Tendency - mean, median, mode
print(paste("Mean is",mean(mydata$TotalWorkingYears )))
print(paste("Median is",median(mydata$TotalWorkingYears )))


#---------Spread - min, max, range, sd, var, IQR
print(paste("Minimum Total Working Years is",min(mydata$TotalWorkingYears )))
print(paste("Maximum Total Working Years is",max(mydata$TotalWorkingYears )))
print(paste("Range is from",min(mydata$TotalWorkingYears),"To",max(mydata$TotalWorkingYears)))
print(paste("Standard deviation is",sd(mydata$TotalWorkingYears )))
print(paste("Variance is",var(mydata$TotalWorkingYears )))
print(paste("Inter Quartile Range is",IQR(mydata$TotalWorkingYears )))

#---------Shape - Five number summary

print(paste("Five Number Summary is as follows"))
print(fivenum(mydata$TotalWorkingYears ))
print(paste("View summary"))
print(summary(mydata$TotalWorkingYears ))

#---Visualization
#?par  # Set or Query More Graphical Parameters
png(filename = "TotalWorkingYears.png",height=480, width= 960, unit ="px") 
par(mfrow = c(2,2))
#-----Histogram
#-----Total Working Years wise breaks
hist(mydata$TotalWorkingYears , breaks = c(0,5,10,15,20,25,30,35,40,45),
     main = "Histogram of Total Working Years of employees",
     xlab = "Total Working Years", ylab="No of Employees",
     col = c(1:8))
abline(v= mean(mydata$TotalWorkingYears ), col = "Red")
abline(v= mean(mydata$TotalWorkingYears )+2*sd(mydata$TotalWorkingYears ), col = "Orange")


text(11,500, "Mean")
text(27,500, "Mean+2SD", col = "Red")

#-----Boxplot
boxplot(mydata$TotalWorkingYears , col ="Red",
        main = "Boxplot of Total Working Years of employees",
        xlab = "Total Working Years", ylab="No of Emplyoees"
)  
text(1.3, fivenum(mydata$TotalWorkingYears ),c("Min","Q1","Med","Q3","Max")) 

# Dot Chart
dotchart(mydata$TotalWorkingYears ,col ="Red",
         main = "Dot Chart of Total Working Years of employees",
         xlab = "Total Working Years", ylab="No of Emplyoees")

# Density Plot
plot(density(mydata$TotalWorkingYears),col ="Red",
     main = "Density plot of Total Working Years of employees",
     xlab = "Total Working Years", ylab="No of Emplyoees")

dev.off()


#30. TrainingTimesLastYear which is quantitative data
#---Summerizarion
#----- mydata$TrainingTimesLastYear 

#---------Central Tendency - mean, median, mode
print(paste("Mean is",mean(mydata$TrainingTimesLastYear )))
print(paste("Median is",median(mydata$TrainingTimesLastYear )))


#---------Spread - min, max, range, sd, var, IQR
print(paste("Minimum Training Times Last Year is",min(mydata$TrainingTimesLastYear )))
print(paste("Maximum Training Times Last Year is",max(mydata$TrainingTimesLastYear )))
print(paste("Range is from",min(mydata$TrainingTimesLastYear),"To",max(mydata$TrainingTimesLastYear)))
print(paste("Standard deviation is",sd(mydata$TrainingTimesLastYear )))
print(paste("Variance is",var(mydata$TrainingTimesLastYear )))
print(paste("Inter Quartile Range is",IQR(mydata$TrainingTimesLastYear )))

#---------Shape - Five number summary

print(paste("Five Number Summary is as follows"))
print(fivenum(mydata$TrainingTimesLastYear ))
print(paste("View summary"))
print(summary(mydata$TrainingTimesLastYear ))

#---Visualization
#?par  # Set or Query More Graphical Parameters
png(filename = "TrainingTimesLastYear.png",height=480, width= 960, unit ="px") 
par(mfrow = c(2,2))
#-----Histogram
#-----Training Times Last Year wise breaks
hist(mydata$TrainingTimesLastYear , breaks = c(0,1,2,3,4,5,6,7),
     main = "Histogram of Training Times Last Year of employees",
     xlab = "Training Times Last Year", ylab="No of Employees",
     col = c(1:8))
abline(v= mean(mydata$TrainingTimesLastYear ), col = "Red")
abline(v= mean(mydata$TrainingTimesLastYear )+2*sd(mydata$TrainingTimesLastYear ), col = "Orange")


text(2.8,550, "Mean")
text(5.4,550, "Mean+2SD", col = "Red")

#-----Boxplot
boxplot(mydata$TrainingTimesLastYear , col ="Red",
        main = "Boxplot of Training Times Last Year of employees",
        xlab = "Training Times Last Year", ylab="No of Emplyoees"
)  
text(1.3, fivenum(mydata$TrainingTimesLastYear ),c("Min","Q1","Med","Q3","Max")) 

# Dot Chart
dotchart(mydata$TrainingTimesLastYear ,col ="Red",
         main = "Dot Chart of Training Times Last Year of employees",
         xlab = "Training Times Last Year", ylab="No of Emplyoees")

# Density Plot
plot(density(mydata$TrainingTimesLastYear ),col ="Red",
     main = "Density plot of Training Times Last Year of employees",
     xlab = "Training Times Last Year", ylab="No of Emplyoees")

dev.off()


#31. WorkLifeBalance which is Categorical data
#---Summerizarion
#------count, proportion,%,Ratio, mode

print(paste("Count = ",length(mydata$WorkLifeBalance))) # Count =1470
table(mydata$WorkLifeBalance)

WorkLifeBalance.matrix = as.matrix(table(mydata$WorkLifeBalance))
cat("Ratio of Work Life Balance (1 Vs 2 Vs 3 Vs 4) is: ",
    WorkLifeBalance.matrix[1]/length(mydata$WorkLifeBalance),
    WorkLifeBalance.matrix[2]/length(mydata$WorkLifeBalance),
    WorkLifeBalance.matrix[3]/length(mydata$WorkLifeBalance),
    WorkLifeBalance.matrix[4]/length(mydata$WorkLifeBalance),sep=" ") 

print(prop.table(table(mydata$WorkLifeBalance))) # Proportion
print(paste("Percentage"))
print(round(prop.table(table(mydata$WorkLifeBalance)) * 100,2))#Percentage
print(max(table(mydata$WorkLifeBalance)))
#Mode
print(paste("Mode is",names(which.max(table(mydata$WorkLifeBalance))),"Which is equal to",max(table(mydata$WorkLifeBalance))))

#---Visualization
png(filename = "WorkLifeBalance.png",height=480, width= 960, unit ="px")
par(mfrow = c(1,2))
#------Pie Chart, Bar Chart
str(mydata$WorkLifeBalance)
levels(mydata$WorkLifeBalance)
#Pie chart of WorkLifeBalance
pie(WorkLifeBalance.matrix, radius = 1,
    main = "Pie Chart of Work Life Balance",
    col = c("Red", "Blue","Green","Yellow"),
    labels = levels(mydata$WorkLifeBalance),
    clockwise = TRUE , init.angle = 90
)
#Bar chart of WorkLifeBalance
barplot(WorkLifeBalance.matrix, beside = TRUE, 
        space = 0.25, main = "Barplot of Work Life Balance",
        xlab="Work Life Balance", ylab="No of Employees", 
        names.arg = levels(mydata$WorkLifeBalance),# Alternate Option
        col = c("Red", "Blue","Green","Yellow")
        #col = c("#FF0000FF",3)  # Alternate Option using palettes
)
dev.off()

#32. YearsAtCompany which is quantitative data
#---Summerizarion
#----- mydata$YearsAtCompany 

#---------Central Tendency - mean, median, mode
print(paste("Mean is",mean(mydata$YearsAtCompany )))
print(paste("Median is",median(mydata$YearsAtCompany )))


#---------Spread - min, max, range, sd, var, IQR
print(paste("Minimum Years At Company is",min(mydata$YearsAtCompany )))
print(paste("Maximum Years At Company is",max(mydata$YearsAtCompany )))
print(paste("Range is from",min(mydata$YearsAtCompany),"To",max(mydata$YearsAtCompany)))
print(paste("Standard deviation is",sd(mydata$YearsAtCompany )))
print(paste("Variance is",var(mydata$YearsAtCompany )))
print(paste("Inter Quartile Range is",IQR(mydata$YearsAtCompany )))

#---------Shape - Five number summary

print(paste("Five Number Summary is as follows"))
print(fivenum(mydata$YearsAtCompany ))
print(paste("View summary"))
print(summary(mydata$YearsAtCompany ))

#---Visualization
#?par  # Set or Query More Graphical Parameters
png(filename = "YearsAtCompany.png",height=480, width= 960, unit ="px") 
par(mfrow = c(2,2))
#-----Histogram
#-----Years At Company wise breaks
hist(mydata$YearsAtCompany , breaks = c(0,5,10,15,20,25,30,35,40),
     main = "Histogram of Years At Company of employees",
     xlab = "Years At Company", ylab="No of Employees",
     col = c(1:8))
abline(v= mean(mydata$YearsAtCompany ), col = "Red")
abline(v= mean(mydata$YearsAtCompany )+2*sd(mydata$YearsAtCompany ), col = "Orange")


text(7,700, "Mean")
text(19,700, "Mean+2SD", col = "Red")

#-----Boxplot
boxplot(mydata$YearsAtCompany , col ="Red",
        main = "Boxplot of Years At Company of employees",
        xlab = "Years At Company", ylab="No of Emplyoees"
)  
text(1.3, fivenum(mydata$YearsAtCompany ),c("Min","Q1","Med","Q3","Max")) 

# Dot Chart
dotchart(mydata$YearsAtCompany ,col ="Red",
         main = "Dot Chart of Years At Company of employees",
         xlab = "Years At Company", ylab="No of Emplyoees")

# Density Plot
plot(density(mydata$YearsAtCompany ),col ="Red",
     main = "Density plot of Years At Company of employees",
     xlab = "Years At Company", ylab="No of Emplyoees")

dev.off()


#33. YearsInCurrentRole which is quantitative data
#---Summerizarion
#----- mydata$YearsInCurrentRole 

#---------Central Tendency - mean, median, mode
print(paste("Mean is",mean(mydata$YearsInCurrentRole )))
print(paste("Median is",median(mydata$YearsInCurrentRole )))


#---------Spread - min, max, range, sd, var, IQR
print(paste("Minimum Years In Current Role is",min(mydata$YearsInCurrentRole )))
print(paste("Maximum Years In Current Role is",max(mydata$YearsInCurrentRole )))
print(paste("Range is from",min(mydata$YearsInCurrentRole),"To",max(mydata$YearsInCurrentRole)))
print(paste("Standard deviation is",sd(mydata$YearsInCurrentRole )))
print(paste("Variance is",var(mydata$YearsInCurrentRole )))
print(paste("Inter Quartile Range is",IQR(mydata$YearsInCurrentRole )))

#---------Shape - Five number summary

print(paste("Five Number Summary is as follows"))
print(fivenum(mydata$YearsInCurrentRole ))
print(paste("View summary"))
print(summary(mydata$YearsInCurrentRole ))

#---Visualization
#?par  # Set or Query More Graphical Parameters
png(filename = "YearsInCurrentRole.png",height=480, width= 960, unit ="px") 
par(mfrow = c(2,2))
#-----Histogram
#-----Years In Current Role wise breaks
hist(mydata$YearsInCurrentRole , breaks = c(0,2,4,6,8,10,12,14,16,18,20),
     main = "Histogram of Years In Current Role of employees",
     xlab = "Years In Current Role", ylab="No of Employees",
     col = c(1:8))
abline(v= mean(mydata$YearsInCurrentRole ), col = "Red")
abline(v= mean(mydata$YearsInCurrentRole )+2*sd(mydata$YearsInCurrentRole ), col = "Orange")


text(4.2,300, "Mean")
text(11.2,300, "Mean+2SD", col = "Red")

#-----Boxplot
boxplot(mydata$YearsInCurrentRole , col ="Red",
        main = "Boxplot of Years In Current Role of employees",
        xlab = "Years In Current Role", ylab="No of Emplyoees"
)  
text(1.3, fivenum(mydata$YearsInCurrentRole ),c("Min","Q1","Med","Q3","Max")) 

# Dot Chart
dotchart(mydata$YearsInCurrentRole ,col ="Red",
         main = "Dot Chart of Years In Current Role of employees",
         xlab = "Years In Current Role", ylab="No of Emplyoees")

# Density Plot
plot(density(mydata$YearsInCurrentRole ),col ="Red",
     main = "Density plot of Years In Current Role of employees",
     xlab = "Years In Current Role", ylab="No of Emplyoees")

dev.off()


#34. YearsSinceLastPromotion which is quantitative data
#---Summerizarion
#----- mydata$YearsSinceLastPromotion 

#---------Central Tendency - mean, median, mode
print(paste("Mean is",mean(mydata$YearsSinceLastPromotion )))
print(paste("Median is",median(mydata$YearsSinceLastPromotion )))


#---------Spread - min, max, range, sd, var, IQR
print(paste("Minimum Years Since Last Promotion is",min(mydata$YearsSinceLastPromotion )))
print(paste("Maximum Years Since Last Promotion is",max(mydata$YearsSinceLastPromotion )))
print(paste("Range is from",min(mydata$YearsSinceLastPromotion),"To",max(mydata$YearsSinceLastPromotion)))
print(paste("Standard deviation is",sd(mydata$YearsSinceLastPromotion )))
print(paste("Variance is",var(mydata$YearsSinceLastPromotion )))
print(paste("Inter Quartile Range is",IQR(mydata$YearsSinceLastPromotion )))

#---------Shape - Five number summary

print(paste("Five Number Summary is as follows"))
print(fivenum(mydata$YearsSinceLastPromotion ))
print(paste("View summary"))
print(summary(mydata$YearsSinceLastPromotion ))

#---Visualization
#?par  # Set or Query More Graphical Parameters
png(filename = "YearsSinceLastPromotion.png",height=480, width= 960, unit ="px") 
par(mfrow = c(2,2))
#-----Histogram
#-----Years Since Last Promotion wise breaks
hist(mydata$YearsSinceLastPromotion , breaks = c(0,2,4,6,8,10,12,14,16),
     main = "Histogram of Years Since Last Promotion of employees",
     xlab = "Years Since Last Promotion", ylab="No of Employees",
     col = c(1:8))
abline(v= mean(mydata$YearsSinceLastPromotion ), col = "Red")
abline(v= mean(mydata$YearsSinceLastPromotion )+2*sd(mydata$YearsSinceLastPromotion ), col = "Orange")


text(3,800, "Mean")
text(8.4,800, "Mean+2SD", col = "Red")

#-----Boxplot
boxplot(mydata$YearsSinceLastPromotion , col ="Red",
        main = "Boxplot of Years Since Last Promotion of employees",
        xlab = "Years Since Last Promotion", ylab="No of Emplyoees"
)  
text(1.3, fivenum(mydata$YearsSinceLastPromotion ),c("Min","Q1","Med","Q3","Max")) 

# Dot Chart
dotchart(mydata$YearsSinceLastPromotion ,col ="Red",
         main = "Dot Chart of Years Since Last Promotion of employees",
         xlab = "Years Since Last Promotion", ylab="No of Emplyoees")

# Density Plot
plot(density(mydata$YearsSinceLastPromotion ),col ="Red",
     main = "Density plot of Years Since Last Promotion of employees",
     xlab = "Years Since Last Promotion", ylab="No of Emplyoees")

dev.off()

#35. YearsWithCurrManager which is quantitative data
#---Summerizarion
#----- mydata$YearsWithCurrManager 

#---------Central Tendency - mean, median, mode
print(paste("Mean is",mean(mydata$YearsWithCurrManager )))
print(paste("Median is",median(mydata$YearsWithCurrManager )))


#---------Spread - min, max, range, sd, var, IQR
print(paste("Minimum Years With Current Manager is",min(mydata$YearsWithCurrManager )))
print(paste("Maximum Years With Current Manager is",max(mydata$YearsWithCurrManager )))
print(paste("Range is from",min(mydata$YearsWithCurrManager),"To",max(mydata$YearsWithCurrManager)))
print(paste("Standard deviation is",sd(mydata$YearsWithCurrManager )))
print(paste("Variance is",var(mydata$YearsWithCurrManager )))
print(paste("Inter Quartile Range is",IQR(mydata$YearsWithCurrManager )))

#---------Shape - Five number summary

print(paste("Five Number Summary is as follows"))
print(fivenum(mydata$YearsWithCurrManager ))
print(paste("View summary"))
print(summary(mydata$YearsWithCurrManager ))

#---Visualization
#?par  # Set or Query More Graphical Parameters
png(filename = "YearsWithCurrManager.png",height=480, width= 960, unit ="px") 
par(mfrow = c(2,2))
#-----Histogram
#-----Years With Current Manager wise breaks
hist(mydata$YearsWithCurrManager , breaks = c(0,2,4,6,8,10,12,14,16,18),
     main = "Histogram of Years With Current Manager of employees",
     xlab = "Years With Current Manager", ylab="No of Employees",
     col = c(1:8))
abline(v= mean(mydata$YearsWithCurrManager ), col = "Red")
abline(v= mean(mydata$YearsWithCurrManager )+2*sd(mydata$YearsWithCurrManager ), col = "Orange")


text(4.1,500, "Mean")
text(11.1,500, "Mean+2SD", col = "Red")

#-----Boxplot
boxplot(mydata$YearsWithCurrManager , col ="Red",
        main = "Boxplot of Years With Current Manager of employees",
        xlab = "Years With Current Manager", ylab="No of Emplyoees"
)  
text(1.3, fivenum(mydata$YearsWithCurrManager ),c("Min","Q1","Med","Q3","Max")) 

# Dot Chart
dotchart(mydata$YearsWithCurrManager ,col ="Red",
         main = "Dot Chart of Years With Current Manager of employees",
         xlab = "Years With Current Manager", ylab="No of Emplyoees")

# Density Plot
plot(density(mydata$YearsWithCurrManager ),col ="Red",
     main = "Density plot of Years With Current Manager of employees",
     xlab = "Years With Current Manager", ylab="No of Emplyoees")

dev.off()
print("After an EDA of all Univariates we will need to do EDA and TOH of all other variables
      against Attrition which will give us an idea about some factors causing an Attrition.")
setwd("C://Users//DELL//Documents//Attrition")

