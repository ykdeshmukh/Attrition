#We are going to use multiple alorithms as many as available
#And after testing all the algorithms, whichever is giving 
#us the best value, we will select it as our Final Model.
#So This file will cover Supervised Approach.
#For Supervised Approach We are going to use following as our Y-Values
#1.For Regression Y is MonthlyIncome (Q)
#2.For Classification Y is Attrition (C)

setwd("C:\\Users\\DELL\\Documents\\Attrition")
source("source.R")

#Here before starting Regression, we want to save all plots to Regression plot folder
#So set the working directory accordingly and then at the end of 
#Regression, we will set it back to "C://Users//DELL//Documents//Attrition"

#Create New Data Frame and assign all the data to it First
new_data <- data.frame(read.csv("New_Attrition_Data.csv",na.strings = "."))

setwd("C://Users//DELL//Documents//Attrition//_plots//Regression")

#------Regression
#-------1.Multilinear Regression (MLR)
###---------Model-1
Att_reg_model_1 <- lm(MonthlyIncome~., data =new_data)

coef(Att_reg_model_1)  

#Measures

#TSS (Sum(Y-Ybar)^2)
TSS1 <- sum((new_data$MonthlyIncome-mean(new_data$MonthlyIncome))^2)
print(paste("Total Sum of Square is: ",TSS1))        

#RSS ((Sum((Y-Yhat)^2))
yhat1<- Att_reg_model_1$fitted.values
RSS1 <- sum((new_data$MonthlyIncome-yhat1)^2)
print(RSS1)
#Easier way to calculate RSS
RSS1a <- sum((Att_reg_model_1$residuals)^2)
print(paste("Residual Sum of Square is: ",RSS1a)) 


#Rsq (TSS- RSS)/TSS
Rsq1 <- (TSS1- RSS1)/TSS1
print(paste("Residual Square is: ",Rsq1))  # Which shows that This much % of error is reduced
#Easiear way to calculate RSquare
summary(Att_reg_model_1)$r.squared

#Adj Rsq
Rsq_adj <- summary(Att_reg_model_1)$adj.r.squared
print(paste("Residual Square adjusted is: ",Rsq_adj))

#RSE-Residual standard Error
RSE <- summary(Att_reg_model_1)$sigma
print(paste("Residual standard Error is: ",RSE))

#Summary
summary(Att_reg_model_1)  # It gives all the above values directly

#So if we check all variables against Monthly Income, we get following observations.
#Residual standard error: 1123 on 1429 degrees of freedom
#Multiple R-squared:  0.9446,	Adjusted R-squared:  0.9431 
#F-statistic: 609.6 on 40 and 1429 DF,  p-value: < 2.2e-16
#From above observations, we cme to know that 
#JobRole, JobLevel and TotalWorkingYears got *** Rating.
#While YearsSinceLastPromotion, YearsWithCurrManager and JobInvolvement got * Rating
#And BusinessTravel has got . Rating.
#So, now we will check only these variables against MonthlyIncome.

#Homoscedastic (mean(residual)=0, var= constant, residual =normal)
mean(Att_reg_model_1$residuals) # almost 0
print(shapiro.test(Att_reg_model_1$residuals)) # Ho: Normal, Ha: Not Normal
#From above test (i.e. P-Value < 0.05) it is clear that this model is not normal.
  

#---Visualization
png(filename = "MLR_Initial_Model.png",height=480, width= 960, unit ="px")
par(mfrow = c(2,3))
plot(Att_reg_model_1,1)# Gives us Residuals Vs Fitted Values 
plot(Att_reg_model_1,2)# Gives us Standrdized Residuals Vs Theoretical Quantiles
plot(Att_reg_model_1,3)# Gives us sq. root of Standrdized Residuals Vs Fitted Values
plot(Att_reg_model_1,4)# Gives us Cook's Distance
plot(Att_reg_model_1,5)# Gives us Standrdized Residuals Vs Leverage
plot(Att_reg_model_1,6)# Gives us Cook's Distance Vs Leverage
dev.off()


###---------Model-2 (MonthlyIncome Vs *** rated variables)
Att_reg_model_2 <- lm(MonthlyIncome~JobRole+JobLevel+TotalWorkingYears, data =new_data)

print(summary(Att_reg_model_2))

#Residual standard error: 1123 on 1459 degrees of freedom
#Multiple R-squared:  0.9434,	Adjusted R-squared:  0.9431 
#F-statistic:  2434 on 10 and 1459 DF,  p-value: < 2.2e-16 


###---------Model-3 (MonthlyIncome Vs Model-2 + * rated variables)
Att_reg_model_3 <- lm(MonthlyIncome~JobRole+JobLevel+TotalWorkingYears
                      +YearsSinceLastPromotion+YearsWithCurrManager
                      +JobInvolvement, data =new_data)

print(summary(Att_reg_model_3))


#Residual standard error: 1119 on 1456 degrees of freedom
#Multiple R-squared:  0.944,	Adjusted R-squared:  0.9435 
#F-statistic:  1888 on 13 and 1456 DF,  p-value: < 2.2e-16
#Homoscedastic (mean(residual)=0, var= constant, residual =normal)
mean(Att_reg_model_3$residuals) # almost 0
print(shapiro.test(Att_reg_model_3$residuals)) # Ho: Normal, Ha: Not Normal

#---Visualization
png(filename = "MLR_Final_Model.png",height=480, width= 960, unit ="px")
par(mfrow = c(2,3))
plot(Att_reg_model_3,1)# Gives us Residuals Vs Fitted Values 
plot(Att_reg_model_3,2)# Gives us Standrdized Residuals Vs Theoretical Quantiles
plot(Att_reg_model_3,3)# Gives us sq. root of Standrdized Residuals Vs Fitted Values
plot(Att_reg_model_3,4)# Gives us Cook's Distance
plot(Att_reg_model_3,5)# Gives us Standrdized Residuals Vs Leverage
plot(Att_reg_model_3,6)# Gives us Cook's Distance Vs Leverage
dev.off()

###---------Model-4 (MonthlyIncome Vs Model-3 + . rated variables)
Att_reg_model_4 <- lm(MonthlyIncome~JobRole+JobLevel+TotalWorkingYears
                      +YearsSinceLastPromotion+YearsWithCurrManager
                      +JobInvolvement+BusinessTravel, data =new_data)

print(summary(Att_reg_model_4))


#Residual standard error: 1119 on 1454 degrees of freedom
#Multiple R-squared:  0.9441,	Adjusted R-squared:  0.9435 
#F-statistic:  1638 on 15 and 1454 DF,  p-value: < 2.2e-16

#Homoscedastic (mean(residual)=0, var= constant, residual =normal)
mean(Att_reg_model_4$residuals) # almost 0
print(shapiro.test(Att_reg_model_4$residuals)) # Ho: Normal, Ha: Not Normal


#Now lets do Anova Test to decide which model is Best out 4
anova(Att_reg_model_1,Att_reg_model_2,Att_reg_model_3,Att_reg_model_4)
print("So Finally If we want to use Multi-linear Regression, then third Model is the Best.")

#Predictions
print("Following are the predictions of what should be the Monthly Income of 4 employees:")
print(predict(Att_reg_model_3, newdata = data.frame(JobRole=c('Research Scientist',
                                                        'Laboratory Technician',
                                                        'Manager', 'Sales Representative'),
                                              JobLevel=c(1,2,3,4),
                                              TotalWorkingYears=c(8,10,7,8),
                                              YearsSinceLastPromotion=c(5,7,3,6),
                                              YearsWithCurrManager=c(4,6,5,1),
                                              JobInvolvement=c(3,3,2,4)
                                              )))


#2.------Subset Regression
library(leaps) # For subset regression
# Exhaustive search, forward or backward stepwise, or sequential replacement
# method="exhaustive", "backward", "forward", "seqrep"

par(mfrow=c(1,1))
regfit.fwd=regsubsets(MonthlyIncome~JobRole+JobLevel+TotalWorkingYears
                      +YearsSinceLastPromotion+YearsWithCurrManager
                      +JobInvolvement+BusinessTravel,
                       data = new_data,nbest=1,force.in = NULL, force.out = NULL,nvmax=NULL,method="exhaustive")
reg.summary<-summary(regfit.fwd)
reg.summary$rss
reg.summary$rsq
reg.summary$adjr2
plot(reg.summary$rss,xlab="Number of Variables",ylab = "RSS",type = "l")
plot(reg.summary$rsq,xlab="Number of Variables",ylab = "RSq",type="l")
plot(reg.summary$adjr2,xlab="Number of Variables",
     ylab = "Adjusted RSq",type="l")
max.pt<-which.max(reg.summary$adjr2)
max.pt
points(max.pt,reg.summary$adjr2[max.pt],col="red",cex=2,pch=20)
reg.summary
png(filename = "SubsetForward.png",height=480, width= 1080, unit ="px")
plot(regfit.fwd,scale="adjr2")
dev.off()
#Note that, this method is computationally expensive and becomes unfeasible for a large data set with many variables.
#A better alternative is provided by the stepwise regression method. 


#-------3.Ridge Regression 
#---------A.Sampling without Replacement Crosss Validation)
#---------B.Sampling with Replacement (BootStrap Method)
X <- model.matrix(MonthlyIncome~.,data = new_data)
head(X)
X <- X[,-1] # Remove Intercept
head(X)
y <- new_data$MonthlyIncome

set.seed(1)
train <- sample(1:nrow(X),0.70*nrow(X))
test <- (-train)

X.train <- X[train,]
X.test <- X[test,]
y.train <- y[train]
y.test <- y[test]

ridge.mod <- glmnet(X.train, y.train, alpha = 0, lambda = 20)
beta20 <- ridge.mod$beta
coef(ridge.mod)
#-------4.LASSO Regression 
#---------A.Sampling without Replacement Crosss Validation)
#---------B.Sampling with Replacement (BootStrap Method)


#-------5. Time Series - Model="ETS"
#--------- "E"=Error Type,"T"=Trend Type,"S"=Season Type 
#--------- "N"=None,"A"=Additive,"M"=Multiplicative,"Z"=Automatically Selected 
#--------- ANN - Simple Exponential Smoothing
#--------- AAA - All components Additive 
#--------- MMM - All components multiplicative
#--------- MAM - multiplicative Holt-Winters' method with multiplicative errors
#--------- Holt Winters consider Level, trend and Seasonality

#---------A.Sampling without Replacement Crosss Validation)
#---------B.Sampling with Replacement (BootStrap Method)


#------6. ANN-Artificial Neural Network
