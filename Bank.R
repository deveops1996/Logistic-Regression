library(readr)
library(ggplot2)
library(AER)
library(plyr)
library(ROCR)
library(dplyr)
library(GGally)
library(car)
library(corpcor)







bank_full <- read_csv("D:/DataScience/Logistic Regression/bank-full.csv")
View(bank_full)
sum(is.na(bank_full))

dim(bank_full)

colnames(bank_full)
View(bank_full)
attach(bank_full)


pairs(bank_full)
ggpairs(bank_full)

summary(bank_full)
str(bank_full)

View(bank_full)
bank_full$y <- revalue(bank_full$y, c("yes"=1))
bank_full$y <- revalue(bank_full$y, c("no"=0))
bank_full$y <- as.numeric(bank_full$y)


# We can also include NA values but where ever it finds NA value
# probability values obtained using the glm will also be NA 
# So they can be either filled using imputation technique or
# exlclude those values 


# GLM function use sigmoid curve to produce desirable results 
# The output of sigmoid function lies in between 0-1

View(bank_full)
model <- glm(y~.,data=bank_full,family = "binomial")
model <- glm(y~job, data = bank_full, family = "binomial")
model <- glm(y~marital, data = bank_full, family = "binomial")
model <- glm(y~default, data = bank_full, family = "binomial")
model <- glm(y~balance, data = bank_full, family = "binomial")
model <- glm(y~housing, data = bank_full, family = "binomial")
model <- glm(y~loan, data = bank_full, family = "binomial")
model <- glm(y~balance+job, data = bank_full, family = "binomial")


# To calculate the odds ratio manually we going r going to take exp of coef(model)
exp(coef(model))

# Confusion matrix table 
prob <- predict(model,bank_full,type="response")
summary(model)
# We are going to use NULL and Residual Deviance to compare the between different models

# Confusion matrix and considering the threshold value as 0.5 
confusion<-table(prob>0.5,bank_full$y)
confusion
# Model Accuracy 
Accuracy<-sum(diag(confusion)/sum(confusion))
Accuracy


# Creating empty vectors to store predicted classes based on threshold value
pred_values <- NULL
yes_no <- NULL

pred_values <- ifelse(prob>=0.5,1,0)
yes_no <- ifelse(prob>=0.5,"yes","no")

# Creating new column to store the above values
bank_full[,"prob"] <- prob
bank_full[,"pred_values"] <- pred_values
bank_full[,"yes_no"] <- yes_no

View(bank_full[,c(0,18:20)])

table(bank_full$y,bank_full$pred_values)
# Calculate the below metrics
# precision | recall | True Positive Rate | False Positive Rate | Specificity | Sensitivity
# from the above table - 1833


# ROC Curve => used to evaluate the betterness of the logistic model
# more area under ROC curve better is the model 
# We will use ROC curve for any classification technique not only for logistic
library(ROCR)
rocrpred<-prediction(prob,bank_full$y)
rocrperf<-performance(rocrpred,'tpr','fpr')

str(rocrperf)

plot(rocrperf,colorize=T,text.adj=c(-0.2,1.7))
# More area under the ROC Curve better is the logistic regression model obtained

## Getting cutt off or threshold value along with true positive and false positive rates in a data frame 
str(rocrperf)
rocr_cutoff <- data.frame(cut_off = rocrperf@alpha.values[[1]],fpr=rocrperf@x.values,tpr=rocrperf@y.values)
colnames(rocr_cutoff) <- c("cut_off","FPR","TPR")
View(rocr_cutoff)

library(dplyr)
rocr_cutoff$cut_off <- round(rocr_cutoff$cut_off,6)
# Sorting data frame with respect to tpr in decreasing order 
rocr_cutoff <- arrange(rocr_cutoff,desc(TPR))
View(rocr_cutoff)


#Final Model


final_model <- glm(y~.,data=bank_full,family = "binomial")

# To calculate the odds ratio manually we going r going to take exp of coef(model)
exp(coef(final_model))

# Confusion matrix table 
prob <- predict(final_model,bank_full,type="response")
summary(model)
# We are going to use NULL and Residual Deviance to compare the between different models

# Confusion matrix and considering the threshold value as 0.5 
confusion<-table(prob>0.5,bank_full$y)
confusion
# Model Accuracy 
Accuracy<-sum(diag(confusion)/sum(confusion))
Accuracy


# Creating empty vectors to store predicted classes based on threshold value
pred_values <- NULL
yes_no <- NULL

pred_values <- ifelse(prob>=0.5,1,0)
yes_no <- ifelse(prob>=0.5,"yes","no")

# Creating new column to store the above values
bank_full[,"prob"] <- prob
bank_full[,"pred_values"] <- pred_values
bank_full[,"yes_no"] <- yes_no

View(bank_full[,c(0,18:20)])

table(bank_full$y,bank_full$pred_values)
# Calculate the below metrics
# precision | recall | True Positive Rate | False Positive Rate | Specificity | Sensitivity
# from the above table - 1833


# ROC Curve => used to evaluate the betterness of the logistic model
# more area under ROC curve better is the model 
# We will use ROC curve for any classification technique not only for logistic
library(ROCR)
rocrpred<-prediction(prob,bank_full$y)
rocrperf<-performance(rocrpred,'tpr','fpr')

str(rocrperf)

plot(rocrperf,colorize=T,text.adj=c(-0.2,1.7))
# More area under the ROC Curve better is the logistic regression model obtained

## Getting cutt off or threshold value along with true positive and false positive rates in a data frame 
str(rocrperf)
rocr_cutoff <- data.frame(cut_off = rocrperf@alpha.values[[1]],fpr=rocrperf@x.values,tpr=rocrperf@y.values)
colnames(rocr_cutoff) <- c("cut_off","FPR","TPR")
View(rocr_cutoff)

library(dplyr)
rocr_cutoff$cut_off <- round(rocr_cutoff$cut_off,6)
# Sorting data frame with respect to tpr in decreasing order 
rocr_cutoff <- arrange(rocr_cutoff,desc(TPR))
View(rocr_cutoff)

