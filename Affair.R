library(ggplot2)
library(AER)
library(plyr)
library(ROCR)
library(dplyr)
library(GGally)
library(car)




data(Affairs,package="AER")

sum(is.na(Affairs))
Affairs <- na.omit(Affairs) # Omitting NA values from the Data 
# na.omit => will omit the rows which has atleast 1 NA value
dim(Affairs)

colnames(Affairs)
View(Affairs)
attach(Affairs)
#Affairs <- claimants[,-1] # Removing the first column which is is an Index

pairs(Affairs)
ggpairs(Affairs)

summary(Affairs)
boxplot(affairs)
boxplot(age)
boxplot(yearsmarried)
boxplot(religiousness)
boxplot(education)
boxplot(occupation)
Affairs$affairs[Affairs$affairs > 0] <- 1
Affairs$affairs[Affairs$affairs == 0] <- 0
Affairs$gender <- as.factor(revalue(Affairs$gender,c("male"=0, "female"=1)))
Affairs$children <- as.factor(revalue(Affairs$children,c("yes"=1, "no"=0)))

View(Affairs)

# Preparing a linear regression 

mod_lm <- lm(affairs~.,data=Affairs)
pred1 <- predict(mod_lm,Affairs)
pred1
plot(Affairs$affairs,pred1)
# We can no way use the linear regression technique to classify the data
plot(pred1)


# We can also include NA values but where ever it finds NA value
# probability values obtained using the glm will also be NA 
# So they can be either filled using imputation technique or
# exlclude those values 


# GLM function use sigmoid curve to produce desirable results 
# The output of sigmoid function lies in between 0-1
model_1 <- glm(affairs~.,data=Affairs,family = "binomial")
influencePlot(model_1)
# To calculate the odds ratio manually we going r going to take exp of coef(model)
exp(coef(model_1))
#?exp

# Confusion matrix table 
prob <- predict(model_1,Affairs,type="response")
summary(model_1)
plot(prob)
# We are going to use NULL and Residual Deviance to compare the between different models

# Confusion matrix and considering the threshold value as 0.5 
confusion<-table(prob>0.5,Affairs$affairs)
confusion
# Model Accuracy 
Accuracy<-sum(diag(confusion)/sum(confusion))
Accuracy # 76.53


# Creating empty vectors to store predicted classes based on threshold value
pred_values <- NULL
yes_no <- NULL

pred_values <- ifelse(prob>=0.5,1,0)
yes_no <- ifelse(prob>=0.5,"yes","no")

# Creating new column to store the above values
Affairs[,"prob"] <- prob
Affairs[,"pred_values"] <- pred_values
Affairs[,"yes_no"] <- yes_no

View(Affairs[,c(1,10:12)])

table(Affairs$affairs,Affairs$pred_values)


# ROC Curve => used to evaluate the betterness of the logistic model
# more area under ROC curve better is the model 
# We will use ROC curve for any classification technique not only for logistic

rocrpred<-prediction(prob,Affairs$affairs)
rocrperf<-performance(rocrpred,'tpr','fpr')

str(rocrperf)

plot(rocrperf,colorize=T,text.adj=c(-0.2,1.7))
# More area under the ROC Curve better is the logistic regression model obtained

## Getting cutt off or threshold value along with true positive and false positive rates in a data frame 
str(rocrperf)
rocr_cutoff <- data.frame(cut_off = rocrperf@alpha.values[[1]],fpr=rocrperf@x.values,tpr=rocrperf@y.values)
colnames(rocr_cutoff) <- c("cut_off","FPR","TPR")
View(rocr_cutoff)


rocr_cutoff$cut_off <- round(rocr_cutoff$cut_off,6)
# Sorting data frame with respect to tpr in decreasing order 
rocr_cutoff <- arrange(rocr_cutoff,desc(TPR))
View(rocr_cutoff)





# GLM function use sigmoid curve to produce desirable results 
# The output of sigmoid function lies in between 0-1
model_2 <- glm(affairs~rating,data=Affairs,family = "binomial")
summary(model_2)
influencePlot(model_2)
model_2 <- glm(affairs~rating,data=Affairs,family = "binomial")
# To calculate the odds ratio manually we going r going to take exp of coef(model)
exp(coef(model_2))

# Confusion matrix table 
prob <- predict(model_2,Affairs,type="response")
prob
summary(model_2)
plot(prob)
# We are going to use NULL and Residual Deviance to compare the between different models

# Confusion matrix and considering the threshold value as 0.5 
confusion<-table(prob>0.5,Affairs$affairs)
confusion
# Model Accuracy 
Accuracy<-sum(diag(confusion)/sum(confusion))
Accuracy # 76.53


# Creating empty vectors to store predicted classes based on threshold value
pred_values <- NULL
yes_no <- NULL

pred_values <- ifelse(prob>=0.5,1,0)
yes_no <- ifelse(prob>=0.5,"yes","no")

# Creating new column to store the above values
Affairs[,"prob"] <- prob
Affairs[,"pred_values"] <- pred_values
Affairs[,"yes_no"] <- yes_no

View(Affairs[,c(1,10:12)])

table(Affairs$affairs,Affairs$pred_values)
# Calculate the below metrics
# precision | recall | True Positive Rate | False Positive Rate | Specificity | Sensitivity
# from the above table - 59


# ROC Curve => used to evaluate the betterness of the logistic model
# more area under ROC curve better is the model 
# We will use ROC curve for any classification technique not only for logistic

rocrpred<-prediction(prob,Affairs$affairs)
rocrperf<-performance(rocrpred,'tpr','fpr')

str(rocrperf)

plot(rocrperf,colorize=T,text.adj=c(-0.2,1.7))
# More area under the ROC Curve better is the logistic regression model obtained

## Getting cutt off or threshold value along with true positive and false positive rates in a data frame 
str(rocrperf)
rocr_cutoff <- data.frame(cut_off = rocrperf@alpha.values[[1]],fpr=rocrperf@x.values,tpr=rocrperf@y.values)
colnames(rocr_cutoff) <- c("cut_off","FPR","TPR")
View(rocr_cutoff)


rocr_cutoff$cut_off <- round(rocr_cutoff$cut_off,6)
# Sorting data frame with respect to tpr in decreasing order 
rocr_cutoff <- arrange(rocr_cutoff,desc(TPR))
View(rocr_cutoff)




# GLM function use sigmoid curve to produce desirable results 
# The output of sigmoid function lies in between 0-1
model_3 <- glm(affairs~rating+religiousness,data=Affairs,family = "binomial")
summary(model_3)
# To calculate the odds ratio manually we going r going to take exp of coef(model)
exp(coef(model_3))

# Confusion matrix table 
prob <- predict(model_3,Affairs,type="response")
prob
summary(model_3)
plot(prob)
# We are going to use NULL and Residual Deviance to compare the between different models

# Confusion matrix and considering the threshold value as 0.5 
confusion<-table(prob>0.5,Affairs$affairs)
confusion
# Model Accuracy 
Accuracy<-sum(diag(confusion)/sum(confusion))
Accuracy # 76.53


# Creating empty vectors to store predicted classes based on threshold value
pred_values <- NULL
yes_no <- NULL

pred_values <- ifelse(prob>=0.5,1,0)
yes_no <- ifelse(prob>=0.5,"yes","no")

# Creating new column to store the above values
Affairs[,"prob"] <- prob
Affairs[,"pred_values"] <- pred_values
Affairs[,"yes_no"] <- yes_no

View(Affairs[,c(1,10:12)])

table(Affairs$affairs,Affairs$pred_values)
# Calculate the below metrics
# precision | recall | True Positive Rate | False Positive Rate | Specificity | Sensitivity
# from the above table - 59


# ROC Curve => used to evaluate the betterness of the logistic model
# more area under ROC curve better is the model 
# We will use ROC curve for any classification technique not only for logistic

rocrpred<-prediction(prob,Affairs$affairs)
rocrperf<-performance(rocrpred,'tpr','fpr')

str(rocrperf)

plot(rocrperf,colorize=T,text.adj=c(-0.2,1.7))
# More area under the ROC Curve better is the logistic regression model obtained

## Getting cutt off or threshold value along with true positive and false positive rates in a data frame 
str(rocrperf)
rocr_cutoff <- data.frame(cut_off = rocrperf@alpha.values[[1]],fpr=rocrperf@x.values,tpr=rocrperf@y.values)
colnames(rocr_cutoff) <- c("cut_off","FPR","TPR")
View(rocr_cutoff)


rocr_cutoff$cut_off <- round(rocr_cutoff$cut_off,6)
# Sorting data frame with respect to tpr in decreasing order 
rocr_cutoff <- arrange(rocr_cutoff,desc(TPR))
View(rocr_cutoff)





# GLM function use sigmoid curve to produce desirable results 
# The output of sigmoid function lies in between 0-1
model_4 <- glm(affairs~yearsmarried+rating+religiousness,data=Affairs,family = "binomial")
summary(model_4)
# To calculate the odds ratio manually we going r going to take exp of coef(model)
exp(coef(model_4))

# Confusion matrix table 
prob <- predict(model_3,Affairs,type="response")
prob
summary(model_4)
plot(prob)
# We are going to use NULL and Residual Deviance to compare the between different models

# Confusion matrix and considering the threshold value as 0.5 
confusion<-table(prob>0.5,Affairs$affairs)
confusion
# Model Accuracy 
Accuracy<-sum(diag(confusion)/sum(confusion))
Accuracy # 76.53


# Creating empty vectors to store predicted classes based on threshold value
pred_values <- NULL
yes_no <- NULL

pred_values <- ifelse(prob>=0.5,1,0)
yes_no <- ifelse(prob>=0.5,"yes","no")

# Creating new column to store the above values
Affairs[,"prob"] <- prob
Affairs[,"pred_values"] <- pred_values
Affairs[,"yes_no"] <- yes_no

View(Affairs[,c(1,10:12)])

table(Affairs$affairs,Affairs$pred_values)
# Calculate the below metrics
# precision | recall | True Positive Rate | False Positive Rate | Specificity | Sensitivity
# from the above table - 59


# ROC Curve => used to evaluate the betterness of the logistic model
# more area under ROC curve better is the model 
# We will use ROC curve for any classification technique not only for logistic

rocrpred<-prediction(prob,Affairs$affairs)
rocrperf<-performance(rocrpred,'tpr','fpr')

str(rocrperf)

plot(rocrperf,colorize=T,text.adj=c(-0.2,1.7))
# More area under the ROC Curve better is the logistic regression model obtained

## Getting cutt off or threshold value along with true positive and false positive rates in a data frame 
str(rocrperf)
rocr_cutoff <- data.frame(cut_off = rocrperf@alpha.values[[1]],fpr=rocrperf@x.values,tpr=rocrperf@y.values)
colnames(rocr_cutoff) <- c("cut_off","FPR","TPR")
View(rocr_cutoff)


rocr_cutoff$cut_off <- round(rocr_cutoff$cut_off,6)
# Sorting data frame with respect to tpr in decreasing order 
rocr_cutoff <- arrange(rocr_cutoff,desc(TPR))
View(rocr_cutoff)




# GLM function use sigmoid curve to produce desirable results 
# The output of sigmoid function lies in between 0-1
model_5 <- glm(affairs~education+rating+religiousness,data=Affairs,family = "binomial")
summary(model_5)
# To calculate the odds ratio manually we going r going to take exp of coef(model)
exp(coef(model_5))

# Confusion matrix table 
prob <- predict(model_5,Affairs,type="response")
prob
summary(model_3)
plot(prob)
# We are going to use NULL and Residual Deviance to compare the between different models

# Confusion matrix and considering the threshold value as 0.5 
confusion<-table(prob>0.5,Affairs$affairs)
confusion
# Model Accuracy 
Accuracy<-sum(diag(confusion)/sum(confusion))
Accuracy # 76.53


# Creating empty vectors to store predicted classes based on threshold value
pred_values <- NULL
yes_no <- NULL

pred_values <- ifelse(prob>=0.5,1,0)
yes_no <- ifelse(prob>=0.5,"yes","no")

# Creating new column to store the above values
Affairs[,"prob"] <- prob
Affairs[,"pred_values"] <- pred_values
Affairs[,"yes_no"] <- yes_no

View(Affairs[,c(1,10:12)])

table(Affairs$affairs,Affairs$pred_values)
# Calculate the below metrics
# precision | recall | True Positive Rate | False Positive Rate | Specificity | Sensitivity
# from the above table - 59


# ROC Curve => used to evaluate the betterness of the logistic model
# more area under ROC curve better is the model 
# We will use ROC curve for any classification technique not only for logistic

rocrpred<-prediction(prob,Affairs$affairs)
rocrperf<-performance(rocrpred,'tpr','fpr')

str(rocrperf)

plot(rocrperf,colorize=T,text.adj=c(-0.2,1.7))
# More area under the ROC Curve better is the logistic regression model obtained

## Getting cutt off or threshold value along with true positive and false positive rates in a data frame 
str(rocrperf)
rocr_cutoff <- data.frame(cut_off = rocrperf@alpha.values[[1]],fpr=rocrperf@x.values,tpr=rocrperf@y.values)
colnames(rocr_cutoff) <- c("cut_off","FPR","TPR")
View(rocr_cutoff)


rocr_cutoff$cut_off <- round(rocr_cutoff$cut_off,6)
# Sorting data frame with respect to tpr in decreasing order 
rocr_cutoff <- arrange(rocr_cutoff,desc(TPR))
View(rocr_cutoff)





# GLM function use sigmoid curve to produce desirable results 
# The output of sigmoid function lies in between 0-1
model_7 <- glm(affairs~education+occupation+rating+gender,data=Affairs,family = "binomial")
summary(model_7)
# To calculate the odds ratio manually we going r going to take exp of coef(model)
exp(coef(model_7))

# Confusion matrix table 
prob <- predict(model_7,Affairs,type="response")
prob
summary(model_7)
plot(prob)
# We are going to use NULL and Residual Deviance to compare the between different models

# Confusion matrix and considering the threshold value as 0.5 
confusion<-table(prob>0.5,Affairs$affairs)
confusion
# Model Accuracy 
Accuracy<-sum(diag(confusion)/sum(confusion))
Accuracy # 76.53


# Creating empty vectors to store predicted classes based on threshold value
pred_values <- NULL
yes_no <- NULL

pred_values <- ifelse(prob>=0.5,1,0)
yes_no <- ifelse(prob>=0.5,"yes","no")

# Creating new column to store the above values
Affairs[,"prob"] <- prob
Affairs[,"pred_values"] <- pred_values
Affairs[,"yes_no"] <- yes_no

View(Affairs[,c(1,10:12)])

table(Affairs$affairs,Affairs$pred_values)


# ROC Curve => used to evaluate the betterness of the logistic model
# more area under ROC curve better is the model 
# We will use ROC curve for any classification technique not only for logistic

rocrpred<-prediction(prob,Affairs$affairs)
rocrperf<-performance(rocrpred,'tpr','fpr')

str(rocrperf)

plot(rocrperf,colorize=T,text.adj=c(-0.2,1.7))
# More area under the ROC Curve better is the logistic regression model obtained

## Getting cutt off or threshold value along with true positive and false positive rates in a data frame 
str(rocrperf)
rocr_cutoff <- data.frame(cut_off = rocrperf@alpha.values[[1]],fpr=rocrperf@x.values,tpr=rocrperf@y.values)
colnames(rocr_cutoff) <- c("cut_off","FPR","TPR")
View(rocr_cutoff)


rocr_cutoff$cut_off <- round(rocr_cutoff$cut_off,6)
# Sorting data frame with respect to tpr in decreasing order 
rocr_cutoff <- arrange(rocr_cutoff,desc(TPR))
View(rocr_cutoff)




# GLM function use sigmoid curve to produce desirable results 
# The output of sigmoid function lies in between 0-1
model_6 <- glm(affairs~education+yearsmarried+occupation+rating+religiousness,data=Affairs,family = "binomial")
summary(model_6)
# To calculate the odds ratio manually we going r going to take exp of coef(model)
exp(coef(model_6))

# Confusion matrix table 
prob <- predict(model_6,Affairs,type="response")
prob
summary(model_6)
plot(prob)
# We are going to use NULL and Residual Deviance to compare the between different models

# Confusion matrix and considering the threshold value as 0.5 
confusion<-table(prob>0.5,Affairs$affairs)
confusion
# Model Accuracy 
Accuracy<-sum(diag(confusion)/sum(confusion))
Accuracy # 76.53


# Creating empty vectors to store predicted classes based on threshold value
pred_values <- NULL
yes_no <- NULL

pred_values <- ifelse(prob>=0.5,1,0)
yes_no <- ifelse(prob>=0.5,"yes","no")

# Creating new column to store the above values
Affairs[,"prob"] <- prob
Affairs[,"pred_values"] <- pred_values
Affairs[,"yes_no"] <- yes_no

View(Affairs[,c(1,10:12)])

table(Affairs$affairs,Affairs$pred_values)


# ROC Curve => used to evaluate the betterness of the logistic model
# more area under ROC curve better is the model 
# We will use ROC curve for any classification technique not only for logistic

rocrpred<-prediction(prob,Affairs$affairs)
rocrperf<-performance(rocrpred,'tpr','fpr')

str(rocrperf)

plot(rocrperf,colorize=T,text.adj=c(-0.2,1.7))
# More area under the ROC Curve better is the logistic regression model obtained

## Getting cutt off or threshold value along with true positive and false positive rates in a data frame 
str(rocrperf)
rocr_cutoff <- data.frame(cut_off = rocrperf@alpha.values[[1]],fpr=rocrperf@x.values,tpr=rocrperf@y.values)
colnames(rocr_cutoff) <- c("cut_off","FPR","TPR")
View(rocr_cutoff)


rocr_cutoff$cut_off <- round(rocr_cutoff$cut_off,6)
# Sorting data frame with respect to tpr in decreasing order 
rocr_cutoff <- arrange(rocr_cutoff,desc(TPR))
View(rocr_cutoff)






# GLM function use sigmoid curve to produce desirable results 
# The output of sigmoid function lies in between 0-1
model_final <- glm(affairs~.,data=Affairs,family = "binomial")
summary(model_final)
# To calculate the odds ratio manually we going r going to take exp of coef(model)
exp(coef(model_final))

# Confusion matrix table 
prob <- predict(model_6,Affairs,type="response")
prob
summary(model_6)
plot(prob)
# We are going to use NULL and Residual Deviance to compare the between different models

# Confusion matrix and considering the threshold value as 0.5 
confusion<-table(prob>0.5,Affairs$affairs)
confusion
# Model Accuracy 
Accuracy<-sum(diag(confusion)/sum(confusion))
Accuracy # 76.53


# Creating empty vectors to store predicted classes based on threshold value
pred_values <- NULL
yes_no <- NULL

pred_values <- ifelse(prob>=0.5,1,0)
yes_no <- ifelse(prob>=0.5,"yes","no")

# Creating new column to store the above values
Affairs[,"prob"] <- prob
Affairs[,"pred_values"] <- pred_values
Affairs[,"yes_no"] <- yes_no

View(Affairs[,c(1,10:12)])

table(Affairs$affairs,Affairs$pred_values)


# ROC Curve => used to evaluate the betterness of the logistic model
# more area under ROC curve better is the model 
# We will use ROC curve for any classification technique not only for logistic

rocrpred<-prediction(prob,Affairs$affairs)
rocrperf<-performance(rocrpred,'tpr','fpr')

str(rocrperf)

plot(rocrperf,colorize=T,text.adj=c(-0.2,1.7))
# More area under the ROC Curve better is the logistic regression model obtained

## Getting cutt off or threshold value along with true positive and false positive rates in a data frame 
str(rocrperf)
rocr_cutoff <- data.frame(cut_off = rocrperf@alpha.values[[1]],fpr=rocrperf@x.values,tpr=rocrperf@y.values)
colnames(rocr_cutoff) <- c("cut_off","FPR","TPR")
View(rocr_cutoff)


rocr_cutoff$cut_off <- round(rocr_cutoff$cut_off,6)
# Sorting data frame with respect to tpr in decreasing order 
rocr_cutoff <- arrange(rocr_cutoff,desc(TPR))
View(rocr_cutoff)

























































































































































