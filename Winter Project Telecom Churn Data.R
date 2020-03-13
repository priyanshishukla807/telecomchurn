install.packages("ggplot2")
install.packages("boot")
install.packages("caret")
install.packages("caTools")
install.packages("dummies")
install.packages("MASS")
install.packages("QuantPsyc")
install.packages("car")
install.packages("Hmisc")
install.packages("aod")
install.packages("pROC")
install.packages("ResourceSelection")
install.packages("ROCR")
install.packages("gmodels")
install.packages("BaylorEdPsych")
##Important libraries:-
library(ggplot2)##for creating elegant Data Visualisations Using the grammer of graphics
library(boot)## For generating estimates of bias, bootstrap confidence intervals, or plots of your bootstrap replicates.
library(caret)## For classification and regression training
library(caTools)##For moving window statistic,GIF,ROC AUC,etc
library(dummies)##For creating dummy /indicator variables flexibly and efficiently
library(MASS)##For supporting Functions and Datasets for variables and Ripley's MASS
library(QuantPsyc)##useful in screening univariate and multivariate data, testing simple mod- erating relationships, extimating indirect effects based on simple (proximal) and complex (distal) mediating relationships. A tool for computing power in a given F distribution is also included.
library(car)##Companion to Applied Reggression
library(Hmisc)##Harrell Miscellaneous contains many functions like  high-level graphics, utility operations, functions for computing sample size and power, imputing missing values, advanced table making, variable clustering, character string manipulation.
library(aod)##Analysis of Overdispersed data
library(pROC)##Display and analyz ROC Curves
library(ResourceSelection)##Reasource Selection(Probability) Functions for use Availability Data.
library(ROCR)##Visualizing  the performance of scoring classifiers.
library(gmodels)##Various R programming Tools for Model Fitting.
library(BaylorEdPsych)##R Package for Baylor University Educational Psychology Quantitstive Course.
setwd("C:\\Users\\Priyanshi\\Downloads")## setting the working directory
getwd()
my_data<-read.csv("Churn DataSet.csv")##Exporting the data.
data<-my_data##creating a duplicate data
str(data)## For seeing the structure of data.
dim(data)##checking the dimension of data.
table(data$Churn)## for checking the lavels of Churn(variable)
data$Churn<-as.factor(data$Churn)##For convert the data type  in factor form because there is only two levels in the variable Churn
str(data)## For checking the structure of data
table(data$CustServ.Calls)##checking the levels of data
data$CustServ.Calls<-as.factor(data$CustServ.Calls)##converting the data type in factor form.
data$Int.l.Plan<-as.factor(data$Int.l.Plan)
data$VMail.Plan<-as.factor(data$VMail.Plan)
table(data$VMail.Plan)## For checking the levels of the data
table(data$Int.l.Plan)
table(data$Intl.Calls)
#We will check na values:-
data.frame(colSums(is.na(data)))## For checking the na values in the data
install.packages("Amelia")## Also for checking the na values of data in graphical pattern
library(Amelia)
missmap(data,
        col=c("Red","Yellow"),
        x.cex = 0.8,
        ylab = "Row Number",
        y.cex = .3,
        main = "Missing Churn Data",
        rank.order = T,
        y.labels = c(seq(1,245500,by=500)),
        y.at = c(seq(1,245500,by=500)))
        
#there is no na values in our data.
#Spliting of data:-
set.seed(200)
split<-sample.split(data$Churn,SplitRatio = 0.7)## For splitting the data in train and test in 70:30 ,70% data is for train data and 30% is for testing the data.
train<-subset(data,split = TRUE)##split 70% of data in train data
test<-subset(data,split = FALSE)## split 30% of data in test data
#Fitting the model For train data:-
fitted<-glm(Churn~Account.Length+VMail.Message+Day.Mins+Eve.Mins+Night.Mins+Intl.Mins+CustServ.Calls+Int.l.Plan+VMail.Plan+Day.Calls+Day.Charge+Eve.Calls+Eve.Charge+Night.Calls+Night.Charge+Intl.Calls+Intl.Charge+State+Area.Code+Phone,data = train,family = binomial)
summary(fitted)
fitted<-glm(Churn~Account.Length+VMail.Message+Day.Mins+Eve.Mins+Night.Mins+Intl.Mins+CustServ.Calls+Int.l.Plan+VMail.Plan+Day.Calls+Day.Charge+Eve.Calls+Eve.Charge+Night.Calls+Night.Charge+Intl.Calls+Intl.Charge+Area.Code,data = train,family = binomial)
summary(fitted)
fitted<-glm(Churn~VMail.Message+Eve.Mins+Night.Mins+Intl.Mins+CustServ.Calls+Int.l.Plan+VMail.Plan+Day.Calls+Day.Charge+Eve.Calls+Eve.Charge+Night.Calls+Night.Charge+Intl.Calls+Intl.Charge+Area.Code,data = train,family = binomial)
summary(fitted)
fitted<-glm(Churn~VMail.Message+Night.Mins+Intl.Mins+CustServ.Calls+Int.l.Plan+VMail.Plan+Day.Calls+Day.Charge+Eve.Calls+Eve.Charge+Night.Calls+Night.Charge+Intl.Calls+Intl.Charge+Area.Code,data = train,family = binomial)
summary(fitted)
fitted<-glm(Churn~VMail.Message+Intl.Mins+CustServ.Calls+Int.l.Plan+VMail.Plan+Day.Calls+Day.Charge+Eve.Calls+Eve.Charge+Night.Calls+Night.Charge+Intl.Calls+Intl.Charge+Area.Code,data = train,family = binomial)
summary(fitted)
fitted<-glm(Churn~VMail.Message+CustServ.Calls+Int.l.Plan+VMail.Plan+Day.Calls+Day.Charge+Eve.Calls+Eve.Charge+Night.Calls+Night.Charge+Intl.Calls+Intl.Charge+Area.Code,data = train,family = binomial)
summary(fitted)
fitted<-glm(Churn~VMail.Message+I(CustServ.Calls==4)+I(CustServ.Calls==5)+I(CustServ.Calls==6)+I(CustServ.Calls==7)+I(CustServ.Calls==8)+Int.l.Plan+VMail.Plan+Day.Calls+Day.Charge+Eve.Calls+Eve.Charge+Night.Calls+Night.Charge+Intl.Calls+Intl.Charge+Area.Code,data = train,family = binomial)
summary(fitted)
fitted<-glm(Churn~VMail.Message+I(CustServ.Calls==4)+I(CustServ.Calls==5)+I(CustServ.Calls==6)+I(CustServ.Calls==7)+I(CustServ.Calls==8)+Int.l.Plan+VMail.Plan+Day.Charge+Eve.Calls+Eve.Charge+Night.Calls+Night.Charge+Intl.Calls+Intl.Charge+Area.Code,data = train,family = binomial)
summary(fitted)
fitted<-glm(Churn~VMail.Message+I(CustServ.Calls==4)+I(CustServ.Calls==5)+I(CustServ.Calls==6)+I(CustServ.Calls==7)+I(CustServ.Calls==8)+Int.l.Plan+VMail.Plan+Day.Charge+Eve.Charge+Night.Calls+Night.Charge+Intl.Calls+Intl.Charge+Area.Code,data = train,family = binomial)
summary(fitted)
fitted<-glm(Churn~VMail.Message+I(CustServ.Calls==4)+I(CustServ.Calls==5)+I(CustServ.Calls==6)+I(CustServ.Calls==7)+I(CustServ.Calls==8)+Int.l.Plan+VMail.Plan+Day.Charge+Eve.Charge+Night.Charge+Intl.Calls+Intl.Charge+Area.Code,data = train,family = binomial)
summary(fitted)
fitted<-glm(Churn~VMail.Message+I(CustServ.Calls==4)+I(CustServ.Calls==5)+I(CustServ.Calls==6)+I(CustServ.Calls==7)+I(CustServ.Calls==8)+Int.l.Plan+VMail.Plan+Day.Charge+Eve.Charge+Night.Charge+Intl.Calls+Intl.Charge,data = train,family = binomial)
summary(fitted)
vif(fitted)##One way to measure multicollinearity is the variance inflation factor (VIF), which assesses how much the variance of an estimated regression coefficient increases if your predictors are correlated. A VIF between 5 and 10 indicates high correlation that may be problematic.
fitted<-glm(Churn~I(CustServ.Calls==4)+I(CustServ.Calls==5)+I(CustServ.Calls==6)+I(CustServ.Calls==7)+I(CustServ.Calls==8)+Int.l.Plan+VMail.Plan+Day.Charge+Eve.Charge+Night.Charge+Intl.Calls+Intl.Charge,data = train,family = binomial)
summary(fitted)## For checking the significant variables
##again checking the the vif :-
vif(fitted)
wald.test(b = coef(fitted),Sigma = vcov(fitted),Terms = 1:10)
## Wald test is used to determine whether a certain predictor variable X is significant or not. It rejects the null hypothesis of the corresponding coefficient being zero. The test consists of dividing the value of the coefficient by standard error ??.
#We reject null-hypothesis because p-value is less than 0.05. 
FMchi <- fitted$null.deviance-fitted$deviance
FMchi
FMdf<-fitted$df.null-fitted$df.residual
FMdf
chi_pro<-1-pchisq(FMchi,FMdf)
chi_pro
PseudoR2(fitted)
#Hoshmer Lamshow Test:-Test for goodness of fit for logistic regression models. The ordered logit model is also known as the proportional odds model, or a cumulative logit model.
HL<-FMchi/fitted$null.deviance
## Cox and Snell R Square:-Cox & Snell R-square that adjusts the scale of the statistic to cover the full range from 0 to 1.
Cox_snell<-  1 - exp ((fitted$deviance - fitted$null.deviance) /nrow(train))
## HL Test statistic calculation##
hl <- hoslem.test(as.integer(train$Churn), fitted(fitted), g=10)
##Importance of the model##
varImp(fitted)
# in-sample accuracy at 0.5##
pred<-predict(fitted ,type = "response")##In simple linear regression, we predict scores on one variable from the scores on a second variable. The variable we are predicting is called the criterion variable .
table(train$Churn,pred >= 0.5)
#calculations
(2734+114)/(2734+116+369+114)
#accuracy =0.8544854
#Model is 85% accurate.
#ROC curve## for evaluating and visualizing the performance of scoring classifiers in the statistical language R.
ROCRpred<-prediction(pred,data$Churn)
ROCRperf<-performance(ROCRpred,"tpr","fpr")
plot(ROCRperf,colorize = TRUE,print.cutoffs.at = seq(0,1,0.1),text.adj = c(-0.2,1.7))
auc = as.numeric(performance(ROCRpred, "auc")@y.values)
auc
## Model is 86% accurate
## again we will check accuracy at 0.6

## in sample accuracy at 0.6
pred1<-predict(fitted ,type = "response")
table(train$Churn,pred1 >= 0.6)
#calculations
(2779+82)/(2779+82+401+71)
#accuracy =0.8583858
#Model is 85.8% accurate.
#ROC curve
ROCRpred<-prediction(pred1,data$Churn)
ROCRperf<-performance(ROCRpred,"tpr","fpr")
plot(ROCRperf,colorize = TRUE,print.cutoffs.at = seq(0,1,0.1),text.adj = c(-0.2,1.7))
auc = as.numeric(performance(ROCRpred, "auc")@y.values)
aucE##for visualization of accuracy of model
## Model is 86% accurate.
## For roc curve
rocCurvetest <-roc(response = train$Churn,predictor = pred1,levels = rev(levels(train$Churn)))
rocCurvetest
plot(rocCurvetest)
##Area under the curve is 0.8604 means 86% .Means 86% accurate.


################Fitting Model for test data################################################

fit_test<-glm(Churn~Account.Length+VMail.Message+Day.Mins+Eve.Mins+Night.Mins+Intl.Mins+CustServ.Calls+Int.l.Plan+VMail.Plan+Day.Calls+Day.Charge+Eve.Calls+Eve.Charge+Night.Calls+Night.Charge+Intl.Calls+Intl.Charge+Area.Code,data = test,family = binomial)
summary(fit_test)
fit_test<-glm(Churn~VMail.Message+Day.Mins+Eve.Mins+Night.Mins+Intl.Mins+CustServ.Calls+Int.l.Plan+VMail.Plan+Day.Calls+Day.Charge+Eve.Calls+Eve.Charge+Night.Calls+Night.Charge+Intl.Calls+Intl.Charge+Area.Code,data = test,family = binomial)
summary(fit_test)
fit_test<-glm(Churn~VMail.Message+Eve.Mins+Night.Mins+Intl.Mins+CustServ.Calls+Int.l.Plan+VMail.Plan+Day.Calls+Day.Charge+Eve.Calls+Eve.Charge+Night.Calls+Night.Charge+Intl.Calls+Intl.Charge+Area.Code,data = test,family = binomial)
summary(fit_test)
fit_test<-glm(Churn~VMail.Message+Night.Mins+Intl.Mins+CustServ.Calls+Int.l.Plan+VMail.Plan+Day.Calls+Day.Charge+Eve.Calls+Eve.Charge+Night.Calls+Night.Charge+Intl.Calls+Intl.Charge+Area.Code,data = test,family = binomial)
summary(fit_test)
fit_test<-glm(Churn~VMail.Message+Intl.Mins+CustServ.Calls+Int.l.Plan+VMail.Plan+Day.Calls+Day.Charge+Eve.Calls+Eve.Charge+Night.Calls+Night.Charge+Intl.Calls+Intl.Charge+Area.Code,data = test,family = binomial)
summary(fit_test)
fit_test<-glm(Churn~VMail.Message+CustServ.Calls+Int.l.Plan+VMail.Plan+Day.Calls+Day.Charge+Eve.Calls+Eve.Charge+Night.Calls+Night.Charge+Intl.Calls+Intl.Charge+Area.Code,data = test,family = binomial)
summary(fit_test)
fit_test<-glm(Churn~VMail.Message+CustServ.Calls+Int.l.Plan+VMail.Plan+Day.Calls+Day.Charge+Eve.Calls+Eve.Charge+Night.Calls+Night.Charge+Intl.Calls+Intl.Charge+Area.Code,data = test,family = binomial)
summary(fit_test)
fit_test<-glm(Churn~VMail.Message+CustServ.Calls+Int.l.Plan+VMail.Plan+Day.Calls+Day.Charge+Eve.Calls+Eve.Charge+Night.Calls+Night.Charge+Intl.Calls+Intl.Charge+Area.Code,data = test,family = binomial)
summary(fit_test)
fit_test<-glm(Churn~VMail.Message+I(CustServ.Calls==4)+I(CustServ.Calls==5)+I(CustServ.Calls==6)+I(CustServ.Calls==7)+I(CustServ.Calls==8)+Int.l.Plan+VMail.Plan+Day.Calls+Day.Charge+Eve.Calls+Eve.Charge+Night.Calls+Night.Charge+Intl.Calls+Intl.Charge+Area.Code,data = test,family = binomial)
summary(fit_test)
fit_test<-glm(Churn~VMail.Message+I(CustServ.Calls==4)+I(CustServ.Calls==5)+I(CustServ.Calls==6)+I(CustServ.Calls==7)+I(CustServ.Calls==8)+Int.l.Plan+VMail.Plan+Day.Charge+Eve.Calls+Eve.Charge+Night.Calls+Night.Charge+Intl.Calls+Intl.Charge+Area.Code,data = test,family = binomial)
summary(fit_test)
fit_test<-glm(Churn~VMail.Message+I(CustServ.Calls==4)+I(CustServ.Calls==5)+I(CustServ.Calls==6)+I(CustServ.Calls==7)+I(CustServ.Calls==8)+Int.l.Plan+VMail.Plan+Day.Charge+Eve.Charge+Night.Calls+Night.Charge+Intl.Calls+Intl.Charge+Area.Code,data = test,family = binomial)
summary(fit_test)
fit_test<-glm(Churn~VMail.Message+I(CustServ.Calls==4)+I(CustServ.Calls==5)+I(CustServ.Calls==6)+I(CustServ.Calls==7)+I(CustServ.Calls==8)+Int.l.Plan+VMail.Plan+Day.Charge+Eve.Charge+Night.Charge+Intl.Calls+Intl.Charge+Area.Code,data = test,family = binomial)
summary(fit_test)
fit_test<-glm(Churn~VMail.Message+I(CustServ.Calls==4)+I(CustServ.Calls==5)+I(CustServ.Calls==6)+I(CustServ.Calls==7)+I(CustServ.Calls==8)+Int.l.Plan+VMail.Plan+Day.Charge+Eve.Charge+Night.Charge+Intl.Calls+Intl.Charge,data = test,family = binomial)
summary(fit_test)
#checking VIF of the variables:
vif(fit_test)
##again fitting the model:-
fit_test<-glm(Churn~I(CustServ.Calls==4)+I(CustServ.Calls==5)+I(CustServ.Calls==6)+I(CustServ.Calls==7)+I(CustServ.Calls==8)+Int.l.Plan+VMail.Plan+Day.Charge+Eve.Charge+Night.Charge+Intl.Calls+Intl.Charge,data = test,family = binomial)
summary(fit_test)
#again checking vif
vif(fit_test)
wald.test(b = coef(fit_test),Sigma = vcov(fit_test),Terms = 1:12)
#We reject null-hypothesis because p-value is less than 0.5. 
FMchi <- fit_test$null.deviance-fit_test$deviance
FMchi
FMdf<-fit_test$df.null-fit_test$df.residual
FMdf
chi_pro<-1-pchisq(FMchi,FMdf)
chi_pro
PseudoR2(fit_test)
#Hoshmer Lamshow Test:-
HL<-FMchi/fit_test$null.deviance
## Cox and Snell R Square##
Cox_snell<-  1 - exp ((fit_test$deviance - fit_test$null.deviance) /nrow(test))
## HL Test statistic calculation##
hl <- hoslem.test(as.integer(test$Churn), fitted(fit_test), g=10)
##Importance of the model##
varImp(fitted)
# in-sample accuracy at 0.5##
pred<-predict(fit_test ,type = "response")
table(test$Churn,pred >= 0.5)
#calculations
(2734+114)/(2734+116+369+114)
#accuracy =0.8544854
#Model is 85% accurate.
#ROC curve
ROCRpred<-prediction(pred,test$Churn)
ROCRperf<-performance(ROCRpred,"tpr","fpr")
plot(ROCRperf,colorize = TRUE,print.cutoffs.at = seq(0,1,0.1),text.adj = c(-0.2,1.7))
auc = as.numeric(performance(ROCRpred, "auc")@y.values)
auc
## Model is 86% accurate
## again we will check accuracy at 0.6

## in sample accuracy at 0.6
pred1<-predict(fit_test ,type = "response")
table(test$Churn,pred1 >= 0.6)
#calculations
(2779+82)/(2779+82+401+71)
#accuracy =0.8583858
#Model is 85.8% accurate.
#ROC curve
ROCRpred<-prediction(pred1,test$Churn)
ROCRperf<-performance(ROCRpred,"tpr","fpr")
plot(ROCRperf,colorize = TRUE,print.cutoffs.at = seq(0,1,0.1),text.adj = c(-0.2,1.7))
auc = as.numeric(performance(ROCRpred, "auc")@y.values)
auc
## Model is 86% accurate.


## in sample accuracy at 0.4
pred2<-predict(fit_test ,type = "response")
table(test$Churn,pred2 >= 0.4)
#calculations
(2679+165)/(2679+165+171+318)
#accuracy =0.8532853
#Model is 85% accurate.
#ROC curve
ROCRpred<-prediction(pred2,test$Churn)
ROCRperf<-performance(ROCRpred,"tpr","fpr")
plot(ROCRperf,colorize = TRUE,print.cutoffs.at = seq(0,2,0.1),text.adj = c(-0.2,1))
auc = as.numeric(performance(ROCRpred, "auc")@y.values)
auc
## Model is 86% accurate.
## For roc curve
rocCurvetest <-roc(response = data$Churn,predictor = pred2,levels = rev(levels(test$Churn)))
rocCurvetest
plot(rocCurvetest)
##Area under the curve is 0.8604 means 86% .Means 86% accurate.


