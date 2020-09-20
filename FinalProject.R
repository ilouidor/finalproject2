#download the college.csv data in your working directory
College <-read.csv("C:/Users/adminUser/Desktop/PreAnalytics/College.csv", stringsAsFactor=FALSE)



#instal the require package 
library(dplyr)
require(ISLR)
require(boot)
library(caret)

##delete first column since its catergorical of each colleges name  and don't want R to treat it as data. 
pickcol <- College[,-1]
dim(pickcol)


#Setting seed to 1
set.seed(1)


# creating train and test set
inTrain <- createDataPartition(pickcol$Grad.Rate, p = 2/3, list = FALSE)
nrow(inTrain)

training <- pickcol[inTrain,]
testing <- pickcol[-inTrain,]

 ##fit regression model1 using the train set
lin_model <-lm(Grad.Rate ~ F.Undergrad + P.Undergrad +Top10perc, data=training)



#cross validation using linear regression
ControlParameters <-trainControl(method="repeatedcv",
                                 number=10,
                                 repeats=10,
                                 savePredictions = TRUE,
                                 )


lin_modellm <-train(Grad.Rate~ F.Undergrad + P.Undergrad +Top10perc, data=training,
                method="lm",
                trControl=ControlParameters)

#print the value of RMSE
lin_modellm

#record the RMSE for model 1
 RMSE_train <- 14.32866
 
# summary of the model 
summary(lin_modellm)

######################################################################################################

##Model2

##fit regression model using the train set
lin_modeltwo <-lm(Grad.Rate ~ F.Undergrad + P.Undergrad +Top10perc+Top25perc + S.F.Ratio + Outstate, data=training)



##creating ten fold cross validation due to large sample size.

ControlParameters <-trainControl(method="repeatedcv",
                                 number=10,
                                 repeats=10,
                                 savePredictions = TRUE)

lin_modellmtwo <-train(Grad.Rate~F.Undergrad + P.Undergrad +Top10perc+Top25perc + S.F.Ratio + Outstate , data=training,
                    method="lm",
                    trControl=ControlParameters)

#print the value of RMSE
lin_modellmtwo

#record the RMSE
RMSE_trainingtwo <- 13.1783

# summary of the model 
summary(lin_modellmtwo)

##############################################################################################################
#model 3

modellm_three <-glm(Grad.Rate ~ ., data=training)

# 10 fold cross validation because our sample size is big
ControlParameters <-trainControl(method="repeatedcv",
                                 number=10,
                                 repeats=10,
                                 savePredictions = TRUE)


modellmthree <-train(Grad.Rate~., data=training,
                method="glm",
                trControl=ControlParameters
)

# print value of RSME for Model 3
modellmthree

RMSE_traingthree<- 12.80171

#summarize model 3 
summary(modellmthree)




######################################  Prediction accuracy on test set ##########################################################################
#Fit trainning model 1 on test set
pred <- predict(lin_modellm, testing)
pred2 <- predict(lin_modellmtwo, testing)
pred3 <- predict(modellmthree, testing)

pred
pred2
pred3

#computing errors for  model 1,2,3  to  predict on test data

error <- pred - testing$Grad.Rate

#second model
error2 <- pred2 - testing$Grad.Rate

#third model
error3 <- pred3 - testing$Grad.Rate



###calculating the RMSE from the previous error. 

RMSE_testing <-sqrt(mean(error^2))

#second model
RMSE_testing2 <-sqrt(mean(error2^2))

#third model
RMSE_testing3 <-sqrt(mean(error3^2))



####print out the result from RMSE testing 
RMSE_testing

#second model 
RMSE_testing2

#third model
RMSE_testing3



#comparing the  RMSE result from model 1, 2, 3 and RMSE_testing result

RMSE_testing-RMSE_train 

RMSE_testing2- RMSE_trainingtwo

RMSE_testing3- RMSE_traingthree 




#Predict on full "pickcol" dataset. Is the full data RMSE lower

pfull <- predict(lin_modellm, pickcol)

pfull2 <- predict(lin_modellmtwo, pickcol)

pfull3 <- predict(modellmthree, pickcol)

efull <- pfull - pickcol$Grad.Rate
efull2 <- pfull2 - pickcol$Grad.Rate
efull3 <- pfull3 - pickcol$Grad.Rate

 #calculating RMSE from the full data  
RMSE_full <- sqrt(mean(efull^2))
RMSE_full2 <- sqrt(mean(efull2^2))
RMSE_full3 <- sqrt(mean(efull3^2))

##print results calculating RMSE for Data set to predict if full data RMSE lower than train data set and test data for all 3 models 
RMSE_full

#second model 
RMSE_full2

# for third model 
RMSE_full3


####print out the result from RMSE testing 
RMSE_testing

#second model 
RMSE_testing2

#third model
RMSE_testing3

#print out RMSE training results
RMSE_train

RMSE_trainingtwo

RMSE_traingthree



