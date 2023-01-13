library(boot) ##for cv.glm function
library(MASS) ##for lda function
library(ipred) ##for errorest function needed for k fold CV in DA
library(ISLR2)
Data<-ISLR2::College

#############################
##generalized linear models##
#############################

#####################
##linear regression##
#####################

result.lin<-glm(Apps ~ ., data=Data, family="gaussian")

#########
##LOOCV##
#########

start_time <- Sys.time() ##start time
##Supply the data frame, and the model that used glm
loocv.lin<-boot::cv.glm(Data,result.lin)
end_time <- Sys.time() ## end time
end_time - start_time ## see how long LOOCV takes
##Time difference of 5.731329 secs

##extract the estimate of the test MSE. there are two values, the first value is the value based on the formula in our book. The 2nd value is based on an adjustment to correct for the bias
loocv.lin$delta

##########
##k fold##
##########
 
start_time <- Sys.time() ##start time
set.seed(5)
five.fold.lin<-boot::cv.glm(Data,result.lin, K=5) ##specify K now
end_time <- Sys.time() ## end time
end_time - start_time ## see how long k fold takes
##Time difference of 0.06062412 secs
five.fold.lin$delta

#######################
##Logistic regression##
#######################

##fit logistic regression.
result<-glm(Private ~ ., data=Data, family="binomial")

#########
##LOOCV##
#########

start_time <- Sys.time() ##start time
##Supply the data frame, and the model that used glm
loocv<-boot::cv.glm(Data,result)
end_time <- Sys.time() ## end time
end_time - start_time ## see how long LOOCV takes
##Time difference of 9.519251 secs

##extract the estimate of the test error rate. there are two values, the first value is the value based on the formula in our book. The 2nd value is based on an adjustment to correct for the bias
loocv$delta

##########
##k fold##
##########
 
start_time <- Sys.time() ##start time
set.seed(5)
five.fold<-boot::cv.glm(Data,result, K=5) ##k specified
end_time <- Sys.time() ## end time
end_time - start_time ## see how long k fold takes
##Time difference of 0.06513596 secs
five.fold$delta

#########################
##discriminant analysis##
#########################

#########
##LOOCV##
#########

##The lda function can perform LOOCV by adding the CV=TRUE argument. same with qda function.
lda.result<-MASS::lda(Private ~ ., data=Data, CV=TRUE)

##display the estimated posterior probabilities of each observation with LOOCV
head(lda.result$posterior)

##create a confusion matrix, which can be used to calculate the LOOCV estimate for the test classification error rate
table(Data$Private, lda.result$class)

##error rate
1-mean(Data$Private==lda.result$class)

##########
##k fold##
##########

##need to write own function to use with errorest function from ipred package

##this function returns the estimated posterior probabilities for a model with new data
cv.da <- function(object, newdata) 

{
  
  return(predict(object, newdata = newdata)$class)

} 

##this returns the k fold CV estimate for the test classification error rate
set.seed(5)
ipred::errorest(Private ~ ., data=Data, model=lda, estimator="cv", est.para=control.errorest(k=5), predict=cv.da)$err 

