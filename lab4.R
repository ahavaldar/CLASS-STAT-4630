library(faraway)
library(ROCR)

dat <- wcgs

#1)
set.seed(199)
sample.data<-sample.int(nrow(dat), floor(.50*nrow(dat)), replace = F)
train<-dat[sample.data, ]
test<-dat[-sample.data, ]

#2)
boxplot(train$sdp~train$chd, xlab="chd", ylab="sdp", main="chd vs. sdp")
# not much variation between yes and no
boxplot(train$cigs~train$chd, xlab="chd", ylab="cigs", main="cigs vs. chd")
# here there is a more prominent difference where people who develop chd tend to be smoking more
boxplot(train$dbp~train$chd, xlab="chd", ylab="dbp", main="dbp vs. chd")
# still not much variation but no has a lot of outliers
boxplot(train$age~train$chd, xlab="chd", ylab="age", main="age vs. chd")
# trending towards older people

#3) 
mytab<-table(train$dibep, train$chd)
mytab
prop.table(mytab, 1) 
# there might be an influence because the dataset is very heavily favoring no's

#4)
result_train<-glm(chd~age+sdp+dbp+cigs+dibep, family=binomial, data=train)
summary(result_train)

(0.009507 -0)/0.014510 
2 * (1-pnorm(abs(0.6552033)))
# Age is: increasing age by 1 unit changes the log odds by 0.05
# Behavior type: changing the behavior type to A changes the log odds by -0.84

#5) 
# I will consider dropping dbp as it is not close to being significant
result_4<-glm(chd~age+sdp++cigs+dibep, family=binomial, data=train)
summary(result_4)

TS2<-result_4$dev - result_train$dev
1-pchisq(TS2,1)
# this is greater than 0.05 so we can drop the predictor

#6) 
preds<-predict(result_4,newdata=test, type="response")

##produce the numbers associated with classification table
rates<-ROCR::prediction(preds, test$chd)

##store the true positive and false postive rates
roc_result<-ROCR::performance(rates,measure="tpr", x.measure="fpr")

##plot ROC curve and overlay the diagonal line for random guessing
plot(roc_result, main="ROC Curve")
lines(x = c(0,1), y = c(0,1), col="red")

#7)
auc<-ROCR::performance(rates, measure = "auc")
auc@y.values
# this auc tells us the model is better than random guessing

#8)
confusion.mat<-table(test$chd,preds > 0.5)
confusion.mat
# accuracy: 1440/1577 = 91.313%
# FPR: 0/
# FNR: 137/137+0 = 100%

#9) 
# just based on the confusion matrix the regression does a good job
# with accuracy. This is only because the dataset is so lopsided.

#10)
# Yes the threshold should be adjusted. It should be brought down
# to reduce the FNR.

#11) 
confusion.mat2<-table(test$chd,preds > 0.07)
confusion.mat2
# accuracy: 874+90/1577 = 61.129%
#FPR: 566/874+566 = 39.3%
# FNR: 47/47+90 = 34.3%

#12) 
#0.07 would be better. The lower threshold can somewhat offset the
# imbalanced dataset.