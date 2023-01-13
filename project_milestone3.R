# 3.1: Binary Response Variable
library(reshape2)
library(ROCR)
library(MASS)
library(boot)
library(ipred)

data <- read.csv("wine.csv", header=TRUE)
data$quality <- factor(data$quality)
data2 <- melt(data, id="quality")

#a) We turned our quality variable into factors by using the factor() method. 

#b) We decided to use all our predictors as none of them are categorical except for
# quality, which is our response variable anyways.

set.seed(4630)
sample.data <- sample.int(nrow(data), floor(.50*nrow(data)), replace=F)
train <- data[sample.data, ]
test <- data[-sample.data, ]

#c)

#logistic regression**
contrasts(train$quality)
result_train <- glm(quality~., family="binomial", data=train)
summary(result_train)

#ROC Curve
preds <- predict(result_train, newdata=test, type="response")
rates <- ROCR::prediction(preds, test$quality)
roc_result <- ROCR::performance(rates, measure="tpr", x.measure="fpr")
plot(roc_result, main="ROC Curve")
lines(x = c(0,1), y = c(0,1), col="red")

#AUC
auc <- ROCR::performance(rates, measure="auc")
auc@y.values

#k-fold cv
result <- glm(quality ~., family="binomial", data=data)
five.fold <- boot::cv.glm(data, result, K=5)
five.fold$delta[1]
ten.fold <- boot::cv.glm(data, result, K=10)
ten.fold$delta[1]

#actual test error rate
confusion.mat <- table(test$quality, preds > 0.5)
error <- (105 + 100) / (281 + 100 + 105 + 314)
  
#other comparisons
confusion.mat
fpr <- 100 / (281+100)
fnr <- 105 / (105+314)

#----------------------------------

#LDA**
lda.wine <- MASS::lda(quality~., data=train)
lda.test <- predict(lda.wine, test)

#ROC Curve
preds2 <- lda.test$posterior[,2]
rates2 <- ROCR::prediction(preds2, test$quality)
roc_result2 <- ROCR::performance(rates2, measure="tpr", x.measure="fpr")
plot(roc_result2, main="ROC Curve")
lines(x = c(0,1), y = c(0,1), col="red")

#AUC
auc2 <- ROCR::performance(rates2, measure = "auc")
auc2@y.values

#k-fold CV
cv.da <- function(object, newdata)
{
  return(predict(object, newdata = newdata)$class)
}

five.fold2 <- ipred::errorest(quality~., data=data, model=lda, estimator="cv",
                est.para=control.errorest(k=5), predict=cv.da)$err

ten.fold2 <- ipred::errorest(quality~., data=data, model=lda, estimator="cv",
                est.para=control.errorest(k=10), predict=cv.da)$err

#actual test error rate
confusion.mat2 <- table(test$quality, lda.test$class)
error2 <- (107+98) / (283+98+107+312)

#other comparisons
confusion.mat2
fpr2 <- 98 / (98+283)
fnr2 <- 107 / (107+312)


par(mfrow=c(1,2))
plot(roc_result, main="ROC Curve")
lines(x = c(0,1), y = c(0,1), col="red")
plot(roc_result2, main="ROC Curve")
lines(x = c(0,1), y = c(0,1), col="red")
