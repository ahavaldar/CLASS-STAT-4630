library(MASS)
library(klaR)
library(ICS)
library(palmerpenguins)
library(faraway)
library(ROCR)

wcgs <- wcgs
wcgs <- wcgs[,c("chd", "age", 'sdp', 'dbp', 'cigs')]
set.seed(199)

# 1) 
# dibep is a categorical variable so we cannot use it for DA

#2) 
sample.data<-sample.int(nrow(wcgs), floor(.50*nrow(wcgs)), replace = F)
train<-wcgs[sample.data, ]
test<-wcgs[-sample.data, ]
chdno<-train[which(train$chd=="no"),]
chdyes<-train[which(train$chd=="yes"),]
##MVN tests for no
ICS::mvnorm.kur.test(chdno[,2:5])
ICS::mvnorm.skew.test(chdno[,2:5])
# for yes
ICS::mvnorm.kur.test(chdyes[,2:5])
ICS::mvnorm.skew.test(chdyes[,2:5])
# we reject the null that they follow a multivariate normal 
# for both yes and no. Since we are performing classification, 
# assumptions may be less crucial so it won't impact the results as much

#3) 
contrasts(train$chd)
lda.chd <- MASS::lda(chd ~ ., data=train)
##obtain ouput from LDA
lda.chd
# since they are all positive, the more or increase of any of them 
# leads to a classification of yes
# larger coeff for age means this predictor has a larger impact on classification

#4) 
klaR::partimat(chd ~ ., nplots.vert=2, nplots.hor=3, data=train, method="lda")
# the decision boundaries are extremely skewed because the data
# is very heavily "no". So most of the classifications based on 
# these decision boundaries will be no.

#5) 
lda.test <- predict(lda.chd,test)
preds<-lda.test$posterior[,2]
rates<-ROCR::prediction(preds, test$chd)
roc_result<-ROCR::performance(rates,measure="tpr", x.measure="fpr")
plot(roc_result, main="ROC Curve for Gender of Adelie Penguins")
lines(x = c(0,1), y = c(0,1), col="red")
# this regression does better than random guessing

#6) 
auc<-ROCR::performance(rates, measure = "auc")
auc@y.values
# this auc tells us the model is better than random guessing

#7) 
table(test$chd, lda.test$posterior[,2]>0.07)
# accuracy: 891+89/1577 = 0.621
# FPR: 549/(891+549) = 0.38125
# FNR: 48/(48+89) = 0.35036

#8) 
klaR::partimat(chd ~ ., nplots.vert=2, nplots.hor=3, data=train, method="qda")
# here the boundaries are more curved, and allow for more 
#flexibility. Classifications may still be skewed, but not
# by as much as LDA

#9)
qda.chd <- MASS::qda(chd ~ ., train)
qda.test <- predict(qda.chd,test)
preds2<-qda.test$posterior[,2]
rates2<-ROCR::prediction(preds2, test$chd)
auc2<-ROCR::performance(rates2, measure = "auc")
auc2@y.values

#10) 
table(test$chd, qda.test$posterior[,2]>0.07)
# accuracy: 932+83/1577= 0.6436
# FPR: 508/(508+932) = 0.3528
# FNR: 54/(54+83)= 0.3942

#11)
# Based on results, I would use QDA as it yields a higher 
# accuracy, with a lower FPR but a slightly higher FNR. 
# Also QDA has more flexible decision boundaries which is
# good for this dataset. 

#12)
# With the same threshold of 0.07, QDA and LDA yield higher
# accuracy, with roughly similar FPR and FNR. I would say
# QDA seems to be the best. 
