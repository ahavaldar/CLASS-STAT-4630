Data<-read.table("students.txt", header=T)

##convert DrivDrnk and Gender to factors
Data$DrivDrnk<-factor(Data$DrivDrnk)
Data$Gender<-factor(Data$Gender)

##set seed so results are reproducible
set.seed(111)

##evenly split data into train and test sets
sample.data<-sample.int(nrow(Data), floor(.50*nrow(Data)), replace = F)
train<-Data[sample.data, ]
test<-Data[-sample.data, ]

##fit logistic regression using training data
result<-glm(DrivDrnk~DaysBeer+Gender, family=binomial, data=train)
summary(result)

##predicted probabilities for test data based on training data
preds<-predict(result,newdata=test, type="response")

##see the predicted probabilities for the test data and their classification based on threshold of 0.5
display<-test[,c(2,5,8)]
display<-cbind(display,preds,preds>0.5)
names(display)[5]<-"ClassYes"
display

##confusion matrix when threshold is 0.5. True values in the rows. 
table(test$DrivDrnk,preds > 0.5)

##confusion matrix when threshold is 0.7. True values in the rows. 
table(test$DrivDrnk,preds > 0.7)

##need ROCR package to produce ROC curve
library(ROCR)

##produce the numbers associated with classification table
rates<-ROCR::prediction(preds, test$DrivDrnk)

##store the true positive and false postive rates
roc_result<-ROCR::performance(rates,measure="tpr", x.measure="fpr")

##plot ROC curve and overlay the diagonal line for random guessing
plot(roc_result, main="ROC Curve")
lines(x = c(0,1), y = c(0,1), col="red")

##compute the AUC
auc<-ROCR::performance(rates, measure = "auc")
auc@y.values





