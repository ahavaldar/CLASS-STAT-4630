##Fitting Regression Trees
library(MASS) ##for Boston dataset
library(tree) ##to fit trees
library(randomForest) ##for random forests (and bagging)
library(gbm) ##for boosting

?Boston ##check variables. Categorical predictors must be factors, not 0/1 dummy codes for decision trees and random forests

Boston$chas<-factor(Boston$chas) ##convert chas from 0/1 dummy code to factor

##split data into training and test sets
set.seed(1)
sample.data<-sample.int(nrow(Boston), floor(.50*nrow(Boston)), replace = F)
train<-Boston[sample.data, ]
test<-Boston[-sample.data, ]
boston.test<-test[,"medv"] ##y values for test data, need for test MSE later

##########################################
##Recursive binary splitting and pruning##
##########################################

##Create regression tree with recursive binary splitting
tree.boston<-tree(medv~., data=train)
summary(tree.boston)

##Plot tree
plot(tree.boston)
text(tree.boston, cex=0.75)

##test MSE with recursive binary splitting
yhat<-predict(tree.boston, newdata=test)
mse<-mean((boston.test-yhat)^2)
mse

##############
##Prune tree##
##############

##use 10-fold CV to prune tree
set.seed(11)
cv.boston<-cv.tree(tree.boston, K=10)
cv.boston

##plot of res mean deviance vc size of tree
plot(cv.boston$size, cv.boston$dev, type='b', ylab="Deviance", xlab="Size")

##see size of tree which gives best tree based on pruning and 10-fold CV
trees.num<-cv.boston$size[which.min(cv.boston$dev)]
trees.num ##same number as recursive binary splitting

##To build a tree using the appropriate number of nodes based on CV. In this case, since we are using the same number of terminal nodes as the unpruned tree, this function will output a tree that is the same as the one with recursive binary splitting
prune.boston<-prune.tree(tree.boston, best=trees.num)

##just for practice. This tree should be the same as unpruned tree since 7 terminal nodes are used
plot(prune.boston)
text(prune.boston, cex=0.75)

##test MSE with tree
yhat<-predict(prune.boston, newdata=test)
boston.test<-test[,"medv"]
mse.tree<-mean((boston.test-yhat)^2)
mse.tree ##same as mse with recursive binary splitting since trees were same

##############################
##Bagging and Random Forests##
##############################

set.seed(111)
##bagging is special case of random forest when mtry = number of predictors
bag.boston<-randomForest(medv~., data=train, mtry=13, importance=TRUE)
bag.boston

##Note: Reported MS residuals is for the training data.

##test MSE with bagging
yhat.bag<-predict(bag.boston, newdata=test)
mse.bag<-mean((boston.test-yhat.bag)^2)
mse.bag

##see which predictors are important
round(importance(bag.boston),2)
varImpPlot(bag.boston)


##Random Forest. Just change the value in mtry to be less than number of predictors
set.seed(1111)
rf.boston<-randomForest(medv~., data=train, mtry=4,importance=TRUE)
rf.boston

##test MSE with Random Forest
yhat.rf<-predict(rf.boston, newdata=test)
mse.rf<-mean((boston.test-yhat.rf)^2)
mse.rf

round(importance(rf.boston),2)
varImpPlot(rf.boston)

############
##Boosting##
############

set.seed(11111)
boost.boston<-gbm(medv~., data=train, distribution="gaussian", n.trees=500)
##summary displays which predictors are important
summary(boost.boston)

##Marginal plot of how predictor influences the response
plot(boost.boston,i="rm")
plot(boost.boston,i="lstat")


##Test MSE with boosting
yhat.boost<-predict(boost.boston, newdata=test, n.trees=500)
mse.boost<-mean((yhat.boost-boston.test)^2)
mse.boost

##########################
##Did not show in slides##
##########################

##use 10 fold CV to choose number of trees for boosting
set.seed(11111)
boost.boston2<-gbm(medv~., data=train, distribution="gaussian", n.trees=2000, cv.folds=10)

gbm.perf(boost.boston2)
##1306 trees should be chosen

##test MSE with 1306 trees
set.seed(11111)
boost.boston3<-gbm(medv~., data=train, distribution="gaussian", n.trees=1306, cv.folds=10)

yhat.boost3<-predict(boost.boston3, newdata=test, n.trees=1306)
mse.boost3<-mean((boston.test-yhat.boost3)^2)
mse.boost3


