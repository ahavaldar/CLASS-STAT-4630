library(ISLR2)
library(tree)
library(randomForest)
library(gbm)

#a
oj <- OJ
oj <- oj[,-18]
oj$StoreID <- as.factor(oj$StoreID)
oj$SpecialCH <- as.factor(oj$SpecialCH)
oj$SpecialMM <- as.factor(oj$SpecialMM)

set.seed(11)
sample.data<-sample.int(nrow(oj), floor(0.7476636*nrow(oj)), replace = F)
train<-oj[sample.data, ]
test<-oj[-sample.data, ]

#b
tree.oj<-tree(Purchase~., data=train)
summary(tree.oj)
# training error rate = 0.1438
# terminal nodes = 10

#c
tree.oj
#  4) LoyalCH < 0.0608385 64   10.30 MM ( 0.01562 0.98438 ) *
# there are 64 observations in this terminal node
# the prop of MM is .98438 

#d
plot(tree.oj)
text(tree.oj, cex=0.6, pretty=0)

#e
tree.pred.test<-predict(tree.oj, newdata=test, type="class")
y.test<-test[,"Purchase"]
table(y.test, tree.pred.test)
1-mean(tree.pred.test==y.test)
# test error rate is 0.2333

#f
set.seed(22)
cv.class<-tree::cv.tree(tree.oj, K=10, FUN=prune.misclass)
cv.class

#g
plot(cv.class$size, cv.class$dev,type='b')

#h
# optimal tree size from CV is 4

#i
trees.num.class<-cv.class$size[which.min(cv.class$dev)]
trees.num.class
prune.class<-tree::prune.misclass(tree.oj, best=trees.num.class)
prune.class

plot(prune.class)
text(prune.class, cex=0.6, pretty=0)

#j
summary(prune.class)
# training error rate for prune = 0.1575 vs 0.1438 for non pruned
# this is surprising because when you prune a tree you expect the error rate to drop
# a reason for a higher value could be due to overfitting 

#k
prune.tree.pred.test<-predict(prune.class, newdata=test, type="class")
y.test<-test[,"Purchase"]
table(y.test, prune.tree.pred.test)
1-mean(prune.tree.pred.test==y.test)
# test error rate for prune=0.2111111 vs. 0.2333 for non pruned
# This is not surprising as we expect the pruned model to perform better on test data than 
# non pruned model
