library(tree) ##for tree() function
library(ISLR2)

##remove missing data
Hitters<-na.omit(Hitters)

##set up tree, using all observations
tree.all<-tree::tree(log(Salary) ~ Years + Hits, data = Hitters)

##create a tree with 3 terminal nodes
prune.all<-tree::prune.tree(tree.all, best=3)
##display tree
plot(prune.all)
##overall text
text(prune.all, cex=0.75) ##cex changes the size of the text

###################################################################################
##Create scatterplot of hits and years, with different colors for salary quartile##
###################################################################################

##find the quartiles for salary
salary.quartiles<-quantile(Hitters$Salary, 0:4/4)

##convert salary into a categorical variable with 4 classes: Q1, Q2, Q3, Q4 baed on quartiles
sal.cat<-cut(Hitters$Salary, breaks=c(salary.quartiles[1],salary.quartiles[2],salary.quartiles[3],salary.quartiles[4],salary.quartiles[5]), labels=c("Q1","Q2","Q3","Q4"))

##new data frame with hits, years, and sal.cat
new.data<-data.frame(Hitters$Hits, Hitters$Years, sal.cat)

##give new names to columns of new.data
colnames(new.data)<-c("hits", "years", "sal")

##create scatter plot of hits and years, with different colors
plot(new.data$hits ~ new.data$years, pch=16, col=c("blue","green", "yellow", "red")[unclass(new.data$sal)], xlab="Years", ylab="Hits", main="Scatterplot of Hits and Years, by Salary")

##add legend
legend("topright", c("Q1", "Q2","Q3","Q4"), pch=16, col=c("blue","green", "yellow", "red"))

##overlay regions from tree
tree::partition.tree(prune.all, ordvars=c("Years", "Hits"), add=TRUE)

##notice two outliers in bottom left. 
##Mike Schmidt, row 173 actually has 160 hits and 15 years.
##Terry Kennedy, row 241 actually has 114 hits and 9 years. 

##fix data
fix(Hitters) ##edit the hits and years for Schmidt and Kenney

##Re-Create Tree with 3 terminal nodes

tree.all<-tree::tree(log(Salary) ~ Years + Hits, data = Hitters)
prune.all<-tree::prune.tree(tree.all, best=3)
plot(prune.all)
text(prune.all, cex=0.75)

#########################################################################################################
##Re-Create scatterplot of hits and years, with different colors for salary quartile, after fixing data##
#########################################################################################################

##new data frame with hits, years, and sal.cat
new.data<-data.frame(Hitters$Hits, Hitters$Years, sal.cat)

##give new names to columns of new.data
colnames(new.data)<-c("hits", "years", "sal")

##create scatter plot of hits and years, with different colors
plot(new.data$hits ~ new.data$years, pch=16, col=c("blue","green", "yellow", "red")[unclass(new.data$sal)], xlab="Years", ylab="Hits", main="Scatterplot of Hits and Years, by Salary")

##add legend
legend("topright", c("Q1", "Q2","Q3","Q4"), pch=16, col=c("blue","green", "yellow", "red"))

##overlay regions
tree::partition.tree(prune.all, ordvars=c("Years", "Hits"), add=TRUE)

##########################
##worked example section##
##########################

##split data
set.seed(1999)
sample.data<-sample.int(nrow(Hitters), floor(.50*nrow(Hitters)), replace = F)
train<-Hitters[sample.data, ]
test<-Hitters[-sample.data, ] 

##fit tree model using training data with binary recursive splitting
tree.result<-tree::tree(log(Salary) ~ Years + Hits, data = train)

##see output
summary(tree.result)
##9 nodes for tree. Residual mean deviance analagous to MSE

##decision tree built on training data with recursive binary splitting
plot(tree.result)
text(tree.result, cex=0.75)

##use 10-fold CV to prune tree
set.seed(2000)
cv.baseball<-tree::cv.tree(tree.result, K=10)
cv.baseball

##plot of residual mean deviance vs size of tree with pruning
plot(cv.baseball$size, cv.baseball$dev, type="b", xlab="Size of Tree", ylab="Res Mean Deviance")

##see size of tree which gives best tree based on pruning and 5-fold CV
trees.num<-cv.baseball$size[which.min(cv.baseball$dev)]
trees.num
##tree with 5 nodes is chosen with pruning

##refit with all data points, with 5 nodes
tree.full<-tree::tree(log(Salary) ~ Years + Hits, data = Hitters)
prune.full<-tree::prune.tree(tree.full, best=trees.num)

##decision tree with pruning, with all data
plot(prune.full)
text(prune.full, cex=0.75)

##numerical summary of pruned tree
prune.full

##region space based on pruned tree
##create different plot color to distinguish observations
plot(new.data$hits ~ new.data$years, pch=16, col=c("blue","green", "yellow", "red")[unclass(new.data$sal)], xlab="Years", ylab="Hits", main="Scatterplot of Hits and Years, by Salary")
tree::partition.tree(prune.full, ordvars=c("Years", "Hits"), add=TRUE)