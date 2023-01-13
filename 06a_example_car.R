#########
##Ridge##
#########

Data<-mtcars
head(Data)
library(glmnet)

##linear regression
result<-lm(mpg~.,data=Data)
summary(result)

##to use the glmnet function for shrinkage methods, need to split the response and predictors
x<-model.matrix(mpg~.,data=Data)[,-1] ##the -1 here is to get rid of the column with the response, which was in column 1
y<-mtcars$mpg

ridge<-glmnet::glmnet(x,y,alpha=0,lambda=0, thresh = 1e-23)
lasso<-glmnet::glmnet(x,y,alpha=1,lambda=0, thresh = 1e-23)
cbind(coefficients(result),coefficients(ridge),coefficients(lasso))

##split data
set.seed(12)
sample.data<-sample.int(nrow(Data), floor(.50*nrow(Data)), replace = F) ##observations to belong to the training data
x.train<-x[sample.data,]
x.test<-x[-sample.data,]
y.train<-y[sample.data]
y.test<-y[-sample.data]

##use CV to find optimal lambda based on training set
set.seed(12)
cv.out<-glmnet::cv.glmnet(x.train,y.train,alpha=0, thresh = 1e-23) ##0 for ridge
plot(cv.out)
bestlam<-cv.out$lambda.min
bestlam

##fit ridge regression using training data with bestlam
ridge.mod<-glmnet::glmnet(x.train,y.train,alpha=0,lambda=bestlam, thresh = 1e-23)

##Test MSE with best lambda
ridge.pred<-predict(ridge.mod,s=bestlam,newx=x.test)
mean((ridge.pred-y.test)^2)

##Compare ridge with OLS using best lambda and all observations
out.ridge<-glmnet::glmnet(x,y,alpha=0,lambda=bestlam,thresh = 1e-23)
out.ols<-glmnet::glmnet(x,y,alpha=0, lambda=0, thresh = 1e-23)
cbind(coefficients(out.ridge), coefficients(out.ols)) ##compare the coefficients side by side, ridge then ols
sqrt(sum(coefficients(out.ridge)[-1]^2))
sqrt(sum(coefficients(out.ols)[-1]^2))

##Create plot of ridge coeff against lambda
grid<-10^seq(10,-2,length=100)
out.all<-glmnet::glmnet(x,y,alpha=0,lambda=grid,thresh = 1e-25)
plot(out.all, xvar = "lambda")
abline(v=log(bestlam), lty=2)
legend("bottomright", lwd = 1, col = 1:6, legend = colnames(x), cex = .7)

#########
##LASSO##
#########

##use CV to find optimal lambda based on training set
set.seed(12)
cv.out<-glmnet::cv.glmnet(x.train,y.train,alpha=1, thresh = 1e-23)
plot(cv.out)
bestlam<-cv.out$lambda.min
bestlam

##fit lasso using training data and bestlam
lasso.mod<-glmnet::glmnet(x.train,y.train,alpha=1,lambda=bestlam,thresh = 1e-25)

##Test MSE with best lambda
lasso.pred<-predict(lasso.mod,s=bestlam,newx=x.test)
mean((lasso.pred-y.test)^2)

##Compare lassso with OLS using best lambda and all observations
out.lasso<-glmnet::glmnet(x,y,alpha=1,lambda=bestlam, thresh = 1e-23)
cbind(coefficients(out.lasso), coefficients(out.ridge), coefficients(out.ols))
sqrt(sum(coefficients(out.lasso)[-1]^2))
sqrt(sum(coefficients(out.ridge)[-1]^2))
sqrt(sum(coefficients(out.ols)[-1]^2))

##Create plot of lasso coeff against lambda
out.all2<-glmnet::glmnet(x,y,alpha=1,lambda=grid,thresh = 1e-23)
plot(out.all2, xvar = "lambda")
abline(v=log(bestlam), lty=2)
legend("bottomright", lwd = 1, col = 1:6, legend = colnames(x), cex = .7)

##fit OLS regression using training data 
ridge.mod.0<-glmnet::glmnet(x.train,y.train,alpha=0,lambda=0, thresh = 1e-23)

##Test MSE with OLS
ridge.pred.0<-predict(ridge.mod.0,s=0,newx=x.test)
mean((ridge.pred.0-y.test)^2)

##fit OLS regression using training data 
lasso.mod.0<-glmnet::glmnet(x.train,y.train,alpha=1,lambda=0, thresh = 1e-23)

##Test MSE with OLS
lasso.pred.0<-predict(lasso.mod.0,s=0,newx=x.test)
mean((lasso.pred.0-y.test)^2)

