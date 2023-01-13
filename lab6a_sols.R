library(ISLR2)
library(glmnet)

Data <- College

# 1) there is high variance in OLS regression when there is multicollinearity
# present, or the ratio of observations to number of predictors is small.

#2) High variance leads to poor predictive performance on test data. Because of
# the high variance, we shouldn't interpret the coeffs since the coeffs 
# are likely different from the true parameters. 

#3) I think Lasso will produce better results. Since the dataset has 18
# predictors, multicollinearity among the predictors may exist and we 
# would want to drop those from the model, which Lasso does. Lasso reg
# also helps with the interpretability of the model, which is important
# with so many predictors. 

#4) 
set.seed(2019)
x<-model.matrix(Apps~.,data=Data)
x<-model.matrix(Apps~.,data=Data)[,-1]
y<-Data$Apps

sample.data<-sample.int(nrow(Data), floor(.50*nrow(Data)), replace = F) ##observations to belong to the training data
x.train<-x[sample.data,]
x.test<-x[-sample.data,]
y.train<-y[sample.data]
y.test<-y[-sample.data]

#5) We need to use dummy coding for categorical variables because 
# the glmnet package cannot handle factors. 

#6.a) 
set.seed(4630)
cv.out.r<-glmnet::cv.glmnet(x.train,y.train,alpha=0, thresh = 1e-23) # alpha=0 ridge
bestlam.r<-cv.out.r$lambda.min
bestlam.r                      # 409.6938
ridge.mod<-glmnet::glmnet(x.train,y.train,alpha=0,lambda=bestlam.r, thresh = 1e-23)


#7.a) 
plot(cv.out.r)
# There are 17 predictors in the model that corresponds to the 
# value of lambda chosen by CV. This is not surprising since Ridge reg
# keeps all predictors in the model. 

#8.a) 
##Test MSE with best lambda
ridge.pred<-predict(ridge.mod,newx=x.test)
mse.r <- mean((ridge.pred-y.test)^2)
print(mse.r)        # 977113.1


#9) 6-8
#6.b)
set.seed(4630)
cv.out.l<-glmnet::cv.glmnet(x.train,y.train,alpha=1, thresh = 1e-23) # alpha=0 ridge
bestlam.l<-cv.out.l$lambda.min
bestlam.l                      # 2.890309
lasso.mod<-glmnet::glmnet(x.train,y.train,alpha=1,lambda=bestlam.l, thresh = 1e-23)

#7.b) 
plot(cv.out.l)
# There are 2 predictors in the model from Lasso regression. This is not
# surprising because Lasso regression performs variable selection and
# opts for the simpler model, so in this case it deemed only 2 predictors
# necessary for the regression. 

#8.b) 
##Test MSE with best lambda
lasso.pred<-predict(lasso.mod,newx=x.test)
mse.l <- mean((lasso.pred-y.test)^2)
print(mse.l)        # 1116835


#10) 
##fit OLS by setting lambda=0
ols.mod<-glmnet::glmnet(x.train,y.train,alpha=0,lambda=0, thresh = 1e-23)
##test MSE with lambda=0
ols.pred<-predict(ols.mod,newx=x.test)
mse.ols <- mean((ols.pred-y.test)^2)
print(mse.ols)      # 1125282

#11) 
# The MSE with for Ridge is the smallest, with Lasso having the second
# smallest. This is not surprising because the goals of both types 
# of regression is to reduce variance, which it does when compared to 
# OLS. 

#12) 
#Ridge
grid<-10^seq(10,-2,length=100)
out.all.r<-glmnet::glmnet(x,y,alpha=0,lambda=grid,thresh = 1e-23)
plot(out.all.r, xvar = "lambda")
abline(v=log(bestlam.r), lty=2)
legend("bottomright", lwd = 1, col = 1:17, legend = colnames(x), cex = 0.3)

#Lasso
grid<-10^seq(10,-2,length=100)
out.all.l<-glmnet::glmnet(x,y,alpha=1,lambda=grid,thresh = 1e-23)
plot(out.all.l, xvar = "lambda")
abline(v=log(bestlam.l), lty=2)
legend("bottomright", lwd = 1, col = 1:17, legend = colnames(x), cex = 0.3)

# these are called shrinkage methods as they reduce the values of the 
# coefficients towards 0. 