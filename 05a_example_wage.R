library(ISLR2)

Data<-Wage

##EDA for Wage against Age

result<-lm(wage~age, data=Data)

##set up higher order terms
Data$age2<-(Data$age)^2
Data$age3<-(Data$age)^3
Data$age4<-(Data$age)^4
Data$age5<-(Data$age)^5

##regress with higher degree polynmials
result2<-lm(wage~age+age2, data=Data)
result3<-lm(wage~age+age2+age3, data=Data)
result4<-lm(wage~age+age2+age3+age4, data=Data)
result5<-lm(wage~age+age2+age3++age4+age5, data=Data)

##compute fitted values for each polynomial
age.values<-seq(min(Data$age), max(Data$age), by=0.01) ##create grid of x-values to make predictions for
pred2<-predict(result2,list(age=age.values, age2=age.values^2))
pred3<-predict(result3,list(age=age.values, age2=age.values^2, age3=age.values^3))
pred4<-predict(result4,list(age=age.values, age2=age.values^2, age3=age.values^3, age4=age.values^4))
pred5<-predict(result5,list(age=age.values, age2=age.values^2, age3=age.values^3, age4=age.values^4, age5=age.values^5))

##scatterplot with 1st order regression overlayed
plot(Data$wage~Data$age, xlab="Age", ylab="Wages", main="Plot of Wages against Age")
abline(result, col="red")

##residual plot with 1st order polynomial
plot(result$fit, result$res, xlab="Fitted Y", ylab="Residuals", main="Residual Plot with 1st Order Regression")
abline(h=0, col="red")

##overlay mean of residuals for each value on the x-axis
loess.fit<-loess(y~x, data.frame(x=result$fit,y=result$res)) ##compute loess fit of mean residuals
yhat.grid<-seq(from=min(result$fit),to=max(result$fit),length=200) ##generate a sequence for fitted values, ranging from min to max, 200 points
tmp<-predict(loess.fit,newdata=data.frame(x=yhat.grid),se=T) ##compute the predicted values from loess fit based on values of yhat above. se=T means to compute standard errors for loess fit.
##overlay the mean residuals from loess fit, with pointwise 95% CIs
lines(yhat.grid,tmp$fit,lwd=3, col="blue")
lines(yhat.grid,tmp$fit-2*tmp$se.fit, lwd=2, lty=2, col="blue")
lines(yhat.grid,tmp$fit+2*tmp$se.fit, lwd=2, lty=2, col="blue")

##scatterplot with 2nd order regression added
plot(Data$wage~Data$age, xlab="Age", ylab="Wages", main="Plot of Wages against Age")
abline(result)
lines(age.values,pred2,lwd=3, col="red")
legend("topright", c("Order 1", "Order 2"), lty=c(1,1), col=c("black", "red"))

##residual plot with 2nd order polynomial
plot(result2$fit, result2$res, xlab="Fitted Y", ylab="Residuals", main="Residual Plot with 2nd Order Regression")
abline(h=0, col="red")

##overlay mean of residuals for each value on the x-axis
loess.fit<-loess(y~x, data.frame(x=result2$fit,y=result2$res)) ##compute loess fit
yhat.grid<-seq(from=min(result2$fit),to=max(result2$fit),length=200) ##generate a sequence for fitted values, ranging from min to max, 200 points
tmp<-predict(loess.fit,newdata=data.frame(x=yhat.grid),
             se=T) ##compute the predicted values from loess fit based on values of yhat above. se=T means to compute standard errors for loess fit.
lines(yhat.grid,tmp$fit,lwd=3, col="blue")
lines(yhat.grid,tmp$fit-2*tmp$se.fit, lwd=2, lty=2, col="blue")
lines(yhat.grid,tmp$fit+2*tmp$se.fit, lwd=2, lty=2, col="blue")

##scatterplot with up to 5th order polynomial
plot(Data$wage~Data$age, xlab="Age", ylab="Wages", main="Plot of Wages against Age")
abline(result)
lines(age.values,pred2,lwd=3, col="red")
lines(age.values,pred3,lwd=3, col="blue")
lines(age.values,pred4,lwd=3, col="green")
lines(age.values,pred5,lwd=3, col="brown")
legend("topright", c("Order 1", "Order 2", "Order 3", "Order 4", "Order 5"), lty=c(1,1,1,1,1), col=c("black", "red", "blue", "green","brown"))

#########################################################################
##Validation set approach, up to polynomial degree 10, and run 10 times##
#########################################################################

set.seed(316)

d<-10 ##polynomial up to degree 10

reps<-10 ##repeat 10 times, so each rep has a different split

cv.valid<-matrix(0,nrow=reps, ncol=d) ##store the validation MSE for each rep
deg<-c(0,reps) ##store degree with lowest validation MSE for each rep

start_time <- Sys.time() ##start time

for (i in 1:reps)

{

  sample.data<-sample.int(nrow(Data), floor(.50*nrow(Data)), replace = F) ##split data
  train<-Data[sample.data, ]
  validation<-Data[-sample.data, ]

  for (j in 1:d)
  {
    lm.fit<-lm(wage~poly(age,j),data=train) ##fit polynomial up to degree 10
    cv.valid[i,j]<-mean((validation$wage- predict(lm.fit,validation))^2) ##validation MSE for each polynomial 
  }

  deg[i]<-which.min(cv.valid[i,]) ##which order has minimum MSE for each rep

}

end_time <- Sys.time() ## end time
end_time - start_time ## compute how long it took to run the 10 reps for validation set approach
##Time difference of 0.341085 secs

##Plot of validation MSE for first rep
plot(cv.valid[1,], type="b", col=1, xlab="Degree of Polynomial", ylab="MSE", ylim=c(min(cv.valid),max(cv.valid)), main="Estimate of Test MSE with Validation Set Approach")

##overlay validation MSE for the remaining reps
for (i in 2:reps)

{

  lines(cv.valid[i,], type="b", col=i)

}

##which polynomial has smallest MSE
deg

#########
##LOOCV##
#########

library(boot) ##needed for cv.glm() function to carry out LOOCV and k-fold on generalized linear models

cv.error<-rep(0,d) ##store LOOCV MSE for polynomials up to order 10

start_time <- Sys.time()

for (i in 1:d)

{
  glm.fit<-glm(wage~poly(age,i),data=Data) ##fit polynomial up to degree 10
  cv.error[i]<-boot::cv.glm(Data,glm.fit)$delta[1] ##MSE from LOOCV. First argument is the dataframe, 2nd argument is the model that is fitted for the dataframe in the first argument. The function returns 2 values, they should be the same for LOOCV.
}

end_time <- Sys.time()
end_time - start_time ##notice a much longer time with LOOCV
#Time difference of 3.752783 mins

##plot the LOOCV MSE for each polynomial
plot(cv.error, type="b", xlab="Degree of Polynomial", ylab="MSE", ylim=c(min(cv.valid),max(cv.valid)), main="Estimate of Test MSE with LOOCV")

##which degree gives the lowest LOOCV MSE
which.min(cv.error)

#############
##k fold CV##
#############

reps<-10 ##run 10 times
cv.error.k<-matrix(0,nrow=reps, ncol=d) ##store the k fold MSEs
deg<-c(0,reps) ##store the degree which gives lowest k fold MSE for each rep

start_time <- Sys.time()

for (i in 1:reps)
{

  for (j in 1:d)
  
  {

    glm.fit<-glm(wage~poly(age,j),data=Data)
    cv.error.k[i,j]<-boot::cv.glm(Data,glm.fit, K=10)$delta[1] ##MSE from 10-fold CV. Same function as LOOCV, but with additional argument K for number of folds. The function returns 2 values, the first value is the k fold MSE as defined in the textbook. The 2nd value is a bias-corrected version (recall k-fold has more bias than LOOCV).

  }

  deg[i]<-which.min(cv.error.k[i,])

}

end_time <- Sys.time()
end_time - start_time ##not as long as LOOCV
##Time difference of 7.638835 secs

##plot and overlay the k fold MSE for each rep and polynomial
plot(cv.error.k[1,], type="b", col=1, xlab="Degree of Polynomial", ylab="MSE", ylim=c(min(cv.valid),max(cv.valid)), main="Estimate of Test MSE with 10-Fold CV")

for (i in 2:reps)

{

  lines(cv.error.k[i,], type="b", col=i)

}

##find degree with lowest estimate of test mse for each rep
deg
