library(ISLR2)
Data<-ISLR2::Wage

##EDA for Wage

summary(Data$wage)

hist(Data$wage, main="", xlab="Wage")

set.seed(12)

##Using boot function in boot library

##need to write function that computes the estimator you want
med.boot<-function(data,index)
{
  return(median(data[index]))
}

med.boot(Data$wage,1:3000)
med.boot(Data$wage,sample(3000,3000,replace=TRUE))

##use replicate() to generate 1000 bootstrap samples
##boot.samples<-replicate(1000,sample(Data$wage,3000,replace=TRUE))

library(boot)

##find bootstrap estimates of the median for 1000 bootstrap samples
myboot<-boot::boot(Data$wage, med.boot, R=1000)
myboot

##basic 95% CI
boot::boot.ci(myboot, conf=0.95, type="basic")


