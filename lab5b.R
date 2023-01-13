library(ISLR2)
library(moments)
library(boot)
dat <- College
# 1,2
hist(dat$Outstate)
skewness(dat$Outstate) # 0.5082943
# the distribution is right skewed since this number is positive

#4
set.seed(1819)
skew.boot<-function(data,index)
{
  return(skewness(data[index]))
}
my.boot<-boot::boot(dat$Outstate,skew.boot,R=10000)
my.boot
#4) -0.0004508134
#5) 0.05417496
#6) 
boot::boot.ci(my.boot, conf=0.95,type="basic")
#6 ) ( 0.4029,  0.6130 )  
#7) There is enough evidence to prove that the distribution is right skewed
# the values of the CI are positive