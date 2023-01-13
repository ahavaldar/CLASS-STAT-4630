###################################################
##Function to simulate y values from linear model##
###################################################

gety <- function(x,intercept,slope,eps.sigma)

{
  y <- intercept + slope*(x^2) + rnorm(length(x),0,eps.sigma)
  return(y)
}

####################
##Simulation study##
####################

##generate the values of x
x<-rep(seq(1,10,1),20)

##initialize values for simulation
beta0 <- 0 ##intercept
beta1 <- 2 ##slope
sig <- 1 ##sd of error term

##run simulation 10000 times
reps <- 100000


##create an array to store the estimated slope from each rep
store.slope<-array(0,reps)
store.fit<-array(0,reps)

##if you are curious about how long your loop runs
start_time <- Sys.time() ##start time of loop

##if you want to reproduce results
set.seed(4630)

for (i in 1:reps)

{
  y<-gety(x, intercept=beta0, slope=beta1, eps.sigma=sig) 
  
  ##use least squares to obtain regression equation on simulated data
  result<-lm(y~x)

  ##store the estimated slope from this rep
  store.slope[i]<-result$coeff[2]
  store.fit[i]<- result$fitted.values
  ##optional line if you want to see how fast your loop is going
  #print(paste("Iteration", i))
}

end_time <- Sys.time() ## end time of loop
end_time - start_time ##time taken by loop
summary(result)
xtest1 <- list(7)
f <- as.vector(predict(result, xtest1))




##bias of est slope
mean(store.slope)-beta1 

##empirical variance of est slope
var(store.slope) 

##theoretical formula for variance of estimated slope
sig^2/sum((x-mean(x))^2)


# Quiz Questions
hist(store.slope)       # slopes are normally distributed because of the large sample size
    
#3A: bias is neg and var got bigger
#3b: bias is smaller and neg var is same
#3c: bias is slightly large and var is bigger
#3d: bias is large and pos and var is same