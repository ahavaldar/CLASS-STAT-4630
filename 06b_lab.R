library(pls)
Data<-mtcars


x<-Data[,-1]
y<-Data$mpg

##split data
set.seed(12)
sample.data<-sample.int(nrow(Data), floor(.50*nrow(Data)), replace = F) 
x.train<-x[sample.data,]
x.test<-x[-sample.data,]
y.train<-y[sample.data]
y.test<-y[-sample.data]

#######
##PLS##
#######

##partial least squares with training data. 
pls.fit<-pls::plsr(mpg~., data=Data[sample.data,], scale=TRUE, validation="CV")
summary(pls.fit)

##plot of MSE against number of components. 1 component leads to lowest MSE.
pls::validationplot(pls.fit,val.type="MSEP", legendpos="topleft")

##see the coefficients for each predictor in component 1
pls.fit$loadings[,1]

##Estimate the test MSE
pls.pred<-predict(pls.fit,x.test,ncomp=1)
mean((pls.pred-y.test)^2)
