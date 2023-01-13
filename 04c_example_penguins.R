library(MASS) ##for lda function
library(klaR) ##for partimat function to produce 2-D partition plots
library(ICS) ##for multivariate normality tests
library(palmerpenguins)
library(CompQuadForm)

Data<-penguins
Data<-Data[complete.cases(Data[ , 7]),-c(2,8)]

adelies<-Data[which(Data$species=="Adelie"),-1]

set.seed(49)

##create training and test data
sample.data<-sample.int(nrow(adelies), floor(.70*nrow(adelies)), replace = F)
train<-adelies[sample.data, ]
test<-adelies[-sample.data, ]

#######
##EDA##
#######

##look at first 6 samples
head(train)

##scatterplot matrix of predictors, different colors for each species
pairs(train[,1:4], col = c(1,2)[train$sex], lower.panel=NULL)

##Assumption is that the predictors follow a MVN for each class, so need to assess the assumption for each species

##subset dataframe by gender
gender1<-train[which(train$sex=="female"),]
gender2<-train[which(train$sex=="male"),]

##MVN tests for females
ICS::mvnorm.kur.test(gender1[,1:4])
ICS::mvnorm.skew.test(gender1[,1:4])

##MVN tests for males
ICS::mvnorm.kur.test(gender2[,1:4])
ICS::mvnorm.skew.test(gender2[,1:4])

#######
##LDA##
#######

##Carry out LDA on training data
lda.adelies <- MASS::lda(sex ~ ., data=train)
##obtain ouput from LDA
lda.adelies

##See 1-D groupings by LD1
plot(lda.adelies, dimen = 1, type = "b")

##boundaries based on 2 predictors
klaR::partimat(sex ~ ., data=train, method="lda")

##predictions on test data. 
lda.test <- predict(lda.adelies,test)
table(test$sex,lda.test$class)

## Overall accuracy
mean(test$sex == lda.test$class)

##posterior probabilities for first 6 observations of test data
head(lda.test$posterior)

#######
##QDA##
#######

##Boundaries based on 2 predictors
klaR::partimat(sex ~ ., data=train, method="qda")

##Use QDA on training data
qda.adelies <- MASS::qda(sex ~ ., train)

##predictions on test data
qda.test <- predict(qda.adelies,test)
table(test$sex,qda.test$class)

## Overall accuracy
mean(test$sex == qda.test$class)

