library(tidyverse)
Data<-read.table("students.txt", header=T)

#########################
##DrivDrnk and DaysBeer##
#########################

##side by side boxplots
ggplot(Data, aes(x=DrivDrnk, y=DaysBeer))+
  geom_boxplot()+
  labs(title="Dist of Days Drinking Beer by Whether Student has Driven Drunk")

##tabulate the counts for number of students who have driven after drinking
table(Data$DrivDrnk)
prop.table(table(Data$DrivDrnk))

##check dummy coding
contrasts(Data$DrivDrnk)
##convert to factor
Data$DrivDrnk<-factor(Data$DrivDrnk)
##check dummy coding
contrasts(Data$DrivDrnk)

##fit logistic regression
result<-glm(DrivDrnk~DaysBeer, family=binomial, data=Data) 
summary(result)

##plot of estimated probabilities vs predictor
plot(Data$DaysBeer,result$fitted.values, ylab="Predicted Probability", xlab="DaysBeer", main="Predicted Probability vs Daysbeer")

##################################
##Consider Gender as a predictor##
##################################

##2 way table with gender
mytab<-table(Data$Gender, Data$DrivDrnk)
mytab
prop.table(mytab, 1) 

###########################
##add gender to the model##
###########################

##check dummy coding
contrasts(Data$Gender)
##convert to factor
Data$Gender<-factor(Data$Gender)
##check dummy coding
contrasts(Data$Gender)

result2<-glm(DrivDrnk~DaysBeer+Gender, family=binomial, data=Data)
summary(result2)

##test if 2 pred model is useful
TS1<-result2$null - result2$dev
1-pchisq(TS1,2)

##add marijuana and studyhours to the model
large<-glm(DrivDrnk~DaysBeer+Gender+Marijuan+StudyHrs, family=binomial, data=Data)
summary(large)

##compare 4 pred model to 2 pred model
TS2<-result2$dev - large$dev

1-pchisq(TS2, 2)




