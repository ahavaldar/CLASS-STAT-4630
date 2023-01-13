################
##Reading Data##
################

Data<-read.table("cereals.txt", header=TRUE)

##see how shelf is viewed
class(Data$shelf)

##make R view shelf as categorical
Data$shelf<-factor(Data$shelf)
is.factor(Data$shelf)

head(Data$shelf)

##give descriptive names to shelf
levels(Data$shelf)
levels(Data$shelf) <- c("low", "middle", "top") ##order needs to match

head(Data$shelf)

#######
##EDA##
#######

##scatterplot
plot(Data$sugars,Data$calories,xlab="Sugars per serving",	ylab="Calories per serving", main="Plot of Calories against Sugars")

##correlation
cor(Data$sugars, Data$calories)

##scatterplot matrix
pairs(Data[,3:10], 
      lower.panel = NULL, 
      main="Scatterplot of Quantitative Variables")

##correlation matrix
cor(Data[,3:10])
round(cor(Data[,3:10]),3)

##boxplots
boxplot(Data$calories~Data$mfr, main="Calories by Manufacturer")

##obtain median of calories across manufacturers
tapply(Data$calories,Data$mfr,median)

##2 way table
mytab<-table(Data$shelf, Data$mfr)
mytab

##2 way table with proportions
prop.table(mytab, 2)  # 2 means columns add up to 1

##############
##Regression##
##############

result<-lm(calories~sugars+carbo+protein+fat+sodium+fiber+potass, data=Data)

###############
##Diagnostics##
###############

par(mfrow=c(2,2))        # create 2x2 matrix of graphs to show on one plot
plot(result)

library(MASS)
par(mfrow=c(1,1))
MASS::boxcox(result)  #log likelihood, if lambda=0 then perform a log transformation

#############
##Inference##
#############

summary(result)
# T test: For potass- Ho: B7 = 0, Ha: B7 doesnt equal 0
# can we drop X7 from the model, while leaving the other predictors in?
# Yes since pval is large

# F test: Ho: B1=B2=B3=...= 0, Ha: at least one B in Ho is non zero
# Comparison between our model vs an intercept only model
# Is at least one predictor useful to the response variable
# since p-val is small, reject null

#sigma^2 is residual standard error squared

##fit reduced model
reduced<-lm(calories~sugars+carbo+protein+fat, data=Data)

##partial F test
anova(reduced,result)
# since partial F test pval is large we fail to reject the null
# Ho: supports the reduced model (FTR null means use reduced model)


##CI for parameters
confint(reduced,level = 0.95)

newdata<-data.frame(sugars=5, carbo=15, protein=3, fat=1.5)
predict(reduced,newdata,level=0.95, interval="confidence")
predict(reduced,newdata,level=0.95, interval="prediction")

names(reduced)
reduced$coefficients

##########################
##Categorical predictors##
##########################

class(Data$mfr)
Data$mfr<-factor(Data$mfr)

##change reference class
Data$mfr<-relevel(Data$mfr, ref = "other")
contrasts(Data$mfr)

##add mfr to reduced model
reduced2<-lm(calories~sugars+carbo+protein+fat+mfr, data=Data)
summary(reduced2)

##multiple comparisons
library(multcomp)
pairwise<-multcomp::glht(reduced2, linfct = mcp(mfr= "Tukey"))
summary(pairwise)

# Ha: Ug - Uo = 0 vs Ho: Ug-Uo dne 0
# We FTR the null
# G and 0 have no stat significant difference in cals while controlling for the other predictors

