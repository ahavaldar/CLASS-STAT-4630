library(ISLR2)
data <- Auto

# 1) 392 rows, 9 columns
# 2) Yes we should remove the names of the cars. Cannot run
# a regression with this many unique variables
# 3) 9 variables. 7 quantitative, 1 categorical (origin). 
#  Distinction might not be obvious for year, since it can be put into factor levels. 
#4) 
data$origin <- factor(data$origin)
levels(data$origin) <- c("American", "European", "Japanese")
# We are assuming they can be treated as numeric values without factor levels. 
# These discrete variables can also be used in a regression.
# 5)
pairs(data[,1:7], 
      lower.panel = NULL, 
      main="Scatterplot of Quantitative Variables")
#6) 
cor(data[,1:7])

#7) From the scatterplot, we can see that gas mileage and year
# have a positive correlation (0.58). Meaning that cars are getting better
# gas mileage through the years. With gas mileage and weight,
# we see a strong negative correlation (-0.83) which makes sense. 
# Heavier cars will require more gas, reducing their MPG.
# Engine displacement and weight have a very strong positive
# correlation (0.93), since heavier cars tend to have greater
# cylinder volume. Displacement and horsepower also have a 
# strong positive correlation (0.89). 

#8) 
boxplot(data$mpg~data$origin, main="MPG by Origin")
# From the plots, we can see that Japanese cars have the 
# greatest MPG, while American cars aren't so great.

#9) 
result<-lm(mpg~cylinders+displacement+horsepower
           +weight+acceleration+year, data=data)
par(mfrow=c(2,2))   
plot(result)

library(MASS)
par(mfrow=c(1,1))
MASS::boxcox(result)  

# The QQ plot looks good. The residuals seem to have a slight
# fanning out pattern, which is something we might want to look at. 
# The log likelihood verifies this concern, and we could possible
# try a log transformation or raising the response variable to -0.5. 

# 10)
# I would agree, since 0.5 is in the middle of the log-likelihood graph. 

#11) 
data['mpg2'] = data['mpg'] **-0.5
result2<-lm(mpg2~cylinders+displacement+horsepower
           +weight+acceleration+year, data=data)
par(mfrow=c(2,2))   
plot(result2)

library(MASS)
par(mfrow=c(1,1))
MASS::boxcox(result2)  
# The plots verify our assumptions as does the log likelihood graph. 
summary(result2)
# a) The large F statistic and corresponding low p-val tells us that
# at least one predictor is useful to the response variable. 
# b) I will consider dropping displacement and acceleration. 
# c) 
reduced <- lm(mpg2~cylinders+horsepower
              +weight+year, data=data)
anova(reduced,result2)
cor(data[c(1:7, 10)])
par(mfrow=c(2,2))   
plot(result)
# We fail to reject the null of the anova test, which means 
# we will use the reduced model. 
reduced2 <- lm(mpg2~cylinders+horsepower
              +weight+year+origin, data=data)

library(multcomp)
pairwise<-multcomp::glht(reduced2, linfct = mcp(origin= "Tukey"))
summary(pairwise)
# Japanese and European origin have no stat significant difference 
# in mpg while controlling for the other predictors
# European and American origin does have stat significant difference
# in mpg, and so does Japanese and American origin. 
# Japanese and European cars don't differ in fuel efficiency.
# American cars however do differ in fuel efficienct when compared to 
# European and Japanese cars. 


cor(data[c(1:7, 10)])
pairs(data[c(1:7, 10)], 
      lower.panel = NULL, 
      main="Scatterplot of Quantitative Variables")
