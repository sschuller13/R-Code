#=========================================================
# IDS 462, Session 8 - OLS 
#=========================================================
# Copyright Zack Kertcher, PhD, 2019. All rights reserved. 
# Do not distribute or use outside this class without explicit permission from the instructor.  
#====================================

## OLS 
#Ordinary least sqaures
#squared distance from the line(?)
#used with Linear Regression
#only used when your dependent variable is numeric and continuous
#=====

## LOAD LIBRARIES AND DATA 
#=

library(corrplot)
library(ggfortify)

load("session8.Rdata") # cars, realestate 

# ASSUMPTIONS
#== 
# The DV is normally distributed, (dependent variables HAVE to be continuous, numeric)
# IV values are independent
# There is a linear relationship between the DV and IV(s)
# There is homogeneity of residuals variance, such that the variance of the DV 
# does not change with the levels of IVs (homoscedasticity) 
# There is no multicollinearity (two or more IVs are highly correlated) 
# Also, make sure there are no influencial values ("outliers") (multivariate outliers)

# OLS Example 
#==

# Our DV is price
# Let's correlate numeric vars (in this case only the DV and two IVs: mileage, year)
carsnum <- cars[,c(1,3,4)]
cormat <- cor(carsnum)
corrplot(cormat, addCoef.col = "gray")

# Both IVs exhibit a relationship 

# Now we can model (regress) price given mileage 
mod1<-lm(price ~ mileage, data=cars)#lm stands for linear model , this function creates a linear model
mod1 #for every mile added, on average ,decreases the price by .0932
options(scipen=99)
summary(mod1) 
confint(mod1)

# What about price given year? 
mod2<-lm(price ~ year, data=cars)
mod2
summary(mod2) #on average, the newer the car is by year, the price will increase by 1198
# we can also set confidence interval at 99% 
confint(mod2, level=0.99)

# Predict specific values based on a model

# For example, the predicted price of a car with 5,10,100k miles: 
predict(mod1 , data.frame(mileage =(c(5000 ,10000 ,100000) )),
        interval ="confidence", level=0.95) 

# For example, the predicted price of a car from years 2002,2005,2008: 
predict(mod2 , data.frame(year =(c(2002 ,2005 ,2008) )), 
        interval ="confidence", level=0.95) 
#waaaaaayyyy off, compute age of car rather than year of car
#calculate by 2019-year of car


########################
# Build a regression model using lotsize as the regressor 
# What is the predicted price for lot 500, 1000, 2000, at 99% confidence level? 
mod2.5<-lm(price ~ lotsize, data=realestate)
mod2.5
summary(mod2.5)

predict(mod2.5 , data.frame(lotsize =(c(500 ,1000 ,2000) )), 
        interval ="confidence", level=0.95) 

carsnum$age<- max(carsnum$year)-carsnum$year
mod2.1<-lm(price ~ age, data=carsnum)
predict(mod2.1 , data.frame(age =(c(11 ,8 ,5) )), 
          interval ="confidence", level=0.95) 
########################

# Dummy variables 
#- 
# What about factors? We add them to the model as "dummies" 
  #(in machine learning applications sometimes known as one-hot coding)
#pick a factor type as the dummy, and compare every other factor to that one. Ex male is dummy, compare female to it. 

table(cars$transmission); table(cars$model); table(cars$color) 
# color has too few observations in some categories. we can't use it. 

# We'll add transmission and car model 
mod3 <- lm(price ~ mileage+year+transmission+model, data=cars)  
round(coef(mod3),2)

# Notice the reference categories
levels(cars$model); levels(cars$transmission)

# To change levels order, do the following: 
cars$model <- relevel(cars$model, ref=2) 
levels(cars$model)
#for factor variables, cars w manual transmission are on average 559 dollars shorter
summary(lm(price ~ mileage+year+transmission+model, data=cars)) 
# we need to drop transmission

mod4 <- update(mod3,.~.-transmission) # or simply re-run the model without transmission 

summary(mod4)

#################
# Build a model for price that uses lotsize, bedrooms, bathrms, along with  
# prefarea, and air conditioning 
# Would you keep all the variables in the new model? 
# Interpret the meaning of the coefficients in the best model. 
# What happens if you convert bedrooms to a factor
# and add them to the model? (remove the numeric versions of these variables) 
model <- lm(price ~ lotsize+bedrooms+bathrms+prefarea+airco, data=realestate)  
round(coef(model),3)

summary(model)
#interpretation of coefficients:
#For every increase of one sq ft increase the price 16.62 on average. 
realestate$bedrooms<- as.factor(realestate$bedrooms)

str(realestate)
red_model<-lm(price ~ lotsize+bedrooms+bathrms+prefarea+airco, data=realestate)  
round(coef(red_model),3)

summary(red_model)
red_model2<-update(red_model,.~.-bedrooms)
summary(red_model2)
#i guess I would keep the variables, unless bedrooms is counted as a factor. and if you convert bedroom to a factor you drop it. 

#you could fix them not being significant by grouping 1/2 bedrooms, 3 bedrooms or 4+bedrooms, making it three factors instead of 6, all of a sudden they will become significant
#llooking at r squared, and adjusted r sqaured means that 56% of the variance in price is explained. Usually the more variables you add, the more R-sqaured drops.

#################

# Diagnostics (fit, residuals and influence/leverage)
#=

# Examine model fit using adjusted-r-squared
summary(mod4)$adj.r.squared # nice! 
# improvement from previous models 
summary(mod4)$adj.r.squared-summary(mod1)$adj.r.squared # about 17% improvement 
summary(mod4)$adj.r.squared-summary(mod1.1)$adj.r.squared # about 13% improvement 
summary(mod4)$adj.r.squared-summary(mod2)$adj.r.squared # about 4% improvement 

# Verify regression assumptions 
par(mfrow=c(2,2)) # sets up the canvas to 4 plots, so that we don't need to go through them one by one 

plot(mod1)

plot(mod4)

# if you prefer ggplot
autoplot(mod4)

# Generate each plot separately like so: 
dev.off()
plot(predict (mod4), residuals (mod4)) # we are looking for a "no pattern"/non-linearity 
# finding outliers can be done this way as well
plot(hatvalues(mod4)) 
identify(hatvalues(mod4), col="red")
# or because it looks like we have 2 outliers, we can do this 
tail(sort(hatvalues(mod4)), n=2)

outliers <- c(90, 149)
cars1<-cars[-outliers,]
mod4.1<-lm(price ~ mileage + year + model, data = cars1) 
summary(mod4.1)$adj.r.squared-summary(mod4)$adj.r.squared # slight improvement 

autoplot(mod4.1) # it is an iterative process! 
#THIS DOESNT WORK 
########################
# Are there any outliers in the final house price model you built? 
# Regress without the most "extreme" outliers 
# Did model performance change? 
# Explain your findings. 
########################

## Transformations (dealing with skewed variables)  
# Diagnostics show multivariate outliers. 
# But we should start by removing univariate outliers (recall assumptions!) 
# Pay special attention to the DV! 

boxplot(cars$price, col="coral")

lower_outliers <- fivenum(cars$price)[2]-IQR(cars$price)*1.5
which(cars$price<=lower_outliers)  # 149 150

upper_outliers <- fivenum(cars$price)[4]+IQR(cars$price)*1.5
which(cars$price>=upper_outliers)  # 1 2

# but this variable is not too "bad" 
plot(density(cars$price))

# what about mileage?
plot(density(cars$mileage))
boxplot(cars$mileage, col="lightgreen")
 
# one strategy is to drop the outliers 

# another is to transform the variable 
# first try common transformations 
par(mfrow=c(2,2))
plot(density(cars$mileage))
plot(density(log(cars$mileage)))
plot(density(sqrt(cars$mileage)))

boxplot(sqrt(cars$mileage)) 
 
mod5<-lm(price ~ sqrt(mileage) + year + model, data = cars) 

summary(mod5)$adj.r.squared-summary(mod4.1)$adj.r.squared 

mod5.1<-lm(price ~ sqrt(mileage) + year + model, data = cars[-c(90, 137, 143),] )
summary(mod5.1)$adj.r.squared-summary(mod4.1)$adj.r.squared # better 

########################
# Try to model price using a log transformation of price 
# and square root of lotsize
# Which model do you think is better (logged or non-logged)? Why? 
# Perform regression diagnostics to the logged model. 
# Finally, run the above (logged) regression without outliers. 
########################

## ADDITIONAL RESOURCES
#=
# OpenIntro Statistics. Ch. 7
# R in Action (2nd. Edition), Ch. 8