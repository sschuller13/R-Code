#Sam Schuller
#UIN: 657353888
#Quiz 2

library(dplyr)
library(car) 
library(effects)
library(ggfortify)
library(tidyverse)
library(corrplot) 
library(gmodels) 
library(psych)
library(ggplot2) 
library(gridExtra)
library(plotly)
library(RColorBrewer)
library(vcd)

#Part A

#load data
load("~/IDS462/ads.RData")

#inspect data
dim(ads) #5 variables, 500 observations
describe(ads)


par(mfrow=c(3,2))

#inspect distribution by format
ggplot(data=ads, aes(format, fill=format)) + 
  geom_bar() + 
  labs(title='Distribution by format')
#normal

#inspect distribution by gender
ggplot(data=ads, aes(gender, fill=gender)) + 
  geom_bar() + 
  labs(title='Distribution by Gender')
#pretty even

ggplot(data=ads, aes(format, fill=format)) + 
  geom_bar() + 
  labs(title='Distribution by format')
#normal

#inspect distribution by gender
ggplot(data=ads, aes(age, fill=age)) + 
  geom_bar() + 
  labs(title='Distribution by age')
#very even


par(mfrow=c(1,2))
plot(density(ads$impressions))
# both are very heavily right skewed
plot(density(ads$purchases))


par(mfrow=c(1,2))
boxplot(ads$impressions, col="lightgreen")
boxplot(ads$purchases, col = "lightgreen")

#these box plots are all out of whack

fivenum(ads$impressions)
fivenum(ads$purchases)
summary(ads$impressions)
summary(ads$purchases)
#now lets remove outliers and then check the regression
lower_outliersimp <- fivenum(ads$impressions)[2]-IQR(ads$impressions)*1.5
which(ads$impressions<=lower_outliersimp) 
#low outliers are below -7836, which is impossible
upper_outliersimp <- fivenum(ads$impressions)[4]+IQR(ads$impressions)*1.5
which(ads$impressions>=upper_outliersimp) 
#upper outliers are above 13,661, seems reasonable consdering the mean is 7500ish

lower_outlierspur <- fivenum(ads$purchases)[2]-IQR(ads$purchases)*1.5
which(ads$purchases<=lower_outlierspur) 
#low outlier is -21, again impossible
upper_outlierspur <- fivenum(ads$purchases)[4]+IQR(ads$purchases)*1.5
which(ads$purchases>=upper_outlierspur) 
#upper outliers is 35 which seems alright considering the mean is 18.69
#noticably VERY right skewed because in both cases the median is WAY lower than the mean


ads_minus_outliers<- ads %>% filter(purchases<upper_outlierspur)
ads_minus_outliers2<-ads %>% filter(impressions<upper_outliersimp)


par(mfrow=c(1,2))
boxplot(ads_minus_outliers2$impressions, col="lightgreen")
boxplot(ads_minus_outliers2$purchases, col = "lightgreen")
#improved data slightly, now regression

#Part B

mod1<- lm(purchases~impressions + gender + format + age + purchases:impressions, data = ads_minus_outliers2)
summary(mod1) #interaction has a .00000002 p value, meaning huge significance in the impressions by purchases standard

#Part C
plot(effect(term="purchases:impressions", mod=mod1, default.levels=20), multiline=T) 

#We Can use the p value of .00000000002 to decide with 99% certainty that people on average will purchase 3.816 items online seen in advertisements. 
#Essentially the regression model finds that, on average, people will purchase 3.816 items. For every impression (by itself) an ad has on a website,
# purchases will really not be affected 

#An important factor discovered is that someone who is over the age of 35-54 is more likely to items online, and will purchase
# .95 more items on average than the baseline of 3.81, and this can be seen as the alpha is .0297 which is < .05, making it somewhat significant.

#Having an unknown gender to an advertiser is extremely significant and having a VERY large effect on purchases, but the p-value is .000000001, meaning it is below the alpha standard of .05.
#Essentially, if an advertister doesn't know the gender of a user, they are much less likely to make any purchases. This makes perfect sense,
#as advertisers will not be able to target the person with correct ads as easily, and will likely mistake the given users interests completely


#Lastly, the p-value for the interaction for purchases and impressions is .00000002, meaning This means, 
#that the relationships between impressions and the purchases is dependent on the value of another 3rd  Independent Variable

#The adjusted r-squared is .8507, meaning we did a great job at removing the outliers and making the results of the regression somewhat accurate



#Part 2

# Part A make it better 


#It occurred to me that because the data is heavily skewed that will be a big problem in the model. To fix this model I must 
#apply a function to the data to make it more normalized. I am doing this now instead of redoing all the work I just did because there are 10 min left

summary(powerTransform(ads_minus_outliers2$impressions))#lambda = .123
summary(powerTransform((ads_minus_outliers2$purchases))) #lambda = NA must be positive

par(mfrow=c(2,1))
plot(density(ads_minus_outliers2), main="Distribution of Impressions", col="steelblue", lwd=3) #original price data
plot(density(ads_minus_outliers2^.123))
#will not work

par(mfrow=c(2,2))
plot(density(ads_minus_outliers2$impressions))
plot(density(log(ads_minus_outliers2$impressions)))#winner winner
plot(density(sqrt(ads_minus_outliers2$impressions)))

par(mfrow=c(2,2))
plot(density(ads_minus_outliers2$purchases))
plot(density(log(ads_minus_outliers2$purchases)))#winner winner
plot(density(sqrt(ads_minus_outliers2$purchases)))

#this will improve the data quite a bit

#on top of that we could include more removal of outliers, which we will use a similar process as in part 1

lower_outliersimp2 <- fivenum(ads_minus_outliers2$impressions)[2]-IQR(ads_minus_outliers2$impressions)*1.5
which(ads_minus_outliers2$impressions<=lower_outliersimp2) 
upper_outliersimp2 <- fivenum(ads_minus_outliers2$impressions)[4]+IQR(ads_minus_outliers2$impressions)*1.5
which(ads_minus_outliers2$impressions>=upper_outliersimp2) 

lower_outlierspur2 <- fivenum(ads_minus_outliers2$purchases)[2]-IQR(ads_minus_outliers2$purchases)*1.5
which(ads_minus_outliers2$purchases<=lower_outlierspur2) 
upper_outlierspur2 <- fivenum(ads_minus_outliers2$purchases)[4]+IQR(ads_minus_outliers2$purchases)*1.5
which(ads_minus_outliers2$purchases>=upper_outlierspur2) 


ads_minus_outliers3<- ads_minus_outliers2 %>% filter(purchases<upper_outlierspur)
ads_minus_outliers4<-ads_minus_outliers3 %>% filter(impressions<upper_outliersimp)