library(car)
library(corrplot) 
library(gmodels) 
library(tidyverse) 
library(psych) 
#Load data
load("~/IDS462/realestate.RData")
#Remove NAs, make sure variables are the correct type. Make column names more readable, aka format better.
str(reprep)
reprep1<-reprep

reprep1$lotsize<-as.numeric(reprep1$lotsize)
reprep1$price<-as.numeric(reprep1$price)
reprep1$bedrooms<-as.numeric(reprep1$bedrooms)
reprep1$bathrms<-as.numeric(reprep1$bathrms)
reprep1$recroom<-as.factor(reprep1$recroom)
reprep1$garagepl<-as.factor(reprep1$garagepl)
reprep1$driveway<-as.factor(reprep1$driveway)
reprep1$fullbase<-as.factor(reprep1$fullbase)
reprep1$stories<-as.numeric(reprep1$stories)
reprep1$airco<-as.factor(reprep1$airco)
reprep1$gashw<-as.factor(reprep1$driveway)
reprep1$prefarea<-as.factor(reprep1$prefarea)

colSums(is.na(reprep1))
reprep_full <- reprep1 %>% drop_na() 
str(reprep_full)

re <- reprep_full 
str(re) 
#Alternate way shown to change variable types quickly in Session 6
numeric_vars <- c("lotsize" , "bedrooms", "bathrms" , "stories",  "price")
factor_vars <- c("driveway", "recroom", "fullbase", "gashw", "airco", "garagepl", "prefarea")    

re[numeric_vars] <- lapply(re[numeric_vars],as.numeric)  
re[factor_vars] <- lapply(re[factor_vars],as.factor)  
#filters out the one outlier answer that is making the mean inflated, because the number is like in the trillionsl or something
re <- re %>% filter(price < 1000000)


#PART 2, now evulate these three variables individual, discuss skew, distribution, and other observed features
summary(re$lotsize)
summary(re$price)
summary(re$stories)
hist(re$stories, breaks = 5)
#Skewed to the left side of the histogram, much more commonly 1 or 2 stories rather than 3/4
hist(re$lotsize, breaks =10)
#Skewed as most of the data is on the left again, there is a higher frequency amount of <5000 sq ft homes than >5000. Very few houses >100000
hist(re$price, breaks=5)
#closer to normal than any of the other histograms but still not normal. skewed w all the data on the left side. Houses commonly 200k-300k and >300k has a significant drop off in frequency

fivenum(re$lotsize)
fivenum(re$stories)
fivenum(re$price)

#Here's what prof had for part 2
# Step 1 (examine parameters/stats) 
#
#
#

# The question involves three numeric variables 
summary(re[, c("lotsize", "price", "stories")]) # or refer back to the summary of the entire data frame  

describe(re) # slight right-skweness for lotsize and price, as evident from both having means that are higher than the medians (the relative difference in lotsize is greater than price). their spread, however, is similar. but the stories variable seems suspicious
# should stories be considered numeric? it depends on the data! here the distribution doesn't suggest that it is numeric 
options(scipen=99)#gets rid of 10xe
# Step 2 (plot the distributions) 

par(mfrow=c(2,2))
# lotsize 
hist(re$lotsize, main="Distribution of lotsize", freq=F, col="steelblue", ylim=c(0, 0.0003)) # the ylim argument allows the peak of the curve to show. 
lines(density(re$lotsize), main="Distribution of lotsize", col="red", lwd=2)
boxplot(re$lotsize, main="Boxplot of lotsize", col="coral")
# price 
hist(re$price, main="Distribution of price", freq=F, col="steelblue") # the ylim argument allows the peak of the curve to show. 
lines(density(re$price), main="Distribution of price", col="red", lwd=2)
boxplot(re$price, main="Boxplot of price", col="coral")

# The plots further show that both variables are slightly right-skewed. There are several outliers in both. 

# Step 3 (consider removing outliers) 

lower_outliers <- fivenum(re$price)[2]-IQR(re$price)*1.5 #the 2 is the index value of the five number which is Q1
which(re$price<=lower_outliers)  # as expected, nothing 

upper_outliers <- fivenum(re$price)[4]+IQR(re$price)*1.5 #the 4 in the index value of the five number which is Q3
which(re$price>=upper_outliers) # captures all the upper outliers as shown in boxplot 

re1 <- re %>% filter(price<upper_outliers) # "lost" 19 more cases 

hist(re1$price, main="Distribution of price \nwithout outliers", freq=F, col="steelblue") 
lines(density(re1$price), main="Distribution of price \nwithout outliers", col="red", lwd=2)
boxplot(re1$price, main="Boxplot of price \nwithout outliers", col="coral") # not perfect, but better





##Session 5 examples
# Examine the relationships among factor variables in "your" data. What did you find? 
plot(bp1$total_fee~bp1$est_cost, col="steelblue", pch=20, cex=0.75) # right, we still need to address outliers
bp2 <- bp1[bp1$est_cost<20000,]
plot(bp2$total_fee~bp2$est_cost, col="steelblue", pch=20, cex=0.75)
plot(log(bp2$total_fee+1)~log(bp2$est_cost+1), col="steelblue", pch=20, cex=0.75) #doesn't really look like a relationship, looks like r=0

abline(lm(log(total_fee+1)~log(est_cost+1), data=bp2), col="red", lwd=2) # Not great , doesn

cor(bp1$est_cost, bp1$total_fee)
cor.test(bp1$est_cost, bp1$total_fee)# returns an r of .5 with 95% confidence. this is significant, as the p value is <.00000000022
#Always gonna do the stats, plot, test
# A factor and a numeric variable 

# Explore 
bp1 %>% group_by(contractor_city) %>% summarize(avg=mean(est_cost), median=median(est_cost), sd=sd(est_cost)) #comparing inside chicago/not in chiago

boxplot(est_cost ~ contractor_city, data=bp2, main="Comparing Distributions by City", 
        xlab="City", ylab="Estimated Cost", col=c("orange", "steelblue")) #oddly the box plots look similar, but the 'other' median is lower
cost_city_model <- aov(log(est_cost+1)~contractor_city, data=bp2) #this is ANOVA test, aka analysis of variance
summary(cost_city_model) #tells us dF, Sum sq, Mean Sq, F value and p-value. p value of .0389 which is somewhat significant




#back to the real estate work
#PART 3
# Question 1 
#===========
#We want to generate a chi squared test to check relaitionship of 2 variables
# Step 1 (hypothesize - intuitively, what should be the relationship?) 

# I hypothesize that there should be a relationship between the two variables. More expensive houses are more likely to have a/c. 

# Step 2 (consider univariate distributions)  

# see above for price 

# for a/c:
# we can use the summary function in step 2, or
table(re1$airco) # There are 347 houses without a/c compared to 129 with a/c in the data. 

re1 %>% group_by(airco) %>% summarize(mean=mean(price), median=median(price), std=sd(price))


boxplot(price~airco, data=re1)
aov.mod <- aov(price~airco, data=re1)
summary(aov.mod)
# interpretation: the p-value is very low, much lower than 0.01. This indications a strong relationsip between the two variables. 

# NOTE: if we have more than two levels in the factor, running a Tukey post-hoc test would have helped identifying
# the differences among the factor levels. 

# Step 6 (answer) 
# ANSWER: as hypothesized, all the above suggests that there is a relationship between a/c and house prices. 
# houses with a/c will be, overall, more costly than houses without a/c. 

# Question 2 
#===========

# Step 1 (convert bedrooms into a factor)  
re1$bedrooms_f <- as.factor(re1$bedrooms)

# Step 2 (examine univariate distributions)

# See above for airconditioning 

# For bedrooms 
table(re1$bedrooms_f) # We only have 2 houses with 1 bedroom and 6 bedrooms, and only 10 with 5 bedrooms! 
# So, essentially, we need to focus our analysis on 2-4 bedrooms. Remove the data w too few entries to even be consider
# (additional data prep)  

# remove levels with too few cases 
re2 <- re1[(re1$bedrooms>1 & re1$bedrooms<5),]
re2$bedrooms_f <- as.factor(re2$bedrooms)

# Step 3 (examine the relationship between the two variables/stats)   
tab_ac_bdr <- table(re2$airco,re2$bedrooms_f) 
tab_ac_bdr # there seems to be a difference. Houses with less bdr are less likely to have a/c. 
prop_tabab_margin <- prop.table(tab_ac_bdr, margin=2)
prop_tabab_margin # from the proportion table the difference is clearer. the likelihood of having an a/c increases by about twofold from 2 to 3 bedrooms. 
# however, The difference is clearly smaller between 3 to 4 bedrooms (about 7% higher in 4 bdr), compared to the difference between 2-3 and 2-4 bdr. 

# Step 4 (plot the relationship) 
barplot(prop_tabab_margin, main="Proportion of a/c by bedrooms", col=terrain.colors(2))
# the barplot reflects the above. there is a noticable difference in a/c by bedrooms. but this difference is less visible when comparing 3-4 bedrooms. 

# Step 5 (test the relationship) 
# we have two factors, so a chi-squared test is relevant 
chisq.test(re2$airco, re2$bedrooms_f) 
# p-value is lower than 0.01, so there is a relationship between the two variables. 

# Step 6 (answer) 
#ANSWER: Ignoring houses with 1,5 and 6 bedrooms because they have too few observations, there is a relationship between the two variables, 
# especially when comparing houses with 2 to houses with 3 bedrooms. 

# Question 3 
#===========
# Step 1 (hypothesize) 
# I hypothesize that there should be a positive relationship between the two variables. In general, the larger the lotsize, the higher the price. 

# Step 2 (examine the distribution of each variable)  

# see above 

# Step 3 (remove outliers)

# already done for price, now for lotsize 
upper_outliers <- fivenum(re2$lotsize)[4]+IQR(re2$lotsize)*1.5
which(re2$lotsize>=upper_outliers)  # as expected, nothing 

re3 <- re2 %>% filter(lotsize<upper_outliers) # additional 16 cases "lost" 

# Step 4 (plot the relationship) 
plot(re3$price~re3$lotsize, main="Relationship between lotsize and price \nwithout outliers", col="red", pch=16)
abline(lm(re3$price~re3$lotsize, col="gray", lwd=2)) #A-B line, line from a linear model. a linear regression line.
# The plot indicates a positive relation. 

# Step 5 (stats and test) 
cor.test(re3$price,re3$lotsize)

# interpretation: the correlation coefficient is 0.52, which suggests a moderate-strong relationship. 
# the p-value is much lower than 0.01, which indicats that there is a relationship between the two variables.  

# Step 6 (answer) 
# ANSWER: As expected, there is a positive relationship between the two variables. As shown in the plot, after removing outliers, in general, the larger the lotsize, the higher the price. 
