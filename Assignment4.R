
# Step 0: Load libraries the data 
#= 
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

load("~/Desktop/IDS 462/retailer.RData")
# Step 1: Make sure data are clean 
#= 
#Data provided has factors and numbers
dim(retailer)  
str(retailer)  
View(retailer)

#checking for NA's
colSums(is.na(retailer)) #No NA's found!
# Step 2: Examine at a univariate level
#= 
describe(retailer)
View(describe(retailer))

fivenum(retailer$expense)
fivenum(retailer$expense)[2]
lower_outliers <- fivenum(retailer$expense)[2]-IQR(retailer$expense)*1.5
which(retailer$expense<=lower_outliers)
upper_outliers <- fivenum(retailer$expense)[4]+IQR(retailer$expense)*1.5
which(retailer$expense>=upper_outliers) 

retailer <- retailer %>% filter(expense<upper_outliers)

retailer1<-retailer
summary(retailer)

str(retailer)

plot(density(retailer$expense))
plot(density(retailer$income))
boxplot(retailer$income, col="lightgreen")



re_mods <- lm(expense ~ homeownership+maritalstatus, data = retailer)
options(scipen=99)
summary(re_mods)
cor.test(retailer$homeownership, retailer$maritalstatus)
vif(re_mods)
sqrt(vif(re_mods))>2 


