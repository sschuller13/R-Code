#Sam Schuller UIN: 657353888
#Team Number (?)
#Team Members:
#
#
#
#
#Load data into R
forbest <- read.csv("C:/Users/s_sch/Documents/IDS462/forbes.csv", header=T)
# Two methods of mean of data of CEOs
forbest[order(forbest$Salary)]
##Figure out how to remove NA values from data
##try creating a new data column that signifies 1 and 0 for above the mean salary and below the mean salary, then use that to sort 
# the five year returns, use an if else that signifies that NA is a 0 so then that way the data won't be fucked\



# save mean salary,  set threshold for mean salary and sort 5 year returns based on salary mean

RemoveNa_Salary<-na.omit(forbest)
Salarymean<-mean(RemoveNa_Salary$Salary)
Salarymean
RemoveNa_FiveYrReturn<-na.omit(ForbesTable$FiveYrReturn)
#Found the mean now subsetting

#Method1
target1 <- subset(forbest, forbest$Salary>=Salarymean, # selects rows 
                  select=c("FiveYrReturn", "Salary", "YearsCEO", "Overmean")) # selects columns
target1.complete<-na.omit(target1)

target1mean<-mean(target1.complete$FiveYrReturn)
target1mean
#Method2
target2<- forbest %>% filter(Salary>=Salarymean) %>% select(FiveYrReturn, Salary, Company, YearsCEO) # filter for rows, selects for columns 
view(target2)
target2.complete<-na.omit(target2)
target2mean<-mean(target2.complete$FiveYrReturn)
target2mean

#Method1
target2$FactorReturn<- NA
target2$FactorReturn[target2$FiveYrReturn<0]<-"Poor"
target2$FactorReturn[target2$FiveYrReturn>=1&target2$FiveYrReturn<=8]<-"Satisfactory"
target2$FactorReturn[target2$FiveYrReturn>=9]<-"Good"

#Method2
target2.complete$ReturnFactor<- ifelse(target2.complete$FiveYrReturn<=0, "Poor", ifelse(target2.complete$FiveYrReturn>=9, "Good", "Satisfactory")) 
#grepl look into its use 

table(forbest$FactorReturn)


#PartB
# 
#read, sort, data type conversion, selecting cases, and recoding variables. Explain each procedure and the thought behind it
library(readr)
carprice <- read_csv("IDS462/carprice.csv")
View(carprice)


carprice$X1<-NULL

carprice<-carprice[order(carprice$Type),]
carprice

#selecting cases
carprice1 <- subset(carprice, carprice$Type=="Compact", # selects rows 
                  select=c("Type", "MPG.city", "MPG.highway", "Price")) # selects columns 
