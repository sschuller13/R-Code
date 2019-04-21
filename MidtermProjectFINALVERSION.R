#Project Team 5
#Sam S
#Michael K
#Edgar O
#Jose M
#Charles F
library(readr)#used
library(car)
library(corrplot) #gonna use
library(gmodels) 
library(tidyverse) #used
library(psych) 
library(ggplot2) #gonna use
library(gridExtra)
library(plotly) #maybe gonna use
library(RColorBrewer)#used
library(vcd)
library(corrplot) #gonna use
library(ggfortify)
#overloaded libraries to cover all our bases
FEVS_2018_PRDF <- read_csv("IDS462/FEVS_2018_PRDF.csv")
View(FEVS_2018_PRDF)

Data<-FEVS_2018_PRDF
#Creates a backup table
#Remove Rows that aren't AR, NV, AF, CE, SEC, and IRS.
agencies<-c("AF","AR","CM","NV","SE","TR") #Values of Rows we need to keep, aka necessary data
NotIRS<-c("TR91", "TRAD", "TRAI", "TRAJ", "TRCC", "TRFD", "TRTG", "TRZZ") #List of Values we need to remove from TR LEVEL1 Column, not used other than for easy reading
Data1.1<-filter(Data, AGENCY == agencies)
Data1.1.1<-Data1.1[-grep("TR91",Data1.1$LEVEL1),]
Data1.1.2<-Data1.1.1[-grep("TRAD",Data1.1.1$LEVEL1),]
Data1.1.3<-Data1.1.2[-grep("TRAI",Data1.1.2$LEVEL1),]
Data1.1.4<-Data1.1.3[-grep("TRCC",Data1.1.3$LEVEL1),]
Data1.1.5<-Data1.1.4[-grep("TRFD",Data1.1.4$LEVEL1),]
Data1.1.6<-Data1.1.5[-grep("TRTG",Data1.1.5$LEVEL1),]
Data1.1.7<-Data1.1.6[-grep("TRZZ",Data1.1.6$LEVEL1),]
Data1.1.8<-Data1.1.7[-grep("TRAJ",Data1.1.7$LEVEL1),]
#Rows Filtered
View(Data1.1.8)
#Remove Useless columns
Data1.1.8$Q41<-NULL
Data1.1.8$Q56<-NULL
Data1.1.8$POSTWT<-NULL
Data1.1.8$Q17<-NULL
Data1.1.8$Q36<-NULL
Data1.1.8$Q38<-NULL
Data1.1.8$Q39<-NULL
Data1.1.8$RANDOM<-NULL
#Convert X's into NAs since keeping them means we can't do numeric calculations
Data1.1.8[Data1.1.8== "X"] <- NA
#Remove NAs
colSums(is.na(Data1.1.8))
Data1.2 <- Data1.1.8 %>% drop_na() 
str(Data1.2)

#Check for any straggler NA's
colSums(is.na(Data1.2))

# All Good
#Now make sure all data types are numeric
#If we didn't include some of the columns types it's because they were already numeric
Q10.19 <- c("Q10", "Q11", "Q12", "Q13", "Q14", "Q15", "Q16", "Q18", "Q19")
Q21.27<- c("Q21", "Q22", "Q23", "Q24", "Q25", "Q26", "Q27")
Q29.39<-c("Q29", "Q30", "Q31", "Q32", "Q33", "Q34", "Q35", "Q37")
Q41.47<-c("Q42", "Q43", "Q44", "Q45", "Q46", "Q47")
Q53.62<-c("Q53", "Q54", "Q55", "Q57", "Q58", "Q59", "Q60", "Q61", "Q62")

Data1.2.1<-Data1.2
Data1.2.1[Q10.19] <- lapply(Data1.2.1[Q10.19],as.numeric) 
Data1.2.1[Q21.27] <- lapply(Data1.2.1[Q21.27],as.numeric) 
Data1.2.1[Q29.39] <- lapply(Data1.2.1[Q29.39],as.numeric) 
Data1.2.1[Q41.47] <- lapply(Data1.2.1[Q41.47],as.numeric) 
Data1.2.1[Q53.62] <- lapply(Data1.2.1[Q53.62],as.numeric) 

#making and including character types as factors
Factors<-c("DSEX", "DEDUC", "DFEDTEN", "DSUPER", "DMINORITY", "DLEAVING")
Data1.2.1[Factors] <- lapply(Data1.2.1[Factors],as.factor)
#Finished converting data to correct types

names(Data1.2.1)[names(Data1.2.1) == "DSEX"] <- "Gender"
names(Data1.2.1)[names(Data1.2.1) == "DEDUC"] <- "Education"
names(Data1.2.1)[names(Data1.2.1) == "DFEDTEN"] <- "FederalTenure"
names(Data1.2.1)[names(Data1.2.1) == "DSUPER"] <- "Supervisor"
names(Data1.2.1)[names(Data1.2.1) == "DMINORITY"] <- "Minority?"
names(Data1.2.1)[names(Data1.2.1) == "DLEAVING"]<- "Leaving?"
names(Data1.2.1)[names(Data1.2.1) == "Minority?"] <- "Minority"
names(Data1.2.1)[names(Data1.2.1) == "Leaving?"]<- "Leaving"
#DATA CLEANING COMPLETE
#Now create a new Column that is a numeric average of questions 1-71
#Creates new Data Table that just has the Agency, questions, and Avg
AvgTable<- Data1.2.1 %>%
  select(Q1,Q2,Q3,Q4,Q5,Q6,Q7,Q8,Q9,Q10,Q11,Q12,Q13,Q14,Q15,Q16,Q18,Q19,Q20,
         Q21,Q22,Q23,Q24,Q25,Q26,Q27,Q28,Q29,Q30,Q31,Q32,Q33,Q34,Q35,Q37,Q40,
         Q42,Q43,Q44,Q45,Q46,Q47,Q48,Q49,Q50,Q51,Q52,Q53,Q54,Q55,Q57,Q58,Q59,Q60,
         Q61,Q62,Q63,Q64,Q65,Q66,Q67,Q68,Q69,Q70,Q71)


#Creates a column that is the average of the Questions

#Copying average column to original data table
Data1.3<-Data1.2.1
Data1.3$AvgScore<-rowSums(AvgTable)/ncol(AvgTable)
view(Data1.3)

Data1.3[, 'AGENCY'] <- lapply(Data1.3[, 'AGENCY'], factor)
#this is very important if we want to be able to graph anything with agency


#Updated so now we have average scores in the columns. We would have to do it again if we wanted to take questions out or put certain similar questions into their own tables to compare/test or run tests. But now we have the line of code so it'll be way easier
#Questions relating to Personal Work Satisfaction
#Table with Questions and Averages of 1-14
TableQ1Q14<-Data1.3 %>%
  select(Q1,Q2,Q3,Q4,Q5,Q6,Q7,Q8,Q9,Q10,Q11,Q12,Q13,Q14)
TableQ1Q14$AvgScore<- rowSums(TableQ1Q14)/ncol(TableQ1Q14)
TableQ1Q14$AGENCY<-Data1.3$AGENCY
TableQ1Q14$Gender<-Data1.3$Gender
TableQ1Q14$Education<-Data1.3$Education
TableQ1Q14$FederalTenure<-Data1.3$FederalTenure
TableQ1Q14$Supervisor<-Data1.3$Supervisor
TableQ1Q14$Minority<- Data1.3$`Minority`
TableQ1Q14$Leaving<-Data1.3$`Leaving`


#Graphs
#Boxplot

plot(AvgScore~AGENCY, data=TableQ1Q14,
     main="Relationship between Agency and Personal Work Satisfaction",
     ylab="Average Satisfaction With My Work Experience",
     xlab="Federal Agency",
     pch=20,
     col=brewer.pal(n = 6, name = "Set1"),
     xlim=c(0, 7))


#Dot Chart of Agency Satisfaction Means by Agency
#Mean of numeric var over levels of Factor var
meanagg1.14 = aggregate(TableQ1Q14$AvgScore, list(TableQ1Q14$AGENCY), mean)
ggplot(meanagg1.14, aes(x = Group.1, y = x)) + geom_point() + coord_flip()


#Questions relating to supervisor evaluations of personal work
#table with Questions and Averages of 15,16,18 and 19
TableQ15Q19<-Data1.3 %>%
  select(Q15, Q16, Q18, Q19)
TableQ15Q19$AvgScore<- rowSums(TableQ15Q19)/ncol(TableQ15Q19)
TableQ15Q19$AGENCY<-Data1.3$AGENCY
TableQ15Q19$Gender<-Data1.3$Gender
TableQ15Q19$Education<-Data1.3$Education
TableQ15Q19$FederalTenure<-Data1.3$FederalTenure
TableQ15Q19$Supervisor<-Data1.3$Supervisor
TableQ15Q19$Minority<- Data1.3$`Minority`
TableQ15Q19$Leaving<-Data1.3$`Leaving`

plot(AvgScore~AGENCY, data=TableQ15Q19,
     main="Relationship between Agency and Personal Work Satisfaction",
     ylab="Average Satisfaction With Performance Review",
     xlab="Federal Agency",
     pch=20,
     col=brewer.pal(n = 6, name = "Set1"),
     xlim=c(0, 7))


#Scatter Plot 
meanagg1.15 = aggregate(TableQ15Q19$AvgScore, list(TableQ15Q19$AGENCY), mean)
ggplot(meanagg1.15, aes(x = Group.1, y = x)) + geom_point() + coord_flip()

#Questions relating to unit/department work ethic and environment
#Table with Questions and Averages of 20-27
TableQ20Q27<-Data1.3 %>%
  select(Q20, Q21,Q22,Q23,Q24,Q25,Q26,Q27)
TableQ20Q27$AvgScore<- rowSums(TableQ20Q27)/ncol(TableQ20Q27)
TableQ20Q27$AGENCY<-Data1.3$AGENCY
TableQ20Q27$Gender<-Data1.3$Gender
TableQ20Q27$Education<-Data1.3$Education
TableQ20Q27$FederalTenure<-Data1.3$FederalTenure
TableQ20Q27$Supervisor<-Data1.3$Supervisor
TableQ20Q27$Minority<- Data1.3$`Minority`
TableQ20Q27$Leaving<-Data1.3$`Leaving`

#graph
plot(AvgScore~AGENCY, data=TableQ20Q27,
     main="Relationship between Agency and Unit Work Environment",
     ylab="Average Satisfaction With Individual Work Units",
     xlab="Federal Agency",
     pch=20,
     col=brewer.pal(n = 6, name = "Set1"),
     xlim=c(0, 7))



#Scatter Plot 
meanagg1.16 = aggregate(TableQ20Q27$AvgScore, list(TableQ20Q27$AGENCY), mean)
ggplot(meanagg1.16, aes(x = Group.1, y = x)) + geom_point() + coord_flip()


#Table w/ Questions 29-40 and Averages
#questions about satisfaction with organization as a whole
TableQ29Q40<-Data1.3 %>%
  select(Q29, Q30, Q31, Q32, Q33, Q34, Q35, Q37, Q40)
TableQ29Q40$AvgScore<- rowSums(TableQ29Q40)/ncol(TableQ29Q40)
TableQ29Q40$AGENCY<-Data1.3$AGENCY
TableQ29Q40$Gender<-Data1.3$Gender
TableQ29Q40$Education<-Data1.3$Education
TableQ29Q40$FederalTenure<-Data1.3$FederalTenure
TableQ29Q40$Supervisor<-Data1.3$Supervisor
TableQ29Q40$Minority<- Data1.3$`Minority`
TableQ29Q40$Leaving<-Data1.3$`Leaving`

#boxplot
plot(AvgScore~AGENCY, data=TableQ29Q40,
     main="Relationship between Agency and Satisfaction with Agency",
     ylab="Average Satisfaction With Overall Agency ",
     xlab="Federal Agency",
     pch=20,
     col=brewer.pal(n = 6, name = "Set1"),
     xlim=c(0, 7))

#Scatter Plot
meanagg1.17 = aggregate(TableQ29Q40$AvgScore, list(TableQ29Q40$AGENCY), mean)
ggplot(meanagg1.17, aes(x = Group.1, y = x)) + geom_point() + coord_flip()



#Table w/ Questions 42-62 and Averages 
#Questions about satisfaction and evalution of managers/higher ups
TableQ42Q62<-Data1.3 %>%
  select(Q42,Q43,Q44,Q45,Q46,Q47,Q48,Q49,Q50,Q51,Q52,Q53,Q54,Q55,Q57,Q58,Q59,Q60,
         Q61,Q62)
TableQ42Q62$AvgScore<- rowSums(TableQ42Q62)/ncol(TableQ42Q62)
TableQ42Q62$AGENCY<-Data1.3$AGENCY
TableQ42Q62$Gender<-Data1.3$Gender
TableQ42Q62$Education<-Data1.3$Education
TableQ42Q62$FederalTenure<-Data1.3$FederalTenure
TableQ42Q62$Supervisor<-Data1.3$Supervisor
TableQ42Q62$Minority<- Data1.3$`Minority`
TableQ42Q62$Leaving<-Data1.3$`Leaving`


#boxplot
plot(AvgScore~AGENCY, data=TableQ42Q62,
     main="Relationship between Agency and Management Satisfaction",
     ylab="Average Satisfaction With Supervisor",
     xlab="Federal Agency",
     pch=20,
     col=brewer.pal(n = 6, name = "Set1"),
     xlim=c(0, 7))


#Scatter plot 
meanagg1.18 = aggregate(TableQ42Q62$AvgScore, list(TableQ42Q62$AGENCY), mean)
ggplot(meanagg1.18, aes(x = Group.1, y = x)) + geom_point() + coord_flip()


#Table w/ Questions 63-71 and Averages 
# True satisfaction questions relating to personal position, pay and goals in the company
TableQ63Q71<-Data1.3 %>%
  select(Q63,Q64,Q65,Q66,Q67,Q68,Q69,Q70,Q71)
TableQ63Q71$AvgScore<- rowSums(TableQ63Q71)/ncol(TableQ63Q71)
TableQ63Q71$AGENCY<-Data1.3$AGENCY
TableQ63Q71$Gender<-Data1.3$Gender
TableQ63Q71$Education<-Data1.3$Education
TableQ63Q71$FederalTenure<-Data1.3$FederalTenure
TableQ63Q71$Supervisor<-Data1.3$Supervisor
TableQ63Q71$Minority<- Data1.3$`Minority`
TableQ63Q71$Leaving<-Data1.3$`Leaving`

#boxplot
plot(AvgScore~AGENCY, data=TableQ63Q71,
     main="Relationship between Agency and Personal Goals/Pay/Happiness",
     ylab="Average Satisfaction With My Goals",
     xlab="Federal Agency",
     pch=20,
     col=brewer.pal(n = 6, name = "Set1"),
     xlim=c(0, 7))


#Scatter plot
meanagg1.19 = aggregate(TableQ63Q71$AvgScore, list(TableQ63Q71$AGENCY), mean)
ggplot(meanagg1.19, aes(x = Group.1, y = x)) + geom_point() + coord_flip()

#Regressions

#Agency and Tenure vs Avg Score

mod1<-lm(AvgScore ~ FederalTenure+AGENCY, data=Data1.3)
options(scipen = 99)
summary(mod1) 



#Education and Minorities vs Avg Score
mod2<-lm(AvgScore ~ Education+Minority, data=Data1.3)
options(scipen = 99)
summary(mod2) 



#Supervisor and Gender vs Avg Score
mod3<-lm(AvgScore ~ Supervisor+Gender, data=Data1.3)
options(scipen = 99)
summary(mod3) 

#Leaving vs Avg Score
mod4<-lm(AvgScore ~ AGENCY, data=Data1.3)
options(scipen = 99)
summary(mod4) 


#removed 
#Minority and Supervisor vs Avg Score
mod5<-lm(AvgScore ~ Minority+Supervisor, data=Data1.3)
options(scipen = 99)
summary(mod5)

