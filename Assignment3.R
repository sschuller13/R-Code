#IDS 462 Team 6
# Sam Schuller 
# Michael Kijas
# Edgar Oseguera
# Jose Ramirez Moreno
# Charles Federico
#Assignment 3 (Sorry I named the tibble assignment 4tibble)

library(tidyverse)
library(lubridate)
library(stringr)
library(data.table)
#Part1
library(readr)
install.packages("zoo")
library(zoo)

#Read in as tibble
Assignment4tibble <- read_csv("IDS462/Assignment4table.csv")
View(Assignment4tibble)

#Part2
#Create second table of data without NAs and renamed columns
natest<-na.locf(Assignment4tibble)
natest<-setnames(natest, old=c("Table 78","X2","X3","X4","X5","X6"), new=c("State", "City", "Population", "Law Enforcement Employees", "Officers", "Civilians"))
natest<-natest[-c(1),]
natest2<-natest

#Removes commas from data to avoid NAs, and changes necessary data types to numeric
natest2$Population<-as.numeric(gsub(",","",natest2$Population))
natest2$`Law Enforcement Employees`<-as.numeric(gsub(",","",natest2$`Law Enforcement Employees`))
natest2$Officers<-as.numeric(gsub(",","",natest2$Officers))
natest2$Civilians<-as.numeric(gsub(",","",natest2$Civilians))
summary(natest2)

#Part 3
#Creates Table w/ cities over a 2M population
over2M<-subset(natest2, natest2$Population>=2000000,
select=c("State","City","Population","Law Enforcement Employees","Officers"))
View(over2M)

#Creates Table w/ cities that have a population between 1 Mil and 200k
BetweenPop<-subset(natest2,natest2$Population<=1000000&natest2$Population>=200000, select=c("State","City","Population","Law Enforcement Employees","Officers", "Civilians"))
View(BetweenPop)

is.na(BetweenPop)
table(is.na(BetweenPop))
#Creates alternative table of BetweenPop w/o NAs
BetweenPop2<-na.omit(BetweenPop)

EmployeesSum<-sum(BetweenPop2$`Law Enforcement Employees`)
EmployeesSum #Total Law Enforcement Employees in smaller cities

PopulationSum<-sum(BetweenPop2$Population)
PopulationSum #Total Population in smaller cities

#Per Capita Law Enforcement Employees in the inbetween population
PerCapitaSmall<-EmployeesSum/PopulationSum

PerCapita1<-PerCapitaSmall*100000
PerCapita1 #Number of Law Enforcement Employees per 100K 


#Compairing Over 2M Cities
EmployeesSum1<-sum(Over2M$`Total Law Enforcement Employees`)
PopulationSum1<-sum(Over2M$Population)

Capita_2M<-EmployeesSum1/PopulationSum1
Capita_2M_1<-Capita_2M*100000
Capita_2M_1

DiffCapita<-Capita_2M_1 - PerCapita1
DiffCapita #Difference in Capita
#These numbers compared the number of law enforcement employees per 100,000 people in each given category(>2M and b/w 1M&200k)


#Part 4
#Creates table with all 6 Cities, Top5 in population and Chicago(don't wanna completely include w/ top 5)
ChicagoComparison<-subset(natest2, natest2$City=="Chicago", select=c("State","City","Population","Law Enforcement Employees","Officers"))
ChicagoComparison<-subset(natest2, natest2$Population>=1627000, select=c("State","City","Population","Law Enforcement Employees","Officers"))

#Table of only Top 5 Population Cities w/o Chicago
top5table<-ChicagoComparison[-c(3),]

#Total number of law enforcement employees and population total of 5 top cities together
PopulationTop5<-sum(top5table$Population)
PopTop5Employees<-sum(top5table$`Law Enforcement Employees`)

#Finds Per Capita Ratio of Law Enforcement to Population per 100,000
PerCapitaTop5<-PopTop5Employees/PopulationTop5
PerCapitaTop5Data<-PerCapitaTop5*100000

#Creates A Table w/ just Chicago's Data
ChicagoEmp<-ChicagoComparison[c(3),]
ChicagoEmployees<-ChicagoEmp$`Law Enforcement Employees`
chicagopop<-ChicagoEmp$Population

#finds Per Capita Ratio of Law Enforcement employees to Population per 100,000 in Chicago
PerCapitaChicago<- ChicagoEmployees/chicagopop
PerCapitaChicago_<-PerCapitaChicago*100000

view(PerCapitaChicago_)
View(PerCapitaTop5Data)
diffChicagoBigCities<-PerCapitaChicago_ - PerCapitaTop5Data
#We find Chicago has a higher Per Capita Ratio of Police compared to the averaged out Top 5 Cities
#If we compared the two categories we'd find that Chicago has about 60 more law enforcement employees per 100,000 citizens


save.image(file=Assignment3.R)