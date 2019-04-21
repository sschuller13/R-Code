#Quiz1
#Sam Schuller UIN:657353888
#Queston1: Prepare Data for analysis
load("~/IDS462/ceo (1).Rdata")
library(tidyverse)
library(lubridate)
library(stringr)
library(data.table)
library(readr)
install.packages("zoo")
library(zoo)

#Cleaning Data. Removing NA's, removing $, million or other various extra data

ceoDATA<-na.locf(ceo)
ceoDATA$`One-Year Change in Compensation`<-as.numeric(gsub("NA","",ceoDATA$`One-Year Change in Compensation`))
ceoDATA2<-na.locf(ceoDATA)
digitpattern <- "\\$|\\million|\\," #removes all $, million and comma
ceoDATA2$`Total Direct Compensation`<-as.numeric(gsub(digitpattern,"",ceoDATA2$`Total Direct Compensation`))


ceoDATA2$`One-Year Shareholder Return`<-as.numeric(gsub("NA","",ceoDATA2$`One-Year Shareholder Return`)) #I can't get this line to work to remove actual NA data
#I spent way too much time cleaning the data, 

#Part2 
BiggestCompanies<-subset(ceoDATA2, ceoDATA2$`One-Year Change in Compensation`>=
               select=c(""))
industryCEOs <-numrows(ceoDATA2$Industry)
industry1<-