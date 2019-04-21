#Final Project
#Sam Schuller, Jose Ramirez, Charles Federico, Michael Kijas, Edgar Oseguera
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
library(readxl)
library(caret)  
library(mosaic)
library(rms)
library(xlsx)

#2018-2019 NBA DATA


NBA2019Stats <- read_excel("NBA2019Stats.xlsx")
View(NBA2019Stats)

NBA2019AdvancedStats <- read_excel("NBA2019AdvancedStats.xlsx")
View(NBA2019AdvancedStats)

NBA2019Contracts <- read_excel("NBA2019Contracts.xlsx")
View(NBA2019Contracts)

#kept stats we will use
usedstats<-c("Player", "Tm", "Age", "Pos","G","GS","PTS","TRB", "AST", "STL", "BLK","MP", "FG%", "3P%", "2P%", "FT%", "TOV")
usedAdvStats<-c("Player", "PER", "TS%", "3PAr", "FTr", "WS", "OWS", "DWS")
usedContracts<-c("Player", "2018-2019", "Guaranteed")

#added them to data tables and then combined them
Data1.0<-NBA2019Stats[usedstats]
Data1.0.1<-NBA2019AdvancedStats[usedAdvStats]
names(Data1.0.1)[names(Data1.0.1) == "Player"]<- "Player2"
Data1.1 <- cbind(Data1.0, Data1.0.1)

#handpicked thru every single entry and deleted duplicates that resulted from trades. Kept overall season stats.
Data1.1.1<-Data1.1
repeatnames<-c(17,18,26,27,33,40,41,72,73,76,77,93,94,96,97,99,100,101,104,105,110,111,119,120,
               127,128,130,131,135,136,149,150,166,167,170,171,182,183,194,195,197,198,201,202,209,210,
               222,223,231,232,234,235,255,256,262,263,271,272,275,276,291,292,297,298,303,304,325,326,
               331,332,339,340,342,343,345,346,360,361,367,368,373,374,379,380,386,387,397,398,409,410,
               415,416,421,422,424,425,430,431,432,436,437,452,453,460,461,466,476,477,485,486,488,500,
               514,515,518,519,526,527,536,537,539,540,554,555,571,572,576,577,580,581,584,585,587,588,593,594,599,600,
               604,605,606,610,611,614,615,620,621,625,626,649,650,684,685,687,688)
Data1.1.1 <- Data1.1[-c(repeatnames),]
#removed naems
Data1.2<-Data1.1.1
Data1.2$Contract18.19 <-NBA2019Contracts$`2018-19`[match(Data1.1.1$Player, NBA2019Contracts$Player)]
Data1.2$Guaranteed<-NBA2019Contracts$Guaranteed[match(Data1.2$Player, NBA2019Contracts$Player)]
#added salary column as long as player names match

#check for and remove NAs
colSums(is.na(Data1.2))
str(Data1.2)
Data1.3 <- Data1.2%>% drop_na()
colSums(is.na(Data1.3))

summary(Data1.3$PTS)
boxplot(Data1.3$PTS)
boxplot(Data1.3$Salary)

#after this maybe take out players who havent played 10 games
#then possibly change age to a factor
#make position a factor

Data1.3$Pos<-sub("-.*","",Data1.3$Pos) 
view(Data1.3)

FactorsPos<-c("PG", "SG", "SF", "PF", "C")
FactorsPos <- factor(FactorsPos)
FactorsPos #didn't even need this
Data1.3$Pos <- as.factor(Data1.3$Pos)
summary(Data1.3$Pos)


Data1.4<-Data1.3
#Removes any players with 16 or less games played
Data1.4.1 <- Data1.4 %>% filter(Data1.4$G > 16)

#still have 401 observations/545 = 73.4% of data still here
summary(Data1.4.1$G)

summary(Data1.4.1$PTS)
#how do we fix the skew
summary(Data1.4.1$Contract18.19)

summary(Data1.4.1$MP)
summary(Data1.4.1$`FG%`)
summary(Data1.4.1$`3P%`)
summary(Data1.4.1$TOV)
summary(Data1.4.1$PER)

summary(Data1.4.1$Age)

#thinking about making age a factor 
#may be too few for 39-42
AgeGroups<-c("19-23", "24-28", "29-33","34-38", "39-42")
Data1.4.1$Age<-as.character(Data1.4.1$Age)
#change text of anyone age 18-22 to "18-22", etc..)
Data1.4.2<-Data1.4.1

Data1.4.2$Tm <- as.factor(Data1.4.2$Tm)
summary(Data1.4.2$Tm)
#shows team distribution, and also TOT is the players that were traded over the course of the season

#renames FG%,3P%,FT% among others
names(Data1.4.2)[names(Data1.4.2) == "FG%"] <- "FGP"
names(Data1.4.2)[names(Data1.4.2) == "3P%"] <- "3PP"
names(Data1.4.2)[names(Data1.4.2) == "FT%"] <- "FTP"
names(Data1.4.2)[names(Data1.4.2) == "2P%"] <- "2PP"
names(Data1.4.2)[names(Data1.4.2) == "2PP"] <- "Percent2s"
names(Data1.4.2)[names(Data1.4.2) == "3PP"] <- "Percents3s"
names(Data1.4.2)[names(Data1.4.2) == "TS%"] <- "TrueShooting"
names(Data1.4.2)[names(Data1.4.2) == "Attempted3s/TotalShots"] <- "Attempted3perTotalShots"
names(Data1.4.2)[names(Data1.4.2) == "FGP"] <- "PercentFGs"
names(Data1.4.2)[names(Data1.4.2) == "FTP"] <- "PercentFTs"
names(Data1.4.2)[names(Data1.4.2) == "Attempted3sperTotalShots"] <- "Percent3sOfAllFG"




#Added more data at this point, combined more tables and removed unneccesary data
Data1.5<- Data1.4.2
Data1.5[22:27]<- NULL
#removing contract columns and reinserting them after we put the extra data back in
#also removing win shares as they don't seem to factor into contract money


Player.Info...Sheet1 <- read.csv("C:/Users/s_sch/Downloads/Player Info - Sheet1.csv")

Data1.5$Height.In.Cm <-Player.Info...Sheet1$Height.Centimeters.[match(Data1.5$Player, Player.Info...Sheet1$Player)]
Data1.5$School <-Player.Info...Sheet1$School[match(Data1.5$Player, Player.Info...Sheet1$Player)]
Data1.5$NBA.Tenure <-Player.Info...Sheet1$NBA.Tenure[match(Data1.5$Player, Player.Info...Sheet1$Player)]
Data1.5$DraftPick <-Player.Info...Sheet1$Draft.Pick[match(Data1.5$Player, Player.Info...Sheet1$Player)]
Data1.5$AllStar <-Player.Info...Sheet1$All.Star.2019[match(Data1.5$Player, Player.Info...Sheet1$Player)]

#now reinserting the contract data
Data1.6<- Data1.5
Data1.6$Contract18.19 <-NBA2019Contracts$`2018-19`[match(Data1.6$Player, NBA2019Contracts$Player)]


write.xlsx(Data1.6, file = "PutTogetherData.xlsx", sheetName = "Player_List",
           col.names = TRUE, row.names =TRUE, append = FALSE)

view(Data1.6)
summary(Data1.6)
str(Data1.6)

removingfactorsforanalysis<- c(2,4,18,23,26)

#age isnt working have to make it numeric again

Data1.6$Age<-as.numeric(Data1.6$Age)

Data1.6Loadings<-Data1.6[,-removingfactorsforanalysis]
Data1.6Loadings2 <- data.frame(scale(Data1.6Loadings[,-1])) # omit the brand variable (factor) 

sapply(Data1.6Loadings2, FUN=mean) %>% round(4)
sapply(Data1.6Loadings2, FUN=sd) %>% round(4)
# as expected
par(mfrow=c(1,2))
corrplot(cor(Data1.6Loadings[,2:21]), method="circle", addCoef.col="grey", type="upper") 
corrplot(cor(Data1.6Loadings2), method="circle", addCoef.col="grey", type="upper") 

fa.parallel(Data1.6Loadings2, fa="fa", n.iter=100, show.legend=T)#recommends 5 factors

Data1.6Loadsingsfa <- factanal(Data1.6Loadings2, 5) 
Data1.6Loadsingsfa$loadings 
#notes-- columns to group into similar factors for regression, anything greater than .4 will be notable
#Factor 1.... GS-.736, PTS .893, TRB .616, AST .83, STL .739, MP .859, TOV .928, PER, .631, Contract 18.19 .624
#Factor 2.... PercentFGS-.921, Percent2s .827, PER .571, TrueShooting .915
#Factor 3.... Age-.922, NBA Tenure-.952
#Factor 4.... Percent3s-.686, PercentFTs-.535, Attempted3sperTotalShots-.826 
#Factor 5.... TRB-.572, BLK .587, HeightInCm .714




#time to add multiple regression based on factor types

mod1 <- lm(Contract18.19~ PTS*TOV+TRB+AST+STL+PER+GS+MP, data=Data1.6)
options(scipen=99)
summary(mod1)
#significant: PTS .019273, TOV: .000468, TRB .00005, AST .0006, STL .073 
#Estimate Start: 1,027,763, PTS +616,439, TOV -5,285,751, TRB +1,070,978, AST +1,691,603
#R-squared: .4342

mod2<-lm(Contract18.19~ PercentFGs+Percent2s+PER+TrueShooting, data= Data1.6)
summary(mod2)
#significant: PercentFG: .000188, PER <.000000000002
#Estimate Start: 7,745,104, PercentFGs -44,891,538, PER: +182639
#R-squared: .3051

mod3<-lm(Contract18.19~ Age*NBA.Tenure, data = Data1.6)
summary(mod3)
#significant: NBA.Tenure <.00000000002, Age*Tenure: .00000000272
#Estimate Start: -5,853,601, NBA.Tenure +4,368,705, Age*Tenure -107,139
#R-squared: .3086

mod4<-lm(Contract18.19~ Percents3s+PercentFTs+Attempted3perTotalShots, data=Data1.6)
summary(mod4)
#significant: #PercentFTs: .000371, Attempted.00450
#Estimate: -2,248,014, PercentFTs +13,381,043, Attempted3perTotalShots-89,226,97
#Rsquared- .050

mod5<-lm(Contract18.19~ TRB+BLK+Height.In.Cm, data=Data1.6)
summary(mod5)
#significant: TRB: <.000000000000002, Height.In.Cm: .00000466
#Estimate Start: 43796224, TRB +2074079 per rebound, -219096 per cm of height
#R-squared- .2726

mod6<-lm(Contract18.19~AllStar+DraftPick+School+Pos, data=Data1.6)
summary(mod6)
#significant: AllStarNo: .0052, AllStarYes: .00000000164, DraftPick: .000000009
#Estimate Start: 20637000, AllStarYes +11165995, DraftPick -105635
#R-squared- .311


# to combine the best aspects of these 5 into one regression
mod7<-lm(Contract18.19~ PTS+TOV+TRB+AST+PER+PercentFGs+Age*NBA.Tenure+PercentFTs+AllStar+DraftPick, data=Data1.6)
summary(mod7)
#significant: PTS: .0000823, TRB: .000118, AST: .0152, Age: .000018, NBA.Tenure: .0000000000011, AllStarYes: .009982, DraftPick: .019
# Age*Tenure: .000000004752
#Estimate Start: -12,438,579, PTS: +440,811, TRB:+719,192, AST:+759,653, Age:+740,740, NBATenure:+2,674,338, AllStarYes: +3,455,768
#Draft Pick: -$38,557, Age:NBA.Tenure: -77,076
#R-sqaured- .5885

vif(mod1)#PTS, TOV, AST, PER, MP, PTS:TOV over 5
vif(mod2)# PercentFGs over 5
vif(mod3)#NBA Tenure, Age:NBATenure
vif(mod4)#None
vif(mod5)#None
vif(mod6)#School, but doesn't matter, Pos is affected as well
vif(mod7)#PTS, TOV, PER, Age, NBA Tenure, Age:NBA Tenure





#2017-2018 NBA DATA


NBAStats17_18 <- read_excel("IDS462/NBAStats17.18.xlsx")
View(NBAStats17_18)

NBAAdvancedStats17_18 <- read_excel("IDS462/NBAAdvancedStats17.18.xlsx")
View(NBAAdvancedStats17_18)

NBAContracts17_18 <- read_excel("IDS462/NBAContracts17.18.xlsx")
View(NBAContracts17_18)

usedstats<-c("Player", "Tm", "Age", "Pos","G","GS","PTS","TRB", "AST", "STL", "BLK","MP", "FG%", "3P%", "2P%", "FT%", "TOV")
usedAdvStats<-c("Player", "PER", "TS%", "3PAr", "FTr", "WS", "OWS", "DWS")
usedContracts<-c("Player", "2017-18")

Data2.0<-NBAStats17_18[usedstats]
Data2.0.1<-NBAAdvancedStats17_18[usedAdvStats]
names(Data2.0.1)[names(Data2.0.1) == "Player"]<- "Player2"
Data2.1 <- cbind(Data2.0, Data2.0.1)

RepeatNames1718<-c(24,25,28,29,45,46,59,60,68,69,70,73,74,77,78,97,98,102,103,118,119,122,123,142,143,145,146,167,168,174,175,
                   180,181,190,191,202,203,229,230,237,238,242,243,250,251,252,259,260,266,267,276,277,288,289,299,300,306,307,
                   314,315,319,320,328,329,331,332,341,342,343,344,368,369,386,387,401,402,419,420,425,426,427,439,440,442,443,450,451,
                   660,661,651,650,642,643,623,624,611,610,607,606,604,603,602,587,586,569,568,551,550,536,535,528,527,524,523,
                   497,496,494,493,482,481,472,471,455,454)

Data2.1.1 <- Data2.1[-c(RepeatNames1718),]
#removed names
Data2.2<-Data2.1.1
Data2.2$Contract17.18 <-NBAContracts17_18$`2017-18`[match(Data2.1.1$Player, NBAContracts17_18$Player)]
#added salary column as long as player names match

#check for and remove NAs
colSums(is.na(Data2.2))
str(Data2.2)
Data2.3 <- Data2.2%>% drop_na()
colSums(is.na(Data2.3))

summary(Data2.3$PTS)
boxplot(Data2.3$PTS)
boxplot(Data2.3$Salary)

Data2.3$Pos<-sub("-.*","",Data2.3$Pos) 
view(Data2.3)


Data2.3$Pos <- as.factor(Data2.3$Pos)
summary(Data2.3$Pos)


Data2.4<-Data2.3
#Removes any players with 16 or less games played
Data2.4.1 <- Data2.4 %>% filter(Data2.4$G > 16)

#still have 401 observations/545 = 73.4% of data still here
summary(Data2.4.1$G)

summary(Data2.4.1$PTS)
#how do we fix the skew
summary(Data2.4.1$Contract17.18)

summary(Data2.4.1$MP)
summary(Data2.4.1$`FG%`)
summary(Data2.4.1$`3P%`)
summary(Data2.4.1$TOV)
summary(Data2.4.1$PER)
summary(Data2.4.1$Age)


Data2.4.1$Tm <- as.factor(Data2.4.1$Tm)
summary(Data2.4.1$Tm)
#shows team distribution, and also TOT is the players that were traded over the course of the season

Data2.4.2<-Data2.4.1
#renames FG%,3P%,FT% among others
names(Data2.4.2)[names(Data2.4.2) == "FG%"] <- "FGP"
names(Data2.4.2)[names(Data2.4.2) == "3P%"] <- "3PP"
names(Data2.4.2)[names(Data2.4.2) == "FT%"] <- "FTP"
names(Data2.4.2)[names(Data2.4.2) == "2P%"] <- "2PP"
names(Data2.4.2)[names(Data2.4.2) == "2PP"] <- "Percent2s"
names(Data2.4.2)[names(Data2.4.2) == "3PP"] <- "Percents3s"
names(Data2.4.2)[names(Data2.4.2) == "TS%"] <- "TrueShooting"
names(Data2.4.2)[names(Data2.4.2) == "Attempted3s/TotalShots"] <- "Attempted3perTotalShots"
names(Data2.4.2)[names(Data2.4.2) == "FGP"] <- "PercentFGs"
names(Data2.4.2)[names(Data2.4.2) == "FTP"] <- "PercentFTs"
names(Data2.4.2)[names(Data2.4.2) == "3PAr"] <- "Percent3sOfAllFG"

Data2.5<- Data2.4.2
Data2.5[22:26]<- NULL

write.xlsx(Data2.5, file = "PlayerList17.18.xlsx", sheetName = "Player_List",
           col.names = TRUE, row.names =TRUE, append = FALSE)

Player.Info.Sheet.17.18...Sheet1 <- read.csv("C:/Users/s_sch/Downloads/Player Info Sheet 17.18 - Sheet1.csv")

Data2.5$Height.In.Cm <-Player.Info.Sheet.17.18...Sheet1$Height.Centimeters.[match(Data2.5$Player, Player.Info.Sheet.17.18...Sheet1$Player)]
Data2.5$School <-Player.Info.Sheet.17.18...Sheet1$School[match(Data2.5$Player, Player.Info.Sheet.17.18...Sheet1$Player)]
Data2.5$NBA.Tenure <-Player.Info.Sheet.17.18...Sheet1$NBA.Tenure[match(Data2.5$Player, Player.Info.Sheet.17.18...Sheet1$Player)]
Data2.5$DraftPick <-Player.Info.Sheet.17.18...Sheet1$Draft.Pick[match(Data2.5$Player, Player.Info.Sheet.17.18...Sheet1$Player)]
Data2.5$AllStar <-Player.Info.Sheet.17.18...Sheet1$All.Star.2018[match(Data2.5$Player, Player.Info.Sheet.17.18...Sheet1$Player)]

#now reinserting the contract data
Data2.6<- Data2.5
Data2.6$Contract17.18 <-NBAContracts17_18$`2017-18`[match(Data2.6$Player, NBAContracts17_18$Player)]


view(Data2.6)
summary(Data2.6)
str(Data2.6)

removingfactorsforanalysis<- c(2,4,18,23,26)


Data2.6Loadings<-Data2.6[,-removingfactorsforanalysis]
Data2.6Loadings2 <- data.frame(scale(Data2.6Loadings[,-1])) # omit the brand variable (factor) 

sapply(Data2.6Loadings2, FUN=mean) %>% round(4)
sapply(Data2.6Loadings2, FUN=sd) %>% round(4)
# as expected
par(mfrow=c(1,2))
corrplot(cor(Data2.6Loadings[,2:21]), method="circle", addCoef.col="grey", type="upper") 
corrplot(cor(Data2.6Loadings2), method="circle", addCoef.col="grey", type="upper") 

fa.parallel(Data2.6Loadings2, fa="fa", n.iter=100, show.legend=T)#recommends 6 factors

Data2.6Loadsingsfa <- factanal(Data2.6Loadings2, 6) 
Data2.6Loadsingsfa$loadings 

#notes-- columns to group into similar factors for regression, anything greater than .4 will be notable
#Factor 1.... GS-.733, PTS .899, TRB .544, AST .807, STL .762, MP .91, TOV .873, PER, .606, Contract 18.19 .563
#Factor 2.... PercentFGS-.891, Percent2s .823, PER .565, TrueShooting .944
#Factor 3.... Percent3s-.569, PercentFTs-.570, Attempted3sperTotalShots-.834 
#Factor 4.... Age-.923, NBA Tenure-.987
#Factor 5.... TRB-.65, BLK .645, HeightInCm .765
#Factor 6.... N/A, No points are over .335




#time to add multiple regression based on factor types

mod8 <- lm(Contract17.18~ PTS*TOV+AST+STL+PER+GS+MP, data=Data2.6)
options(scipen=99)
summary(mod8)
#significant:  TOV: .000881, PER .004691, GS .0000103, PTS:TOV .000429 
#Estimate Start: -1220729, TOV -4002598, PER +276262, GS +76,040
#R-squared: .4558

mod9<-lm(Contract17.18~ PercentFGs+Percent2s+PER+TrueShooting, data= Data2.6)
summary(mod9)
#significant: PercentFG: .0000581, PER <.000000000002
#Estimate Start: -1431000, PercentFGs -44421113, PER: +999,472, TrueShooting: +22,653,227
#R-squared: .2949

mod10<-lm(Contract17.18~ Age*NBA.Tenure, data = Data2.6)
summary(mod10)
#significant: NBA.Tenure <.00000000002, Age*Tenure: .<00000000002
#Estimate Start: -116,761, NBA.Tenure +5,257,935, Age*Tenure -124,079
#R-squared: .348

mod11<-lm(Contract17.18~ Percent3sOfAllFG+PercentFTs+Percents3s, data=Data2.6)
summary(mod11)
#significant: #PercentFTs: .0000745, Percent3sOfAllFG:00497
#Estimate: -4234572, PercentFTs +15,278,865, Percent3sOfAllFG: -6,225,995
#Rsquared- .04878

mod12<-lm(Contract17.18~ TRB+BLK+Height.In.Cm, data=Data2.6)
summary(mod12)
#significant: Intercept: .00011, TRB: .00000000000000578, Height.In.Cm: .000207
#Estimate Start: 33560522, TRB +1689921 per rebound, --166863 per cm of height
#R-squared- .2435

mod13<-lm(Contract17.18~AllStar+DraftPick+School+Pos, data=Data2.6)
summary(mod13)
#significant: AllStarNo: .0052, AllStarYes: .00000000164, DraftPick: .000000009
#Estimate Start: 20637000, AllStarYes +11165995, DraftPick -105635
#R-squared- .311


# to combine the best aspects of these 5 into one regression
mod14<-lm(Contract17.18~ PTS+TOV+TRB+AST+PER+PercentFGs+Age*NBA.Tenure+PercentFTs+AllStar+DraftPick, data=Data2.6)
summary(mod14)
#significant: PTS: .0000823, TRB: .000118, AST: .0152, Age: .000018, NBA.Tenure: .0000000000011, AllStarYes: .009982, DraftPick: .019
# Age*Tenure: .000000004752
#Estimate Start: -12,438,579, PTS: +440,811, TRB:+719,192, AST:+759,653, Age:+740,740, NBATenure:+2,674,338, AllStarYes: +3,455,768
#Draft Pick: -$38,557, Age:NBA.Tenure: -77,076
#R-sqaured- .5885

vif(mod8)#PTS, TOV, AST, PER, MP, PTS:TOV over 5
vif(mod9)# PercentFGs over 5
vif(mod10)#NBA Tenure, Age:NBATenure
vif(mod11)#None
vif(mod12)#None
vif(mod13)#School, but doesn't matter, Pos is affected as well
vif(mod14)#PTS, TOV, PER, Age, NBA Tenure, Age:NBA Tenure
