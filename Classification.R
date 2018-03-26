library("xlsx")
library("tidyverse")
library("lme4")
library("MuMIn")
library("rlang")
library("rpart")
library("rpart.plot")
library("randomForest")

#setwd("Y:/Projects/Pool Study/Analysis/2017")
ps_data<-read.xlsx("PS_HabSnorkelSum_2017.xlsx", "AllData")
#Filter data to remove null values
ps_data<-filter(ps_data, !is.na(SiteCode))

#Simple classification Tree Models

ps_data$DOLevel[ps_data$Average_DO<3]<-"Critical (<3)"
ps_data$DOLevel[ps_data$Average_DO>=3 & ps_data$Average_DO<5]<-"Poor (>3 & <5)"
ps_data$DOLevel[ps_data$Average_DO>=5 & ps_data$Average_DO<7]<-"OK (>5 & <7)"
ps_data$DOLevel[ps_data$Average_DO>=7]<-"Good (>7)"
ps_data$DOLevelMin[ps_data$Min_DO<6]<-"Bad"
ps_data$DOLevelMin[ps_data$Min_DO>=6]<-"Good"
ps_data$DOCode[ps_data$Min_DO<6]<-0
ps_data$DOCode[ps_data$Min_DO>=6]<-1

ps_data$DOLevelMin<-as.factor(ps_data$DOLevelMin)
ps_data$PoolStudyDate<-as.Date(ps_data$PoolStudyDate)
ps_data$DaysSinceStart<-ps_data$PoolStudyDate-min(ps_data$PoolStudyDate)
#Makes sure that R knows that Sample Number is a category
ps_data$SampleNumber<-factor(ps_data$SampleNumber)
#add ordering to DOLevel
ps_data$DOLevel<-factor(ps_data$DOLevel, levels = c("Critical (<3)","Poor (>3 & <5)", "OK (>5 & <7)", "Good (>7)"))
#Get rid of any nulls
ps_data_CART<-ps_data%>%filter(!is.na(DOLevel)& !is.na(RiffleCrestThalweg) & !is.na(Site) &!is.na(ReachType) & !is.na(SampleNumber))
#Sort by DO Level
ps_data_CART<-ps_data_CART[order(ps_data_CART$DOLevel),]

#Run a classification tree to determine the major splits of DO quality
model.DO_RCT_SN_SITE<-rpart(DOLevel ~ RiffleCrestThalweg  +SampleNumber +Site, method = "class", data = ps_data_CART, minbucket = 2)
#Graph the model
prp(model.DO_RCT_SN_SITE, 
    #Shows bins on both sides of Split
    type = 4, 
    #Shows number of samples of each category within each grouping
    extra = 101, 
    #prevents the categories from extending to the bottom
    fallen.leaves = F, 
    #Makes text larger
    cex = .5,  
    #Set colors for each category
    box.palette = list("red", " yellow", "light green", "light blue"), 
    #Makes legend bigger
    legend.cex = 1.5)


m1.Binary.DO_RCT_SN_SITE<-rpart(DOLevelMin ~ RiffleCrestThalweg  + DaysSinceStart + ReachType, method = "class", data = ps_data_CART, minbucket = 4)
prp(m1.Binary.DO_RCT_SN_SITE, 
    #Shows bins on both sides of Split
    type = 4, 
    #Shows number of samples of each category within each grouping
    extra = 101, 
    #prevents the categories from extending to the bottom
    fallen.leaves = F, 
    #Makes text larger 
    cex = .5,  
    #Set colors for each category
    box.palette = list("red", "light green"), 
    #Makes legend bigger
    legend.cex = 1.5)

rf_m1<-randomForest(DOLevelMin ~ RiffleCrestThalweg  + DaysSinceStart + ReachType + Volume + Site + MaxDepth + WaterTemp, data = ps_data_CART, ntree = 5000, importance=TRUE)
varImpPlot(rf_m1)

#Logistic Regression
m2.LR<-glm(DOCode ~ RiffleCrestThalweg, data = ps_data, family = binomial)
summary(m2.LR)
ggplot(ps_data, aes(x=RiffleCrestThalweg, y = DOCode)) + geom_point() + stat_smooth(method = "glm", method.args=list(family="binomial"), se = F)
