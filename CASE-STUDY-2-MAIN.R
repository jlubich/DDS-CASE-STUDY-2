##############################################################################
## CASE STUDY 2 - Attrition Analysis
## 2017/11/26 - Jeff, Will, Yejur, Jeremy
## -- FLOW --
## 1 - Explore the available data and clean as necessary
## 2 - Establish the baseline attrition rate
## 3 - Identify 3-4 Attrition Rate factors
## 4 - Identify 3-4 interesting Job Specific Trends
## 5 Conclusions - where focused effort can get you the biggest bang for the buck
## IMPACT
##############################################################################

## Initialization
library(plyr)
library(dplyr)
library(binr)


## Set the WD to where the R file is located
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

##############################################################################
## 1 - Explore the available data and clean as necessary
##############################################################################


## Load up the Talent Data
TalentData <- read.csv("./DATA/casestudy2-data.csv")

## Review basic summary of data contents
str(TalentData)
summary(TalentData)

## Fix Garbled Column Name
names(TalentData)[1] <- "Age"

## Recode Attrition flag for counting
TalentData$AttritionCount <- ifelse(TalentData$Attrition=="Yes",1,0)

## Map numerical flags to Categorical Labels
TalentData$Education.Label <- mapvalues(
  TalentData$Education, 
  from = c(1,2,3,4,5), 
  to = c("Below College", "College", "Bachelor", "Master", "Doctor")
)

TalentData$EnvironmentSatisfaction.Label <- mapvalues(
  TalentData$EnvironmentSatisfaction, 
  from = c(1,2,3,4), 
  to = c("Low", "Medium", "High", "Very High")
)

# set up boundaries for intervals/bins
breaks <- c(0,25,35,45,55,65,1000)
# specify interval/bin labels
labels <- c("18-24", "25-34", "35-44", "45-54", "55-64", "65+")
# bucketing data points into bins
TalentData$Age.Bin <- cut(TalentData$Age, breaks, include.lowest = T, right=FALSE, labels=labels)



?bins.split(TalentData$Age, 8, 0, 2000)
bins.quantiles(TalentData$Age, 8, 8)


?cut2split(TalentData, cut2(TalentData$Age, g=10))
  
# 
# Education	1 'Below College'
# 2 'College'
# 3 'Bachelor'
# 4 'Master'
# 5 'Doctor'
# 
# EnvironmentSatisfaction	1 'Low'
# 2 'Medium'
# 3 'High'
# 4 'Very High'
# 
# JobInvolvement	1 'Low'
# 2 'Medium'
# 3 'High'
# 4 'Very High'
# 
# JobSatisfaction	1 'Low'
# 2 'Medium'
# 3 'High'
# 4 'Very High'
# 
# PerformanceRating	1 'Low'
# 2 'Good'
# 3 'Excellent'
# 4 'Outstanding'
# 
# RelationshipSatisfaction	1 'Low'
# 2 'Medium'
# 3 'High'
# 4 'Very High'
# 
# WorkLifeBalance	1 'Bad'
# 2 'Good'
# 3 'Better'
# 4 'Best'


##############################################################################
## 2 - Establish the baseline attrition rate
##############################################################################

EmployeeCount.Total = sum(TalentData$EmployeeCount)
AttritionCount.Total = sum(TalentData$AttritionCount)

AttritionRate.Percent = round(AttritionCount.Total / EmployeeCount.Total, 2) * 100 






##############################################################################
## 3 - Identify 3-4 Attrition Rate factors
##############################################################################

##############################################################################
## 4 - Identify 3-4 interesting Job Specific Trends
##############################################################################

##############################################################################
## 5 Conclusions - where focused effort can get you the biggest bang for the buck
##############################################################################

