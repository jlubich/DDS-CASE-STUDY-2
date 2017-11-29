##############################################################################
## CASE STUDY 2 - Attrition Analysis
## 2017/11/26 - Jeff, Will, Yejur, Jeremy
## -- FLOW --
## 1 - Explore the available data and clean as necessary
## 2 - Establish the baseline attrition rate
## 3 - Identify 3-4 Attrition Rate factors
## 4 - Identify 3-4 interesting Job Specific Trends
## 5 Conclusions - where focused effort can get you the biggest bang for the buck
##      Lack of low/med JobPerformance measure
## IMPACT
##############################################################################

## Executive Summary
## 2DS analyzed DDSAnalytics' 2016 employee data and determined the top three leading factors that contributed to the organization's attrition rate of 16.12% were: VARIABLE1, VARIABLE2, VARIABLE3. 2DS based this conclusion on the variables that produced the three highest attrition rates relative to the benchmark of 16.12%. Variables that lacked a clear definition or deep understanding were ommitted, but can be included in a refrshed analysis or in any next steps. This is dependant on DDSAnalytcis providing 2DS with sufficient information. The variables ommitted from the analysis included: Daily Rate, Hourly Rate, Monthly Income, Monthly Rate. 
##Based on these findings, 2DS recommends DDSAnalytics moves forwars with a predictive model that focuses on, but is not limited to the variables mentioned above. As the model is built DDSAnalytics must consider variables that overlap and be sure to only include one. For example, age and the number of companies worked may have a strong overlap and may make it difficult for the organization to properly understand which variable is influencing attrition.

## Background
## DDSAnalytics contacted 2DS to conduct an exploratory data analysis (EDA) to determine the three leading factors that contributed to attitrion. DDSAnalytics is planning to leverage the insights to approve a project that will leverage data science within the talent management market. 

## Scope
## 2DS was requested to condcut an EDA on the interaction of 34 variables with attrition. Additional variables were calculated to assist with the analysis. The project did not include statistical analyses, machine learning techniques, or predictive models. 2DS was also requested to report additional trends including those related to job roles.

## Approach
## 2DS calculated the 2016 attrition rate within the organizaiton as 16.12% and used this as a benchmark to compare other variables. 2DS then calculated attrition rates within each variable to identify if any of the subpopulations had an attrition rate greater than the benchmark. Once all variables were examined, 2DS identified the variables that contained the three highest subpopulations. 

## Key Findings
## 2DS identified VARIABLE1, VARIABLE2, and VARIABLE3 with the three highest attrition rates within the organizaiton. VARIABLE1, VARIABLE2, and VARIABLE3 had attrition rates of XX.X%, XX.X%, XX.X% and "Yes" attrion response of XX, XX, XX. 
## FILL IN HERE FOR ADDITIONAL INSIGHTS


## Next Steps
## While the percentages mentioned above represent the three highest attrition rates within DDSAnalytics, it is recommended that a statistical analysis is conducted to determine if the percentages and the number of "Yes" responses are statistically significant. If they are not significant then variable should not be considered. For example, if a variable had an attrition rate or 75% but there were only 3 "Yes" responses DDSAnalytics should not consider this a reliable variable when determining variables that may help build a predictive model.
## 2DS also recommends that all salary information be listed as yearly income. This will allow for a future analysis that can determine if income influences attrition significantly more than other variables. Based on that new information DDSAnalytics can potentially build a stronger predictive model.
## 2DS also recommends that DDSAnalytics begins to build and/or strengthen its abaility to integrate machine learning into its Data Science project if it is approved. This will allow for future models to be built through automation, which may reduce cost and increase return on investment.

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

TalentData$JobInvolvement.Label <- mapvalues(
  TalentData$JobInvolvement, 
  from = c(1,2,3,4), 
  to = c("Low", "Medium", "High", "Very High")
)

TalentData$JobSatisfaction.Label <- mapvalues(
  TalentData$JobSatisfaction, 
  from = c(1,2,3,4), 
  to = c("Low", "Medium", "High", "Very High")
)

TalentData$PerformanceRating.Label <- mapvalues(
  TalentData$PerformanceRating, 
  from = c(1,2,3,4), 
  to = c("Low", "Good", "Excellent", "Outstanding")
)

TalentData$RelationshipSatisfaction.Label <- mapvalues(
  TalentData$RelationshipSatisfaction, 
  from = c(1,2,3,4), 
  to = c("Low", "Medium", "High", "Very High")
)

TalentData$WorkLifeBalance.Label <- mapvalues(
  TalentData$WorkLifeBalance, 
  from = c(1,2,3,4), 
  to = c("Bad", "Good", "Better", "Best")
)


# set up boundaries for intervals/bins
breaks <- c(0,25,35,45,55,65,1000)
# specify interval/bin labels
labels <- c("18-24", "25-34", "35-44", "45-54", "55-64", "65+")
# bucketing data points into bins
TalentData$Age.Bin <- cut(TalentData$Age, breaks, include.lowest = T, right=FALSE, labels=labels)

# set up boundaries for intervals/bins

breaks <- c(0,2500,5000,7500,10000,12500,15000,17500,20000,22500,25000,Inf)
# specify interval/bin labels
labels <- c("0-2500", "2501-5000", "5001-7500", "7501-10000","10001-12500", "12501-15000","15001-17500","17501-20000","20001-22500","22501-25000","25000+")
# bucketing data points into bins
TalentData$MonthlyRate.Bin <- cut(TalentData$MonthlyRate, breaks, include.lowest = T, right=FALSE, labels=labels)
ac <- table(TalentData$MonthlyRate.Bin)
barplot(ac)









ac <- table(TalentData$EducationField)
barplot(ac)

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


## We can create mini pivot tables that allow us to calculate a percentage for each category within a given variable.
## For example we can build a table that shows us the attrition rate of each BusinessTravel group as seen below:

library(tidyr)
library(dplyr)

### Read data into R
HR <- read.csv("CaseStudy2.csv")

## Note: There is no need to convert yes and no to 1 and 0

### Turn desired variables into a table
HRtable <- table(HR$BusinessTravel, HR$Attrition)

#Obtain the rate of attrition for each group
round(prop.table(HRtable,1),2)


### Use the comments below if row% is not desired
### for cell %
## round(prop.table(DF$NAME),2)

### for row %
## round(prop.table(DF$NAME,1),2)

### for column %
## round(prop.table(DF$NAME,2),2)


## If we wanted to include something like hourly rate, even though we don't at this point, we can still use the above structure and then we need to find a way to group the data within the table. 

##############################################################################
## 4 - Identify 3-4 interesting Job Specific Trends
##############################################################################

##############################################################################
## 5 Conclusions - where focused effort can get you the biggest bang for the buck
##############################################################################

