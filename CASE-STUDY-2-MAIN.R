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
##Based on these findings, 2DS recommends DDSAnalytics moves forward with a predictive model that focuses on, but is not limited to the variables mentioned above. As the model is built DDSAnalytics must consider variables that overlap and be sure to only include one. For example, age and the number of companies worked may have a strong overlap and may make it difficult for the organization to properly understand which variable is influencing attrition.

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
library(ggplot2)

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
breaks <- c(0,5,10,15,20,25,30,35,40,Inf)
# specify interval/bin labels
labels <- c("0-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34", "35-39", "40+")
# bucketing data points into bins
TalentData$DistanceFromHome.Bin <- cut(TalentData$DistanceFromHome, breaks, include.lowest = T, right=FALSE, labels=labels)

# set up boundaries for intervals/bins
breaks <- c(0,3,6,11,21,Inf)
# specify interval/bin labels
labels <- c("0-1", "2-5", "6-10", "11-20", "20+")
# bucketing data points into bins
TalentData$YearsAtCompany.Bin <- cut(TalentData$YearsAtCompany, breaks, include.lowest = T, right=FALSE, labels=labels)


# set up boundaries for intervals/bins
breaks <- c(0,3,6,11,21,Inf)
# specify interval/bin labels
labels <- c("0-1", "2-5", "6-10", "11-20", "20+")
# bucketing data points into bins
TalentData$TotalWorkingYears.Bin <- cut(TalentData$TotalWorkingYears, breaks, include.lowest = T, right=FALSE, labels=labels)

# set up boundaries for intervals/bins
breaks <- c(0,5000,10000,15000,20000,25000,Inf)
# specify interval/bin labels
labels <- c("0-5000", "5001-10000","10001-15000","15001-20000","20001-25000","25000+")
# bucketing data points into bins
TalentData$MonthlyRate.Bin <- cut(TalentData$MonthlyRate, breaks, include.lowest = T, right=FALSE, labels=labels)
ac <- table(TalentData$MonthlyRate.Bin)


### barplot(ac)









ac <- table(TalentData$EducationField)
barplot(ac)

?bins.split(TalentData$Age, 8, 0, 2000)
bins.quantiles(TalentData$Age, 8, 8)


?cut2split(TalentData, cut2(TalentData$Age, g=10))
 


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
HR <- read.csv("./DATA/casestudy2-data.csv")

## Note: There is no need to convert yes and no to 1 and 0

### Turn desired variables into a table
HRtable <- table(HR$BusinessTravel, HR$Attrition)

#Obtain the rate of attrition for each group
round(prop.table(HRtable,1),2)

Attrition.List <- list()

for(i in names(HR)){
  HRtable2 <- table(HR[[i]], HR$Attrition)
  HRtable3 <- round(prop.table(HRtable2,1),2)
  #ColumnName <- as.character(names(HR)[i])
  #HRtable3$ColumnName <- ColumnName
  Attrition.List[[i]] <- HRtable3
}



## Here's the magic function which computes the Attrition Rates, and Counts
fun.AttritionRate <- function(input.table, group.by.column) {
  input.table %>%
    dplyr::group_by(CategoryVar = input.table[,group.by.column]) %>%
    dplyr::summarise(
      AttritionRate = sum(AttritionCount)/sum(EmployeeCount), 
      AttritionCount = sum(AttritionCount),
      EmployeeCount = sum(EmployeeCount)) %>%
    dplyr::arrange(CategoryVar)
}

## Choose the columns which make sense
ColumnsToAnalyze <- c(
  which( colnames(TalentData)=="NumberOfCompaniesWorked" ),
  which( colnames(TalentData)=="BusinessTravel" ),
  which( colnames(TalentData)=="Department" ),
  which( colnames(TalentData)=="EducationField" ),
  which( colnames(TalentData)=="Gender" ),
  which( colnames(TalentData)=="MaritalStatus" ),
  which( colnames(TalentData)=="JobRole" ),
  which( colnames(TalentData)=="OverTime" ),
  grep("*Label", names(TalentData)), 
  grep("*Bin", names(TalentData))
)


#TalentData <- as_tibble(TalentData)

## Initilize a list to hold all the attrition rate details
Attrition.List = list()

## Loop through all of the columns in TalentData and get the attrition rate details
for(i in ColumnsToAnalyze){
  #Get the name of the current column 
  SummaryColumn <- names(TalentData)[i]
  #Get the attrition rate details into a DF
  SummaryResults <- fun.AttritionRate(TalentData, i)
  #Concatenate the name of the current column to the details
  SummaryResults$ColumnName <- SummaryColumn
  #Store the details into a list with all other column details
  Attrition.List[[i]] <- SummaryResults
}  

## Create a single data frame out of all of the detail results
attrition_data = do.call(rbind, Attrition.List)


## Plot
ggplot(data = attrition_data, mapping = aes(x = CategoryVar, y = AttritionRate, width=AttritionCount/100)) + 
  geom_bar(stat="identity", position="identity") + 
  geom_hline(yintercept = .16) + 
  geom_text(aes(label=AttritionCount), vjust=1.6, color="white", size=3.5)+
  #theme(axis.text.x=element_text(angle=45,vjust=0)) +  
  facet_wrap(~ColumnName, scales = 'free_x', ncol = 6)

jobrole <- attrition_data[which(attrition_data$ColumnName == "JobRole"),]

## Plot
ggplot(data = jobrole, mapping = aes(x = CategoryVar, y = AttritionRate, width=AttritionCount/100)) + 
  geom_bar(stat="identity", position="identity") + 
  geom_hline(yintercept = .16) + 
  geom_text(aes(label=AttritionCount), vjust=1.6, color="white", size=3.5)+
  #theme(axis.text.x=element_text(angle=45,vjust=0)) +  
  facet_wrap(~ColumnName, scales = 'free_x', ncol = 6)

library(dplyr)
library(reshape2)
names(TalentData)
melt(TalentData[,c("JobInvolvement", "JobSatisfaction", "EmployeeCount")], measure.vars = "EmployeeCount")

plyr

TalentData$JobInvolvement.Label <- plyr::mapvalues(
  TalentData$JobInvolvement, 
  from = c(1,2,3,4), 
  to = c("Low", "Medium", "High", "Very High")
)

TalentData$JobSatisfaction.Label <- plyr::mapvalues(
  TalentData$JobSatisfaction, 
  from = c(1,2,3,4), 
  to = c("Low", "Medium", "High", "Very High")
)

TotalEmployees <- sum(TalentData$EmployeeCount)


Job.Sat.vs.Inv <-
TalentData[,c("JobInvolvement", "JobSatisfaction", "JobInvolvement.Label", "JobSatisfaction.Label", "EmployeeCount")] %>%
  dplyr::group_by(JobInvolvement, JobSatisfaction, JobInvolvement.Label, JobSatisfaction.Label) %>%
  dplyr::summarise(EmployeeCount = sum(EmployeeCount)) 

Job.Sat.vs.Inv$Percent <- Job.Sat.vs.Inv$EmployeeCount / TotalEmployees 
names(TalentData)

JobRole.Avg.Age <-
  TalentData[,c("JobRole", "Age", "EmployeeCount")] %>%
  dplyr::group_by(JobRole) %>%
  dplyr::summarise(EmployeeCount = sum(EmployeeCount), AverageAge = mean(Age)) 

Employee.Performance <- TalentData[,c("EmployeeNumber", "Age", "MonthlyIncome", "JobRole")] 

Employee.Performance$Age <- jitter(Employee.Performance$Age, factor = 3, amount = NULL)

ggplot(data=Employee.Performance, aes(x=Age, y=MonthlyIncome, color=JobRole, fill=JobRole, shape=JobRole)) + 
  geom_point(size=3)


ggplot(data=JobRole.Avg.Age, aes(x=EmployeeCount, y=AverageAge, color=JobRole, fill=JobRole)) + 
  geom_point(size=5, shape=23)


, color=cyl
  geom_bar(stat="identity")

ggplot(data=JobRole.Avg.Age, aes(x=JobRole, y=EmployeeCount)) + 
  geom_bar(stat="identity")

  
ggplot(data=JobRole.Avg.Age, aes(x=JobRole)) + 
  geom_bar(aes(y=AverageAge,fill="Sales"), width=.7, stat="identity") +
  geom_line(aes(y=EmployeeCount,group=1,linetype="Cumulative sales"))+
  geom_point(aes(y=EmployeeCount))+
  xlab("JobRole") + ylab("AverageAge") +
  labs(fill="",linetype="")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.5)) +
  scale_x_discrete(limits=data[,1])


<- TalentData[,c("JobInvolvement", "JobSatisfaction", "JobInvolvement.Label", "JobSatisfaction.Label", "EmployeeCount")]


Job.Sat.vs.Inv
library(ggplot2)
ggplot(data = Job.Sat.vs.Inv, aes(JobInvolvement.Label, EmployeeCount, fill = JobSatisfaction.Label)) + 
  geom_bar(stat="identity", position="stack")

library(scales)

ggplot(Job.Sat.vs.Inv,aes(x = reorder(JobInvolvement.Label, JobInvolvement), y = EmployeeCount,fill = reorder(JobSatisfaction.Label, -JobSatisfaction))) + 
  geom_bar(position = "fill",stat = "identity") +
  scale_fill_manual(values=c("#045a8d", "#2b8cbe", "#74a9cf", "#bdc9e1")) +
  xlab("Job Involvement") +
  ylab("100 Percent Stacked") +
  ggtitle("Job Involvement vs Satisfaction\n100 Percent Stacked") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(labels = percent_format()) +
  geom_text(aes(label=EmployeeCount), position="stack", colour="black")

  geom_text(aes(label=EmployeeCount), vjust=-.2, color="navyblue", size=4) 

  
  
  proportion <- proportion %>%
    group_by(cyl) %>%
    mutate(label_y = cumsum(pct))
  
  ggplot(proportion, aes(factor(cyl), pct, fill = factor(am, levels = c(1, 0)))) +
    geom_bar(stat = "identity", color = "grey40") +
    geom_text(aes(label = round(pct, 2), y = label_y), vjust = 1.5, color = "white") +
    scale_fill_manual(values = c("#a1d99b", "#31a354")) +
    labs(fill = "AM")  
  

dat <- read.table(text = "    ONE TWO THREE
1   23  234 324
2   34  534 12
3   56  324 124
4   34  234 124
5   123 534 654",sep = "",header = TRUE)

#Add an id variable for the filled regions
library(reshape)
datm <- melt(cbind(dat, ind = rownames(dat)), id.vars = c('ind'))

library(scales)
ggplot(datm,aes(x = variable, y = value,fill = ind)) + 
  geom_bar(position = "fill",stat = "identity") +
  # or:
  # geom_bar(position = position_fill(), stat = "identity") 
  scale_y_continuous(labels = percent_format())


#bdc9e1
#74a9cf
#2b8cbe
#045a8d

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

