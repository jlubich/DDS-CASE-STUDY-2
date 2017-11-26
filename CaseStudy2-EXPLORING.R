library(dplyr)
library(sqldf)
library(reshape2)
#load required packages
library(ggplot2)
library(tabplot)
library(tidyr)
# import data set
data(diamonds)
# make the plot
tableplot(diamonds)
?
tableplot(TalentData[1:15], sortCol = 2)

plot.ts(TalentData[1:10])

## Set the WD to where the R file is located
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

TalentData <- read.csv("casestudy2-data.csv")

names(TalentData)[1] <- "Age"

str(TalentData)
summary(TalentData)

summarise_all(TalentData)

sqldf("SELECT CASE WHEN Attrition = 'Yes' THEN 1 ELSE 0 END AttritionCount, TotalWorkingYears, TrainingTimesLastYear, WorkLifeBalance FROM TalentData ")
sqldf("SELECT ROUND(DailyRate / 100, 0) * 100, COUNT(*) FROM TalentData GROUP BY DailyRate ORDER BY COUNT(*) DESC")

mtcars

head(melt(TalentData))

EmployeeCount

## Establish the mean attrition rate and 95% confidence interval for it


values <- data.frame(value = c("a", "a", "a", "a", "a", 
                               "b", "b", "b", 
                               "c", "c", "c", "c"))
nr.of.appearances <- aggregate(x = values, 
                               by = list(unique.values = values$value), 
                               FUN = length)

aggregate(
  x = TalentData,
  by = as.list(TalentData[c("Age")]),
  FUN = mean)

# do not run the syntax
aggregate(x = d.f, 
          by = by.list, 
          FUN = function(s.d.f){y <- s.d.f; return(y)}
          
          
assets <- data.frame(
    asset.class = c("equity", "equity","equity","option","option","option","bond", "bond"),
    rating = c("AAA", "A", "A", "AAA", "BB", "BB", "AAA", "A"),
                             counterparty.a = c(runif(3), rnorm(5)),
                             counterparty.b = c(runif(3), rnorm(5)),
                             counterparty.c = c(runif(3), rnorm(5))
)

m <- matrix(data=cbind(rnorm(30, 0), rnorm(30, 2), rnorm(30, 5)), nrow=30, ncol=3)

apply(m, 2, function(x) length(x[x<0]))

TalentData$AttritionCount <- ifelse(TalentData$Attrition=="Yes",1,0)

aggregate(cbind(c("AttritionCount", "EmployeeCount")), data = TalentData, sum)

select_if(TalentData, is.numeric)

select_if(TalentData,is.factor)

names(select_if(TalentData,is.factor))
[1] "Attrition"      "BusinessTravel" "Department"     "EducationField" "Gender"         "JobRole"       
[7] "MaritalStatus"  "Over18"         "OverTime"  

TalentData %>%
  summarise(AttritionRate = sum(AttritionCount)/sum(EmployeeCount)) %>%
  arrange(AttritionRate)

TalentData %>%
  group_by(MaritalStatus) %>%
  summarise(AttritionRate = sum(AttritionCount)/sum(EmployeeCount)) %>%
  arrange(AttritionRate)

TalentData %>%
  group_by(BusinessTravel) %>%
  summarise(AttritionRate = sum(AttritionCount)/sum(EmployeeCount)) %>%
  arrange(AttritionRate)

TalentData %>%
  group_by(Department) %>%
  summarise(AttritionRate = sum(AttritionCount)/sum(EmployeeCount)) %>%
  arrange(AttritionRate)

TalentData %>%
  group_by(EducationField) %>%
  summarise(AttritionRate = sum(AttritionCount)/sum(EmployeeCount)) %>%
  arrange(AttritionRate)

tapply(TalentData$AttritionCount,TalentData$BusinessTravel, sum)

by(TalentData,TalentData$BusinessTravel, fun.AttritionRate)

aggregate(TalentData,TalentData$BusinessTravel, fun.AttritionRate)


names(select_if(TalentData,is.factor))

 TalentData %>%
  group_by(MaritalStatus) %>%
  mutate(AttritionRate = sum(AttritionCount)/sum(EmployeeCount)) 
 
 %>%
  arrange(AttritionRate)

 TalentData

library(dplyr) 

 tbl_df(TalentData) %>% 
   mutate(AtCnt = ifelse(Attrition == "Yes", 1, 0)) %>%
   group_by(Age) %>%
   summarise_each(AttritionRate = sum(AtCnt)/sum(EmployeeCount))
 
 tbl_df(TalentData) %>% 
   mutate(AtCnt = ifelse(Attrition == "Yes", 1, 0)) %>%
   group_by(Age) %>%
   summarise(AttritionRate = sum(AtCnt)/sum(EmployeeCount))
 
fun.AttritionRate <- function(input.table) {
  input.table <- as.data.frame(input.table)
  sum(input.table$AttritionCount) / sum(input.table$EmployeeCount)
}   

tbl_df(TalentData) %>% 
  mutate(AtCnt = ifelse(Attrition == "Yes", 1, 0)) %>%
  group_by(Age) %>%
  summarise_each(fun.AttritionRate)

summarise_all(TalentData, funs(fun.AttritionRate))

gather(TalentData, ) 

ggplot(data = melt(select_if(TalentData, is.numeric)), mapping = aes(x = value)) + 
  geom_histogram(bins = 10) + facet_wrap(~variable, scales = 'free')




exposures <- ?aggregate(
  x = assets[c("counterparty.a", "counterparty.b", "counterparty.c")],
        by = assets[c("asset.class", "rating")],
        FUN = function(market.values){
        sum(pmax(market.values, 0))
        }
)


min(5:1, pi) #-> one number
pmin(5:1, pi) #->  5  numbers

library(MASS)
categories <- data.frame(category = c("a", "a", "a", "a", "a", 
                                      "b", "b", "b", "b", "b",
                                      "c", "c", "c", "c"))
observations <- data.frame(observation = c(rnorm(5, mean = 3, sd = 0.2),
                                           rnorm(5, mean = -2, sd = 0.4),
                                           rnorm(4, mean = 0, sd = 1)))

distr.estimate <- aggregate(x = observations, 
                            by = categories,
                            FUN = function(observations){
                              fitdistr(observations, densfun = "normal")$estimate
                            })




## Compute the averages for the variables in 'state.x77', grouped
## according to the region (Northeast, South, North Central, West) that
## each state belongs to.
aggregate(state.x77, list(Region = state.region), mean)

## Compute the averages according to region and the occurrence of more
## than 130 days of frost.
aggregate(state.x77,
          list(Region = state.region,
               Cold = state.x77[,"Frost"] > 130),
          mean)
## (Note that no state in 'South' is THAT cold.)


## example with character variables and NAs
testDF <- data.frame(v1 = c(1,3,5,7,8,3,5,NA,4,5,7,9),
                     v2 = c(11,33,55,77,88,33,55,NA,44,55,77,99) )
by1 <- c("red", "blue", 1, 2, NA, "big", 1, 2, "red", 1, NA, 12)
by2 <- c("wet", "dry", 99, 95, NA, "damp", 95, 99, "red", 99, NA, NA)
aggregate(x = testDF, by = list(by1, by2), FUN = "mean")

# and if you want to treat NAs as a group
fby1 <- factor(by1, exclude = "")
fby2 <- factor(by2, exclude = "")
aggregate(x = testDF, by = list(fby1, fby2), FUN = "mean")


## Formulas, one ~ one, one ~ many, many ~ one, and many ~ many:
aggregate(weight ~ feed, data = chickwts, mean)
aggregate(breaks ~ wool + tension, data = warpbreaks, mean)
aggregate(cbind(Ozone, Temp) ~ Month, data = airquality, mean)
aggregate(cbind(ncases, ncontrols) ~ alcgp + tobgp, data = esoph, sum)

## Dot notation:
aggregate(. ~ Species, data = iris, mean)
aggregate(len ~ ., data = ToothGrowth, mean)

## Often followed by xtabs():
ag <- aggregate(len ~ ., data = ToothGrowth, mean)
xtabs(len ~ ., data = ag)


## Compute the average annual approval ratings for American presidents.
aggregate(presidents, nfrequency = 1, FUN = mean)
## Give the summer less weight.
aggregate(presidents, nfrequency = 1,
          FUN = weighted.mean, w = c(1, 1, 0.5, 1))


set.seed(1234)

total_bill <- rnorm(50, 25, 3)
tip <- 0.15 * total_bill + rnorm(50, 0, 1)
sex <- rbinom(50, 1, 0.5)
smoker <- rbinom(50, 1, 0.3)
day <- ceiling(runif(50, 0,7))
time <- ceiling(runif(50, 0,3))
size <- 1 + rpois(50, 2)
my.data <- as.data.frame(cbind(total_bill, tip, sex, smoker, day, time, size))
my.data

my.table <- table(my.data$smoker)

my.prop <- prop.table(my.table)

cbind(my.table, my.prop)

prop.table(TalentData[,13:15],1)
