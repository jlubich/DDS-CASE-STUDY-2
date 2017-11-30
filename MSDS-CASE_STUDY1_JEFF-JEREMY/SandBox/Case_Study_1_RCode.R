#########################################################
## DDS - Case Study 1
## Jeffrey Lancon & Jeremy Lubich
## 2017/10/14
## This code will 
## 1. How many breweries are present in each state?
## 2. Merge beer data with the breweries data. Print the first 6 
##  observations and the last six observations to check the merged file. 
## 3. Report the number of NA's in each column.
## 4. Compute the median alcohol content and international bitterness 
##  unit for each state. Plot a bar chart to compare.
## 5. Which state has the maximum alcoholic (ABV) beer? Which state 
##  has the most bitter (IBU) beer?
## 6. Summary statistics for the ABV variable.
## 7. Is there an apparent relationship between the bitterness of the 
##  beer and its alcoholic content? Draw a scatter plot.
#########################################################

library(plyr)

## Initialize Directories
setwd("~/Users/Prodigy/Documents/GitRepositories/MSDS-CASE_STUDY1_JEFF-JEREMY")
getwd()

##Loading In data file ex0525.csv
Beers <- read.csv('Beers.csv',header = T,sep = ",")
head(Beers)
str(Beers)

Brew <- read.csv('Breweries.csv',header = T,sep = ",")
colnames(Brew) <- c("Brewery_id","Brewery Name","City","State")
head(Brew)
str(Brew)
 

apply(apply(BrewMerged, 2, trimws), 2, sum)

## 1. How many breweries are present in each state?

BrewbyState <- table(Brew$State)
BrewbyState

########  Map of USA with Brewery Quantities ########
BrewMap <- as.data.frame(BrewbyState)
names(BrewMap) <-c("state","Breweries")
BrewMap$state <- trimws(BrewMap$state)
us.regions <- read.csv('US_Regions.csv',header = T,sep = ",")
BrewMap <- merge(BrewMap,us.regions, by="state",all=TRUE)

Breweries.by.state <- read.csv("C:/Users/Prodigy/Documents/GitRepositories/MSDS-CASE_STUDY1_JEFF-JEREMY/US-Regions.csv")

library(ggplot2)
library(fiftystater)
library(mapproj)

# map_id creates the aesthetic mapping to the state name column in your data
Mapplot <- ggplot(BrewMap, aes(map_id = region)) + 
  # map points to the fifty_states shape data
  geom_map(aes(fill = Breweries), map = fifty_states) + 
  expand_limits(x = fifty_states$long, y = fifty_states$lat) +
  coord_map() +
  scale_x_continuous(breaks = NULL) + 
  scale_y_continuous(breaks = NULL) +
  labs(fill = "Brewery\nQuantity",
       title = "Breweries by State",
       x = "",
       y = "") +
  scale_fill_continuous(low = "thistle2", high = "darkred", guide="colorbar")+
  theme(legend.position = "bottom", 
        panel.background = element_blank()) +
  # add border boxes to AK/HI
  fifty_states_inset_boxes()
Mapplot

################################################

## 2. Merge beer data with the breweries data. Print the first 6 
##  observations and the last six observations to check the merged file. 

BrewMerged <- merge(Brew,Beers, by="Brewery_id",all=TRUE)

print("First 6 brewery observations")
head(BrewMerged, 6)

print("Last 6 brewery observations")
tail(BrewMerged, 6)

## 3. Report the number of NA's in each column.

apply(apply(BrewMerged, 2, is.na), 2, sum)

## 4. Compute the median alcohol content and international bitterness 
##  unit for each state. Plot a bar chart to compare.

## Trim Whitespace on the state column

BrewMerged$State <- trimws(BrewMerged$State)

Medians.By.State <- aggregate(BrewMerged$ABV, list(BrewMerged$State), median, na.rm=TRUE)
names(Medians.By.State) <- c("State", "ABV")
Medians.By.State$IBU <- aggregate(BrewMerged$IBU, list(BrewMerged$State), median, na.rm=TRUE)$x

library(tidyr)
BrewMedians <- tidyr::gather(Medians.By.State, "Category", "Value", 2:3)

ggplot(BrewMedians, aes(x = Category, y = Value)) + 
  geom_bar(stat = 'identity', position = 'stack') + facet_grid(~ State)


class(BrewbyState)
major_count[ order(major_count$freq),]

Breweries.By.State <- as.data.frame(BrewbyState)
colnames(Breweries.By.State) <- c('State', 'Count')
Breweries.By.State <- Breweries.By.State[order(Breweries.By.State$Count, decreasing = FALSE), ]



par(las=1, mai= c(.5, .8, 1.5, .5), mgp= c(1, 2.2, 0))

barplot(
        Breweries.By.State$Count,
        names.arg = Breweries.By.State$State,
        main="Main",
        xlab="X Label",
        ylab="Y Label",
        horiz = TRUE,
        las=1,
        ylim = c(0,51),
        cex.names=0.75)

## 5. Which state has the maximum alcoholic (ABV) beer? Which state 
##  has the most bitter (IBU) beer?

BrewMerged[ which.max(BrewMerged$ABV), ]$State

BrewMerged[ which.max(BrewMerged$IBU), ]$State


## 6. Summary statistics for the ABV variable.

summary(BrewMerged$ABV)

## 7. Is there an apparent relationship between the bitterness of the 
##  beer and its alcoholic content? Draw a scatter plot.

library(ggplot2)

ggplot(BrewMerged, aes(IBU, ABV))+ geom_point() + geom_smooth(method=lm,  se=FALSE) 





