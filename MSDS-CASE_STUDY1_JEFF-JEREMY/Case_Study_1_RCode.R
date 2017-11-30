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

## Initialize Directories
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(knitr)
library(ggplot2)
library(fiftystater)
library(mapproj)
library(tidyr)
library(grid)
library(gridExtra)


##Loading In data file Beers.csv 
Beers <- read.csv('Beers.csv',header = T,sep = ",")


## Loading in data file Breweries.csv. File contains information
## about Brewerey's Name, Unique ID number, City and State of Headquarters
Brew <- read.csv('Breweries.csv',header = T,sep = ",")
colnames(Brew) <- c("Brewery_id","Brewery Name","City","State")


## 1. How many breweries are present in each state?

BrewbyState <- table(Brew$State)
BrewbyState

#BrewbyStateS <- as.data.frame(BrewbyState)
#colnames(BrewbyStateS) <- c("State","Quantity")
#kable(BrewbyStateS, align="c", caption = "Table 1.1 Quantity of Brew by State",format="markdown")


########  Map of USA with Brewery Quantities ########
BrewMap <- as.data.frame(BrewbyState)
names(BrewMap) <-c("state","Breweries")
BrewMap$state <- trimws(BrewMap$state)
us.regions <- read.csv('US_Regions.csv',header = T,sep = ",")
BrewMap <- merge(BrewMap,us.regions, by="state",all=TRUE)
# BrewMapWest <- subset(BrewMap, Country_Region=="West")
# BrewMapMidwest <- subset(BrewMap, Country_Region=="Midwest")
# BrewMapSouth <- subset(BrewMap, Country_Region=="South")
# BrewMapNortheast <- subset(BrewMap, Country_Region=="Northeast")



# Creates a mapping of the United states with varying degrees of shading
# depending on data values in dataframe 
Mapplot <- ggplot(BrewMap, aes(map_id = region))+ 
  
# map points to the fifty_states shape data
  geom_map(aes(fill = Breweries), map = fifty_states) + 
  expand_limits(x = fifty_states$long, y = fifty_states$lat) +
  coord_map() +
  scale_x_continuous(breaks = NULL) + 
  scale_y_continuous(breaks = NULL) +
  labs(fill = "Brewery\nQuantity",
       title = "           Fig 1.2 Breweries by State",
       x = "",
       y = "") +
  scale_fill_continuous(low = "orange", high = "darkred", guide="colorbar")+
  theme(legend.position = "bottom", 
        panel.background = element_blank()) +
# add border boxes to AK/HI
  fifty_states_inset_boxes()
# Plots Map
Mapplot

################################################

## 2. Merge beer data with the breweries data. Print the first 6 
##  observations and the last six observations to check the merged file. 

BrewMerged <- merge(Brew,Beers, by="Brewery_id",all=TRUE)

# First 6 Brewery Observations
kable(head(BrewMerged,6), align="c", caption = "Table 1.2a BrewMerged Dataset First 6 Observations",format="markdown",row.names = FALSE)

# Last 6 Brewery Observations
kable(tail(BrewMerged,6), align="c", caption = "Table 1.2b BrewMerged Dataset Last 6 Observations",format="markdown",row.names = FALSE)


## 3. Report the number of NA's in each column.

NAs <- as.data.frame(apply(apply(BrewMerged, 2, is.na), 2, sum))
colnames(NAs) <- "Missing Values"
kable(NAs, align="c", caption = "Table 1.3 Number of Missing observations",format="markdown",row.names = FALSE)

## 4. Compute the median alcohol content and international bitterness 
##  unit for each state. Plot a bar chart to compare.

## Trim Whitespace on the state column

BrewMerged$State <- trimws(BrewMerged$State)

Medians.By.State <- aggregate(BrewMerged$ABV, list(BrewMerged$State), median, na.rm=TRUE)
names(Medians.By.State) <- c("State", "ABV")
Medians.By.State$IBU <- aggregate(BrewMerged$IBU, list(BrewMerged$State), median, na.rm=TRUE)$x

BrewMedians <- tidyr::gather(Medians.By.State, "Category", "Value", 2:3)

#ggplot(BrewMedians, aes(x = Category, y = Value)) + 
#  geom_bar(stat = 'identity', position = 'stack') + facet_grid(~ State)

Medians.By.State <- Medians.By.State[order(Medians.By.State$IBU),]

#ggplot(Medians.By.State, aes(x = State, y = IBU)) + 
#geom_bar(stat = 'identity') +
#  geom_line(aes(x=State, y=ABV*max(Medians.By.State$IBU)),stat="identity")


Breweries.By.State <- as.data.frame(BrewbyState)
colnames(Breweries.By.State) <- c('State', 'Count')
Breweries.By.State <- Breweries.By.State[order(Breweries.By.State$Count, decreasing = FALSE), ]


## Code for this double stacked bar graph comes from Stackoverflow.com on a post from Didzis Elferts
## https://stackoverflow.com/questions/18265941/two-horizontal-bar-charts-with-shared-axis-in-ggplot2-similar-to-population-pyr
## https://stackoverflow.com/users/1857266/didzis-elferts

g.mid<-ggplot(Medians.By.State,aes(x=1,y=State))+geom_text(aes(label=State))+
  geom_segment(aes(x=0.94,xend=0.96,yend=State))+
  geom_segment(aes(x=1.04,xend=1.065,yend=State))+
  ggtitle("States")+
  ylab(NULL)+
  scale_x_continuous(expand=c(0,0),limits=c(0.94,1.065))+
  theme(axis.title=element_blank(),
        panel.grid=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        panel.background=element_blank(),
        axis.text.x=element_text(color=NA),
        axis.ticks.x=element_line(color=NA),
        plot.margin = unit(c(1,-1,1,-1), "mm"))
#Both original plots are modified. First, removed the y axis for the second plot and also made left/right margin to -1.

g1 <- ggplot(data = Medians.By.State, aes(x = State, y = ABV)) +
  geom_bar(stat = "identity") + ggtitle("                Median ABV") +
  theme(axis.title.x = element_blank(), 
        axis.title.y = element_blank(), 
        axis.text.y = element_blank(), 
        axis.ticks.y = element_blank(), 
        plot.margin = unit(c(1,-1,1,0), "mm")) +
  scale_y_reverse() + coord_flip()

g2 <- ggplot(data = Medians.By.State, aes(x = State, y = IBU)) +xlab(NULL)+
  geom_bar(stat = "identity") + ggtitle("           Median IBU") +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank(), 
        axis.text.y = element_blank(), axis.ticks.y = element_blank(),
        plot.margin = unit(c(1,0,1,-1), "mm")) +
  coord_flip()
#Now use library gridExtra and function d grid.arrange() to join plots. Before plotting all plots are made as grobs.

gg1 <- ggplot_gtable(ggplot_build(g1))
gg2 <- ggplot_gtable(ggplot_build(g2))
gg.mid <- ggplot_gtable(ggplot_build(g.mid))

grid.arrange(gg1,gg.mid,gg2,ncol=3,widths=c(4/9,1/9,4/9))


## 5. Which state has the maximum alcoholic (ABV) beer? Which state 
##  has the most bitter (IBU) beer?
ABVmax <- BrewMerged[ which.max(BrewMerged$ABV), ]
kable(ABVmax[,c(4,2,5,7)], align="c", caption = "State with Maximum alcohol content (ABV) beer",format="markdown")

IBUmax <- BrewMerged[ which.max(BrewMerged$IBU), ]
kable(IBUmax[,c(4,2,5,8)], align="c", caption = "State with Maximum International Bitterness Unit (IBU) beer",format="markdown")


## 6. Summary statistics for the ABV variable.

summary(BrewMerged$ABV)

## 7. Is there an apparent relationship between the bitterness of the 
##  beer and its alcoholic content? Draw a scatter plot.

library(ggplot2)

ggplot(BrewMerged, aes(IBU, ABV))+ geom_point() + geom_smooth(method=lm,  se=FALSE) 


## Conclusion ##

BrewMapTarget <- subset(BrewMap, Breweries >= 20)

# States with 20 or more Breweries
kable(BrewMapTarget[,c(1,2,5)], align="c", caption = "First 6 Observation of BrewMerged Dataframe",format="markdown",row.names = FALSE)


# Creates a segmented map of United States, displaying states with greater
# than 20 breweries 
MapTarget <- ggplot(BrewMapTarget, aes(map_id = region))+ 
  
  # map points to the fifty_states shape data
  geom_map(aes(fill = Breweries), map = fifty_states) + 
  expand_limits(x = fifty_states$long, y = fifty_states$lat) +
  coord_map() +
  scale_x_continuous(breaks = NULL) + 
  scale_y_continuous(breaks = NULL) +
  labs(fill = "Brewery\nQuantity",
       title = "           Fig 7.1 Targeted States\n                with >20 Breweries",
       x = "",
       y = "") +
  scale_fill_continuous(low = "orange", high = "darkred", guide="colorbar")+
  theme(legend.position = "bottom", 
        panel.background = element_blank())
# Plots Map
MapTarget


