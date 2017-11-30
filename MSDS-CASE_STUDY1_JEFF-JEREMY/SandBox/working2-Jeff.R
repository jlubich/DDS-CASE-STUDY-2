library(maps)
library(ggplot2)
all_states <- map_data("state")

congress <- read.csv("C:/Users/Prodigy/Documents/GitRepositories/MSDS-CASE_STUDY1_JEFF-JEREMY/US-Regions.csv")
statedata <- merge(all_states,congress,by='region')
statedata <- statedata[order(statedata[,5]),]


ditch_the_axes <- theme(
  axis.text = element_blank(),
  axis.line = element_blank(),
  axis.ticks = element_blank(),
  panel.border = element_blank(),
  panel.grid = element_blank(),
  axis.title = element_blank()
)

ggplot() + 
  geom_polygon(data=statedata, aes(x = long, y = lat, fill = Breweries, group = group), color = "grey50") + 
  coord_fixed(1.3) +
  scale_fill_continuous(low = "thistle2", high = "darkred", guide="colorbar")+
  ditch_the_axes+
  labs(fill = "Brewery\nQuantity",
       title = "Breweries by State",
       x="",
       y="")
# guides(fill=FALSE) + # do this to leave off the color legend


