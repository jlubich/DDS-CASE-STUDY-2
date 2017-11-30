
Breweries.by.state <- read.csv("C:/Users/Prodigy/Documents/GitRepositories/MSDS-CASE_STUDY1_JEFF-JEREMY/US-Regions.csv")

library(ggplot2)
library(fiftystater)
library(mapproj)

# map_id creates the aesthetic mapping to the state name column in your data
mapplot <- ggplot(Breweries.by.state, aes(map_id = region)) + 
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
mapplot

