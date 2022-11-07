############################################################
# DAY 5: THEME- UKRAINE 
############################################################

# Library
library(tidyverse)
library(osmdata)
library(showtext) 
library(ggmap)
library(rvest)
library(rstudioapi)
library(ggimage)
library(rjson)
library(sjPlot)
library(deldir)
library(ggvoronoi)
library(viridis)
library(readxl)
library(ggsci)



bbx <- getbb("Kyiv Ukraine")

min_lon <- 30.23615; max_lon <- 30.82636
min_lat <- 50.21324; max_lat <- 50.59081
bbx <- rbind(x=c(min_lon,max_lon),y=c(min_lat,max_lat))
colnames(bbx) <- c("min","max")

highways <- bbx %>%
  opq()%>%
  add_osm_feature(key = "highway", 
                  value=c("motorway", "trunk",
                          "primary","secondary", 
                          "tertiary","motorway_link",
                          "trunk_link","primary_link",
                          "secondary_link",
                          "tertiary_link")) %>%
  osmdata_sf()


streets <- bbx %>%
  opq()%>%
  add_osm_feature(key = "highway", 
                  value = c("residential", "living_street",
                            "service","unclassified",
                            "pedestrian", "footway",
                            "track","path")) %>%
  osmdata_sf()

color_roads <- rgb(0.42,0.449,0.488)

river <- getbb("Kyiv Ukraine")%>%
  opq()%>%
  add_osm_feature(key = "waterway", value = "river") %>%
  osmdata_sf()

railway <- getbb("Kyiv Ukraine")%>%
  opq()%>%
  add_osm_feature(key = "railway", value="rail") %>%
  osmdata_sf()



# add custom fonts
font_add_google(name = "Russo One", family = "Jovanny Lemonad") 
showtext_auto()

# Create the final plot and export
ggplot() +
  geom_sf(data = river$osm_lines,
          inherit.aes = FALSE,
          color = "steelblue",
          size = .8,
          alpha = .6) +
  geom_sf(data = railway$osm_lines,
          inherit.aes = FALSE,
          color = "black",
          size = .2,
          linetype="dotdash",
          alpha = .8) +
  geom_sf(data = streets$osm_lines,
          inherit.aes = FALSE,
          color='#666666',
          size = .4,
          alpha = .65) +
  geom_sf(data = highways$osm_lines,
          inherit.aes = FALSE,
          color='#666666',
          size = .6,
          alpha = .65) +
  coord_sf(xlim = c(30.23615, 30.82636), 
           ylim = c(50.21324, 50.59081), expand = T)  +
  theme_void() + 
  theme(plot.background = element_rect(fill = "gray9")) +
  theme(plot.title = element_text(size = 60, family = "Jovanny Lemonad", 
                                  face="bold", hjust=.5, color = 'yellow'),
        plot.subtitle = element_text(family = "Jovanny Lemonad", size = 40,  color='blue',
                                     hjust=.5, margin=margin(2, 0, 5, 0)),
        plot.caption = element_text(size = 15, family = "Jovanny Lemonad", 
                                    color='yellow', hjust=.5, margin=margin(2, 0, 5, 0))) +
  labs(title = "KYIV", subtitle = "UKRAINE'")







