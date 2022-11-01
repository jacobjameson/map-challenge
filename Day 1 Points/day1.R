############################################################
# DAY 1: THEME- POINTS
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


# Get details of maps
big_streets <- getbb("Cambridge MA") %>%
  opq() %>%
  add_osm_feature(key = "highway", 
                  value = c("motorway", "primary", "motorway_link", "primary_link")) %>%
  osmdata_sf()

med_streets <- getbb("Cambridge MA")%>%
  opq()%>%
  add_osm_feature(key = "highway", 
                  value = c("secondary", "tertiary", "secondary_link", "tertiary_link")) %>%
  osmdata_sf()

small_streets <- getbb("Cambridge MA")%>%
  opq()%>%
  add_osm_feature(key = "highway", 
                  value = c("residential", "living_street",
                            "unclassified",
                            "service", "footway")) %>%
  osmdata_sf()

river <- getbb("Cambridge MA")%>%
  opq()%>%
  add_osm_feature(key = "waterway", value = "river") %>%
  osmdata_sf()

railway <- getbb("Cambridge MA")%>%
  opq()%>%
  add_osm_feature(key = "railway", value="rail") %>%
  osmdata_sf()


# Get coordinates of Cambridge, MA
getbb("Cambridge MA")

#       min      max
# x -71.1604 -71.06398
# y  42.3524  42.40426


# Use the Google API to find Dunkin' Locations
DD.add <- fromJSON(file ="https://maps.googleapis.com/maps/api/place/textsearch/json?query=dunkin'+cambridge/@42.3724169,-71.126531,&radius=20000&region=us&key=AIzaSyA0ChnU51nagO1DjSXrn8LLzmli3gkfHHY")
DD.add2 <- fromJSON(file ="https://maps.googleapis.com/maps/api/place/textsearch/json?query=Dunkin'+cambridge/@42.397825,-71.1480632,&radius=200000&region=us&key=AIzaSyA0ChnU51nagO1DjSXrn8LLzmli3gkfHHY")

DD <- data.frame(lat = rep(NA, 40), lng = rep(NA, 40))

for(i in 1:20){
  DD$lat[i] <- DD.add$results[[i]]$geometry$location$lat
  DD$lng[i] <- DD.add$results[[i]]$geometry$location$lng
}

for(i in 1:20){
  DD$lat[i+20] <- DD.add2$results[[i]]$geometry$location$lat
  DD$lng[i+20] <- DD.add2$results[[i]]$geometry$location$lng
}


# add custom fonts
font_add_google(name = "Russo One", family = "Jovanny Lemonad") 
showtext_auto()

# Create the final plot and export
ggplot() +
  geom_sf(data = river$osm_lines,
          inherit.aes = FALSE,
          color = "steelblue",
          size = .8,
          alpha = .3) +
  geom_sf(data = railway$osm_lines,
          inherit.aes = FALSE,
          color = "orange",
          size = .2,
          linetype="dotdash",
          alpha = .5) +
  geom_sf(data = med_streets$osm_lines,
          inherit.aes = FALSE,
          color = "orange",
          size = .3,
          alpha = .5) +
  geom_sf(data = small_streets$osm_lines,
          inherit.aes = FALSE,
          color = "orange",
          size = .2,
          alpha = .3) +
  geom_sf(data = big_streets$osm_lines,
          inherit.aes = FALSE,
          color = "orange",
          size = .5,
          alpha = .6) +
  coord_sf(xlim = c(-71.1604, -71.06398), 
           ylim = c(42.3524, 42.40426),
           expand = T)  +
  theme_void() + 
  theme(plot.background = element_rect(fill = "gray9")) +
  geom_point(data=DD, aes(x=lng, y=lat), size = 4, fill="deeppink1", color="deeppink1",
             pch=21, inherit.aes = F) + 
  theme(plot.title = element_text(size = 60, family = "Jovanny Lemonad", 
                                  face="bold", hjust=.5, color = 'orange'),
        plot.subtitle = element_text(family = "Jovanny Lemonad", size = 40,  color='deeppink1',
                                     hjust=.5, margin=margin(2, 0, 5, 0)),
        plot.caption = element_text(size = 15, family = "Jovanny Lemonad", 
                                    color='deeppink1', hjust=.5, margin=margin(2, 0, 5, 0))) +
  labs(title = "CAMBRIDGE", subtitle = "Runs on Dunkin'", 
       caption = str_wrap("Points indicate Dunkin' locations that 
                          appear searching the area using the Google Maps API. May include
                          locations that are no longer open.", 70))
