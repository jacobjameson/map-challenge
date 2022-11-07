############################################################
# DAY 3: THEME- POLYGONS
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



# Get details of maps
big_streets <- getbb("Boston MA") %>%
  opq() %>%
  add_osm_feature(key = "highway", 
                  value = c("motorway", "primary", "motorway_link", "primary_link")) %>%
  osmdata_sf()

med_streets <- getbb("Boston MA")%>%
  opq()%>%
  add_osm_feature(key = "highway", 
                  value = c("secondary", "tertiary", "secondary_link", "tertiary_link")) %>%
  osmdata_sf()

small_streets <- getbb("Boston MA")%>%
  opq()%>%
  add_osm_feature(key = "highway", 
                  value = c("residential", "living_street",
                            "unclassified",
                            "service", "footway")) %>%
  osmdata_sf()

river <- getbb("Boston MA")%>%
  opq()%>%
  add_osm_feature(key = "waterway", value = "river") %>%
  osmdata_sf()

railway <- getbb("Boston MA")%>%
  opq()%>%
  add_osm_feature(key = "railway", value="rail") %>%
  osmdata_sf()


# Get coordinates of Boston, MA
getbb("Boston MA")

#       min      max
# x -71.19124 -70.80449
# y  42.22791  42.39698

library(ggmap)
UC_locs <- read_xlsx('RC_UCC.xlsx')
UC_locs<- UC_locs[UC_locs$TYPE == 'Urgent Care Center',]
register_google(key = "AIzaSyA0ChnU51nagO1DjSXrn8LLzmli3gkfHHY")

locs <- c()
for (i in 1:nrow(UC_locs)) {
  locs <- c(locs, paste0(UC_locs[i,]$ADDRESS, UC_locs[i,]$CITY))
}

locations <- geocode(locs)

col <- c()
for (i in 1:nrow(locations)){
  col <- c(col, sample.int(8,1))
}

locations$col <- col


# add custom fonts
font_add_google(name = "Permanent Marker", family = "Font Diner") 
showtext_auto()

# Create the final plot and export
ggplot(data=locations, aes(x=lon, y=lat, fill=factor(col))) +
  geom_voronoi(alpha = 0.3) +
  stat_voronoi(geom = "path") +
  geom_point(data=locations, aes(x=lon, y=lat), size = 3,
             fill="red", color="red", pch=21, inherit.aes = F) +
  scale_fill_brewer(palette = "Set2",guide="none") + 
  #scale_fill_material('amber', guide="none") + 
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
  geom_sf(data = med_streets$osm_lines,
          inherit.aes = FALSE,
          color = "black",
          size = .3,
          alpha = .7) +
  geom_sf(data = small_streets$osm_lines,
          inherit.aes = FALSE,
          color = "#666666",
          size = .2,
          alpha = .5) +
  geom_sf(data = big_streets$osm_lines,
          inherit.aes = FALSE,
          color = "black",
          size = .5,
          alpha = .8) +
  coord_sf(xlim = c(-71.19124, -70.90449), 
           ylim = c(42.22791,  42.39698), expand = T)  +
  theme_void() + 
  theme(plot.background = element_rect(fill = "white"))  +
  theme(plot.title = element_text(size = 40, family = "Font Diner", 
                                  face="bold", hjust=.5, color = 'black'),
        plot.subtitle = element_text(family = "Font Diner", size = 20,  color='black',
                                     hjust=.5, margin=margin(2, 0, 5, 0)),
        plot.caption = element_text(size = 12, family = "Font Diner", 
                                    color='black', hjust=.5, margin=margin(2, 0, 5, 0))) +
  labs(title = "Where's my Urgent Care Center?", subtitle = "Closest Boston UCC by Euclidian Distance", 
       caption = str_wrap("Polygons represent areas for which every point in that area has a 
                          shorter Euclidian Distance to that polygon's urgent care center than 
                          any other.", 120))






