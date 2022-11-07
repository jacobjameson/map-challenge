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
library(ggshadow)
library(viridis)
library(readxl)
library(ggsci)

library('maps')
library('geosphere')

register_google(key = "AIzaSyA0ChnU51nagO1DjSXrn8LLzmli3gkfHHY")


AM <- read_xlsx('AM.xlsx')
AM$counter <- rep(c(1,2,3,4), 296/4)
date <- filter(AM, counter == 1)$tours
venue <- filter(AM, counter == 2)$tours
locs <- filter(AM, counter == 3)$tours

AM <- data.frame(`date` = date, `venue` = venue, `locs` = locs)

loc <- c()
for (i in 1:nrow(AM)) {
  locs <- c(locs, paste0(AM[i,]$locs, AM[i,]$venue))
}

locations <- geocode(locs[74:148])


locations <- locations %>%
  mutate(lat2 = lead(lat),
         lon2 = lead(lon)) %>%
  filter(lat != lat2 & lon != lon2)



# add custom fonts
font_add_google(name = "Russo One", family = "Jovanny Lemonad") 
showtext_auto()

theme_map <- function(...) {
  theme_minimal() +
    theme(
      text = element_text(family = "Jovanny Lemonad", color = "#22211d"),
      axis.line = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      # panel.grid.minor = element_line(color = "#ebebe5", size = 0.2),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      plot.background = element_rect(fill = "grey60", color = NA), 
      panel.background = element_rect(fill = "grey60", color = NA), 
      legend.background = element_rect(fill = "grey60", color = NA),
      panel.border = element_blank(),
      ...
    )
}


world <- map_data("world", colour="black") 

ggplot() + geom_polygon(data = world, aes(x=long, y = lat, group = group), fill='black') + 
  coord_fixed(1.3) + theme_nothing() + 
  geom_polygon() + 
  geom_shadowpoint(data = locations, aes(x = lon, y=lat), color='gold') +
  geom_curve(data = locations, aes(x = lon, y = lat, xend = lon2, yend = lat2),
             col = "gold", size = .9) + 
  theme_void() +
  theme_map() +
  theme(
    legend.position = c(0.5, 0.03),
    legend.text.align = 0,
    legend.background = element_rect(fill = alpha('white', 0.0)),
    legend.text = element_text(size = 7, hjust = 0, color = "#4e4d47"),
    plot.title = element_text(size = 50,hjust = 0.5, color = "white"),
    plot.subtitle = element_text(hjust = 0.5, color = "white", 
                                 margin = margin(b = -0.1, 
                                                 t = -0.1, 
                                                 l = 2, 
                                                 unit = "cm"), 
                                 debug = F),
    plot.caption = element_text(size = 15, 
                                hjust = 0.92, 
                                margin = margin(t = 0.2, 
                                                b = 0, 
                                                unit = "cm"), 
                                color = "white")
  ) +
  labs(x = NULL, 
       y = NULL, 
       title = "Arctic Monkeys 2023 Tour", 
       caption = "Author: Jacob Jameson (@JacobCJameson)") 





