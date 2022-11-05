library(tidyverse)
library(statebins)
library(openintro)
library(showtext) 


prison_covid <- read_csv('https://raw.githubusercontent.com/nytimes/covid-19-data/master/prisons/systems.csv')

prison_covid <- prison_covid %>%
  mutate(inmate_death_rate = total_inmate_deaths/total_inmate_cases,
         officer_death_rate = total_officer_deaths/total_officer_cases,
         inmate_per_10 = inmate_death_rate*10000,
         officer_per_10 = officer_death_rate*10000,
         state = state2abbr(system)) %>%
  filter(system != "Federal Prisons",
         system != "County jails",
         system != "ICE detention centers",
         system != "U.S. Marshalls" )

prison_covid$inmate_death <- cut(prison_covid$inmate_per_10, 
                                 breaks=c(-Inf, 50, 100, 150, 200, 250, Inf), 
                                 labels=c('0 - 50','50 - 100', '100 - 150',
                                          '150 - 200', '200 - 250', '250+'))


# add custom fonts
font_add_google(name = "News Cycle", family = "Nathan Willis") 
showtext_auto()

prison_covid %>% statebins(
  value_col = "inmate_death", 
  ggplot2_scale_function = scale_fill_brewer,
  font_size = 6,
  name = "COVID Deaths Among Inmates \n Per 10,000 Positives",
  radius=grid::unit(6, "pt"), palette="greens", direction=1) +
  labs(title = "Tracking Covid-19 in Prisons, Jails and Detention Facilities",
       subtitle= "March 2020 to March 2021") + 
  theme_statebins(legend_position="right") +
  theme_statebins() + theme_bw() + theme(panel.border=element_blank()) +
  theme(panel.grid=element_blank()) + theme(axis.ticks=element_blank()) +
  theme(axis.text=element_blank()) +
  theme_void() +
  theme(
    text = element_text(color = "#22211d"),
    plot.background = element_rect(fill = "#f5f5f2", color = NA), 
    panel.background = element_rect(fill = "#f5f5f2", color = NA), 
    legend.background = element_rect(fill = "#f5f5f2", color = NA),
    plot.title = element_text(hjust = 0.5, size = 30),
    plot.subtitle = element_text(hjust = 0.5, size=20),
    legend.text = element_text(size=15))

