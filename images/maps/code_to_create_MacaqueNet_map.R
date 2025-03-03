---
title: "creating MacaqueNet map"
author: "Delphine De Moor"
date: "07/12/2022"
---
  
# empty global environment 
rm(list=ls())

# load packages
library(ggplot2)
library(maps)
library(dplyr)

# read in research site table
rs <- read.csv("research sites temporary table.csv") %>% 
  # group_by(site_id, research_site, country, latitude, longitude) %>% 
  # summarise(nr_species = n_distinct(species_common)) %>% 
  mutate(species = recode(species_common, "barbary" = "Barbary", "tibetan" = "Tibetan",
                          "japanese" = "Japanese", "northern pigtailed" = "N pigtailed", 
                          "southern pigtailed" = "S pigtailed", "assamese" = "Assamese")) %>% 
  mutate(species = factor(species, c("stumptailed", "Barbary", "rhesus", "longtailed", "Japanese", 
                                     "liontailed", "tonkean", "moor", "crested", "Assamese", 
                                     "bonnet", "N pigtailed", "S pigtailed", "Tibetan"))) %>% 
  arrange(desc(temp_nr_groupyears)) %>%
  mutate()

regions <- map("world", namesonly=TRUE, plot=FALSE)
world <- map_data("world", regions[-grep("Antarctica", regions)])

map <- ggplot() +
  geom_map(data = world, map = world, aes(long, lat, map_id = region),
           fill = "#F0F0F0", color = "darkgrey", size = 0.05) +
  geom_point(data = rs, aes(longitude, latitude, fill = species, 
                            size = temp_nr_groupyears, shape = temp_condition)) +
  scale_fill_manual(values = alpha(c("#082D41", "#16536F", "#1E9DCF", "#B6D4E4", "#EDF6FD", 
                                     "#FFFAF0", "#E7F2DC", "#C0F190", "#7ABE43", "#2E8B58", 
                                     "#145A2D", "#828B8B", "#C1CDCD", "#D2B48C"), 0.8)) +
  scale_shape_manual(values=c(24, 23, 21)) +
  scale_size_continuous(range = c(2, 13), breaks = c(1, 2, 5, 10)) +
  # geom_text_repel(data = rs, aes(longitude, latitude, label = site_id),
  #             color = "black", size = 1, box.padding = unit(0.1, "lines"),
  #             max.overlaps = 20) +
  # coord_cartesian(ylim = c(-10, 90)) + # to cut map off in the south +
  guides(fill = guide_legend(override.aes = list(size = 4, alpha = 1, shape = 21)),
         shape = guide_legend(override.aes = list(size = 4)), 
         size = guide_legend(override.aes = list(shape = 21))) +
  labs(shape = "condition", size = "# group-years") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.text.x=element_blank(),
        axis.ticks.x=element_blank(), axis.title.x=element_blank(),
        axis.text.y=element_blank(), axis.ticks.y=element_blank(), 
        axis.title.y=element_blank(), axis.line = element_blank(),
        legend.position = c(0.1, 0.7), legend.justification = c("right", "top"),
        legend.box.just = "right", legend.margin = margin(6, 6, 6, 6),
        legend.key=element_rect(fill="white"))
map

just_world <- map <- ggplot() +
  geom_map(data = world, map = world, aes(long, lat, map_id = region),
           fill = "gray47", color = "grey8") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.text.x=element_blank(),
        axis.ticks.x=element_blank(), axis.title.x=element_blank(),
        axis.text.y=element_blank(), axis.ticks.y=element_blank(), 
        axis.title.y=element_blank(), axis.line = element_blank(),
        legend.position = c(0.1, 0.7), legend.justification = c("right", "top"),
        legend.box.just = "right", legend.margin = margin(6, 6, 6, 6),
        legend.key=element_rect(fill="white"))
just_world


soapbox <- map <- ggplot() +
  geom_map(data = world, map = world, aes(long, lat, map_id = region),
           fill = "#B3B3B3", color = "black", size = 0.7) +
           geom_point(data = rs,
                      aes(longitude, latitude), color="black", fill="#6CB8DE", shape=21, size=4, stroke = 1.5) +
                      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                            panel.background = element_blank(), axis.text.x=element_blank(),
                            axis.ticks.x=element_blank(), axis.title.x=element_blank(),
                            axis.text.y=element_blank(), axis.ticks.y=element_blank(),
                            axis.title.y=element_blank(), axis.line = element_blank(),
                            legend.position = c(0.1, 0.7), legend.justification = c("right", "top"),
                            legend.box.just = "right", legend.margin = margin(6, 6, 6, 6),
                            legend.key=element_rect(fill="white"))
soapbox


points_only <- map <- ggplot() +
  geom_map(data = world, map = world, aes(long, lat, map_id = region),
           fill = "white", color = "white", size = 0.7) +
  geom_point(data = rs,
             aes(longitude, latitude), color="black", fill="#6CB8DE", shape=21, size=4, stroke = 1.5) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.text.x=element_blank(),
        axis.ticks.x=element_blank(), axis.title.x=element_blank(),
        axis.text.y=element_blank(), axis.ticks.y=element_blank(),
        axis.title.y=element_blank(), axis.line = element_blank(),
        legend.position = c(0.1, 0.7), legend.justification = c("right", "top"),
        legend.box.just = "right", legend.margin = margin(6, 6, 6, 6),
        legend.key=element_rect(fill="white"))
points_only
                       