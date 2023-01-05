 # SETUP

library("ggplot2")
library("tidyverse")
library("plyr")
library("dplyr")
library("magrittr")
library("cowplot")
library("geojsonio")
library("broom")


postes <- read.csv("./dataset/postes_2020.csv", sep = ";")
mapping <- read.csv("./dataset/varmod_postes_2020.csv", sep = ";")

# Read geojson
spdf <- geojson_read('departements.geojson',what='sp')
spdf_fortified <- tidy(spdf,region="code")

# Percentage of people earing more/less than X euros yearly per region
trancheRevenuParRegion <- postes %>% select(TRBRUTT,DEPR) %>% filter(DEPR != "") %>% dplyr::count(DEPR,TRBRUTT)
nbPeoplePerRegion <- trancheRevenuParRegion %>% group_by(DEPR) %>% dplyr::summarise(n = sum(n))

tmp <- trancheRevenuParRegion %>% filter(TRBRUTT <= 12)
tmp <- tmp %>% group_by(DEPR) %>% dplyr::summarise(regroup = sum(n))
tmp <- tmp %>% left_join(. , nbPeoplePerRegion, by=c("DEPR"="DEPR"))
percentageRevenuParRegion$lowPercentage <- tmp$regroup/tmp$n


tmp <- trancheRevenuParRegion %>% filter(TRBRUTT > 21)
tmp <- tmp %>% group_by(DEPR) %>% dplyr::summarise(regroup = sum(n))
tmp <- tmp %>% left_join(. , nbPeoplePerRegion, by=c("DEPR"="DEPR"))
percentageRevenuParRegion$highPercentage <- tmp$regroup/tmp$n

percentageRevenuParRegion$DEPR = as.character(percentageRevenuParRegion$DEPR)


spdf_fortified <- spdf_fortified %>% left_join(. , percentageRevenuParRegion, by=c("id"="DEPR"))


graph1 <- ggplot() +
    geom_polygon(data = spdf_fortified, aes(fill=lowPercentage,x = long, y = lat, group = group)) +
    scale_fill_gradient(low='#eeebc5',high='#bb0600') +
    theme_void() +
    coord_map()

graph2 <- ggplot() +
    geom_polygon(data = spdf_fortified, aes(fill=highPercentage,x = long, y = lat, group = group)) +
    scale_fill_gradient(low='#eeebc5',high='#bb0600') +
    theme_void() +
    coord_map()

# Save map
jpeg("./images/heat_map_salaries.jpg",width=1920,height=1080)
plot_grid(graph1,graph2)
dev.off()
