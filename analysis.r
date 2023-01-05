# Setup

library("ggplot2")
library("tidyverse")
library("plyr")
library("dplyr")
library("magrittr")
library("cowplot")

postes <- read.csv("./dataset/postes_2020.csv", sep = ";")
mapping <- read.csv("./dataset/varmod_postes_2020.csv", sep = ";")

# Setup carte

spdf <- geojson_read("departements.geojson", what = "sp")
spdf_fortified <- tidy(spdf, region = "code")

# Percentage of people earningh more/less than X euros yearly per region
tranchesRevenuParRegion <- postes %>%
    select(TRBRUTT, DEPR) %>%
    filter(DEPR != "") %>%
    count(DEPR, TRBRUTT)
percentageRevenuParRegion <- tranchesRevenuParRegion %>%
    group_by(DEPR) %>%
    mutate(percentage = n / sum(n)) %>%
    filter(TRBRUTT >= 22) %>%
    group_by(DEPR) %>%
    summarise(highPercentage = sum(percentage))
percentageRevenuParRegion["lowPercentage"] <- tranchesRevenuParRegion %>%
    group_by(DEPR) %>%
    mutate(percentage = n / sum(n)) %>%
    filter(TRBRUTT <= 13) %>%
    group_by(DEPR) %>%
    summarise(lowPercentage = sum(percentage)) %>%
    select(lowPercentage)
percentageRevenuParRegion$DEPR <- as.character(percentageRevenuParRegion$DEPR)

graph1 <- ggplot() +
    geom_polygon(data = spdf_fortified, aes(fill = lowPercentage, x = long, y = lat, group = group)) +
    scale_fill_gradient(low = "#eeebc5", high = "#bb0600") +
    theme_void() +
    coord_map()

graph2 <- ggplot() +
    geom_polygon(data = spdf_fortified, aes(fill = highPercentage, x = long, y = lat, group = group)) +
    scale_fill_gradient(low = "#eeebc5", high = "#bb0600") +
    theme_void() +
    coord_map()

# Generate and save map

jpeg("./images/heat_map_salaries.jpg", width = 1920, height = 1080)
plot_grid(graph1, graph2)
dev.off()


# =========== Age and work category relationship ==========

# get valid data dropping NA and invalid ages
ageCS <- postes %>%
    select(AGE_TR, CS) %>%
    drop_na() %>%
    filter(AGE_TR > 0)
# count quantity and percentage in each range
ageCS <- ageCS %>% dplyr::count(AGE_TR, CS)
ageCS$percentage <- ageCS$n / sum(ageCS$n)
# take out insignificative data (manually selected)
ageCS <- filter(ageCS, !(CS %in% c(10, 21, 22, 23, 31, 44, 69)))
# get data labels
age_mapping <- mapping[mapping$COD_VAR == "CS", ]
ageCS$label <- mapvalues(ageCS$CS, age_mapping$COD_MOD, age_mapping$LIB_MOD)

# generate graphs
graph1 <- ggplot(ageCS[ageCS$CS < 43, ], aes(x = AGE_TR, y = quantity, color = label)) +
    geom_line()
graph2 <- ggplot(ageCS[ageCS$CS >= 43 & ageCS$CS < 52, ], aes(x = AGE_TR, y = quantity, color = label)) +
    geom_line()
graph3 <- ggplot(ageCS[ageCS$CS >= 52 & ageCS$CS < 63, ], aes(x = AGE_TR, y = quantity, color = label)) +
    geom_line()
graph4 <- ggplot(ageCS[ageCS$CS >= 63, ], aes(x = AGE_TR, y = quantity, color = label)) +
    geom_line()

jpeg("./images/age_cs_lines.jpg", width = 1920, height = 1080)
plot_grid(graph1, graph2, graph3, graph4)
dev.off()

# COMPARISON SALAIRE PAR TRANCHE PAR SEXE

repartitionSEXE <- postes %>% select(TRBRUTT, SEXE)
repartitionSEXE$SEXE <- replace(as.character(repartitionSEXE$SEXE), repartitionSEXE$SEXE == "1", "H")
repartitionSEXE$SEXE <- replace(as.character(repartitionSEXE$SEXE), repartitionSEXE$SEXE == "2", "F")

jpeg("SEXE_repartition.jpg")
repartitionSEXE %>%
    ggplot(aes(x = TRBRUTT, fill = SEXE)) +
    geom_histogram(binwidth = 1, color = "white", alpha = .5, position = "identity")
dev.off()

# =============== heatmap salaire x age ===============

# get and filter needed data
df <- postes %>%
    dplyr::count(TRBRUTT, AGE_TR) %>%
    filter(AGE_TR != 0)

# create factors using the varmod labels
trbrutt_mapping <- mapping[mapping$COD_VAR == "TRBRUTT", ]
df$TRBRUTT <- factor(sprintf("%02d", df$TRBRUTT), levels = trbrutt_mapping$COD_MOD, labels = trbrutt_mapping$LIB_MOD)

age_mapping <- mapping[mapping$COD_VAR == "AGE_TR", ]
df$AGE_TR <- factor(sprintf("%02d", df$AGE_TR), levels = age_mapping$COD_MOD, labels = age_mapping$LIB_MOD)

# generate heatmap
jpeg("images/heatmap-salaire-par-age.jpg")
ggplot(df, aes(AGE_TR, TRBRUTT, fill = n)) +
    geom_tile() +
    scale_fill_gradient(low="purple", high="yellow") +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
dev.off()

