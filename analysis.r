# Setup

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

# =========== Map with Percentage of people earing more/less than X euros yearly per region =========

# Read geojson
spdf <- geojson_read("departements.geojson", what = "sp")
spdf_fortified <- tidy(spdf, region = "code")

trancheRevenuParRegion <- postes %>%
    select(TRBRUTT, DEPR) %>%
    filter(DEPR != "") %>%
    dplyr::count(DEPR, TRBRUTT)
nbPeoplePerRegion <- trancheRevenuParRegion %>%
    group_by(DEPR) %>%
    dplyr::summarise(n = sum(n))

tmp <- trancheRevenuParRegion %>% filter(TRBRUTT <= 12)
tmp <- tmp %>%
    group_by(DEPR) %>%
    dplyr::summarise(regroup = sum(n))
tmp <- tmp %>% left_join(., nbPeoplePerRegion, by = c("DEPR" = "DEPR"))
percentageRevenuParRegion$lowPercentage <- tmp$regroup / tmp$n

tmp <- trancheRevenuParRegion %>% filter(TRBRUTT > 21)
tmp <- tmp %>%
    group_by(DEPR) %>%
    dplyr::summarise(regroup = sum(n))
tmp <- tmp %>% left_join(., nbPeoplePerRegion, by = c("DEPR" = "DEPR"))
percentageRevenuParRegion$highPercentage <- tmp$regroup / tmp$n

percentageRevenuParRegion$DEPR <- as.character(percentageRevenuParRegion$DEPR)


spdf_fortified <- spdf_fortified %>% left_join(., percentageRevenuParRegion, by = c("id" = "DEPR"))


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

# normalise data by percentage
ageCS <- ageCS %>% dplyr::count(AGE_TR, CS, name = "n")
ageCS <- ageCS %>%
    group_by(CS) %>%
    dplyr::mutate(cs_total = sum(n))
ageCS$percentage <- 100 * ageCS$n / ageCS$cs_total

# take out insignificative data (manually selected)
ageCS <- filter(ageCS, CS %in% c(31, 55, 67, 69, 33, 45, 62))

age_mapping <- mapping[mapping$COD_VAR == "CS", ]
ageCS$label <- mapvalues(ageCS$CS, age_mapping$COD_MOD, age_mapping$LIB_MOD)

jpeg("./images/age_cs_lines.jpg", width = 960, height = 540)
ggplot(ageCS, aes(x = AGE_TR, y = percentage, color = label)) +
    labs(
        title = "Pourcentage d'employes par age",
        x = "Age",
        y = "Pourcentage",
        color = "categorie professionelle"
    ) +
    theme(legend.position = "bottom") +
    geom_line()
dev.off()

# ============== salaire par tranche de salaire par sexe ================

repartitionSexe <- postes %>% dplyr::count(TRBRUTT, SEXE, name = "count")

# parse data
repartitionSexe$SEXE <- replace(as.character(repartitionSexe$SEXE), repartitionSexe$SEXE == "1", "Homme")
repartitionSexe$SEXE <- replace(as.character(repartitionSexe$SEXE), repartitionSexe$SEXE == "2", "Femme")
trbrutt_mapping <- mapping[mapping$COD_VAR == "TRBRUTT", ]
repartitionSexe$TRBRUTT <- factor(sprintf("%02d", repartitionSexe$TRBRUTT), levels = trbrutt_mapping$COD_MOD, labels = trbrutt_mapping$LIB_MOD)

# normalise data to match france population
repartitionSexe$count <- repartitionSexe$count * 12 / 1000000

jpeg("images/repartition-sexe-salaire.jpg", width = 960, height = 540)
repartitionSexe %>%
    ggplot(aes(x = TRBRUTT, y = count, fill = SEXE)) +
    labs(
        title = "Nombre d'employes par tranche de salaire et sexe",
        x = "Tranche salarial",
        y = "Nombre d'employes (Millions)",
        color = "categorie professionelle"
    ) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
    geom_bar(position = "dodge", stat = "identity", width = 0.5)
dev.off()

# =============== heatmap salaire x age ==================

# get and filter needed data
df <- postes %>%
    dplyr::count(TRBRUTT, AGE_TR) %>%
    filter(AGE_TR != 0)

# create factors using the varmod labels
trbrutt_mapping <- mapping[mapping$COD_VAR == "TRBRUTT", ]
df$TRBRUTT <- factor(sprintf("%02d", df$TRBRUTT), levels = trbrutt_mapping$COD_MOD, labels = trbrutt_mapping$LIB_MOD)

age_mapping <- mapping[mapping$COD_VAR == "AGE_TR", ]
df$AGE_TR <- factor(sprintf("%02d", df$AGE_TR), levels = age_mapping$COD_MOD, labels = age_mapping$LIB_MOD)

df$n <- df$n * 12 / 1000
# generate heatmap
jpeg("images/heatmap-salaire-par-age.jpg", width = 720, height = 540)
ggplot(df, aes(AGE_TR, TRBRUTT, fill = n)) +
    geom_tile() +
    labs(
        title = "Salaire par age",
        x = "Age",
        y = "Tranche salarial",
        fill = "Nombre d'employes (Milliers)"
    ) +
    scale_fill_gradient(low = "purple", high = "yellow") +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
dev.off()

# =============== estratification of man and women per work domain ================

df <- postes %>%
    filter(A6 != "") %>%
    filter(AGE_TR != "") %>%
    dplyr::count(AGE_TR, SEXE, A6)

df$SEXE[df$SEXE == 1] <- "Homme"
df$SEXE[df$SEXE == 2] <- "Femme"

age_mapping <- mapping[mapping$COD_VAR == "AGE_TR", ]
df$AGE_TR <- factor(sprintf("%02d", df$AGE_TR), levels = age_mapping$COD_MOD, labels = age_mapping$LIB_MOD)
df <- mutate(df, n_graphic = ifelse(df$SEXE == "Homme", n, -n))

jpeg("images/travailleurs-par-sexe-par-domaine.jpg")
ggplot(
    df,
        aes(
            x = n_graphic,
            y = AGE_TR,
            fill = SEXE
        )
    ) +
    labs(
        title = "Salaire par age",
        x = "Age",
        y = "Tranche salarial",
        fill = "Nombre d'employes (Milliers)"
    ) +
    facet_wrap(vars(A6)) +
    geom_col()
dev.off()
