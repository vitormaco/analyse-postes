# SETUP

library("ggplot2")
library("data.table")
library("geojsonio")
library("broom")
library("tidyverse")
library("dplyr")
library("magrittr")
library("cowplot")

postes <- read.csv("./dataset/postes_2020.csv", sep = ";")
mapping <- read.csv("./dataset/varmod_postes_2020.csv", sep = ";")

# Check missing elements
colSums(is.na(postes))


# Map setup

spdf <- geojson_read('departements.geojson',what='sp')
spdf_fortified <- tidy(spdf,region="code")

# Percentage of people earning more/less than X euros yearly per region
tranchesRevenuParRegion <- postes %>% select(TRBRUTT,DEPR) %>% filter(DEPR != "") %>% count(DEPR,TRBRUTT)
percentageRevenuParRegion = tranchesRevenuParRegion %>% group_by(DEPR) %>% mutate(percentage = n/sum(n)) %>%  filter(TRBRUTT >= 22) %>% group_by(DEPR) %>% summarise(sum = sum(percentage))
percentageRevenuParRegion$DEPR = as.character(percentageRevenuParRegion$DEPR)

spdf_fortified <- spdf_fortified %>% left_join(. , percentageRevenuParRegion, by=c("id"="DEPR"))

# Save map
jpeg("heat_map_salaries.jpg",width=1920,height=1080)
ggplot() +
  geom_polygon(data = spdf_fortified, aes(fill = sum, x = long, y = lat, group = group)) +
  scale_fill_gradient(low='#eeebc5',high='#bb0600') +
  theme_void() +
  coord_map()
dev.off()


# Age and work category relationship

ageCS = postes %>% select(AGE_TR,CS) %>% drop_na()
ageCS <- ageCS[ageCS$AGE > 0,]
colSums(is.na(ageCS))
ageCS <- ageCS %>% group_by(AGE_TR,CS) %>% mutate(quantity = n())
ageCS <- ageCS[!(ageCS$CS %in% c(10,21,22,23,31,44,69)),]


ageCS <- ageCS %>% group_by(AGE_TR,CS) %>% mutate(percentage = quantity/sum(quantity))


graph1 <- ggplot(ageCS[ageCS$CS < 43,],aes(x=AGE_TR,y=quantity,color=CS)) +
    geom_line()
graph2 <- ggplot(ageCS[ageCS$CS >= 43 & ageCS$CS < 52,],aes(x=AGE_TR,y=quantity,color=CS)) +
    geom_line()
graph3 <- ggplot(ageCS[ageCS$CS >= 52 & ageCS$CS < 63,],aes(x=AGE_TR,y=quantity,color=CS)) +
    geom_line()
graph4 <- ggplot(ageCS[ageCS$CS >= 63,],aes(x=AGE_TR,y=quantity,color=CS)) +
    geom_line()

jpeg("./images/age_cs_lines.jpg",width=1920,height=1080)
plot_grid(graph1,graph2,graph3,graph4)
dev.off()
# Checking distributions =============================================

revenus = postes['TRBRUTT']

# Revenus repartition
jpeg('revenus_repartition.jpg')
revenus['TRBRUTT'] %>%
ggplot(aes(x=TRBRUTT)) +
    geom_histogram(binwidth=1,fill="steelblue",color='black') +
    geom_vline(aes(xintercept=median(TRBRUTT)),color = 'red', linetype='dashed',size=1)
dev.off()


# Repartition population selon domaine
revenusNomenclature = postes['A38']

jpeg('nomenclature_repartition.jpg')
revenusNomenclature['A38'] %>%
ggplot(aes(x=A38)) +
    geom_histogram(stat="count",binwidth=1,fill="beige",color='black')
dev.off()

# Repartition age
repartitionAGE_TR = postes['AGE_TR'] %>% drop_na('AGE_TR')
jpeg('AGE_TR_repartition.jpg')
repartitionAGE_TR['AGE_TR'] %>%
ggplot(aes(x=AGE_TR)) +
    geom_histogram(binwidth=1,fill="pink",color='black')
dev.off()


# Repartition departement residence et entreprise
repartitionResidence = postes['DEPR'] %>% drop_na('DEPR')
repartitionEntreprise = postes['DEPT'] %>% drop_na('DEPT')

jpeg('DEPR_repartition.jpg')
ggplot() +
    geom_histogram(data = repartitionResidence['DEPR'],aes(x=DEPR),fill='yellow',binwidth=1,color='black',stat='count',alpha=0.4)+
    geom_histogram(data=repartitionEntreprise['DEPT'],aes(x=DEPT),fill='red',binwidth=1,color='black',stat='count',alpha=0.4)
dev.off()

# Repartition categorie socio pro emploi
repartitionCS = postes['CS'] %>% drop_na('CS')
jpeg('CS_repartition.jpg')
repartitionCS['CS'] %>%
ggplot(aes(x=CS)) +
    geom_histogram(stat="count",binwidth=1,fill="yellow",color='black')
dev.off()

# nombre genre
postes['SEXE'] %>% count(SEXE)


# Repartition sexe revenus
repartitionSEXE = postes %>% select(TRBRUTT,SEXE)
repartitionSEXE$SEXE = replace(as.character(repartitionSEXE$SEXE),repartitionSEXE$SEXE=='1','H')
repartitionSEXE$SEXE = replace(as.character(repartitionSEXE$SEXE),repartitionSEXE$SEXE=='2','F')

jpeg('SEXE_repartition.jpg')
repartitionSEXE %>%
ggplot(aes(x=TRBRUTT,fill=SEXE)) +
    geom_histogram(binwidth=1,color='white',alpha=.5,position='identity')
dev.off()


# Repartition type de contrat
repartitionCONT_TRAV = postes['CONT_TRAV'] %>% drop_na('CONT_TRAV')
jpeg('CONT_TRAV_repartition.jpg')
repartitionCONT_TRAV['CONT_TRAV'] %>%
ggplot(aes(x=CONT_TRAV)) +
    geom_histogram(binwidth=1,fill="pink",color='black',stat='count')
dev.off()


# =======================================================

# Revenus par age

# Revenus par region
