#  ============= METADATA sur le jeu de donnees =============

# https://www.insee.fr/fr/statistiques/6524154

# Fichier "salariés"
# Le fichier « salariés » décrit les caractéristiques du salarié, de son poste
#  principal ainsi que des données récapitulatives tous postes confondus :
#  rémunérations, heures salariées, total des indemnités de chômage.

# Chaque enregistrement est constitué par la consolidation des périodes de
# travail d'un salarié quels que soient ses employeurs.

# Le fichier « salariés » est disponible au format dBase ou csv. Il contient
# 31 variables et 2 373 366 observations.

# Le fichier est issu d'un échantillon au 1/12e de la population salariée.
# Une variable de pondération égale à 12 est présente dans le fichier Dbase,
# et le fichier csv est pondéré par défaut par 12.

# Avertissement : À partir du millésime 2013, la définition du 1/12e induit
# que les salariés nés une année paire sont surreprésentés dans l'échantillon
# (voir Documentation).

#  ==========================================================



library(ggplot2)
library('data.table')


# Fix age


postes <- read.csv("./dataset/postes_2020.csv", sep = ";")
mapping <- read.csv("./dataset/varmod_postes_2020.csv", sep = ";")


# Checking for missing elements
colSums(is.na(postes))


# Plotting map ====================

library('geojsonio')
library(broom)

spdf <- geojson_read('departements.geojson',what='sp')
spdf_fortified <- tidy(spdf,region="code")
str(spdf_fortified)
head(spdf_fortified)

ggplot() +
  geom_polygon(data = spdf_fortified, aes( x = long, y = lat, group = group), fill="white", color="grey") +
  theme_void() +
  coord_map()

library(tidyverse)
revenusRegionsDf <- postes %>% select(TRBRUTT,DEPR)
# Cleaning unknown values
revenusRegionsDf = revenusRegionsDf[revenusRegionsDf$DEPR != "",]
str(revenusRegionsDf)

trancheRevenusDf = revenusRegionsDf %>% count(DEPR,TRBRUTT)

nrow(trancheRevenusDf)
str(trancheRevenusDf)
head(trancheRevenusDf)



library(dplyr)
library(magrittr)
trancheRevenusDf$DEPR = as.character(trancheRevenusDf$DEPR)
str(trancheRevenusDf)



spdf_fortified <- spdf_fortified %>% left_join(. , trancheRevenusDf[1:2424,], by=c("id"="DEPR"))
str(spdf_fortified)
head(spdf_fortified)




df_0 = spdf_fortified[spdf_fortified$TRBRUTT == 12,]



jpeg("heat_map_salaries.jpg",width=1920,height=1080)
ggplot() +
  geom_polygon(data = spdf_fortified, aes(fill = n, x = long, y = lat, group = group)) +
  scale_fill_gradient(low='#eeebc5',high='#bb0600') +
  theme_void() +
  coord_map() +
  facet_wrap(vars(TRBRUTT))
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
