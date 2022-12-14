#  ============= METADATA sur le jeu de donnees =============

# https://www.insee.fr/fr/statistiques/4994589#consulter

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

library("tidyverse")
library(ggplot2)

postes <- read.csv("./dataset/postes_2020.csv", sep = ";")
mapping <- read.csv("./dataset/varmod_postes_2020.csv", sep = ";")

map <- function(df, key) {
    return(merge(x=df, y=mapping[mapping$COD_VAR == key, ], by.x = key, by.y = "COD_MOD")[])
}

postes %>%
map("TRBRUTT") %>%
count(LIB_MOD) %>%
ggplot(aes(LIB_MOD, n)) +
    geom_bar(stat="identity", fill="steelblue") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))


# mutate(NORMALISED = NOMBRE / sum(NOMBRE))
