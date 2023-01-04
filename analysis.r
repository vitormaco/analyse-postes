 # SETUP

library("ggplot2")
library("tidyverse")
library("plyr")
library("dplyr")

postes <- read.csv("./dataset/postes_2020.csv", sep = ";")
mapping <- read.csv("./dataset/varmod_postes_2020.csv", sep = ";")




create_varmod_factor <- function(df, column) {
    factor_mapping = mapping[mapping$COD_VAR==column,]
    values = sprintf("%02d", df[[column]])
    print(values)
    return(factor(values,
    levels=factor_mapping$COD_MOD,
    labels=factor_mapping$LIB_MOD))
}

df <- postes %>% dplyr::count(TRBRUTT, AGE_TR) %>%
    filter(AGE_TR != 0)

df$TRBRUTT = create_varmod_factor(df, "TRBRUTT")
df$AGE_TR = create_varmod_factor(df, "AGE_TR")

jpeg('heatmap-salaire-par-age.jpg')
ggplot(df, aes(AGE_TR, TRBRUTT, fill = n)) +
geom_tile() +
theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
dev.off()
