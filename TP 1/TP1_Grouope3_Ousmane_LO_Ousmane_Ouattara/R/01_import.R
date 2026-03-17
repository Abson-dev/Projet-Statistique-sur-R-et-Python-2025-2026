#################### IMPORTATION DES DONNEES UTILISEES DANS LE TP1#############
library(haven)
library(dplyr)
library(naniar)
library(purrr) 
library(gtsummary)


# chargement des données relatiifs aux quatre premières questions
sect1_w4 = read_dta("data/raw/sect1_harvestw4.dta")
secta_w4 = read_dta("data/raw/secta_harvestw4.dta")
#Exploration rapide avec glimpse 
glimpse(sect1_w4)

# sommaire 
summary(sect1_w4)
## Vérification des doublons sur hhid et indiv_id
## Il s'agit ici de compter le nombre de doublons !
# Les indivudus au sein d'un même menage doivent être uniques, au risque de compter +sieurs fois une même personne d'un ménage 

nb_doublons <- sum( duplicated(sect1_w4[, c("hhid", "indiv")]))
print(paste("Nombre de doublons détecté : ", nb_doublons))

# Le nombre de valeurs manquantes pour la variable age ( "s1q4)
nb_manquantes = sum(is.na(sect1_w4$s1q4))
print(paste("Nombre de valeurs manquantes pour l'age : " , nb_manquantes))
## représentation des données manquantes (% des données manquantes)
donnees_manquantes <- vis_miss(sect1_w4 , warn_large_data= FALSE)+
  theme(
    axis.text.x = element_blank()
  )
donnees_manquantes
save(donnees_manquantes , file = "output/figures/donnees_manquantes.RDATA")


