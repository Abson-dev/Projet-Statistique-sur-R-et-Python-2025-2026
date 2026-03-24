
# =========================
# 1. Chargement des packages
# =========================

# Installation du tidyverse et des packages du cours 
install.packages("tidyverse")      # Collection principale:ggplot2, dplyr, tidyr, readr... 
install.packages("haven")          # Import données Stata (.dta) et SPSS (.sav) 
install.packages("readxl")         # Import fichiers Excel 
install.packages("janitor")        # Nettoyage des noms de colonnes 
install.packages("gtsummary")      # Tableaux statistiques publication-ready 
install.packages("survey")         # Analyses pondérées (sondages) 
install.packages("sf")             # Données spatiales (shapefiles) 
install.packages("renv")           # Gestion des dépendances du projet 
install.packages("dplyr")  
install.packages("naniar")

library(tidyverse)   # dplyr, ggplot2, tidyr...
library(haven)       # import Stata
library(janitor)     # clean_names()
library(naniar)      # gestion des NA
library(dplyr)

# =========================
# 2. Importation des données
# =========================
data_raw <- read_dta("data/raw/sect1_harvestw4.dta")

# =========================
# 3. Exploration générale
# =========================

# Structure des données
glimpse(data_raw)

# Avec str()
str(data_raw)

# summary()
summary(data_raw)

# Verification des doublons
doublons <- data_raw %>%
  count(hhid, indiv) %>%
  filter(n > 1)

if (nrow(doublons) == 0) {
  message("Aucun doublon détecté")
} else {
  message("Doublons détectés")
  print(doublons)
}

# =========================
# 5. Analyse des valeurs manquantes
# =========================

# Visualisation
vis_miss(data.frame(data_raw$s1q2))
vis_miss(data.frame(data_raw$s1q4))

# Résumé des NA

miss_summary_s1q2 <- miss_var_summary(data.frame(data_raw$s1q2))
print(miss_summary_s1q2)

miss_summary_s1q4 <- miss_var_summary(data.frame(data_raw$s1q4))
print(miss_summary_s1q4)

View(data_raw)
