# =========================
# 1. Chargement des packages
# =========================

# Installation du tidyverse et des packages du cours 
install.packages("renv")           # Gestion des dépendances du projet 
install.packages("tidyverse")      # Collection principale:ggplot2, dplyr, tidyr, readr... 
install.packages("haven")          # Import données Stata (.dta) et SPSS (.sav) 
install.packages("readxl")         # Import fichiers Excel 
install.packages("janitor")        # Nettoyage des noms de colonnes 
install.packages("gtsummary")      # Tableaux statistiques publication-ready 
install.packages("survey")         # Analyses pondérées (sondages) 
install.packages("sf")             # Données spatiales (shapefiles) 
install.packages("dplyr")  
install.packages("naniar")
install.packages("rstatix")  

library(rstatix)
library(tidyverse)   # dplyr, ggplot2, tidyr...
library(haven)       # import Stata
library(janitor)     # clean_names()
library(naniar)      # gestion des NA
library(dplyr)

# =========================
# 2. Importation des données et fusion des bases
# =========================
data_raw_1 <- read_dta("data/raw/sect1_harvestw4.dta")
data_raw_2 <- read_dta("data/raw/sect2_harvestw4.dta")
data_raw_3 <- read_dta("data/raw/secta_harvestw4.dta")
View(data_men)

data_combined <- data_raw_1 %>%
  left_join(data_raw_2, by = c("hhid", "indiv"))

data_combined <- data_combined %>%
  left_join(data_raw_3, by = "hhid")
View(data_combined)

colnames(data_combined)

# =========================
# 3. Exploration générale
# =========================

# Structure des données
glimpse(data_combined)

# Avec str()
str(data_combined)

# summary()
summary(data_combined)

# Verification des doublons
doublons <- data_combined %>%
  count(hhid, indiv) %>%
  filter(n > 1)

if (nrow(doublons) == 0) {
  message("Aucun doublon détecté")
} else {
  message("Doublons détectés")
  print(doublons)
}
#Aucun doublon détecté

# =========================
# 5. Analyse des valeurs manquantes
# =========================

# Visualisation
vis_miss(data.frame(data_combined$s2aq9))

# Résumé des NA

miss_summary_s1q2 <- miss_var_summary(data.frame(data_combined$s2aq9))
print(miss_summary_s1q2)

