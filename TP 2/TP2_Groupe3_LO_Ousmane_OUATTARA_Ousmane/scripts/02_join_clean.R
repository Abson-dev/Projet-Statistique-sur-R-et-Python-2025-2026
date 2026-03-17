
library(rlang)
library(dplyr)
library(here)

# Charger les données brutes

sect1_harvestw4 <- readRDS(here("data", "processed", "sect1_harvestw4.rds"))

sect2_harvestw4 <- readRDS(here("data", "processed", "sect2_harvestw4.rds"))


# Joindre les deux fichiers sur hhid + indiv_id.

data_final <- dplyr::left_join(sect1_harvestw4, sect2_harvestw4, by = c("hhid", "indiv"))

# Pourcentage des valeurs manquantes dans la variable s2aq9 : niveau d'éducation

p = sum(is.na(data_final$s2aq9))/nrow(data_final)*100

print(paste("Le pourcentage de valeurs manquantes est de", round(p, 2), "%"))

# Les lignes avec les valeurs manquantes seront supprimées


data_clean <- data_final %>%
  filter(!is.na(s2aq9))


# Sauvegarder les données brutes dans processed

saveRDS(data_clean, here("data", "processed", "data_clean.rds"))


