

# Lecture des libarairies necessaires pour l'importation des données

library(haven) # pour importer les bases format stata
library(here) # pour permettre la reproductibilité sur un autre pc sans toucher au chemin d'accès


sect1_harvestw4 <- read_dta(here("data", "raw", "sect1_harvestw4.dta"))

sect2_harvestw4 <- read_dta(here("data", "raw", "sect2_harvestw4.dta"))


# Sauvegarder les données brutes dans processed

saveRDS(sect1_harvestw4, here("data", "processed", "sect1_harvestw4.rds"))


saveRDS(sect2_harvestw4, here("data", "processed", "sect2_harvestw4.rds"))



