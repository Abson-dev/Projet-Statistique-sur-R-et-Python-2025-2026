
# Lecture des libarairies necessaires pour l'importation des données

library(haven) # pour importer les bases format stata
library(here) # pour permettre la reproductibilité sur un autre pc sans toucher au chemin d'accès


sect3a_harvestw4 <- read_dta(here("data", "raw", "sect3a_harvestw4.dta"))

sect3b_harvestw4 <- read_dta(here("data", "raw", "sect3b_harvestw4.dta"))


# Sauvegarder les données brutes dans processed

saveRDS(sect3a_harvestw4, here("data", "processed", "sect3a.rds"))


saveRDS(sect3b_harvestw4, here("data", "processed", "sect3b.rds"))


