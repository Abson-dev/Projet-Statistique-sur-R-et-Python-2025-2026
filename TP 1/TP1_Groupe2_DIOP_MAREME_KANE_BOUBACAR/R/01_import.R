# ============================================================
# Chargement et sauvegarde des données brutes
# ============================================================

library(haven)

# Chargement de  la base sect1_harvestw4
sect1_w4 <- read_dta("data/raw/sect1_harvestw4.dta")

# Chargement de sectaa_harvest qui contient les zones urabaine et rurale
secta_w4 <- read_dta("data/raw/sectaa_harvestw4.dta")

# Sauvegarde en .rds dans data/processed/
saveRDS(sect1_w4, "data/processed/sect1_w4.rds")
saveRDS(secta_w4, "data/processed/secta_w4.rds")

