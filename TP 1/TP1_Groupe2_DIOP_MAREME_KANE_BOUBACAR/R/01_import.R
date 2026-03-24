# ============================================================
# Chargement et sauvegarde des données brutes
# ============================================================

library(haven)

# Chargement de  la base sect1_harvestw4
sect1_w4 <- read_dta("data/raw/sect1_harvestw4.dta")

# Chargement de sectaa_harvest qui contient les zones urabaine et rurale
<<<<<<< HEAD
secta_w4 <- read_dta("data/raw/sectaa_harvestw4.dta")
=======
secta_w4 <- read_dta("data/raw/secta_harvestw4.dta")
>>>>>>> 664df72ee5b586307c349b237310d9487e119661

# Sauvegarde en .rds dans data/processed/
saveRDS(sect1_w4, "data/processed/sect1_w4.rds")
saveRDS(secta_w4, "data/processed/secta_w4.rds")

