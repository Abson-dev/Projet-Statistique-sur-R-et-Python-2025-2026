###############################################################################
# 01_import.R -- Chargement et exploration des donnees sante (Tache 13)
# Projet : TP3 -- Acces aux soins et depenses de sante (GHS Panel W4)
###############################################################################

cat("\n========== 01_import : Chargement des donnees ==========\n")

sect4a  <- read_dta(file.path(chemin_data, "sect4a_harvestw4.dta"))
sect1   <- read_dta(file.path(chemin_data, "sect1_harvestw4.dta"))
totcons <- read_dta(file.path(chemin_data, "totcons_final.dta"))
secta   <- read_dta(file.path(chemin_data, "secta_harvestw4.dta"))

cat("sect4a_harvestw4 :", nrow(sect4a), "obs,", ncol(sect4a), "var\n")
cat("sect1_harvestw4  :", nrow(sect1), "obs\n")
cat("totcons_final    :", nrow(totcons), "obs\n")
cat("secta_harvestw4  :", nrow(secta), "obs\n")

cat("[01_import] Chargement termine.\n")
