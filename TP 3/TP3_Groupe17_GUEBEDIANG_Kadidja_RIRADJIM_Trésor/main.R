# =========================================================
# main.R
# Fichier principal : exécute tout le pipeline du projet
# =========================================================

rm(list = ls())
options(stringsAsFactors = FALSE)
options(scipen = 999)

cat("========================================\n")
cat("Lancement du projet TP Analyse 3\n")
cat("========================================\n")

source(file.path("R", "00_packages.R"))
source(file.path("R", "01_paths.R"))
source(file.path("R", "02_inventory_and_extract.R"))
source(file.path("R", "03_detect_relevant_files.R"))
source(file.path("R", "04_utils.R"))
source(file.path("R", "05_build_analysis_data.R"))
source(file.path("R", "06_run_analysis3.R"))

cat("========================================\n")
cat("Pipeline terminé avec succès.\n")
cat("Résultats disponibles dans le dossier output/\n")
cat("========================================\n")