# =========================================================
# main.R
# Pipeline principal du TP 3
# Architecture fidèle au dépôt GitHub :
# téléchargement, extraction, détection des .dta,
# construction de la base, analyses pondérées,
# rendu Word du rapport.
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
source(file.path("R", "07_render_report.R"))

cat("\n========================================\n")
cat("Pipeline terminé avec succès.\n")
cat("Résultats disponibles dans le dossier outputs/\n")
cat("Rapport Word généré dans le dossier report/\n")
cat("========================================\n")