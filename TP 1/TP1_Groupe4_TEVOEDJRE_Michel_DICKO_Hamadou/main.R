# packages nécessaires — installation automatique si absents
pkgs <- c("haven","dplyr","ggplot2","naniar","PropCIs",
          "gtsummary","gt","apyramid","forcats","scales", "curl") # Ajout de curl
miss <- pkgs[!pkgs %in% rownames(installed.packages())]
if (length(miss) > 0) {
  message("Installation : ", paste(miss, collapse = ", "))
  install.packages(miss, repos = "https://cloud.r-project.org")
}

# création des dossiers de sortie et d'entrée
dir.create("data/raw",       showWarnings = FALSE, recursive = TRUE)
dir.create("data/processed", showWarnings = FALSE, recursive = TRUE)
dir.create("outputs",        showWarnings = FALSE, recursive = TRUE)

# récupération guthub des données
github_raw_url <- "https://raw.githubusercontent.com/micheltev229/michel-ensae-2025-2026/main/Projet%20Statistique%20sous%20R%20ou%20Python/TP%201/sect1_harvestw4.dta"
dest_file <- "data/raw/sect1_harvestw4.dta"

if (!file.exists(dest_file)) {
  message("Téléchargement du fichier depuis GitHub...")
  download.file(github_raw_url, destfile = dest_file, mode = "wb")
} else {
  message("Le fichier de données existe déjà localement.")
}

# Préparation des données
source("scripts/01_preparation_donnees.R")

# Analyses et visualisations
source("scripts/02_analyses.R")

cat("\n Terminé !")