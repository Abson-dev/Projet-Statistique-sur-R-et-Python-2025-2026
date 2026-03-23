# packages nécessaires — installation automatique si absents
pkgs <- c("haven","dplyr","ggplot2","naniar","PropCIs",
          "gtsummary","gt","apyramid","forcats","scales","curl","survey","officer")
miss <- pkgs[!pkgs %in% rownames(installed.packages())]
if (length(miss) > 0) {
  message("Installation : ", paste(miss, collapse = ", "))
  install.packages(miss, repos = "https://cloud.r-project.org")
}

# création des dossiers de sortie et d'entrée
dir.create("data/raw",       showWarnings = FALSE, recursive = TRUE)
dir.create("data/processed", showWarnings = FALSE, recursive = TRUE)
dir.create("outputs",        showWarnings = FALSE, recursive = TRUE)

# récupération github des données (sect1 — individus)
github_raw_url <- "https://raw.githubusercontent.com/micheltev229/michel-ensae-2025-2026/main/Projet%20Statistique%20sous%20R%20ou%20Python/TP%201/sect1_harvestw4.dta"
dest_file <- "data/raw/sect1_harvestw4.dta"

if (!file.exists(dest_file)) {
  message("Téléchargement de sect1_harvestw4.dta depuis GitHub...")
  download.file(github_raw_url, destfile = dest_file, mode = "wb")
} else {
  message("Le fichier sect1_harvestw4.dta existe déjà localement.")
}

# récupération github des pondérations (secta — poids ménages Wave 4)
#     Projet Statistique sous R ou Python/TP 1/secta_harvestw4.dta
github_wgt_url <- "https://raw.githubusercontent.com/micheltev229/michel-ensae-2025-2026/main/Projet%20Statistique%20sous%20R%20ou%20Python/TP%201/secta_harvestw4.dta"
dest_wgt <- "data/raw/secta_harvestw4.dta"

if (!file.exists(dest_wgt)) {
  message("Téléchargement de secta_harvestw4.dta depuis GitHub...")
  download.file(github_wgt_url, destfile = dest_wgt, mode = "wb")
} else {
  message("Le fichier secta_harvestw4.dta existe déjà localement.")
}

# Préparation des données (avec jointure des poids)
source("scripts/01_preparation_donnees.R")

# Analyses et visualisations (résultats pondérés)
source("scripts/02_analyses.R")

cat("\n Terminé !")
