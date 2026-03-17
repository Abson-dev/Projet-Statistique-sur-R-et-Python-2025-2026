# Packages
pkgs <- c("haven","dplyr","ggplot2","forcats","scales",
          "naniar","rstatix","ggpubr","patchwork","gtsummary","tidyr")
miss <- pkgs[!pkgs %in% installed.packages()[,"Package"]]
if (length(miss) > 0) install.packages(miss, repos = "https://cloud.r-project.org")

# Répertoires
dir.create("data/raw",       showWarnings = FALSE, recursive = TRUE)
dir.create("data/processed", showWarnings = FALSE, recursive = TRUE)
dir.create("output/figures", showWarnings = FALSE, recursive = TRUE)
dir.create("output/tables",  showWarnings = FALSE, recursive = TRUE)

# Téléchargement des données depuis GitHub
base_url <- "https://raw.githubusercontent.com/micheltev229/michel-ensae-2025-2026/main/Projet%20Statistique%20sous%20R%20ou%20Python/TP%203/"

fichiers_tp3 <- c(
  "sect4a_harvestw4.dta",
  "sect1_harvestw4.dta",
  "secta_harvestw4.dta",
  "totcons_final.dta",
  "sect3a_harvestw4.dta",
  "sect3b_harvestw4.dta"
)

cat("Vérification des données sur GitHub...\n")
for (f in fichiers_tp3) {
  dest_file <- file.path("data/raw", f)
  if (!file.exists(dest_file)) {
    url_file <- gsub(" ", "%20", paste0(base_url, f))
    tryCatch({
      download.file(url_file, destfile = dest_file, mode = "wb")
      cat("  Téléchargé :", f, "\n")
    }, error = function(e) {
      cat("  Erreur sur :", f, "(vérifier si le fichier est sur GitHub)\n")
    })
  }
}

# Préparation des données
source("scripts/01_preparation_donnees.R")

# Analyses et visualisations
source("scripts/02_analyses_visualisations.R")

cat("\n Terminé ! \n")
cat("Résultats : output/\n")