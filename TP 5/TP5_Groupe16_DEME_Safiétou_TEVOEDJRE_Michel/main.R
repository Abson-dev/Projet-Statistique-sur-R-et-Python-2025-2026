# Packages nécessaires
pkgs <- c(
  "haven",      # importer les .dta
  "dplyr",      # transformer les données
  "tidyr",      # pivoter, remodeler
  "ggplot2",    # graphiques
  "forcats",    # gestion des facteurs
  "scales",     # mise en forme des axes
  "rstatix",    # tests non paramétriques
  "ggpubr",     # figures statistiques
  "patchwork",  # composition de graphiques
  "survey",     # estimations pondérées complexes
  "srvyr",      # interface tidyverse pour survey
  "flextable",  # tableaux formatés
  "officer",    # documents Office
  "knitr",      # compilation Rmd
  "rmarkdown",  # rendu Rmd
  "stringr",    # manipulations de chaînes
  "openxlsx"    # écriture Excel
)

miss <- pkgs[!pkgs %in% installed.packages()[, "Package"]]
if (length(miss) > 0) {
  message(">>> Installation en cours : ", paste(miss, collapse = ", "))
  install.packages(miss, repos = "https://cloud.r-project.org")
}

# Préparation de l'espace de travail
for (rep in c("data/raw", "data/processed", "outputs")) {
  dir.create(rep, showWarnings = FALSE, recursive = TRUE)
}

# Téléchargement des données depuis GitHub 
options(timeout = 300)

base_url <- paste0(
  "https://raw.githubusercontent.com/",
  "micheltev229/michel-ensae-2025-2026/main/",
  "Projet%20Statistique%20sous%20R%20ou%20Python/TP%205/"
)

fichiers_tp5 <- c(
  "secta_harvestw4.dta",
  "secta3i_harvestw4.dta",
  "secta3ii_harvestw4.dta",
  "secta11c2_harvestw4.dta"
)

for (f in fichiers_tp5) {
  dest_file <- file.path("data/raw", f)
  if (!file.exists(dest_file)) {
    url_file <- paste0(base_url, f)
    tryCatch({
      download.file(url_file, destfile = dest_file, mode = "wb", quiet = TRUE)
      cat("  Téléchargé :", f, "\n")
    }, error = function(e) {
      cat("  Erreur sur :", f, "—", conditionMessage(e), "\n")
    })
  } else {
    cat("  Déjà présent :", f, "\n")
  }
}

# exécution des scripts
source("scripts/01_preparation_donnees.R")
source("scripts/02_analyses_visualisations.R")

cat("SUCCÈS\n")