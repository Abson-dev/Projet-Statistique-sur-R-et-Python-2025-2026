# installation automatique des packages manquants
pkgs_requis <- c("haven","dplyr","forcats","ggplot2","rstatix",
                 "ggpubr","gtsummary","viridis","patchwork","scales",
                 "knitr","kableExtra","curl","survey","officer")

pkgs_manquants <- pkgs_requis[!pkgs_requis %in% installed.packages()[,"Package"]]
if (length(pkgs_manquants) > 0) {
  message("Installation des packages : ", paste(pkgs_manquants, collapse = ", "))
  install.packages(pkgs_manquants, repos = "https://cloud.r-project.org")
}

# création des dossiers de structure
dir.create("data/raw",       showWarnings = FALSE, recursive = TRUE)
dir.create("data/processed", showWarnings = FALSE, recursive = TRUE)
dir.create("outputs",        showWarnings = FALSE, recursive = TRUE)

# CONFIGURATION GITHUB
base_url <- "https://raw.githubusercontent.com/micheltev229/michel-ensae-2025-2026/main/Projet%20Statistique%20sous%20R%20ou%20Python/TP%202/"
fichiers_a_telecharger <- c("sect1_harvestw4.dta", "sect2_harvestw4.dta", "secta_harvestw4.dta")

for (f in fichiers_a_telecharger) {
  dest_path <- file.path("data/raw", f)
  if (!file.exists(dest_path)) {
    message("Téléchargement de : ", f)
    tryCatch(
      download.file(paste0(base_url, f), destfile = dest_path, mode = "wb"),
      error = function(e) {
        if (f == "secta_harvestw4.dta") {
          stop(
            "\n FICHIER MANQUANT : data/raw/secta_harvestw4.dta\n",
            " Ce fichier contient les poids de sondage (wt_wave4).\n",
            " Placez-le dans data/raw/ ou déposez-le sur votre GitHub.\n",
            " Source : NGA_2018_GHSP-W4_v03_M_Stata12.zip (World Bank Microdata)\n"
          )
        } else {
          stop("Erreur téléchargement : ", f, "\n", conditionMessage(e))
        }
      }
    )
  }
}

# Chargement et préparation
source("scripts/01_chargement_donnees.R")

# Analyses et visualisations
source("scripts/02_analyses_visualisations.R")

cat("\n Analyse terminée ! \n")
cat("Résultats dans : outputs/\n")
