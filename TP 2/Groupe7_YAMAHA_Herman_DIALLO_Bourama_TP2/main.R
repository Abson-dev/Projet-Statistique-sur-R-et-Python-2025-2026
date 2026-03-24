# =============================================================================
# main.R – Script principal du projet
# Thème 2 : Éducation et alphabétisation des membres des ménages
# Enquête GHS Nigeria 2018 (Wave 4) – avec pondérations transversales wt_wave4
# Réalisé par : Herman YAMAHA, Bourama DIALLO
# =============================================================================

# ---- Vérification et installation des dépendances ----
packages_necessaires <- c(
  "haven", "dplyr", "tidyr", "forcats", "ggplot2", "rstatix",
  "ggpubr", "gtsummary", "viridis", "patchwork", "scales",
  "survey", "srvyr",          # plan de sondage pondéré
  "openxlsx",                 # export Excel du tableau gtsummary
  "flextable", "officer",     # tableaux Word dans le rapport
  "knitr"
)

a_installer <- packages_necessaires[
  !packages_necessaires %in% installed.packages()[, "Package"]
]
if (length(a_installer) > 0) {
  message("Installation des packages manquants : ",
          paste(a_installer, collapse = ", "))
  install.packages(a_installer, repos = "https://cloud.r-project.org")
}

# ---- Création des dossiers ----
dir.create("data/raw",       showWarnings = FALSE, recursive = TRUE)
dir.create("data/processed", showWarnings = FALSE, recursive = TRUE)
dir.create("outputs",        showWarnings = FALSE, recursive = TRUE)

# ---- Téléchargement des données depuis GitHub ----
# Les fichiers .dta sont hébergés sur GitHub et téléchargés automatiquement
# dans data/raw/ à la première exécution. Le dossier data/ n'est pas versionné.
cat("Vérification / téléchargement des données (GitHub)...\n")
options(timeout = 300)

base_url <- paste0(
  "https://raw.githubusercontent.com/",
  "Herman-YAMAHA/NYHP/",
  "bf1173ced39831e18d8e21c3b2880e597bbc6300/",
  "TP2_raw/"
)

fichiers <- c(
  "sect1_harvestw4.dta",
  "sect2_harvestw4.dta",
  "secta_harvestw4.dta"
)

for (f in fichiers) {
  dest <- file.path("data/raw", f)
  if (!file.exists(dest)) {
    url <- paste0(base_url, f)
    cat("  Téléchargement :", f, "...\n")
    tryCatch(
      {
        download.file(url, destfile = dest, mode = "wb")
        cat("  \u2713", f, "téléchargé.\n")
      },
      error = function(e) {
        cat("  \u2717 Erreur pour", f, ":", conditionMessage(e), "\n")
        cat("    => Vérifiez votre connexion ou placez manuellement",
            f, "dans data/raw/\n")
      }
    )
  } else {
    cat("  \u2713", f, "déjà présent.\n")
  }
}
cat("Téléchargement terminé.\n\n")

# ---- Phase 1 : Import, nettoyage et pondération ----
cat("=== PHASE 1 : Import et préparation des données ===\n")
source("scripts/01_import_nettoyage.R")

# ---- Phase 2 : Analyses descriptives et graphiques ----
cat("\n=== PHASE 2 : Analyses descriptives et graphiques ===\n")
source("scripts/02_statistiques_graphiques.R")

cat("\n=== Traitement terminé avec succès ! ===\n")
cat("Figures enregistrées dans       : outputs/\n")
cat("Tableau Excel enregistré dans   : outputs/tableau_gtsummary.xlsx\n")
cat("Rapport à compiler              : rapport/Rapport_educ_alphabetisation_menages.Rmd\n")
cat("  => Ouvrir dans RStudio, cliquer Knit -> Word Document\n")
