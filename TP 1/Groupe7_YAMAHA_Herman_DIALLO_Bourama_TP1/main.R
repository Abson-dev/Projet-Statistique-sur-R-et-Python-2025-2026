# =============================================================================
# main.R — Point d'entrée du projet TP1 (avec pondérations)
# Profil démographique des ménages nigérians
# Nigeria GHS-Panel Wave 4 (2018/19)
#
# Auteurs      : Herman YAMAHA | Bourama DIALLO
# Cours        : Projet Statistique sous R et Python — ENSAE ISE 1, 2025-2026
# Superviseur  : M. Aboubacar HEMA
#
# ─────────────────────────────────────────────────────────────────────────────
# MISE À JOUR PRINCIPALE — Intégration des pondérations
# ─────────────────────────────────────────────────────────────────────────────
# Ce projet intègre désormais les pondérations transversales wt_wave4 issues
# de secta_harvestw4.dta. Toutes les statistiques descriptives, proportions
# et tests sont calculés avec ces poids pour que les résultats soient
# représentatifs de la population nigériane (et non seulement de l'échantillon).
#
# Packages supplémentaires requis : survey, srvyr, openxlsx, flextable
#
# ─────────────────────────────────────────────────────────────────────────────
# Arborescence du projet :
#
#   data/
#     raw/          
#     processed/    fichiers .rds intermédiaires (avec poids)
#   scripts/
#     01_import_nettoyage.R      chargement + jointure pondérations + recodages
#     02_exploration_visualisation.R   statistiques pondérées + graphiques
#   outputs/
#     fig01_histogramme_age.png
#     fig02_boxplot_age.png
#     fig03_pyramide_ages.png
#     fig04_lien_parente.png
#     fig05_boxplot_menages.png
#     tableau_gtsummary.xlsx     (remplace l'ancien .html)
#   rapport/
#     Rapport_TP1_profil_demographique.Rmd   → génère un .docx
#
# Pour lancer l'analyse complète :
#   1. Ouvrir le .Rproj dans RStudio
#   2. Exécuter ce fichier (source("main.R"))
#   3. Compiler le rapport : ouvrir le .Rmd et cliquer sur Knit → Word
# =============================================================================

# --------------------------------------------------------------------------
# 0. Packages requis
# --------------------------------------------------------------------------
packages_necessaires <- c(
  "haven",       # lecture des fichiers .dta Stata
  "dplyr",       # manipulation des données
  "ggplot2",     # visualisation
  "naniar",      # diagnostic valeurs manquantes
  "PropCIs",     # intervalles de confiance exacts (Clopper-Pearson)
  "gtsummary",   # tableaux de synthèse statistique
  "gt",          # export des tableaux gtsummary
  "apyramid",    # pyramide des âges
  "forcats",     # réordonner les facteurs
  "scales",      # formatage des axes
  "survey",      # plan de sondage complexe (pondérations)
  "srvyr",       # interface dplyr pour survey
  "openxlsx",    # export Excel du tableau gtsummary
  "flextable",   # tableaux Word dans le rapport R Markdown
  "officer",     # gestion des documents Word
  "knitr"        # compilation du rapport R Markdown
)

a_installer <- packages_necessaires[
  !packages_necessaires %in% rownames(installed.packages())
]
if (length(a_installer) > 0) {
  message("Installation des packages manquants : ",
          paste(a_installer, collapse = ", "))
  install.packages(a_installer, repos = "https://cloud.r-project.org")
}

# --------------------------------------------------------------------------
# 1. Création des dossiers de sortie
# --------------------------------------------------------------------------
dir.create("data/processed", showWarnings = FALSE, recursive = TRUE)
dir.create("data/raw",       showWarnings = FALSE, recursive = TRUE)
dir.create("outputs",        showWarnings = FALSE, recursive = TRUE)

# --------------------------------------------------------------------------
# 2. Téléchargement des données depuis GitHub (si absentes)
# --------------------------------------------------------------------------
# Les fichiers .dta sont hébergés sur GitHub et téléchargés automatiquement
# dans data/raw/ à la première exécution. Le dossier data/ n'est pas versionné.
# --------------------------------------------------------------------------
cat("Vérification / téléchargement des données (GitHub)...\n")
options(timeout = 300)

base_url <- paste0(
  "https://raw.githubusercontent.com/",
  "Herman-YAMAHA/NYHP/",
  "bf1173ced39831e18d8e21c3b2880e597bbc6300/",
  "TP1_raw/"
)

fichiers <- c("sect1_harvestw4.dta", "secta_harvestw4.dta")

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
        cat("    => Vérifiez votre connexion ou placez manuellement", f, "dans data/raw/\n")
      }
    )
  } else {
    cat("  \u2713", f, "déjà présent.\n")
  }
}
cat("Téléchargement terminé.\n\n")

# --------------------------------------------------------------------------
# 3. Étape 1 — Import, jointure pondérations et préparation des données
# --------------------------------------------------------------------------
cat("========================================\n")
cat("  ÉTAPE 1 : Import et nettoyage\n")
cat("========================================\n")
source("scripts/01_import_nettoyage.R")

# --------------------------------------------------------------------------
# 4. Étape 2 — Exploration statistique pondérée et visualisations
# --------------------------------------------------------------------------
cat("\n========================================\n")
cat("  ÉTAPE 2 : Exploration et visualisations\n")
cat("========================================\n")
source("scripts/02_exploration_visualisation.R")

# --------------------------------------------------------------------------
# 5. Récapitulatif des sorties
# --------------------------------------------------------------------------
cat("\n========================================\n")
cat("  PROJET TERMINÉ\n")
cat("========================================\n")
cat("Fichiers produits dans outputs/ :\n")
for (f in list.files("outputs/", full.names = FALSE)) {
  cat("  ->", f, "\n")
}
cat("\nRapport à compiler :\n")
cat("  -> rapport/Rapport_TP1_profil_demographique.Rmd\n")
cat("  -> Ouvrir dans RStudio, cliquer sur Knit → Word Document\n")
cat("\n  NOTE : Le tableau de synthèse est exporté en Excel\n")
cat("         (outputs/tableau_gtsummary.xlsx)\n")
