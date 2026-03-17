# =============================================================================
# main.R — Point d'entrée du projet TP1
# Profil démographique des ménages nigérians
# Nigeria GHS-Panel Wave 4 (2018/19) — sect1_harvestw4.dta
#
# Auteurs  : Herman YAMAHA | Bourama DIALLO
# Cours    : Projet Statistique sous R et Python — ENSAE ISE 1, 2025-2026
# Superviseur : M. Aboubacar HEMA
# =============================================================================
#
# Arborescence du projet :
#
#   data/
#     processed/    fichiers .rds intermédiaires
#   scripts/
#     01_import_nettoyage.R                    (chargement + recodages)
#     02_exploration_visualisation.R           (statistiques + graphiques)
#   outputs/                                   (figures PNG + tableau HTML)
#   rapport/
#     Rapport_TP1_profil_demographique.Rmd     (rapport R Markdown complet)
#     header.tex                               (en-tête LaTeX)
#     logos/                                   (ENSAE, ANSD, SN)
#
# Pour lancer l'analyse complète :
#   1. Ouvrir le .Rproj dans RStudio
#   2. Exécuter ce fichier (source("main.R"))
#
# Les figures sont sauvegardées dans outputs/
# Le rapport est à compiler depuis rapport/Rapport_TP1_profil_demographique.Rmd
# =============================================================================

# --------------------------------------------------------------------------
# 0. Packages requis

# --------------------------------------------------------------------------
packages_necessaires <- c(
  "haven",      # lecture des fichiers .dta Stata
  "dplyr",      # manipulation des données
  "ggplot2",    # visualisation
  "naniar",     # diagnostic valeurs manquantes
  "PropCIs",    # intervalles de confiance exacts (Clopper-Pearson)
  "gtsummary",  # tableaux de synthèse statistique
  "gt",         # export HTML des tableaux
  "apyramid",   # pyramide des âges
  "forcats",    # réordonner les facteurs
  "scales",     # formatage des axes
  "kableExtra" # génération du rapport R-markdown
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
dir.create("outputs",        showWarnings = FALSE, recursive = TRUE)

# --------------------------------------------------------------------------
# 2. Étape 1 — Import et préparation des données
# --------------------------------------------------------------------------
# --------------------------------------------------------------------------
# 1.bis Téléchargement des données depuis GitHub (si absentes)
# --------------------------------------------------------------------------

cat("Vérification / téléchargement des données (GitHub)...\n")

options(timeout = 300)

# URL de base
base_url <- paste0(
  "https://raw.githubusercontent.com/",
  "Herman-YAMAHA/NYHP/",
  "d77e8ff810dc6bcd1bfd14d2805db0be856dcaf0/",
  "TP1_raw/"
)

# Fichier requis
fichiers <- c("sect1_harvestw4.dta")

# Création dossier si besoin
dir.create("data/raw", recursive = TRUE, showWarnings = FALSE)

for (f in fichiers) {
  dest <- file.path("data/raw", f)
  
  if (!file.exists(dest)) {
    url <- paste0(base_url, f)
    cat("  Téléchargement :", f, "...\n")
    
    tryCatch(
      {
        download.file(url, destfile = dest, mode = "wb")
        cat(" ", f, "téléchargé.\n")
      },
      error = function(e) {
        cat(" Erreur pour", f, ":", conditionMessage(e), "\n")
      }
    )
    
  } else {
    cat(" ", f, "déjà présent.\n")
  }
}

cat("Téléchargement terminé.\n\n")
cat("========================================\n")
cat("  ÉTAPE 1 : Import et nettoyage\n")
cat("========================================\n")
source("scripts/01_import_nettoyage.R")

# --------------------------------------------------------------------------
# 3. Étape 2 — Exploration statistique et visualisations
# --------------------------------------------------------------------------
cat("\n========================================\n")
cat("  ÉTAPE 2 : Exploration et visualisations\n")
cat("========================================\n")
source("scripts/02_exploration_visualisation.R")

# --------------------------------------------------------------------------
# 4. Récapitulatif des sorties
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
