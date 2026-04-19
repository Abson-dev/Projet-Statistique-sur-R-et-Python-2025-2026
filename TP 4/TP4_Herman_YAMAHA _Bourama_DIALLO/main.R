#Nettoyage de l'environnement de travail

rm(list = ls())

#============================================================================
#main.R - Point d'entrée du projet TP4 (avec pondération)
#Analyse des parcelles agricoles: superficie, tenue foncière et utilisation des terres
#Nigeria GHS-Panel wave 4 (2018/2019)
#
# Auteurs            : Herman YAMAHA et Bourama DIALLO
# Cours              : Projet Statistique sous R et Python - ENSAE ISE 1, 2025-2026
# Superviseur        : M. Aboubacar HEMA
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
#   docs/
#     Rapport_TP4_Analyse_parcelles_agricoles_Nigeria.Rmd   → génère un .docx
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
  "haven",                 # lecture des fichiers .dta stata
  "dplyr",                 # manipulation des données
  "ggplot2",               # tracer des graphes et visualisation
  "ggrepel",               # gestion des étiquettes de texte sur les graphiques 
  "scales",                # gestion de la transformation et du formatage des axes, légendes et valeurs dans les graphiques 
  "patchwork",             # combiner plusieurs graphiques ggplot2 en une seule figure.
  "rstatix",               # tests statistiques et des résumés compatibles avec le pipe (%>%) et le tidyverse.
  "gtsummary",             # génère des tableaux statistiques publication-ready automatiquement.
  "viridis",               #  palettes de couleurs perceptuellement uniformes conçues pour être à la fois belles, lisibles et accessibles.
  "gt",                    # export des tableaux gtsummary
  "apyramid",              # pyramide des âges
  "forcats",               # réordonner les facteurs
  "survey",                # plan de sondage complexe (pondérations)
  "srvyr",                 # interface dplyr pour survey
  "openxlsx",              # export Excel du tableau gtsummary
  "flextable",             # tableaux Word dans le rapport R Markdown
  "officer",               # gestion des documents Word
  "knitr",                 # compilation du rapport R Markdown
  "naniar",                # diagnostic des valeurs manquantes
  "PropCIs",               # intervalles de confiance exacts (Clopper-Pearson)
  "labelled",              # Voir les labels des variables
  "here",                  # gestion efficace des chemins
  "writexl"                # Création de fichiers excels
  
  
)

a_installer <- packages_necessaires[
  !packages_necessaires %in% rownames(installed.packages())
]

if (length(a_installer)>0){
  message("Installation des packages manquants: ",
          paste(a_installer,collapse = ", ")
          )
  install.packages(a_installer,repos="https://cloud.r-project.org")
  
}



# --------------------------------------------------------------------------
# 1. Création des dossiers de sortie
# --------------------------------------------------------------------------

dir.create("data/processed", showWarnings = FALSE, recursive = TRUE)
dir.create("data/raw", showWarnings =  FALSE, recursive = TRUE)
dir.create("outputs/tables", showWarnings = FALSE, recursive = TRUE)
dir.create("outputs/figures", showWarnings =  FALSE, recursive =  TRUE)
dir.create("docs", showWarnings =  FALSE, recursive =  TRUE)
dir.create("scripts", showWarnings =  FALSE, recursive =  TRUE)

# --------------------------------------------------------------------------
# 2. Téléchargement des données depuis GitHub
# --------------------------------------------------------------------------
# Les fichiers .dta sont hébergés sur GitHub et téléchargés automatiquement
# dans data/raw/ à la première exécution. Le dossier data/ n'est pas versionné.
# --------------------------------------------------------------------------

cat("Vérification/ téléchargements des données (Github)...\n")
options(timeout = 300)

base_url <- paste0(
  "https://raw.githubusercontent.com/",
  "diallobourama851-oss/Donn-es-des-TP/",
  "main/",
  "TP4/"
)

fichiers <- c(
  "sect11a1_plantingw4.dta",
  "secta_harvestw4.dta",
  "sect11b1_plantingw4.dta"
)

for (f in fichiers) {
  dest <- file.path("data/raw", f)
  if (!file.exists(dest)) {
    url <- paste0(base_url, f)
    cat("Téléchargement :", f, "...\n")
    download.file(url, destfile = dest, mode = "wb")
  } else {
    cat("Déjà présent :", f, "\n")
  }
}

cat("Téléchargement terminé.\n\n")


# --------------------------------------------------------------------------
# 3. Étape 1 — Import, jointure pondérations et préparation des données
# --------------------------------------------------------------------------
cat("========================================\n")
cat("  ÉTAPE 1 : Import et nettoyage\n")
cat("========================================\n")

source("scripts/01_nettoyage.R")

# --------------------------------------------------------------------------
# 4. Étape 2 — Exploration statistique pondérée et visualisations
# --------------------------------------------------------------------------
cat("\n========================================\n")
cat("  ÉTAPE 2 : Exploration et visualisations\n")
cat("========================================\n")
source("scripts/02_analyse.R")

# --------------------------------------------------------------------------
# 5. Récapitulatif des sorties
# --------------------------------------------------------------------------
cat("\n========================================\n")
cat("  PROJET TERMINÉ\n")
cat("========================================\n")
cat("Fichiers produits dans outputs/ :\n")

for (f in list.files("outputs/", full.names = FALSE)){
  cat(" ->", f, "\n")
}

cat("\nRapport à compiler :\n")
cat("  -> rapport/Rapport_TP4_Analyse_parcelles_agricoles_Nigeria.Rmd\n")
cat("  -> Ouvrir dans RStudio, cliquer sur Knit → Word Document\n")
cat("\n  NOTE : Le tableau de synthèse est exporté en Excel\n")
cat("         (outputs/tableau_gtsummary.xlsx)\n")


