# ===========================================================================
# main.R — Script principal du projet TP5
# Thème 5 : Analyse des cultures, intrants et rendements agricoles
# Enquête GHS-Panel du Nigeria — Vague 4 (2018/19)
# Pondérations transversales : wt_wave4
#
# Réalisé par  : David Landry AGNANGMA SANAM | Hamadou DICKO
# Formation    : Projet Statistique R & Python — ENSAE ISE 1, 2025-2026
# Encadrant    : M. Aboubacar HEMA
#
# ───────────────────────────────────────────────────────────────────────────
# PRÉSENTATION DU TRAVAIL
# ───────────────────────────────────────────────────────────────────────────
# L'objectif est d'analyser les données agricoles issues de la Wave 4 du
# GHS-Panel nigérian (2018/19). Les fichiers exploités sont :
#
#   - secta3i_harvestw4    : récoltes par parcelle (cropcode, quantités)
#   - secta1_harvestw4     : superficies des parcelles (GPS + auto-déclaré)
#   - secta11c2_harvestw4  : utilisation d'intrants (engrais, pesticides)
#   - secta_harvestw4      : poids d'échantillonnage, géographie
#
# Questions traitées :
#   Q25 — 15 premières cultures par fréquence pondérée
#   Q26 — Mesure de la diversification culturale par exploitation
#   Q27 — Taux d'utilisation des intrants par catégorie et par zone
#   Q28 — Rendement maïs/millet par État (kg/ha, grâce à secta1)
#   Q29 — Effet de l'engrais inorganique sur le rendement (kg/ha)
#
# Toutes les estimations tiennent compte des poids wt_wave4.
#
# ───────────────────────────────────────────────────────────────────────────
# ORGANISATION DES FICHIERS
# ───────────────────────────────────────────────────────────────────────────
#   data/raw/           — données brutes (.dta) récupérées automatiquement
#   data/processed/     — résultats intermédiaires (.rds)
#   scripts/
#     01_chargement_preparation.R  — acquisition et mise en forme
#     02_statistiques_figures.R    — calculs, tests et visualisations
#   outputs/            — graphiques (.png) et tableaux (.xlsx)
#   rapport/            — fichier Rmd compilable en PDF
# ===========================================================================

# ── 0. Vérification et installation des packages ─────────────────────────
liste_packages <- c(
  "haven",       # importer les .dta
  "dplyr",       # transformer les données
  "tidyr",       # pivoter, remodeler
  "ggplot2",     # graphiques
  "forcats",     # gestion des facteurs
  "scales",      # mise en forme des axes
  "rstatix",     # tests non paramétriques
  "ggpubr",      # figures statistiques
  "patchwork",   # composition de graphiques
  "survey",      # estimations pondérées complexes
  "srvyr",       # interface tidyverse pour survey
  "openxlsx",    # écriture de fichiers Excel
  "flextable",   # tableaux formatés
  "officer",     # documents Office
  "knitr",       # compilation
  "kableExtra",  # tableaux LaTeX pour PDF
  "rmarkdown",   # rendu Rmd
  "tinytex"      # moteur LaTeX léger
)

manquants <- liste_packages[
  !liste_packages %in% installed.packages()[, "Package"]
]
if (length(manquants) > 0) {
  message(">>> Installation en cours : ", paste(manquants, collapse = ", "))
  install.packages(manquants, repos = "https://cloud.r-project.org")
}

# Vérifier la présence de TinyTeX
if (!tinytex::is_tinytex()) {
  message(">>> Mise en place de TinyTeX pour la génération PDF...")
  tinytex::install_tinytex()
}

# ── 1. Préparation de l'espace de travail ─────────────────────────────────
for (rep in c("data/raw", "data/processed", "outputs")) {
  dir.create(rep, showWarnings = FALSE, recursive = TRUE)
}

# ── 2. Étape 1 — Chargement et nettoyage des données ─────────────────────
cat("================================================\n")
cat("  ÉTAPE 1 : Acquisition et mise en forme\n")
cat("================================================\n")
source("scripts/01_chargement_preparation.R")

# ── 3. Étape 2 — Calculs statistiques et graphiques ──────────────────────
cat("\n================================================\n")
cat("  ÉTAPE 2 : Analyses statistiques et figures\n")
cat("================================================\n")
source("scripts/02_statistiques_figures.R")


# ── 5. Résumé final ──────────────────────────────────────────────────────
cat("\n================================================\n")
cat("  PROJET TP5 — TERMINÉ AVEC SUCCÈS\n")
cat("================================================\n")
cat("Graphiques générés (outputs/) :\n")
for (fichier in list.files("outputs/", pattern = "\\.png$"))
  cat("  >>", fichier, "\n")
cat("Tableau Excel  : outputs/synthese_intrants.xlsx\n")