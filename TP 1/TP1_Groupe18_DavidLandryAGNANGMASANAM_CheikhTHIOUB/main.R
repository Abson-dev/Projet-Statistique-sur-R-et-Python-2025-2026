###############################################################################
#                                                                             #
#   main.R — Script principal d'exécution du TP1                              #
#   Profil démographique des ménages nigérians (GHS Panel, W1-W4)             #
#                                                                             #
#   ENSAE Pierre Ndiaye — ISE1-CL — 2025-2026                                #
#   Auteurs : David Landry AGNANGMA SANAM & Cheikh THIOUB                     #
#                                                                             #
#   USAGE :                                                                   #
#     1. Définir le répertoire de travail à la racine du projet :             #
#        setwd("chemin/vers/TP1")                                             #
#     2. Exécuter ce script : source("main.R")                                #
#                                                                             #
#   Ce script exécute séquentiellement :                                      #
#     scripts/00_fonctions.R  — Packages, chemins, thème, fonctions           #
#     scripts/01_import.R     — Chargement et exploration des données         #
#     scripts/02_nettoyage.R  — Nettoyage et recodage des variables           #
#     scripts/03_analyse.R    — Analyses, tests, graphiques, livrables        #
#                                                                             #
#   Puis compile le rapport PDF dans docs/ :                                  #
#     TP1_Rapport.Rmd → docs/TP1_Rapport.pdf                                  #
#                                                                             #
###############################################################################

cat("==============================================================\n")
cat("  TP1 — Profil démographique des ménages nigérians\n")
cat("  Exécution complète du pipeline d'analyse\n")
cat("==============================================================\n\n")

# Vérification du répertoire de travail
if (!file.exists("data/sect1_harvestw4.dta")) {
  stop(paste0(
    "ERREUR : Le fichier data/sect1_harvestw4.dta est introuvable.\n",
    "Vérifiez que le répertoire de travail est bien la racine du projet TP1/\n",
    "  getwd() actuel : ", getwd(), "\n",
    "Utilisez : setwd('chemin/vers/TP1') avant de relancer."
  ))
}

# ---- Étape 1 : Fonctions utilitaires, chemins, thème ----
cat(">>> Étape 1/4 : Chargement des fonctions et configuration\n")
source("scripts/00_fonctions.R", encoding = "UTF-8")

# ---- Étape 2 : Import des données ----
cat(">>> Étape 2/4 : Import des données\n")
source("scripts/01_import.R", encoding = "UTF-8")

# ---- Étape 3 : Nettoyage et recodage ----
cat(">>> Étape 3/4 : Nettoyage et recodage\n")
source("scripts/02_nettoyage.R", encoding = "UTF-8")

# ---- Étape 4 : Analyses et génération des livrables ----
cat(">>> Étape 4/4 : Analyses, tests et livrables\n")
source("scripts/03_analyse.R", encoding = "UTF-8")


# ---- Résumé final ----
cat("\n==============================================================\n")
cat("  EXÉCUTION TERMINÉE — Livrables générés :\n")
cat("==============================================================\n")
cat("  outputs/pyramide_ages_w4.png         — Pyramide des âges (W4)\n")
cat("  outputs/tableau_gtsummary_zone.docx  — Tableau gtsummary exportable\n")
cat("  outputs/histogramme_age_w4.png       — Histogramme de l'âge\n")
cat("  outputs/boxplot_taille_zone.png      — Boxplot taille par zone\n")
cat("  outputs/barplot_parente.png          — Barplot lien de parenté\n")
cat("  outputs/figure_synthese_tp1.png      — Figure de synthèse\n")
cat("==============================================================\n")
