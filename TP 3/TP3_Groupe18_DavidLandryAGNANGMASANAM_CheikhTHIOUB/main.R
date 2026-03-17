###############################################################################
#                                                                             #
#   main.R -- Script principal d'execution du TP3                             #
#   Acces aux soins et depenses de sante (GHS Panel, W4 2018)                 #
#                                                                             #
#   ENSAE Pierre Ndiaye -- ISE1-CL -- 2025-2026                              #
#   Auteurs : David Landry AGNANGMA SANAM & Cheikh THIOUB                    #
#                                                                             #
#   USAGE :                                                                   #
#     1. setwd("chemin/vers/TP3")                                             #
#     2. source("main.R")                                                     #
#                                                                             #
#   REMARQUE : Le rapport PDF (docs/TP3_Rapport.pdf) doit etre compile        #
#   separement :                                                              #
#     rmarkdown::render("docs/TP3_Rapport.Rmd", output_dir = "docs")          #
#                                                                             #
###############################################################################

cat("==============================================================\n")
cat("  TP3 -- Acces aux soins et depenses de sante\n")
cat("  Execution complete du pipeline d'analyse\n")
cat("==============================================================\n\n")

# Verification du repertoire de travail
if (!file.exists("data/sect4a_harvestw4.dta")) {
  stop(paste0(
    "ERREUR : data/sect4a_harvestw4.dta introuvable.\n",
    "Verifiez que le repertoire de travail est la racine du projet TP3/\n",
    "  getwd() actuel : ", getwd(), "\n",
    "Utilisez : setwd('chemin/vers/TP3') avant de relancer."
  ))
}

# ---- Etape 1 : Fonctions, chemins, theme ----
cat(">>> Etape 1/3 : Chargement des fonctions et configuration\n")
source("scripts/00_fonctions.R", encoding = "UTF-8")

# ---- Etape 2 : Import des donnees ----
cat(">>> Etape 2/3 : Import des donnees\n")
source("scripts/01_import.R", encoding = "UTF-8")

# ---- Etape 3 : Nettoyage et recodage ----
cat(">>> Etape 3/3 : Nettoyage et recodage\n")
source("scripts/02_nettoyage.R", encoding = "UTF-8")

# ---- Etape 4 : Analyses et livrables ----
cat(">>> Etape 4/4 : Analyses, tests et livrables\n")
source("scripts/03_analyse.R", encoding = "UTF-8")

# ---- Resume final ----
cat("\n==============================================================\n")
cat("  EXECUTION TERMINEE -- Livrables generes :\n")
cat("==============================================================\n")
cat("  outputs/barplot_maladies_prestataires.png\n")
cat("    -> Barplot des types de maladies et prestataires consultes\n")
cat("  outputs/violin_depenses_zone_quintile.png\n")
cat("    -> Violin plots des depenses par zone et par quintile\n")
cat("  outputs/tableau_contingence_soins_quintile.csv\n")
cat("    -> Tableau de contingence recours x quintile\n")
cat("  outputs/tableau_contingence_soins_quintile_tests.txt\n")
cat("    -> Resultats des tests (chi2, V de Cramer)\n")
cat("==============================================================\n")
cat("\n  Pour generer le rapport PDF :\n")
cat("  rmarkdown::render('docs/TP3_Rapport.Rmd', output_dir = 'docs')\n")
cat("==============================================================\n")
