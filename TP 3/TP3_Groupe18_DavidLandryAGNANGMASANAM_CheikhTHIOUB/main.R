###############################################################################
#   main.R -- Script principal d'execution du TP3
#   Acces aux soins et depenses de sante (GHS Panel, W4 2018)
#   ENSAE Pierre Ndiaye -- ISE1-CL -- 2025-2026
#   Auteurs : David Landry AGNANGMA SANAM & Cheikh THIOUB
#
#   USAGE : setwd("chemin/vers/TP3") puis source("main.R")
###############################################################################

cat("==============================================================\n")
cat("  TP3 -- Acces aux soins et depenses de sante\n")
cat("  Analyses ponderees (wt_wave4)\n")
cat("==============================================================\n\n")

if (!file.exists("data/sect4a_harvestw4.dta")) {
  stop("ERREUR : data/sect4a_harvestw4.dta introuvable. Verifiez setwd().")
}

cat(">>> Etape 1/4 : Fonctions et configuration\n")
source("scripts/00_fonctions.R", encoding = "UTF-8")

cat(">>> Etape 2/4 : Import des donnees\n")
source("scripts/01_import.R", encoding = "UTF-8")

cat(">>> Etape 3/4 : Nettoyage et recodage\n")
source("scripts/02_nettoyage.R", encoding = "UTF-8")

cat(">>> Etape 4/4 : Analyses ponderees et livrables\n")
source("scripts/03_analyse.R", encoding = "UTF-8")

cat("\n==============================================================\n")
cat("  TERMINE -- Livrables dans outputs/\n")
cat("  Pour le rapport Word :\n")
cat("  rmarkdown::render('docs/TP3_Rapport.Rmd', output_dir='docs')\n")
cat("==============================================================\n")
