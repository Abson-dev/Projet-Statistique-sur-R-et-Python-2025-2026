# ============================================
# IMPORTATION DES DONNÉES
# ============================================
source('R/chargement_packages.R')

# Chargement des fichiers
donnees <- read_dta('data/raw/sect3a_harvestw4.dta')
demo    <- read_dta('data/raw/sect1_harvestw1.dta')
sect4a <- read_dta("data/raw/sect4a_harvestw4.dta")
# Vérification
dim(donnees)
dim(demo)


