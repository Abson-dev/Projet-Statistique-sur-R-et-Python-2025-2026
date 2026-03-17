# =============================================================
# main.R — Point d'entrée unique du projet
# Exécuter CE fichier suffit pour tout reproduire
# =============================================================
# Installation et chargement des packages nécessaires
# =============================================================
packages <- c("here", "haven", "dplyr", "ggplot2", "apyramid",
              "naniar", "gtsummary", "rstatix", "scales",
              "forcats", "moments","webshot2", "patchwork")

# Installe uniquement ceux qui manquent
a_installer <- packages[!packages %in% rownames(installed.packages())]
if (length(a_installer) > 0) install.packages(a_installer)

# Charge tous les packages
lapply(packages, library, character.only = TRUE)

library(here)  # gestion des chemins depuis la racine du .Rproj

# Les scripts s'exécutent dans l'ordre
source(here("R", "fonctions.R"))    # 1. Fonctions utilitaires
source(here("R", "01_import.R"))    # 2. Import des données brutes
source(here("R", "02_nettoyage.R")) # 3. Nettoyage et sauvegarde
source(here("R", "03_analyse.R"))   # 4. Analyses + outputs