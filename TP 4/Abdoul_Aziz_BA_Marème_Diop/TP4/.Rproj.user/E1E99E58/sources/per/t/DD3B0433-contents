# ============================================================
#  01_import.R — Chargement des données brutes
#  GHS Nigeria — Wave 4
# ============================================================

library(haven)
library(dplyr)
library(ggplot2)
library(survey)
library(scales)
library(patchwork)
library(viridis)

# Fichier principal des parcelles agricoles
parcelles_brut <- read_dta("data/raw/sect11a1_plantingw4.dta")

# Fichier tenure foncière
tenure_brut <- read_dta("data/raw/sect11b1_plantingw4.dta")

# Fichier de couverture — contient les poids de sondage
couverture <- read_dta("data/raw/secta_harvestw4.dta")

# Visualisation
glimpse(parcelles_brut)
glimpse(tenure_brut)
glimpse(couverture )
# parcelles_brut : 11 076 lignes × 31 colonnes
# tenure_brut    : 11 076 lignes
# couverture     : contient wt_wave4, wt_longpanel, strata, ea