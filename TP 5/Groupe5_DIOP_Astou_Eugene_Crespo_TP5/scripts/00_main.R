# =============================================================================
# TP5 - Cultures, Intrants et Rendements Agricoles
# SCRIPT PRINCIPAL (main) — Lance tous les scripts dans l'ordre
# Nigeria GHS Panel Wave 4 (2018/2019)
# Auteur : [Ton prénom]
# Date   : Avril 2026
# =============================================================================
#
# STRUCTURE DU PROJET :
#   TP5/
#   ├── data/           ← bases .dta à placer ici
#   │     secta3i_harvestw4.dta
#   │     secta11c2_harvestw4.dta
#   │     secta11c3_harvestw4.dta
#   │     sect11f_plantingw4.dta
#   │     sect11e1_plantingw4.dta
#   │     sect11a1_plantingw4.dta
#   │     secta_harvestw4.dta
#   ├── outputs/        ← créé automatiquement
#   │     figures/      ← tous les graphiques .png
#   │     tables/       ← tous les tableaux .csv
#   ├── scripts/        ← tous les scripts R
#   │     00_main.R     ← CE FICHIER
#   │     01_setup.R
#   │     02_donnees.R
#   │     03_cultures.R
#   │     04_intrants_rendements.R
#   │     05_semences.R
#   └── docs/           ← rapport .Rmd
#
# COMMENT EXECUTER : ouvrir TP5.Rproj dans RStudio, puis lancer ce fichier.
# =============================================================================

# Vérifier que le package 'here' est disponible pour les chemins relatifs
if (!requireNamespace("here", quietly = TRUE)) install.packages("here")
library(here)

# Chemin vers les scripts
SCRIPTS <- here("scripts")

# =============================================================================
# ETAPE 1 : Configuration (packages + chemins + thème)
# =============================================================================
message("\n", strrep("=", 60))
message("ETAPE 1 : Configuration")
message(strrep("=", 60))
source(file.path(SCRIPTS, "01_setup.R"))

# =============================================================================
# ETAPE 2 : Chargement et nettoyage des données
# =============================================================================
message("\n", strrep("=", 60))
message("ETAPE 2 : Chargement et nettoyage des données")
message(strrep("=", 60))
source(file.path(SCRIPTS, "02_donnees.R"))

# =============================================================================
# ETAPE 3 : Tâches 25 & 26 — Cultures et diversification
# =============================================================================
message("\n", strrep("=", 60))
message("ETAPE 3 : Cultures et diversification (Tâches 25-26)")
message(strrep("=", 60))
source(file.path(SCRIPTS, "03_cultures.R"))

# =============================================================================
# ETAPE 4 : Tâches 27, 28, 29 — Intrants et rendements
# =============================================================================
message("\n", strrep("=", 60))
message("ETAPE 4 : Intrants et rendements (Tâches 27-28-29)")
message(strrep("=", 60))
source(file.path(SCRIPTS, "04_intrants_rendements.R"))

# =============================================================================
# ETAPE 5 : Tâche 30 — Semences améliorées
# =============================================================================
message("\n", strrep("=", 60))
message("ETAPE 5 : Semences améliorées (Tâche 30)")
message(strrep("=", 60))
source(file.path(SCRIPTS, "05_semences.R"))

# =============================================================================
# RÉSUMÉ FINAL
# =============================================================================
message("\n", strrep("=", 60))
message("EXECUTION TERMINÉE AVEC SUCCÈS")
message(strrep("=", 60))

# Lister tous les outputs produits
figs <- list.files(FIG_DIR, pattern = "\\.png$", full.names = FALSE)
tabs <- list.files(TAB_DIR, pattern = "\\.csv$", full.names = FALSE)

message("\n--- Figures produites (", length(figs), ") ---")
for (f in figs) message("  ", f)

message("\n--- Tableaux produits (", length(tabs), ") ---")
for (t in tabs) message("  ", t)

message("\nTous les outputs sont dans : ", OUTPUT_DIR)


