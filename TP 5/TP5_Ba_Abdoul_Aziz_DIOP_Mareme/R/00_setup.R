# ============================================================
# 00_setup.R — Configuration globale du projet TP5
# Nigeria GHS Panel W4 | ENSAE ISE1 2025-2026
# ============================================================

# Packages nécessaires pour tout le projet
library(haven)      # Lire les .dta
library(dplyr)      # Manipulation des données
library(ggplot2)    # Visualisations
library(forcats)    # Réordonner les facteurs dans les graphiques
library(scales)     # Formatage des axes (%)
library(rstatix)    # Tests statistiques (Wilcoxon, chi2, etc.)
library(gtsummary)  # Tableaux récapitulatifs
library(patchwork)  # Assembler plusieurs graphiques
library(ggpubr)     # Annotations de tests sur graphiques
library(survey)   # Base pour les plans de sondage
library(srvyr)    # Wrapper tidyverse de survey
library(conflicted)   # ← Gestionnaire de conflits
library(tidyr)
library(tidyverse)
library(labelled)

library(conflicted)

# Déclarer les préférences une fois pour tout le projet
conflict_prefer("select",    "dplyr")
conflict_prefer("filter",    "dplyr")
conflict_prefer("summarise", "dplyr")
conflict_prefer("mutate",    "dplyr")
conflict_prefer("rename",    "dplyr")
conflict_prefer("count",     "dplyr")


# Chemins (à adapter à ta machine)
path_raw  <- "data/raw/"
path_proc <- "data/processed/"
path_fig  <- "outputs/figures/"
path_tab  <- "outputs/tables/"

# Thème ggplot2 global (appliqué à tous les graphiques)
theme_set(theme_minimal(base_size = 13))
make_design <- function(data) {
  data %>%
    as_survey_design(
      ids     = ea,       # Grappes = Enumeration Areas
      strata  = sector,   # Strates = état × zone
      weights = wt_wave4, # Poids de sondage
      nest    = TRUE       # EA numérotées au sein de chaque strate
    )
}




# ── Poids depuis la bonne visite (Post-Planting) ─────────────
poids <- read_dta(paste0(path_raw, "secta_plantingw4.dta")) %>%
  dplyr::select(hhid, wt_wave4) %>%
  filter(!is.na(wt_wave4))

# ── Jointure ──────────────────────────────────────────────────
cultures <- read_dta(paste0(path_raw, "sect11f_plantingw4.dta")) %>%
  left_join(poids, by = "hhid") %>%
  filter(!is.na(wt_wave4))

# ── Vérifications ─────────────────────────────────────────────
sum(is.na(cultures$wt_wave4))  # Doit être 0
dim(cultures)                  # Nombre de lignes restantes

# ── Design ────────────────────────────────────────────────────
design_cultures <- make_design(cultures)
print(design_cultures)

# ── Sauvegarder ───────────────────────────────────────────────
saveRDS(cultures, paste0(path_proc, "cultures_w4.rds"))
