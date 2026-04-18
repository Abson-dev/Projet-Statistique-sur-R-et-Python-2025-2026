# ==============================================================================
# TP4 — Analyse des parcelles agricoles
# Fichier : 01_import.R
# Objectif : Importation des bases brutes (Vague 4)
# Auteurs  : Boubacar KANE / Cheikh Omar TRAORE
# Date     : 2025 - 2026
# ==============================================================================

# ---------------------------------------------------------------------------- #
# 0. Librairies
# ---------------------------------------------------------------------------- #
if (!require("pacman")) install.packages("pacman")
library(pacman)
p_load(haven, dplyr, ggplot2, ggrepel, scales, patchwork,
       rstatix, gtsummary, viridis, sf, survey, flextable,
       rprojroot, here)

# ---------------------------------------------------------------------------- #
# 1. Racine du projet (compatible RStudio et Rmd)
# ---------------------------------------------------------------------------- #
proj_root <- rprojroot::find_rstudio_root_file()
setwd(proj_root)

# ---------------------------------------------------------------------------- #
# 2. Chargement des fichiers bruts
# ---------------------------------------------------------------------------- #

# Roster des parcelles — planting wave 4
# Variables clés : hhid, plotid, s11aq4aa (superficie), s11aq4b (unité),
#                  s11aq4c (superficie GPS en m²), zone, sector, state
sect11a1_p4 <- read_dta(file.path(proj_root, "data/raw/sect11a1_plantingw4.dta"))

# Tenure foncière — planting wave 4
# Variables clés : hhid, plotid, s11b1q4 (mode d'acquisition), sector
sect11b1_p4 <- read_dta(file.path(proj_root, "data/raw/sect11b1_plantingw4.dta"))

# Poids de sondage (wt_wave4) — harvest wave 4
secta_w4 <- read_dta(file.path(proj_root, "data/raw/secta_harvestw4.dta"))

# ---------------------------------------------------------------------------- #
# 3. Exploration rapide
# ---------------------------------------------------------------------------- #
cat("\n=== sect11a1_plantingw4 ===\n")
cat("Observations :", nrow(sect11a1_p4), "| Variables :", ncol(sect11a1_p4), "\n")
glimpse(sect11a1_p4)

cat("\n=== sect11b1_plantingw4 ===\n")
cat("Observations :", nrow(sect11b1_p4), "| Variables :", ncol(sect11b1_p4), "\n")

cat("\n=== secta_harvestw4 (poids) ===\n")
cat("Observations :", nrow(secta_w4), "| Variables :", ncol(secta_w4), "\n")

# ---------------------------------------------------------------------------- #
# 4. Vérification des identifiants
# ---------------------------------------------------------------------------- #
doublons_a1 <- sect11a1_p4 %>%
  group_by(hhid, plotid) %>%
  filter(n() > 1) %>%
  nrow()
cat("\nDoublons (hhid, plotid) dans sect11a1 :", doublons_a1, "\n")

doublons_b1 <- sect11b1_p4 %>%
  group_by(hhid, plotid) %>%
  filter(n() > 1) %>%
  nrow()
cat("Doublons (hhid, plotid) dans sect11b1 :", doublons_b1, "\n")

communs <- length(intersect(unique(sect11a1_p4$hhid), unique(secta_w4$hhid)))
cat("Ménages communs a1 ↔ secta :", communs, "\n")

# ---------------------------------------------------------------------------- #
# 5. Inspection des variables clés de superficie
# ---------------------------------------------------------------------------- #
cat("\n--- s11aq4aa (superficie agricole déclarée) ---\n")
cat("Label :", attr(sect11a1_p4$s11aq4aa, "label"), "\n")
summary(sect11a1_p4$s11aq4aa)

cat("\n--- s11aq4b (unité de mesure) ---\n")
cat("Label :", attr(sect11a1_p4$s11aq4b, "label"), "\n")
table(as_factor(sect11a1_p4$s11aq4b), useNA = "ifany")

cat("\n--- s11aq4c (superficie GPS en m²) ---\n")
cat("Label :", attr(sect11a1_p4$s11aq4c, "label"), "\n")
summary(sect11a1_p4$s11aq4c)

cat("\n--- s11b1q4 (mode d'acquisition de la parcelle) ---\n")
cat("Label :", attr(sect11b1_p4$s11b1q4, "label"), "\n")
table(sect11b1_p4$s11b1q4, useNA = "ifany")

cat("\n--- sector (milieu de résidence) ---\n")
table(sect11a1_p4$sector, useNA = "ifany")

