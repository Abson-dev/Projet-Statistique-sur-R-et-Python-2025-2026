###############################################################################
# 00_fonctions.R — Fonctions utilitaires, chemins et thème graphique
# Projet : TP1 — Profil démographique des ménages nigérians
# Auteurs : David Landry AGNANGMA SANAM & Cheikh THIOUB
###############################################################################

# ---- Packages nécessaires ----
packages <- c("haven", "dplyr", "ggplot2", "tidyr", "naniar", "gtsummary",
              "rstatix", "PropCIs", "patchwork", "scales", "moments",
              "forcats", "knitr", "kableExtra")

for (pkg in packages) {
  if (!requireNamespace(pkg, quietly = TRUE))
    install.packages(pkg, repos = "https://cran.r-project.org")
}

library(haven); library(dplyr); library(ggplot2); library(tidyr)
library(naniar); library(gtsummary); library(rstatix); library(PropCIs)
library(patchwork); library(scales); library(moments); library(forcats)

options(scipen = 999)

# ---- Chemins reproductibles (relatifs à la racine du projet) ----
chemin_data    <- "data"
chemin_outputs <- "outputs"
chemin_docs    <- "docs"
chemin_scripts <- "scripts"

dir.create(chemin_outputs, showWarnings = FALSE, recursive = TRUE)
dir.create(chemin_docs,    showWarnings = FALSE, recursive = TRUE)

# ---- Thème graphique commun ----
theme_tp1 <- theme_minimal(base_size = 12) +
  theme(
    plot.title    = element_text(face = "bold", size = 13, hjust = 0.5,
                                 color = "#2166AC"),
    plot.subtitle = element_text(size = 10, hjust = 0.5, color = "grey40"),
    axis.title    = element_text(size = 11),
    legend.position = "bottom"
  )
theme_set(theme_tp1)

# ---- Fonction : charger une vague sect1_harvest ----
charger_vague <- function(wave_num, chemin = chemin_data) {
  df <- read_dta(file.path(chemin, paste0("sect1_harvestw", wave_num, ".dta")))
  df %>%
    select(hhid, indiv, s1q2, s1q3, s1q4, zone, state, sector) %>%
    mutate(
      vague = paste0("W", wave_num),
      age   = as.numeric(as.character(s1q4)),
      sexe  = factor(s1q2, levels = c(1, 2), labels = c("Homme", "Femme"))
    )
}

cat("[00_fonctions] Fonctions, chemins et thème chargés.\n")
