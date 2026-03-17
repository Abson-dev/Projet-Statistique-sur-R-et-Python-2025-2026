###############################################################################
# 00_fonctions.R -- Fonctions utilitaires, chemins et theme graphique
# Projet : TP3 -- Acces aux soins et depenses de sante (GHS Panel W4)
# Auteurs : David Landry AGNANGMA SANAM & Cheikh THIOUB
###############################################################################

# ---- Packages necessaires ----
packages <- c("haven", "dplyr", "ggplot2", "tidyr", "naniar", "gtsummary",
              "rstatix", "patchwork", "scales", "moments", "forcats",
              "knitr", "kableExtra", "flextable")

for (pkg in packages) {
  if (!requireNamespace(pkg, quietly = TRUE))
    install.packages(pkg, repos = "https://cran.r-project.org")
}

library(haven)
library(dplyr)
library(ggplot2)
library(tidyr)
library(naniar)
library(gtsummary)
library(rstatix)
library(patchwork)
library(scales)
library(moments)
library(forcats)

options(scipen = 999)

# ---- Chemins reproductibles (relatifs a la racine du projet TP3/) ----
chemin_data    <- "data"
chemin_outputs <- "outputs"
chemin_docs    <- "docs"
chemin_scripts <- "scripts"

dir.create(chemin_outputs, showWarnings = FALSE, recursive = TRUE)
dir.create(chemin_docs,    showWarnings = FALSE, recursive = TRUE)

# ---- Theme graphique commun ----
theme_tp3 <- theme_minimal(base_size = 12) +
  theme(
    plot.title    = element_text(face = "bold", size = 13, hjust = 0.5,
                                 color = "#2166AC"),
    plot.subtitle = element_text(size = 10, hjust = 0.5, color = "grey40"),
    axis.title    = element_text(size = 11),
    legend.position = "bottom"
  )
theme_set(theme_tp3)

# ---- Fonction : categoriser les types de maladies ----
categoriser_maladie <- function(code) {
  case_when(
    code %in% c(1, 3, 4, 5, 6, 7, 8, 9, 19, 20, 21, 22, 23) ~ "Infectieuse",
    code %in% c(13, 18, 24)                                    ~ "Chronique",
    code == 11                                                  ~ "Traumatique",
    code %in% c(10, 14, 15, 16, 17, 25, 26, 27)                ~ "Symptomatique",
    code == 2                                                   ~ "Infectieuse",
    code == 12                                                  ~ "Autre",
    TRUE                                                        ~ NA_character_
  )
}

# ---- Fonction : labelliser le type de prestataire consulte ----
labelliser_prestataire <- function(code) {
  case_when(
    code == 0  ~ "Personne",
    code == 1  ~ "Guerisseur traditionnel",
    code == 2  ~ "Medecin",
    code == 3  ~ "Dentiste",
    code == 4  ~ "Infirmier(e)",
    code == 5  ~ "Assistant medical",
    code == 6  ~ "Sage-femme",
    code == 7  ~ "Pharmacien",
    code == 8  ~ "Chimiste/Vendeur PMV",
    code == 9  ~ "Accoucheuse trad.",
    code == 10 ~ "Spiritualiste",
    code == 11 ~ "Vendeur brevete",
    code %in% c(14, 15) ~ "Agent de sante comm.",
    TRUE       ~ "Autre"
  )
}

# ---- Fonction : labelliser les types de maladies ----
labelliser_maladie <- function(code) {
  case_when(
    code == 1  ~ "Paludisme",
    code == 2  ~ "Tuberculose",
    code == 3  ~ "Fievre jaune",
    code == 4  ~ "Typhoide",
    code == 5  ~ "Cholera",
    code == 6  ~ "Diarrhee",
    code == 7  ~ "Meningite",
    code == 8  ~ "Varicelle",
    code == 9  ~ "Pneumonie",
    code == 10 ~ "Rhume",
    code == 11 ~ "Blessure",
    code == 13 ~ "Hypertension",
    code == 14 ~ "Grippe",
    code == 15 ~ "Rhinite/Catarrhe",
    code == 16 ~ "Toux",
    code == 17 ~ "Cephalees",
    code == 18 ~ "Diabete",
    code == 20 ~ "Dysenterie",
    code == 21 ~ "Gale",
    code == 22 ~ "Teigne",
    code == 23 ~ "Hepatite B",
    code == 24 ~ "Ulcere/Douleur abdo.",
    code == 25 ~ "Probleme oculaire",
    code == 26 ~ "Probleme dentaire",
    code == 27 ~ "Douleurs corporelles",
    TRUE       ~ "Autre"
  )
}

cat("[00_fonctions] Packages, chemins, theme et fonctions charges.\n")
