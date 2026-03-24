###############################################################################
# 00_fonctions.R -- Fonctions utilitaires, chemins et theme graphique
# Projet : TP3 -- Acces aux soins et depenses de sante (GHS Panel W4)
# Auteurs : David Landry AGNANGMA SANAM & Cheikh THIOUB
###############################################################################

packages <- c("haven", "dplyr", "ggplot2", "tidyr", "naniar", "rstatix",
              "patchwork", "scales", "moments", "forcats", "flextable",
              "officer", "survey")

for (pkg in packages) {
  if (!requireNamespace(pkg, quietly = TRUE))
    install.packages(pkg, repos = "https://cran.r-project.org")
}

library(haven); library(dplyr); library(ggplot2); library(tidyr)
library(naniar); library(rstatix); library(patchwork); library(scales)
library(moments); library(forcats); library(survey)

options(scipen = 999, survey.lonely.psu = "adjust")

chemin_data    <- "data"
chemin_outputs <- "outputs"
chemin_docs    <- "docs"

dir.create(chemin_outputs, showWarnings = FALSE, recursive = TRUE)
dir.create(chemin_docs,    showWarnings = FALSE, recursive = TRUE)

theme_tp3 <- theme_minimal(base_size = 12) +
  theme(
    plot.title    = element_text(face = "bold", size = 13, hjust = 0.5, color = "#2166AC"),
    plot.subtitle = element_text(size = 10, hjust = 0.5, color = "grey40"),
    axis.title    = element_text(size = 11),
    legend.position = "bottom"
  )
theme_set(theme_tp3)

# Categoriser les types de maladies
categoriser_maladie <- function(code) {
  case_when(
    code %in% c(1,2,3,4,5,6,7,8,9,19,20,21,22,23) ~ "Infectieuse",
    code %in% c(13,18,24)                            ~ "Chronique",
    code == 11                                        ~ "Traumatique",
    code %in% c(10,14,15,16,17,25,26,27)              ~ "Symptomatique",
    code == 12                                        ~ "Autre",
    TRUE                                              ~ NA_character_
  )
}

# Labelliser le type de maladie
labelliser_maladie <- function(code) {
  case_when(
    code==1~"Paludisme", code==2~"Tuberculose", code==3~"Fi\u00e8vre jaune",
    code==4~"Typho\u00efde", code==5~"Chol\u00e9ra", code==6~"Diarrh\u00e9e",
    code==7~"M\u00e9ningite", code==8~"Varicelle", code==9~"Pneumonie",
    code==10~"Rhume", code==11~"Blessure", code==13~"Hypertension",
    code==14~"Grippe", code==15~"Rhinite", code==16~"Toux",
    code==17~"C\u00e9phal\u00e9es", code==18~"Diab\u00e8te", code==20~"Dysenterie",
    code==24~"Ulc\u00e8re/Douleur abdo.", code==25~"Pb oculaire",
    code==26~"Pb dentaire", code==27~"Douleurs corporelles", TRUE~"Autre"
  )
}

# Labelliser le prestataire consulte
labelliser_prestataire <- function(code) {
  case_when(
    code==0~"Personne", code==1~"Gu\u00e9risseur trad.", code==2~"M\u00e9decin",
    code==3~"Dentiste", code==4~"Infirmier(e)", code==5~"Asst m\u00e9dical",
    code==6~"Sage-femme", code==7~"Pharmacien", code==8~"Chimiste/PMV",
    code==9~"Accoucheuse trad.", code==10~"Spiritualiste",
    code %in% c(14,15)~"Agent sant\u00e9 comm.", TRUE~"Autre"
  )
}

cat("[00_fonctions] Packages, chemins, theme et fonctions charges.\n")
