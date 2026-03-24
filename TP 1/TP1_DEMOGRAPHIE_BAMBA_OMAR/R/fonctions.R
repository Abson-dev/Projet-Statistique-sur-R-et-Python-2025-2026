library(here)
library(haven)
library(dplyr)
library(ggplot2)
library(naniar)
library(apyramid)
library(gtsummary)
library(rstatix)
library(scales)
library(forcats)
library(moments)
library(patchwork)
library(survey)
library(srvyr)
library(Hmisc)

sauvegarder_fig <- function(plot, nom_fichier, largeur = 10, hauteur = 7) {
  chemin <- here("output", "figures", nom_fichier)
  ggsave(chemin, plot = plot, width = largeur, height = hauteur,
         dpi = 300, bg = "white")
  message("Figure sauvegardée : ", nom_fichier)
}

sauvegarder_tab <- function(tableau, nom_fichier) {
  chemin <- here("output", "tables", nom_fichier)
  write.csv(tableau, chemin, row.names = FALSE)
  message("Tableau sauvegardé : ", nom_fichier)
}

charger_dta <- function(prefixe, vague) {
  nom_fichier <- paste0(prefixe, "w", vague, ".dta")
  chemin      <- here("data", "raw", paste0("w", vague), nom_fichier)
  haven::read_dta(chemin)
}
