# =============================================================
# fonctions.R — Fonctions utilitaires réutilisables
# =============================================================
library(here)        # gestion des chemins depuis la racine
library(haven)       # lecture des fichiers .dta
library(dplyr)       # manipulation des données
library(ggplot2)     # graphiques
library(naniar)      # valeurs manquantes
library(apyramid)    # pyramide des âges
library(gtsummary)   # tableaux récapitulatifs
library(rstatix)     # tests statistiques
library(scales)      # formatage des axes
library(forcats)     # recodage des facteurs
library(moments)     # asymétrie, kurtosis
library(patchwork)   # assembler plusieurs graphiques

# --- Fonction 1 : Sauvegarder un graphique ggplot ----------
sauvegarder_fig <- function(plot, nom_fichier, largeur = 10, hauteur = 7) {
  chemin <- here("output", "figures", nom_fichier)
  ggsave(chemin, plot = plot, width = largeur, height = hauteur,
         dpi = 300, bg = "white")
  message("Figure sauvegardée : ", nom_fichier)
}

# --- Fonction 2 : Sauvegarder un tableau (data.frame/tibble)
sauvegarder_tab <- function(tableau, nom_fichier) {
  chemin <- here("output", "tables", nom_fichier)
  write.csv(tableau, chemin, row.names = FALSE)
  message("Tableau sauvegardé : ", nom_fichier)
}

# --- Fonction 3 : Charger un fichier .dta d'une vague ------
charger_dta <- function(prefixe, vague) {
  nom_fichier <- paste0(prefixe, "w", vague, ".dta")
  chemin      <- here("data", "raw", paste0("w", vague), nom_fichier)
  haven::read_dta(chemin)
}