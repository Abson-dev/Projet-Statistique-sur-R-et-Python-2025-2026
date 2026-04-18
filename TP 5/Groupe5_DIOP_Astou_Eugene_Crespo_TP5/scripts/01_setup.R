# =============================================================================
# TP5 - Cultures, Intrants et Rendements Agricoles
# Script 01 : Configuration - Packages et Chemins
# Données : Nigeria GHS Panel Wave 4 (2018/2019)
# =============================================================================

# --- 1. Installation et chargement des packages -----------------------------

packages <- c(
  "haven",      # Lecture fichiers .dta
  "dplyr",      # Manipulation données
  "tidyr",      # Restructuration données
  "ggplot2",    # Visualisations
  "forcats",    # Manipulation facteurs
  "scales",     # Formatage axes
  "patchwork",  # Combinaison graphiques
  "rstatix",    # Tests statistiques
  "ggpubr",     # Graphiques publication
  "gtsummary",  # Tableaux descriptifs
  "knitr",      # Tableaux kable
  "kableExtra", # Formatage tableaux
  "labelled",   # Gestion labels variables
  "stringr"     # Manipulation chaînes
)

for (pkg in packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg, repos = "https://cloud.r-project.org")
  }
  library(pkg, character.only = TRUE)
}

# --- 2. Définition des chemins -----------------------------------------------

# Chemin racine du projet (relatif au .Rproj)
ROOT     <- here::here()  # ou remplacer par le chemin absolu si nécessaire

DATA_DIR    <- file.path(ROOT, "data")
OUTPUT_DIR  <- file.path(ROOT, "outputs")
FIG_DIR     <- file.path(OUTPUT_DIR, "figures")
TAB_DIR     <- file.path(OUTPUT_DIR, "tables")
SCRIPTS_DIR <- file.path(ROOT, "scripts")

# Création automatique des dossiers outputs
dir.create(OUTPUT_DIR, showWarnings = FALSE, recursive = TRUE)
dir.create(FIG_DIR,    showWarnings = FALSE, recursive = TRUE)
dir.create(TAB_DIR,    showWarnings = FALSE, recursive = TRUE)

# --- 3. Paramètres graphiques globaux ----------------------------------------

theme_tp5 <- theme_minimal(base_size = 12) +
  theme(
    plot.title    = element_text(face = "bold", size = 13, hjust = 0.5),
    plot.subtitle = element_text(size = 10, hjust = 0.5, color = "grey40"),
    plot.caption  = element_text(size = 8, color = "grey50", hjust = 1),
    axis.title    = element_text(size = 10),
    legend.position = "bottom",
    legend.title  = element_text(size = 9),
    panel.grid.minor = element_blank()
  )

# Palette de couleurs cohérente
PALETTE <- c(
  "#2166AC", "#4DAC26", "#D01C8B", "#F1A340",
  "#92C5DE", "#B8E186", "#F7F7F7", "#E66101"
)

# --- 4. Fonction utilitaire : sauvegarde graphique ---------------------------

save_plot <- function(plot, filename, width = 10, height = 7, dpi = 300) {
  ggsave(
    filename = file.path(FIG_DIR, paste0(filename, ".png")),
    plot     = plot,
    width    = width,
    height   = height,
    dpi      = dpi,
    bg       = "white"
  )
  message("Figure sauvegardée : ", filename, ".png")
}

# --- 5. Fonction utilitaire : sauvegarde tableau -----------------------------

save_table <- function(df, filename) {
  write.csv(df, file = file.path(TAB_DIR, paste0(filename, ".csv")),
            row.names = FALSE, fileEncoding = "UTF-8")
  message("Tableau sauvegardé : ", filename, ".csv")
}

message("=== Setup TP5 chargé avec succès ===")
message("Dossier données    : ", DATA_DIR)
message("Dossier outputs    : ", OUTPUT_DIR)
