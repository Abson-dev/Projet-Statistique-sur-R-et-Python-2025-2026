# =============================================================================
# TP4 - Parcelles agricoles : superficie, tenure foncière, utilisation des terres
# Script 01 : Configuration — Packages, chemins, thème
# Données : Nigeria GHS Panel Wave 4 (2018/2019)
# =============================================================================

# --- 1. Packages -------------------------------------------------------------
packages <- c(
  "haven",      # Lecture .dta
  "dplyr",      # Manipulation données
  "tidyr",      # Restructuration
  "ggplot2",    # Visualisations
  "forcats",    # Facteurs
  "scales",     # Formatage axes
  "patchwork",  # Combinaison graphiques
  "rstatix",    # Tests statistiques
  "ggpubr",     # Graphiques publication
  "gtsummary",  # Tableaux descriptifs
  "knitr",      # Tableaux
  "kableExtra", # Formatage tableaux
  "labelled",   # Labels variables
  "stringr",    # Chaînes
  "viridis",    # Palette heatmap
  "here"        # Chemins relatifs
)

for (pkg in packages) {
  if (!requireNamespace(pkg, quietly = TRUE))
    install.packages(pkg, repos = "https://cloud.r-project.org")
  library(pkg, character.only = TRUE)
}

# --- 2. Chemins --------------------------------------------------------------
ROOT       <- here::here()
DATA_DIR   <- file.path(ROOT, "data")
OUTPUT_DIR <- file.path(ROOT, "outputs")
FIG_DIR    <- file.path(OUTPUT_DIR, "figures")
TAB_DIR    <- file.path(OUTPUT_DIR, "tables")
SCRIPTS    <- file.path(ROOT, "scripts")

dir.create(FIG_DIR, showWarnings = FALSE, recursive = TRUE)
dir.create(TAB_DIR, showWarnings = FALSE, recursive = TRUE)

# --- 3. Table noms des États nigérians ---------------------------------------
etats_nigeria <- c(
  "1"  = "Abia",        "2"  = "Adamawa",    "3"  = "Akwa Ibom",
  "4"  = "Anambra",     "5"  = "Bauchi",      "6"  = "Bayelsa",
  "7"  = "Benue",       "8"  = "Borno",       "9"  = "Cross River",
  "10" = "Delta",       "11" = "Ebonyi",      "12" = "Edo",
  "13" = "Ekiti",       "14" = "Enugu",       "15" = "FCT Abuja",
  "16" = "Gombe",       "17" = "Imo",         "18" = "Jigawa",
  "19" = "Kaduna",      "20" = "Kano",        "21" = "Katsina",
  "22" = "Kebbi",       "23" = "Kogi",        "24" = "Kwara",
  "25" = "Lagos",       "26" = "Nasarawa",    "27" = "Niger",
  "28" = "Ogun",        "29" = "Ondo",        "30" = "Osun",
  "31" = "Oyo",         "32" = "Plateau",     "33" = "Rivers",
  "34" = "Sokoto",      "35" = "Taraba",      "36" = "Yobe",
  "37" = "Zamfara"
)

# --- 4. Thème graphique ------------------------------------------------------
theme_tp4 <- theme_minimal(base_size = 12) +
  theme(
    plot.title    = element_text(face = "bold", size = 13, hjust = 0.5),
    plot.subtitle = element_text(size = 10, hjust = 0.5, color = "grey40"),
    plot.caption  = element_text(size = 8, color = "grey50", hjust = 1),
    axis.title    = element_text(size = 10),
    legend.position  = "bottom",
    legend.title     = element_text(size = 9),
    panel.grid.minor = element_blank()
  )

# --- 5. Fonctions utilitaires ------------------------------------------------
save_plot <- function(plot, filename, width = 10, height = 7, dpi = 300) {
  ggsave(
    filename = file.path(FIG_DIR, paste0(filename, ".png")),
    plot = plot, width = width, height = height, dpi = dpi, bg = "white"
  )
  message("Figure sauvegardée : ", filename, ".png")
}

save_table <- function(df, filename) {
  write.csv(df, file = file.path(TAB_DIR, paste0(filename, ".csv")),
            row.names = FALSE, fileEncoding = "UTF-8")
  message("Tableau sauvegardé : ", filename, ".csv")
}

message("=== Setup TP4 chargé avec succès ===")
