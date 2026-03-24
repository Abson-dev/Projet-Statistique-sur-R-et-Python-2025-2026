#importation des packages et creation des fonctions
packages <- c(
  "tidyverse",
  "readr",
  "stringr",
  "haven",
  "apyramid",
  "naniar",
  "gtsummary",
  "rstatix",
  "PropCIs",
  "patchwork",
  "dplyr",
  "forcats",
  "ggplot2",
  "ggpubr",
  "viridis",
  "patchwork",
  "dunn.test",
  "srvyr",
  "forcats",
  "scales"
  
)

# Installer les packages manquants
installed <- packages %in% rownames(installed.packages())

if (any(installed == FALSE)) {
  install.packages(packages[!installed])
}

# Charger les packages
invisible(lapply(packages, library, character.only = TRUE))


# details pour alleger le code.

data_raw <- "data/raw/"
data_processed <- "data/processed/"
output_figures <- "output/figures/"
output_tables <- "output/tables/"

# Créer les dossiers s'ils n'existent pas
dirs <- c(data_raw, data_processed, output_figures, output_tables)

for (d in dirs) {
  if (!dir.exists(d)) {
    dir.create(d, recursive = TRUE)
  }
}
