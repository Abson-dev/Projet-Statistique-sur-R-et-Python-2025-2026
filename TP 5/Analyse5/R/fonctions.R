# ==============================
# PACKAGES
# ==============================

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
  "moments",
  "gt",
  "htmltools",
  "here",
  "survey",
  "srvyr",
  "flextable",
  "coin"
)

# Installer les packages manquants
installed <- packages %in% rownames(installed.packages())

if (any(installed == FALSE)) {
  install.packages(packages[!installed])
}

# Charger les packages
invisible(lapply(packages, library, character.only = TRUE))


# ==============================
# STRUCTURE DES DOSSIERS
# ==============================

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
