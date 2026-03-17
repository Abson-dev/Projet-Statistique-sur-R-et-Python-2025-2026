# Script principal pour exécuter toute la pipeline

# Restaurer environnement
source("setup.R")

# Charger package pour chemins relatifs
library(here)
library(rmarkdown)

# Exécuter tous les scripts dans l'ordre
source(here("scripts", "01_import_data.R"))
source(here("scripts", "02_join_clean.R"))
source(here("scripts", "03_construction_variables.R"))
source(here("scripts", "04_descriptives_analysis.R"))
# source(here("scripts", "05_econometric_model.R"))

# Rendu automatique du notebook en Word
 render(
  input = here("rapports", "Rapport_TP2.Rmd"),
  output_format = "word_document",
  output_file = here("rapports", "Rapport_TP2.docx"),
  envir = globalenv()  # permet d'utiliser toutes les données déjà chargées
)
