#Les packages nécessaires
pckgs = c("haven", "dplyr", "ggplot2", "curl", "scales", "forcats", "tibble", "tidyverse", "rstatix", "boot")

#Les packages non disponibles
manquant = pckgs[!pckgs %in% rownames(installed.packages())]

#Installations des packages non disponibles
if (length(manquant) > 0){
  message("installation : ", paste(manquant, sep = ","))
  install.packages(manquant, repos = "https://cloud.r-project.org")
}

#Création des répertoires de sorties
dir.create("data/processed", showWarnings = FALSE, recursive = TRUE)
dir.create("data/row", showWarnings = FALSE, recursive = TRUE)
dir.create("outputs", showWarnings = FALSE, recursive = TRUE)

#Exécution des scripts

source("scripts/donnees.R")
source("scripts/Analyses.R")

cat("\n terminé !!!")