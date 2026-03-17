# =============================================================================
# main.R – Script principal du projet
# Thème 2 : Éducation et alphabétisation des membres des ménages
# Enquête GHS Nigeria 2018 (Wave 4)
# Réalisé par : Herman YAMAHA, Bourama DIALLO 
# =============================================================================



# ---- Vérification et installation des dépendances ----
packages_necessaires <- c("haven","dplyr","forcats","ggplot2","rstatix",
                          "ggpubr","gtsummary","viridis","patchwork","scales",
                          "knitr","kableExtra")

a_installer <- packages_necessaires[!packages_necessaires %in% installed.packages()[,"Package"]]
if (length(a_installer) > 0) {
  message("Installation des packages manquants : ", paste(a_installer, collapse = ", "))
  install.packages(a_installer, repos = "https://cloud.r-project.org")
}

# ---- Création des dossiers de sortie ----
dir.create("data/processed", showWarnings = FALSE, recursive = TRUE)
dir.create("outputs",        showWarnings = FALSE, recursive = TRUE)

# ---- Phase 1 : Import et nettoyage des données ----
cat("=== PHASE 1 : Import et préparation des données ===\n")
source("scripts/01_import_nettoyage.R")

# ---- Phase 2 : Analyses descriptives et graphiques ----
cat("\n=== PHASE 2 : Analyses descriptives et graphiques ===\n")
source("scripts/02_statistiques_graphiques.R")

cat("\n=== Traitement terminé avec succès ! ===\n")
cat("Figures enregistrées dans : outputs/\n")
cat("Rapport à compiler       : rapport/Rapport_educ_alphabetisation_menages.Rmd\n")
