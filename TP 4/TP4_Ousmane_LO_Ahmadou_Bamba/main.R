#===============================================
# fichier principale pour l'exécution du projet
#===============================================

# 1. Définir les chemins des fichiers du projet
scripts_dir <- "script"
rapport_dir  <- "rapport"

# 2. Exécution des scripts de traitement
source(file.path(scripts_dir, "01_import.R"))
source(file.path(scripts_dir, "02_recodage.R"))
source(file.path(scripts_dir, "03_nettoyage.R"))
source(file.path(scripts_dir, "04_analyse.R"))

# 3. Génération du rapport Quarto
quarto::quarto_render(file.path(rapport_dir, "rapport_final.qmd"))

message("✅ Projet exécuté avec succès. Rapport final disponible dans le dossier 'rapport'.")


