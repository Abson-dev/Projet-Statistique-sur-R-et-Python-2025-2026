# =============================================================================
# main.R — Point d'entrée du projet TP5
# Thème 5 : Cultures pratiquées, intrants utilisés et rendements agricoles
# Nigeria GHS-Panel Wave 4 (2018/19)
#
# Auteurs      : Herman YAMAHA | Bourama DIALLO
# Cours        : Projet Statistique sous R et Python — ENSAE ISE 1, 2025-2026
# Superviseur  : M. Aboubacar HEMA
#
# ─────────────────────────────────────────────────────────────────────────────
# Arborescence :
#   data/raw/         — fichiers .dta téléchargés depuis GitHub
#   data/processed/   — objets .rds intermédiaires
#   outputs/          — figures PNG + tableau Excel
#   rapport/          — Rapport_TP5_cultures_rendements.Rmd → PDF
# =============================================================================

# ── Packages requis ───────────────────────────────────────────────────────────
packages_requis <- c(
  "haven",       # lecture des fichiers .dta Stata
  "dplyr",       # manipulation des données
  "tidyr",       # restructuration
  "forcats",     # réordonner les facteurs
  "ggplot2",     # visualisation
  "scales",      # formatage des axes
  "rstatix",     # tests statistiques (Kruskal-Wallis, Wilcoxon)
  "ggpubr",      # combinaison de graphiques
  "patchwork",   # layout multi-panneaux
  "survey",      # plan de sondage complexe
  "srvyr",       # interface dplyr pour survey
  "openxlsx",    # export Excel
  "purrr",       # programmation fonctionnelle
  "stringr",     # manipulation de chaînes (str_wrap)
  "knitr",       # compilation du rapport
  "kableExtra",  # tableaux LaTeX dans le rapport PDF
  "xfun"         # utilitaires knitr
)

a_installer <- packages_requis[!packages_requis %in% rownames(installed.packages())]
if (length(a_installer) > 0) {
  message("Installation des packages manquants : ", paste(a_installer, collapse=", "))
  install.packages(a_installer, repos="https://cloud.r-project.org")
}

# ── Création des dossiers de sortie ──────────────────────────────────────────
dir.create("data/raw",       showWarnings=FALSE, recursive=TRUE)
dir.create("data/processed", showWarnings=FALSE, recursive=TRUE)
dir.create("outputs",        showWarnings=FALSE, recursive=TRUE)

# ── Étape 1 : Import, superficies GPS et préparation des données ─────────────
cat("\n========================================\n")
cat("  ÉTAPE 1 : Import et nettoyage\n")
cat("========================================\n")
source("scripts/01_import_nettoyage.R")

# ── Étape 2 : Analyses descriptives et graphiques ────────────────────────────
cat("\n========================================\n")
cat("  ÉTAPE 2 : Analyses et visualisations\n")
cat("========================================\n")
source("scripts/02_analyses_graphiques.R")

# ── Étape 3 (optionnelle) : Compilation du rapport PDF ───────────────────────
# Nécessite une installation LaTeX (TinyTeX recommandé : tinytex::install_tinytex())
# Pour compiler manuellement : ouvrir le .Rmd dans RStudio et cliquer sur Knit -> PDF
cat("\n========================================\n")
cat("  ÉTAPE 3 : Rapport PDF (optionnel)\n")
cat("========================================\n")
if (requireNamespace("rmarkdown", quietly=TRUE)) {
  tryCatch({
    rmarkdown::render(
      "rapport/Rapport_TP5_cultures_rendements.Rmd",
      output_format = "pdf_document",
      quiet = TRUE
    )
    cat("  \u2713 Rapport PDF généré : rapport/Rapport_TP5_cultures_rendements.pdf\n")
  }, error = function(e) {
    cat("  \u2717 Compilation PDF échouée (LaTeX requis) :", conditionMessage(e), "\n")
    cat("  -> Ouvrir le .Rmd dans RStudio et cliquer sur Knit -> PDF Document\n")
  })
} else {
  cat("  -> rmarkdown non installé. Installer avec : install.packages('rmarkdown')\n")
}

# ── Récapitulatif des sorties ─────────────────────────────────────────────────
cat("\n========================================\n")
cat("  PROJET TERMINÉ\n")
cat("========================================\n")
cat("Figures exportées dans outputs/ :\n")
for (f in list.files("outputs/", full.names=FALSE)) cat("  ->", f, "\n")
cat("\nRapport à compiler :\n")
cat("  -> rapport/Rapport_TP5_cultures_rendements.Rmd\n")
cat("  -> Ouvrir dans RStudio, cliquer sur Knit -> PDF Document\n")
