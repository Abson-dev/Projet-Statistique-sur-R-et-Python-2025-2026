# ╔═════════════════════════════════════════════════════════════════════════════╗
# ║                                                                             ║
# ║   ████████╗██████╗  ███████╗      ██████╗ ██╗  ██╗███████╗                 ║
# ║   ╚══██╔══╝██╔══██╗ ██╔════╝     ██╔════╝ ██║  ██║██╔════╝                 ║
# ║      ██║   ██████╔╝ ███████╗     ██║  ███╗███████║███████╗                 ║
# ║      ██║   ██╔═══╝  ╚════██║     ██║   ██║██╔══██║╚════██║                 ║
# ║      ██║   ██║      ███████║     ╚██████╔╝██║  ██║███████║                 ║
# ║      ╚═╝   ╚═╝      ╚══════╝      ╚═════╝ ╚═╝  ╚═╝╚══════╝                 ║
# ║                                                                             ║
# ║   Nigeria General Household Survey — Panel Wave 4 (2018-2019)               ║
# ║   Analyse 5 : Cultures pratiquées, intrants et rendements agricoles         ║
# ║                                                                             ║
# ║   Auteurs  : Nkwa T. Leslye & Ouattara Ousmane, ENSAE ISE1 2025-2026                     ║
# ║   Données  : NBS Nigeria / World Bank LSMS-ISA                              ║
# ║   Superviseur : M. Aboubacar HEMA, IFPRI                                   ║
# ║                                                                             ║
# ╚═════════════════════════════════════════════════════════════════════════════╝
#
# ┌────────────────────────────────────────────────────────────────────────────┐
# │  POURQUOI CE SCRIPT ?                                                      │
# │                                                                            │
# │  Ce script orchestre l'ensemble du projet TP5. Il centralise :             │
# │  1. Le chargement de TOUS les packages nécessaires                         │
# │  2. La graine aléatoire unique (reproductibilité)                          │
# │  3. La vérification de la structure des dossiers et des fichiers bruts     │
# │  4. L'exécution séquentielle des scripts d'analyse                         │
# │  5. Le rendu du rapport RMarkdown en Word                                  │
# │                                                                            │
# │  Avantage : un seul fichier à lancer (source("main.R"))                    │
# │  et tous les scripts partagent le même environnement de packages.          │
# └────────────────────────────────────────────────────────────────────────────┘


# ═══════════════════════════════════════════════════════════════════════════════
# ÉTAPE 0 — Nettoyage de l'environnement
# ═══════════════════════════════════════════════════════════════════════════════

rm(list = ls())

cat("\n")
cat("╔═══════════════════════════════════════════════════════════════════╗\n")
cat("║                                                                 ║\n")
cat("║   TP5 GHS Nigeria — Lancement du pipeline d'analyse             ║\n")
cat("║   Cultures, intrants et rendements agricoles                    ║\n")
cat("║   Patientez, on s'occupe de tout...                             ║\n")
cat("║                                                                 ║\n")
cat("╚═══════════════════════════════════════════════════════════════════╝\n\n")


# Vérification renv
if (!requireNamespace("renv", quietly = TRUE)) {
  cat("[!] renv non detecte. Installation...\n")
  install.packages("renv")
}

# ═══════════════════════════════════════════════════════════════════════════════
# ÉTAPE 1 — Chargement des packages
# ═══════════════════════════════════════════════════════════════════════════════

cat("┌─────────────────────────────────────────────────────────────────┐\n")
cat("│  ÉTAPE 1/4 — Chargement des packages                           │\n")
cat("└─────────────────────────────────────────────────────────────────┘\n\n")

packages <- c(
  # Données
  "haven",      # Lecture des fichiers .dta
  "labelled",   # Gestion et suppression des labels Stata (haven_labelled)
  # Manipulation
  "dplyr",      # Manipulation de données
  "tidyr",      # Restructuration (pivot_longer, etc.)
  "forcats",    # Manipulation des facteurs
  "stringr",    # Manipulation de chaînes de caractères
  # Visualisation
  "ggplot2",    # Graphiques
  "ggpubr",     # Compléments ggplot2 (stat_compare_means, etc.)
  "scales",     # Formatage des axes (percent_format, label_comma)
  "patchwork",  # Assemblage de figures
  # Tests statistiques
  "rstatix",    # Tests Wilcoxon, taille d'effet (wilcox_test, wilcox_effsize)
  # Tableaux
  "flextable",  # Tableaux Word formatés
  "officer",    # Bordures flextable (fp_border)
  "knitr",      # Intégration figures dans RMarkdown
  # Rapport
  "rmarkdown",  # Rendu du rapport .Rmd -> .docx
  # Chemins
  "here",        # Chemins relatifs reproductibles
  "coin"
)

packages_manquants <- packages[!packages %in% installed.packages()[, 1]]

if (length(packages_manquants) > 0) {
  cat("\n[!] ATTENTION : Packages manquants détectés !\n")
  cat("    Packages manquants :", paste(packages_manquants, collapse = ", "), "\n\n")
  cat("    --> Veuillez exécuter : install.packages(c('",
      paste(packages_manquants, collapse = "','"), "'))\n")
  cat("    --> Puis relancer : source('main.R')\n\n")
  stop("Environnement non initialisé. Installez les packages manquants.")
}

cat(sprintf("  %d packages à charger...\n\n", length(packages)))

for (pkg in packages) {
  suppressPackageStartupMessages(library(pkg, character.only = TRUE))
  cat(sprintf("  + %s\n", pkg))
}

cat(sprintf("\n  ── %d/%d packages chargés ──\n\n", length(packages), length(packages)))


# ═══════════════════════════════════════════════════════════════════════════════
# ÉTAPE 2 — Graine aléatoire, constantes globales et vérifications
# ═══════════════════════════════════════════════════════════════════════════════

cat("┌─────────────────────────────────────────────────────────────────┐\n")
cat("│  ÉTAPE 2/4 — Paramètres globaux et vérifications               │\n")
cat("└─────────────────────────────────────────────────────────────────┘\n\n")

set.seed(2025)
options(scipen = 999, OutDec = ",")
cat("  Graine aléatoire fixée : set.seed(2025)\n")

# Caption standard utilisé sur toutes les figures
source_ghs <- paste0(
  "Source : Nigeria GHS Panel W4 (2018-2019), ",
  "NBS / World Bank LSMS-ISA. Calculs des auteurs."
)
cat("  Caption standard défini\n")

# Vérification des dossiers de sortie
dirs_needed <- c(
  here("data", "raw"),
  here("data", "processed"),
  here("outputs", "figures"),
  here("outputs", "tables"),
  here("rapports")
)

for (d in dirs_needed) {
  if (!dir.exists(d)) {
    dir.create(d, recursive = TRUE)
    cat(sprintf("  [+] Dossier créé : %s\n", d))
  }
}
cat("  Structure des dossiers vérifiée\n")

# Vérification des fichiers bruts
raw_files <- c(
  "secta3i_harvestw4.dta",
  "secta3ii_harvestw4.dta",
  "secta11c2_harvestw4.dta",
  "sect11a1_plantingw4.dta",
  "secta_harvestw4.dta"
)

cat("\n  Vérification des fichiers bruts :\n")
for (f in raw_files) {
  path_f <- here("data", "raw", f)
  if (!file.exists(path_f)) {
    stop(sprintf("\n  [!] Fichier manquant : %s\n      Placez-le dans data/raw/", f))
  }
  cat(sprintf("  [OK] %s\n", f))
}
cat("\n")


# ═══════════════════════════════════════════════════════════════════════════════
# ÉTAPE 3 — Exécution séquentielle des scripts
# ═══════════════════════════════════════════════════════════════════════════════

# ── Script 01 : Préparation ──────────────────────────────────────────────────
cat("┌─────────────────────────────────────────────────────────────────┐\n")
cat("│  ÉTAPE 3/4 — Exécution de 01_preparation.R                     │\n")
cat("│  Import, nettoyage, jointures, construction des variables       │\n")
cat("└─────────────────────────────────────────────────────────────────┘\n\n")

# t1 <- Sys.time()
source(here("scripts", "01_preparation.R"), encoding = "UTF-8")
t1_fin <- round(difftime(Sys.time(), Sys.time(), units = "secs"), 1)

cat(sprintf("\n  ── 01_preparation.R terminé en %s secondes ──\n\n", t1_fin))


# ── Script 02 : Analyses ─────────────────────────────────────────────────────
cat("┌─────────────────────────────────────────────────────────────────┐\n")
cat("│  ÉTAPE 4/4 — Exécution de 02_analyses.R                        │\n")
cat("│  Tests statistiques, visualisations, tableaux                  │\n")
cat("└─────────────────────────────────────────────────────────────────┘\n\n")

t2 <- Sys.time()
source(here("scripts", "02_analyses.R"), encoding = "UTF-8")
t2_fin <- round(difftime(Sys.time(), t2, units = "secs"), 1)

cat(sprintf("\n  ── 02_analyses.R terminé en %s secondes ──\n\n", t2_fin))

# ═══════════════════════════════════════════════════════════════════════════════
# ÉTAPE 4 — Génération du rapport RMarkdown
# ═══════════════════════════════════════════════════════════════════════════════

cat("┌─────────────────────────────────────────────────────────────────┐\n")
cat("│  RENDU — Génération du rapport Word (rapport.Rmd -> .docx)     │\n")
cat("└─────────────────────────────────────────────────────────────────┘\n\n")

t3 <- Sys.time()

rapport_rmd <- here("rapports", "rapport.Rmd")
rapport_out <- here("rapports", "rapport.docx")

tryCatch({
  rmarkdown::render(
    input         = rapport_rmd,
    output_format = "word_document",
    output_file   = rapport_out,
    quiet         = FALSE
  )
  cat(sprintf("\n  [OK] Rapport généré : rapports/rapport.docx\n"))
}, error = function(e) {
  cat(sprintf("\n  [!] Erreur lors du rendu : %s\n", conditionMessage(e)))
})

t3_fin <- round(difftime(Sys.time(), t3, units = "secs"), 1)
cat(sprintf("\n  ── Rapport généré en %s secondes ──\n\n", t3_fin))


# ═══════════════════════════════════════════════════════════════════════════════
# RÉSUMÉ FINAL
# ═══════════════════════════════════════════════════════════════════════════════

t_total <- round(as.numeric(t1_fin) + as.numeric(t2_fin) + as.numeric(t3_fin), 1)

cat("╔═══════════════════════════════════════════════════════════════════╗\n")
cat("║                                                                 ║\n")
cat("║   *  Pipeline TP5 exécuté avec succès                          ║\n")
cat("║                                                                 ║\n")
cat(sprintf("║   -  Préparation  : %5s sec                                  ║\n", t1_fin))
cat(sprintf("║   -  Analyses     : %5s sec                                  ║\n", t2_fin))
cat(sprintf("║   -  Rapport      : %5s sec                                  ║\n", t3_fin))
cat(sprintf("║   -  Durée totale : %5s sec                                  ║\n", t_total))
cat("║                                                                 ║\n")
cat("║   ┌───────────────────────────────────────────────────────┐     ║\n")
cat("║   │  Livrables produits :                                 │     ║\n")
cat("║   │                                                       │     ║\n")
cat("║   │  data/processed/df_main.rds                           │     ║\n")
cat("║   │  data/processed/df_menage.rds                         │     ║\n")
cat("║   │  outputs/figures/fig01_top15_cultures.png             │     ║\n")
cat("║   │  outputs/figures/fig02_diversification.png            │     ║\n")
cat("║   │  outputs/figures/fig03_taux_engrais.png               │     ║\n")
cat("║   │  outputs/figures/fig04_rendements_etat.png            │     ║\n")
cat("║   │  outputs/figures/fig05_rdt_engrais.png                │     ║\n")
cat("║   │  outputs/figures/fig_synthese.png                     │     ║\n")
cat("║   │  rapports/rapport.docx                                │     ║\n")
cat("║   └───────────────────────────────────────────────────────┘     ║\n")
cat("║                                                                 ║\n")
cat("║   Bonne lecture !                                               ║\n")
cat("║                                                                 ║\n")
cat("║             [Votre Nom & Prénom]  —  ENSAE ISE1 2025-2026      ║\n")
cat("║                                                                 ║\n")
cat("╚═══════════════════════════════════════════════════════════════════╝\n")
