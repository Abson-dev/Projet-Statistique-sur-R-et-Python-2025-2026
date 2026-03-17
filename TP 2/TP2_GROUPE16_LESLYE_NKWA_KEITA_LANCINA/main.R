# ╔═════════════════════════════════════════════════════════════════════════════╗
# ║                                                                             ║
# ║   ████████╗██████╗  ██████╗      ██████╗ ██╗  ██╗███████╗                   ║
# ║   ╚══██╔══╝██╔══██╗╚════██╗    ██╔════╝ ██║  ██║██╔════╝                    ║
# ║      ██║   ██████╔╝ █████╔╝    ██║  ███╗███████║███████╗                    ║
# ║      ██║   ██╔═══╝ ██╔═══╝     ██║   ██║██╔══██║╚════██║                    ║
# ║      ██║   ██║     ███████╗    ╚██████╔╝██║  ██║███████║                    ║
# ║      ╚═╝   ╚═╝     ╚══════╝     ╚═════╝ ╚═╝  ╚═╝╚══════╝                    ║
# ║                                                                             ║
# ║   Nigeria General Household Survey — Panel Wave 4 (2018-2019)               ║
# ║   Capital humain | Éducation et alphabétisation                             ║
# ║                                                                             ║
# ║   Auteurs : Leslye Patricia Nkwa Tsamo & Lancina KEITA                      ║
# ║   Données : NBS Nigeria / World Bank LSMS-ISA                               ║
# ║                                                                             ║
# ╚═════════════════════════════════════════════════════════════════════════════╝
#
# ┌────────────────────────────────────────────────────────────────────────────┐
# │  POURQUOI CE SCRIPT ?                                                      │
# │                                                                            │
# │  Ce script orchestre l'ensemble du projet TP2. Il centralise :             │
# │  1. L'installation et le chargement de TOUS les packages nécessaires       │
# │  2. La graine aléatoire unique (reproductibilité)                          │
# │  3. L'exécution séquentielle des scripts d'analyse                         │
# │                                                                            │
# │  Avantage : on n'a qu'un seul fichier à lancer (source("main.R"))          │
# │  et tous les scripts partagent le même environnement de packages.          │
# │  Aucun script individuel ne charge ni n'installe de packages.              │
# └────────────────────────────────────────────────────────────────────────────┘


# ═══════════════════════════════════════════════════════════════════════════════
# ETAPE 0 — Nettoyage de l'environnement
# ═══════════════════════════════════════════════════════════════════════════════

rm(list = ls())

cat("\n")
cat("╔═══════════════════════════════════════════════════════════════════╗\n")
cat("║                                                                 ║\n")
cat("║   TP2 GHS Nigeria — Lancement du pipeline d'analyse             ║\n")
cat("║   Patientez, on s'occupe de tout...                             ║\n")
cat("║                                                                 ║\n")
cat("╚═══════════════════════════════════════════════════════════════════╝\n\n")


# ═══════════════════════════════════════════════════════════════════════════════
# ETAPE 1 — Chargement des packages
# ═══════════════════════════════════════════════════════════════════════════════
# Ce projet utilise renv pour la gestion des dépendances.
# Si vous clonez ce dépôt pour la première fois, exécutez d'abord :
#   renv::restore()
# Cela installera automatiquement toutes les dépendances avec les versions
# exactes spécifiées dans renv.lock (reproductibilité garantie).

cat("┌─────────────────────────────────────────────────────────────────┐\n")
cat("│  ETAPE 1/3 — Chargement des packages                          │\n")
cat("└─────────────────────────────────────────────────────────────────┘\n\n")

# Vérification renv
if (!requireNamespace("renv", quietly = TRUE)) {
  cat("[!] renv non detecte. Installation...\n")
  install.packages("renv")
}

# Check si les packages sont installés (via renv::restore())
packages <- c(
  "tidyverse", "haven", "labelled", "here", "scales",
  "naniar", "patchwork",
  "moments", "rstatix", "coin", "PropCIs", "cards",
  "gtsummary", "gt"
)

packages_manquants <- packages[!packages %in% installed.packages()[,1]]

if (length(packages_manquants) > 0) {
  cat("\n[!] ATTENTION : Packages manquants detectes !\n")
  cat("   Packages manquants :", paste(packages_manquants, collapse = ", "), "\n\n")
  cat("   --> Veuillez executer : renv::restore()\n")
  cat("   --> Puis relancer : source('main.R')\n\n")
  stop("Environnement non initialise. Executez renv::restore() d'abord.")
}

# Chargement des packages (versions déjà installées par renv)
cat(sprintf("  %d packages à charger...\n\n", length(packages)))

for (pkg in packages) {
  library(pkg, character.only = TRUE)
  cat(sprintf("  + %s\n", pkg))
}

cat(sprintf("\n  ── %d/%d packages chargés ──\n\n", length(packages), length(packages)))


# ═══════════════════════════════════════════════════════════════════════════════
# ETAPE 2 — Graine aléatoire et constantes globales
# ═══════════════════════════════════════════════════════════════════════════════
# Une graine unique pour tout le projet garantit la reproductibilité :
# le même échantillon Shapiro-Wilk, les mêmes résultats à chaque exécution.

set.seed(2070)
cat("  Graine aléatoire fixée : set.seed(2070)\n")

# Caption standardisé utilisé sur toutes les figures
source_ghs <- paste0(
  "Source : Nigeria GHS Panel W4 (2018-2019), ",
  "NBS / World Bank LSMS-ISA. Calculs des auteurs."
)
cat("  Caption standard défini\n\n")


# ═══════════════════════════════════════════════════════════════════════════════
# ETAPE 3 — Exécution séquentielle des scripts
# ═══════════════════════════════════════════════════════════════════════════════
# source() exécute chaque script dans l'environnement global courant :
# les objets créés par 01_nettoyage.R sont disponibles pour 02_analyses.R.
# encoding = "UTF-8" assure la bonne lecture des accents sous Windows.

# ── Script 01 : Nettoyage ────────────────────────────────────────────────────
cat("┌─────────────────────────────────────────────────────────────────┐\n")
cat("│  ETAPE 2/3 — Exécution de 01_nettoyage.R                      │\n")
cat("│  Import, exploration, nettoyage, construction des variables    │\n")
cat("└─────────────────────────────────────────────────────────────────┘\n\n")

t1 <- Sys.time()
source(here("scripts", "01_nettoyage.R"), encoding = "UTF-8")
t1_fin <- round(difftime(Sys.time(), t1, units = "secs"), 1)

cat(sprintf("\n  ── 01_nettoyage.R terminé en %s secondes ──\n\n", t1_fin))


# ── Script 02 : Analyses ─────────────────────────────────────────────────────
cat("┌─────────────────────────────────────────────────────────────────┐\n")
cat("│  ETAPE 3/3 — Exécution de 02_analyses.R                       │\n")
cat("│  Analyses éducatives, tests statistiques, visualisations       │\n")
cat("└─────────────────────────────────────────────────────────────────┘\n\n")

t2 <- Sys.time()
source(here("scripts", "02_analyses.R"), encoding = "UTF-8")
t2_fin <- round(difftime(Sys.time(), t2, units = "secs"), 1)

cat(sprintf("\n  ── 02_analyses.R terminé en %s secondes ──\n\n", t2_fin))


# ═══════════════════════════════════════════════════════════════════════════════
# RÉSUMÉ FINAL
# ═══════════════════════════════════════════════════════════════════════════════

t_total <- round(as.numeric(t1_fin) + as.numeric(t2_fin), 1)

cat("╔═══════════════════════════════════════════════════════════════════╗\n")
cat("║                                                                 ║\n")
cat("║   *  Pipeline TP2 execute avec succes                          ║\n")
cat("║                                                                 ║\n")
cat(sprintf("║   -  Nettoyage    : %5s sec                                  ║\n", t1_fin))
cat(sprintf("║   -  Analyses     : %5s sec                                  ║\n", t2_fin))
cat(sprintf("║   -  Duree totale : %5s sec                                  ║\n", t_total))
cat("║                                                                 ║\n")
cat("║   ┌───────────────────────────────────────────────────────┐     ║\n")
cat("║   │  Livrables produits :                                 │     ║\n")
cat("║   │                                                       │     ║\n")
cat("║   │  data/processed/df_educ_clean.rds                     │     ║\n")
cat("║   │  outputs/figures/fig00_diagnostic_NA_educ.png         │     ║\n")
cat("║   │  outputs/figures/fig01_niveau_educ_distribution.png   │     ║\n")
cat("║   │  outputs/figures/fig02_niveau_educ_sexe.png           │     ║\n")
cat("║   │  outputs/figures/fig03_age_niveau_educ.png            │     ║\n")
cat("║   │  outputs/figures/fig04_scolarisation_milieu.png       │     ║\n")
cat("║   │  outputs/figures/fig05_heatmap_analphabetisme_etats.png│   ║\n")
cat("║   │  outputs/tables/tab01_gtsummary_education.html        │     ║\n")
cat("║   │  outputs/tables/tab01_gtsummary_education.csv         │     ║\n")
cat("║   │  outputs/tables/tab02_top10_analphabetisme.csv        │     ║\n")
cat("║   └───────────────────────────────────────────────────────┘     ║\n")
cat("║                                                                 ║\n")
cat("║   Merci d'avoir patienté — on espère que le résultat            ║\n")
cat("║   est à la hauteur. Bonne lecture !                             ║\n")
cat("║                                                                 ║\n")
cat("║             L.P. Nkwa Tsamo & L. KEITA  -  2025-2026           ║\n")
cat("║                                                                 ║\n")
cat("╚═══════════════════════════════════════════════════════════════════╝\n")
