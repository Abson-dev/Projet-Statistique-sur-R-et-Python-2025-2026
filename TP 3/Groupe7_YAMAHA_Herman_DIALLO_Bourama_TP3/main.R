# =============================================================================
# PROJET STATISTIQUE SOUS R — ENSAE ISE 1, 2025-2026
# Analyse 3 : Accès aux services de santé et chocs sanitaires des ménages
# Sections : sect4a / sect3b [Ménage-Post-Harvest, Wave 4]
# Données  : Nigeria General Household Survey (GHS) Panel — Wave 4 (2018)
#
# Auteurs  : Groupe 7 — Herman YAMAHA | Bourama DIALLO
#            - Herman YAMAHA
#            - Bourama DIALLO
#
# Année académique  : 2025-2026
# =============================================================================
#
# STRUCTURE DU PROJET
# ───────────────────
# main.R                         ← ce fichier (point d'entrée)
# scripts/
#   01_morbidite_sexe_age.R      ← Tâche 13 : taux de morbidité
#   02_types_maladies.R          ← Tâche 14 : types de maladies
#   03_recours_soins.R           ← Tâche 15 : recours aux prestataires
#   04_depenses_sante.R          ← Tâche 16 : distribution dépenses
#   05_test_independance.R       ← Tâche 17 : chi-deux × quintile
#   06_violin_rural_urbain.R     ← Tâche 18 : violin plot rural/urbain
# data/

# rapport/
#   rapport_analyse3.Rmd         ← rapport R Markdown complet
# outputs/
#   figures/                     ← graphiques PNG produits
#   tables/                      ← tableaux CSV produits
#
# NOTE SUR LES DONNÉES
# ────────────────────
# Dans le GHS Panel Wave 4 (2018), les variables de santé se trouvent dans
# sect4a_harvestw4.dta (et non sect3a comme indiqué dans certaines docs) :
#   s4aq3     = maladie/blessure dans les 4 dernières semaines
#   s4aq3b_1  = type d'affection principale
#   s4aq1     = consultation d'un praticien de santé
#   s4aq6a    = type de praticien consulté
#   s4aq9     = dépense consultation (Naira)
#   s4aq14    = dépense médicaments (Naira)
#   s4aq17    = dépense hospitalisation (Naira)
# =============================================================================

# ── 0. Packages requis ───────────────────────────────────────────────────────

packages_requis <- c(
  "haven", "dplyr", "ggplot2", "forcats", "scales",
  "naniar", "rstatix", "ggpubr", "patchwork",
  "gtsummary", "tidyr"
)

packages_manquants <- packages_requis[
  !sapply(packages_requis, requireNamespace, quietly = TRUE)
]

if (length(packages_manquants) > 0) {
  cat("Installation des packages manquants :\n")
  cat(paste(packages_manquants, collapse = ", "), "\n")
  install.packages(packages_manquants,
                   repos = "https://cran.rstudio.com/")
}

invisible(lapply(packages_requis, library, character.only = TRUE))
cat("✓ Tous les packages chargés.\n\n")

# ── 1. Répertoire de travail -------------------------------------------------
# S'assurer que le répertoire courant est la racine du projet
# (là où se trouve ce fichier main.R)

cat("Répertoire de travail :", getwd(), "\n\n")

# ── 2. Création des dossiers de sortie si absents ----------------------------

dirs <- c("outputs/figures", "outputs/tables", "data")
for (d in dirs) {
  if (!dir.exists(d)) {
    dir.create(d, recursive = TRUE)
    cat("Dossier créé :", d, "\n")
  }
}
# Téléchargement des données depuis GitHub (si absentes)

cat("Vérification / téléchargement des données depuis GitHub...\n")

# URL de base (raw GitHub)
base_url <- paste0(
  "https://raw.githubusercontent.com/",
  "Herman-YAMAHA/NYHP/",
  "25c41fb80cf9fe368c5b5a52140b794b9aef447e/",
  "TP3_raw/"
)

# Fichiers à récupérer
fichiers <- c(
  "sect4a_harvestw4.dta",
  "sect1_harvestw4.dta",
  "secta_harvestw4.dta",
  "totcons_final.dta"
)

# Dossier cible
dir.create("data/raw", recursive = TRUE, showWarnings = FALSE)

for (f in fichiers) {
  dest <- file.path("data/raw", f)
  
  if (!file.exists(dest)) {
    url <- paste0(base_url, f)
    cat("  Téléchargement :", f, "...\n")
    
    tryCatch(
      {
        download.file(url, destfile = dest, mode = "wb")
        cat("  ✓", f, "téléchargé.\n")
      },
      error = function(e) {
        cat("  ✗ Erreur pour", f, ":", conditionMessage(e), "\n")
      }
    )
    
  } else {
    cat("  ✓", f, "déjà présent.\n")
  }
}

cat("Téléchargement terminé.\n\n")

# ── 3. Vérification des données brutes ---------------------------------------

fichiers_requis <- c(
  "data/raw/sect4a_harvestw4.dta",
  "data/raw/sect1_harvestw4.dta",
  "data/raw/secta_harvestw4.dta",
  "data/raw/totcons_final.dta"
)

manquants <- fichiers_requis[!file.exists(fichiers_requis)]
if (length(manquants) > 0) {
  stop("Fichiers de données manquants :\n",
       paste(manquants, collapse = "\n"),
       "\nVeuillez placer les fichiers .dta dans data/raw/")
}
cat("✓ Tous les fichiers de données trouvés.\n\n")

# ── 4. Exécution des scripts -------------------------------------------------

scripts <- list(
  list(fichier = "scripts/01_morbidite_sexe_age.R",
       desc    = "Tâche 13 — Taux de morbidité par sexe et âge"),
  list(fichier = "scripts/02_types_maladies.R",
       desc    = "Tâche 14 — Types de maladies déclarées"),
  list(fichier = "scripts/03_recours_soins.R",
       desc    = "Tâche 15 — Recours aux soins par prestataire"),
  list(fichier = "scripts/04_depenses_sante.R",
       desc    = "Tâche 16 — Distribution des dépenses de santé"),
  list(fichier = "scripts/05_test_independance.R",
       desc    = "Tâche 17 — Test chi-deux recours × quintile"),
  list(fichier = "scripts/06_violin_rural_urbain.R",
       desc    = "Tâche 18 — Violin plots dépenses rural/urbain")
)

for (s in scripts) {
  cat("─────────────────────────────────────────────\n")
  cat(s$desc, "\n")
  cat("  Fichier :", s$fichier, "\n")
  tryCatch(
    {
      source(s$fichier, encoding = "UTF-8")
      cat("  Terminé avec succès\n\n")
    },
    error = function(e) {
      cat("  ERREUR :", conditionMessage(e), "\n\n")
    }
  )
}

# ── 5. Résumé des sorties ----------------------------------------------------

cat("═══════════════════════════════════════════════\n")
cat("RÉSUMÉ DES SORTIES PRODUITES\n")
cat("═══════════════════════════════════════════════\n")

figures <- list.files("outputs/figures", pattern = "\\.png$",
                      full.names = FALSE)
tables  <- list.files("outputs/tables",  pattern = "\\.csv$",
                      full.names = FALSE)

cat("\n Figures (", length(figures), ") :\n")
for (f in figures) cat("  -", f, "\n")

cat("\n Tableaux (", length(tables), ") :\n")
for (t in tables) cat("  -", t, "\n")

cat("\n Analyse 3 complète.\n")
cat(" Pour le rapport d'analyse, compiler : rapport/rapport_court.Rmd \n")
