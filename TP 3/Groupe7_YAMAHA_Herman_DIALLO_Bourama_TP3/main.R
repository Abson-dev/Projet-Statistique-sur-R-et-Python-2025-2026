# =============================================================================
# PROJET STATISTIQUE SOUS R — ENSAE ISE 1, 2025-2026
# Analyse 3 : Accès aux services de santé et chocs sanitaires des ménages
# Nigeria General Household Survey (GHS) Panel — Wave 4 (2018)
#
# Auteurs     : Groupe 7 — Herman YAMAHA | Bourama DIALLO
# Superviseur : M. Aboubacar HEMA
# Année       : 2025-2026
#
# PONDÉRATIONS :
#   Toutes les analyses intègrent les poids transversaux wt_wave4
#   issus de secta_harvestw4.dta. Cela permet de généraliser les résultats
#   à la population nigériane et non seulement à l'échantillon.
#
# SORTIES :
#   outputs/figures/   graphiques PNG (11 figures)
#   outputs/tables/    tableaux CSV + Excel (tableau gtsummary pondéré)
#   rapport/rapport_court.Rmd → compiler en Word depuis RStudio
#
# NOTE SUR LES DONNÉES :
#   Dans le GHS Wave 4, les variables santé sont dans sect4a_harvestw4.dta.
#   Les fichiers sect4a, totcons_final et sect2 sont à placer dans data/raw/
#   (cf. section téléchargement ci-dessous).
# =============================================================================

pkgs <- c("haven", "dplyr", "ggplot2", "forcats", "scales",
          "naniar", "rstatix", "ggpubr", "patchwork",
          "gtsummary", "tidyr", "survey", "srvyr",
          "flextable", "openxlsx", "officedown", "officer")

manquants <- pkgs[!sapply(pkgs, requireNamespace, quietly = TRUE)]
if (length(manquants) > 0) {
  message("Installation : ", paste(manquants, collapse = ", "))
  install.packages(manquants, repos = "https://cran.rstudio.com/")
}
invisible(lapply(pkgs, library, character.only = TRUE))
cat("Packages chargés.\n\n")

# Répertoires
for (d in c("outputs/figures", "outputs/tables", "data/raw", "data/processed")) {
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
}

# Téléchargement des données depuis GitHub
base_url <- paste0(
  "https://raw.githubusercontent.com/Herman-YAMAHA/NYHP/",
  "25c41fb80cf9fe368c5b5a52140b794b9aef447e/TP3_raw/"
)
fichiers_dl <- c("sect4a_harvestw4.dta", "totcons_final.dta")

for (f in fichiers_dl) {
  dest <- file.path("data/raw", f)
  if (!file.exists(dest)) {
    cat("Téléchargement :", f, "...\n")
    tryCatch(
      download.file(paste0(base_url, f), destfile = dest, mode = "wb"),
      error = function(e) cat("Erreur pour", f, ":", conditionMessage(e), "\n")
    )
  } else {
    cat("Déjà présent :", f, "\n")
  }
}

# Vérification
requis <- c("data/raw/sect4a_harvestw4.dta", "data/raw/sect1_harvestw4.dta",
            "data/raw/secta_harvestw4.dta",  "data/raw/totcons_final.dta")
manq <- requis[!file.exists(requis)]
if (length(manq) > 0)
  stop("Fichiers manquants :\n", paste(manq, collapse="\n"))
cat("Tous les fichiers trouvés.\n\n")

# Exécution des scripts
scripts <- list(
  list(f="scripts/01_morbidite_sexe_age.R", d="Tâche 13 — Morbidité par sexe et âge"),
  list(f="scripts/02_types_maladies.R",     d="Tâche 14 — Types de maladies"),
  list(f="scripts/03_recours_soins.R",      d="Tâche 15 — Recours aux prestataires"),
  list(f="scripts/04_depenses_sante.R",     d="Tâche 16 — Distribution dépenses"),
  list(f="scripts/05_test_independance.R",  d="Tâche 17 — Chi-deux × quintile"),
  list(f="scripts/06_violin_rural_urbain.R",d="Tâche 18 — Violin rural/urbain")
)

for (s in scripts) {
  cat("─────────────────────────────────────\n", s$d, "\n")
  tryCatch(source(s$f, encoding="UTF-8"),
           error = function(e) cat("ERREUR :", conditionMessage(e), "\n"))
}

cat("\n═══════════════════════════════════════\n")
cat("SORTIES PRODUITES\n═══════════════════════════════════════\n")
cat("Figures :\n"); for(f in list.files("outputs/figures","*.png")) cat(" -",f,"\n")
cat("Tableaux :\n"); for(f in list.files("outputs/tables")) cat(" -",f,"\n")
cat("\nRapport Word : ouvrir rapport/rapport d'analyse.Rmd et cliquer Knit.\n")
