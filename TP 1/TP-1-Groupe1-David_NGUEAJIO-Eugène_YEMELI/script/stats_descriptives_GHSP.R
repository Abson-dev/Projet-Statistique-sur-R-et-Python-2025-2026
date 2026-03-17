# =============================================================================
# STATISTIQUES DESCRIPTIVES - GHSP Nigeria Wave 4 (2018/19)
# Post-Harvest Household Questionnaire
# =============================================================================
# Ce script calcule pour chaque variable :
#   - Variables QUALITATIVES  : mode, min, max, effectifs par modalité
#   - Variables QUANTITATIVES : min, max, mode, Q1, Q2, Q3, moyenne,
#                               écart-type, coefficient de variation
# =============================================================================

library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(knitr)
library(writexl)


# -----------------------------------------------------------------------------
# 1. CHARGEMENT DES DONNÉES
# -----------------------------------------------------------------------------
chemin <- "DATA/NGA_2018_GHSP-W4_v03_M_Stata12/"

# Fichiers principaux à charger (choisis selon ton analyse)

sect1  <- read_dta(paste0(chemin, "sect1_harvestw4.dta"))    # Roster ménage
sect2  <- read_dta(paste0(chemin, "sect2_harvestw4.dta"))    # Education
sect3  <- read_dta(paste0(chemin, "sect3_plantingw4.dta"))   # Labour
sect4  <- read_dta(paste0(chemin, "sect4a_harvestw4.dta"))   # Health
sect10 <- read_dta(paste0(chemin, "sect10b_harvestw4.dta"))  # Food expenditure
sect12 <- read_dta(paste0(chemin, "sect12_harvestw4.dta"))   # Food security
totcons <- read_dta(paste0(chemin, "totcons_final.dta"))     # Consommation totale


# -----------------------------------------------------------------------------
# 2. IDENTIFICATION DES VARIABLES QUALITATIVES ET QUANTITATIVES
#    Les variables codées avec des labels Stata sont traitées comme qualitatives
# -----------------------------------------------------------------------------

identifier_type_variable <- function(df) {
  types <- sapply(df, function(col) {
    # Variable labellisée Stata OU factor OU character => qualitative
    if (inherits(col, "haven_labelled") && length(attr(col, "labels")) > 0) {
      return("qualitative")
    } else if (is.factor(col) || is.character(col)) {
      return("qualitative")
    } else if (is.numeric(col)) {
      # Si peu de valeurs uniques (<=15), probablement qualitative
      n_unique <- length(unique(na.omit(col)))
      if (n_unique <= 15) return("qualitative")
      return("quantitative")
    } else {
      return("autre")
    }
  })
  return(types)
}


# -----------------------------------------------------------------------------
# 3. FONCTION : CALCUL DU MODE
# -----------------------------------------------------------------------------
calcul_mode <- function(x) {
  x <- na.omit(x)
  if (length(x) == 0) return(NA)
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}


# -----------------------------------------------------------------------------
# 4. FONCTION PRINCIPALE : STATISTIQUES DESCRIPTIVES COMPLÈTES
# -----------------------------------------------------------------------------
stats_descriptives <- function(df, nom_fichier = "dataset") {

  types_var <- identifier_type_variable(df)

  # Convertir les variables haven_labelled en factor pour les quali
  df_traite <- df %>%
    mutate(across(
      where(~ inherits(., "haven_labelled") && length(attr(., "labels")) > 0),
      ~ as_factor(.)
    ))

  resultats_quanti <- list()
  resultats_quali  <- list()

  for (var in names(df_traite)) {

    col <- df_traite[[var]]
    type <- types_var[[var]]

    # ── VARIABLE QUANTITATIVE ──────────────────────────────────────────────
    if (type == "quantitative" && is.numeric(col)) {

      x <- na.omit(col)
      if (length(x) == 0) next

      moyenne  <- mean(x)
      ecart_type <- sd(x)
      cv <- ifelse(moyenne != 0, (ecart_type / abs(moyenne)) * 100, NA)

      resultats_quanti[[var]] <- data.frame(
        Fichier      = nom_fichier,
        Variable     = var,
        Type         = "Quantitative",
        N_obs        = length(x),
        N_manquants  = sum(is.na(col)),
        Minimum      = round(min(x), 4),
        Q1           = round(quantile(x, 0.25), 4),
        Mediane      = round(median(x), 4),
        Q3           = round(quantile(x, 0.75), 4),
        Maximum      = round(max(x), 4),
        Moyenne      = round(moyenne, 4),
        Ecart_type   = round(ecart_type, 4),
        CV_pct       = round(cv, 2),
        Mode         = calcul_mode(x),
        stringsAsFactors = FALSE
      )

    # ── VARIABLE QUALITATIVE ───────────────────────────────────────────────
    } else if (type == "qualitative") {

      x <- na.omit(col)
      if (length(x) == 0) next

      # Tableau des fréquences
      freq_table <- sort(table(x), decreasing = TRUE)
      mode_val   <- names(freq_table)[1]
      vals       <- as.numeric(col)
      vals       <- vals[!is.na(vals)]

      resultats_quali[[var]] <- data.frame(
        Fichier       = nom_fichier,
        Variable      = var,
        Type          = "Qualitative",
        N_obs         = length(x),
        N_manquants   = sum(is.na(col)),
        Minimum       = ifelse(length(vals) > 0, min(vals), NA),
        Maximum       = ifelse(length(vals) > 0, max(vals), NA),
        Mode          = mode_val,
        Freq_mode     = as.integer(freq_table[1]),
        Pct_mode      = round(freq_table[1] / length(x) * 100, 2),
        N_modalites   = length(freq_table),
        stringsAsFactors = FALSE
      )
    }
  }

  # Combiner les résultats
  quanti_df <- if (length(resultats_quanti) > 0)
    bind_rows(resultats_quanti) else data.frame()

  quali_df  <- if (length(resultats_quali) > 0)
    bind_rows(resultats_quali) else data.frame()

  return(list(quantitatives = quanti_df, qualitatives = quali_df))
}


# -----------------------------------------------------------------------------
# 5. APPLICATION SUR CHAQUE FICHIER
# -----------------------------------------------------------------------------
cat("Calcul des statistiques en cours...\n")

# Crée une liste nommée des datasets à analyser
datasets <- list(
  "sect1_roster"     = sect1,
  "sect2_education"  = sect2,
  "sect12_food_sec"  = sect12,
  "totcons"          = totcons
)
# Ajoute d'autres fichiers ici si nécessaire : sect3, sect4, sect10, etc.

# Applique la fonction sur chaque dataset
tous_resultats <- imap(datasets, ~ stats_descriptives(.x, nom_fichier = .y))

# Rassemble toutes les variables quantitatives
toutes_quanti <- bind_rows(map(tous_resultats, "quantitatives"))

# Rassemble toutes les variables qualitatives
toutes_quali  <- bind_rows(map(tous_resultats, "qualitatives"))


# -----------------------------------------------------------------------------
# 6. AFFICHAGE DANS LA CONSOLE
# -----------------------------------------------------------------------------
cat("\n========================================================\n")
cat("  STATISTIQUES DESCRIPTIVES - VARIABLES QUANTITATIVES\n")
cat("========================================================\n")
print(kable(toutes_quanti, format = "simple", digits = 3))

cat("\n========================================================\n")
cat("  STATISTIQUES DESCRIPTIVES - VARIABLES QUALITATIVES\n")
cat("========================================================\n")
print(kable(toutes_quali, format = "simple"))


# -----------------------------------------------------------------------------
# 7. EXPORT DES RÉSULTATS DANS UN FICHIER EXCEL
# -----------------------------------------------------------------------------
fichier_export <-  "stats_descriptives_GHSP_W4.xlsx"

write_xlsx(
  list(
    "Quantitatives" = toutes_quanti,
    "Qualitatives"  = toutes_quali
  ),
  path = fichier_export
)

cat("\n Résultats exportés dans :", fichier_export, "\n")


