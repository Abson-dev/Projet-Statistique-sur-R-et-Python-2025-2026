# =========================================================
# 05_build_analysis_data.R
# Construction de la base analytique W4
# - Individus : sect1_harvestw4
# - Santé : sect4a_harvestw4
# - Consommation : totcons_final
# Gestion explicite et traçable des valeurs manquantes
# =========================================================

relevant_files_df <- readr::read_csv(
  file.path(path_logs, "relevant_files_detected.csv"),
  show_col_types = FALSE
)

get_path <- function(name) {
  out <- relevant_files_df %>%
    filter(dataset == name) %>%
    pull(path)
  
  if (length(out) == 0) return(NA_character_)
  out[[1]]
}

path_indiv_w4  <- get_path("indiv_w4")
path_health_w4 <- get_path("health_w4")
path_cons_w4   <- get_path("cons_w4")

if (is.na(path_indiv_w4)) {
  stop("Chemin indiv_w4 introuvable.")
}

if (is.na(path_health_w4)) {
  stop("Chemin health_w4 introuvable.")
}

if (basename(path_health_w4) != "sect4a_harvestw4.dta") {
  stop("La base santé utilisée n'est pas sect4a_harvestw4.dta.")
}

indiv_w4 <- haven::read_dta(path_indiv_w4) %>%
  janitor::clean_names()

health_w4 <- haven::read_dta(path_health_w4) %>%
  janitor::clean_names()

cons_w4 <- NULL
if (!is.na(path_cons_w4)) {
  cons_w4 <- haven::read_dta(path_cons_w4) %>%
    janitor::clean_names()
}

cat("Colonnes indiv_w4 :\n")
print(names(indiv_w4))

cat("Colonnes health_w4 :\n")
print(names(health_w4))

if (!is.null(cons_w4)) {
  cat("Colonnes cons_w4 :\n")
  print(names(cons_w4))
}

expected_health_vars <- c("s4aq1", "s4aq3")
missing_expected <- setdiff(expected_health_vars, names(health_w4))

if (length(missing_expected) > 0) {
  stop(
    paste0(
      "La base santé chargée ne contient pas les variables attendues : ",
      paste(missing_expected, collapse = ", "),
      "."
    )
  )
}

# ---------------------------------------------------------
# Fonctions utilitaires robustes
# ---------------------------------------------------------
to_num <- function(x) {
  suppressWarnings(as.numeric(as.character(x)))
}

to_chr_factor <- function(x) {
  as.character(haven::as_factor(x))
}

recode_yes_no <- function(x) {
  x_num <- to_num(x)
  x_chr <- tolower(trimws(to_chr_factor(x)))
  
  dplyr::case_when(
    x_num == 1 ~ "Oui",
    x_num == 2 ~ "Non",
    grepl("yes|oui", x_chr) ~ "Oui",
    grepl("no|non", x_chr) ~ "Non",
    TRUE ~ NA_character_
  )
}

recode_sex_safe <- function(x) {
  x_num <- to_num(x)
  x_chr <- tolower(trimws(to_chr_factor(x)))
  
  dplyr::case_when(
    x_num == 1 ~ "Homme",
    x_num == 2 ~ "Femme",
    grepl("male|homme", x_chr) ~ "Homme",
    grepl("female|femme", x_chr) ~ "Femme",
    TRUE ~ NA_character_
  )
}

recode_sector_safe <- function(x) {
  x_num <- to_num(x)
  x_chr <- tolower(trimws(to_chr_factor(x)))
  
  dplyr::case_when(
    x_num == 1 ~ "Urbain",
    x_num == 2 ~ "Rural",
    grepl("urban|urbain", x_chr) ~ "Urbain",
    grepl("rural", x_chr) ~ "Rural",
    TRUE ~ NA_character_
  )
}

make_age_group_safe <- function(age) {
  dplyr::case_when(
    is.na(age) ~ NA_character_,
    age < 5 ~ "0-4",
    age >= 5  & age <= 14 ~ "5-14",
    age >= 15 & age <= 24 ~ "15-24",
    age >= 25 & age <= 44 ~ "25-44",
    age >= 45 & age <= 64 ~ "45-64",
    age >= 65 ~ "65+",
    TRUE ~ NA_character_
  )
}

clean_text <- function(x) {
  x <- as.character(x)
  x <- stringr::str_trim(x)
  x
}

make_quintile_safe <- function(x) {
  if (all(is.na(x))) return(rep(NA_character_, length(x)))
  
  probs <- stats::quantile(x, probs = seq(0, 1, 0.2), na.rm = TRUE, type = 7)
  probs <- unique(probs)
  
  if (length(probs) < 6) {
    return(rep(NA_character_, length(x)))
  }
  
  as.character(
    cut(
      x,
      breaks = probs,
      include.lowest = TRUE,
      labels = c("Q1", "Q2", "Q3", "Q4", "Q5")
    )
  )
}

# ---------------------------------------------------------
# Individus
# ---------------------------------------------------------
indiv_core <- indiv_w4 %>%
  transmute(
    hhid = to_num(hhid),
    indiv = to_num(indiv),
    sexe = recode_sex_safe(s1q2),
    age = to_num(s1q4),
    age_groupe = make_age_group_safe(to_num(s1q4)),
    milieu = recode_sector_safe(sector)
  )

# ---------------------------------------------------------
# Santé
# ---------------------------------------------------------
health_core <- health_w4 %>%
  transmute(
    hhid = to_num(hhid),
    indiv = to_num(indiv),
    
    consulte = if ("s4aq1" %in% names(.)) recode_yes_no(s4aq1) else NA_character_,
    malade   = if ("s4aq3" %in% names(.)) recode_yes_no(s4aq3) else NA_character_,
    
    type_maladie_1 = if ("s4aq3b_1" %in% names(.)) to_chr_factor(s4aq3b_1) else NA_character_,
    type_maladie_2 = if ("s4aq3b_2" %in% names(.)) to_chr_factor(s4aq3b_2) else NA_character_,
    
    prestataire_1 = if ("s4aq6a" %in% names(.)) to_chr_factor(s4aq6a) else NA_character_,
    prestataire_2 = if ("s4aq6b" %in% names(.)) to_chr_factor(s4aq6b) else NA_character_,
    
    lieu_consult = if ("s4aq7" %in% names(.)) to_chr_factor(s4aq7) else NA_character_,
    type_gestion = if ("s4aq8" %in% names(.)) to_chr_factor(s4aq8) else NA_character_,
    
    dep_consult   = if ("s4aq9"  %in% names(.)) to_num(s4aq9)  else NA_real_,
    dep_transport = if ("s4aq10" %in% names(.)) to_num(s4aq10) else NA_real_,
    dep_medic     = if ("s4aq14" %in% names(.)) to_num(s4aq14) else NA_real_,
    dep_hospit    = if ("s4aq17" %in% names(.)) to_num(s4aq17) else NA_real_
  ) %>%
  mutate(
    dep_sante_totale = rowSums(
      cbind(dep_consult, dep_transport, dep_medic, dep_hospit),
      na.rm = TRUE
    ),
    dep_sante_totale = ifelse(
      is.na(dep_consult) & is.na(dep_transport) & is.na(dep_medic) & is.na(dep_hospit),
      NA_real_,
      dep_sante_totale
    )
  )

# ---------------------------------------------------------
# Consommation
# ---------------------------------------------------------
cons_core <- NULL

if (!is.null(cons_w4)) {
  cons_var <- dplyr::case_when(
    "totcons_adj" %in% names(cons_w4) ~ "totcons_adj",
    "totcons_pc"  %in% names(cons_w4) ~ "totcons_pc",
    TRUE ~ NA_character_
  )
  
  if (!is.na(cons_var)) {
    cons_core <- cons_w4 %>%
      transmute(
        hhid = to_num(hhid),
        consommation_pc = to_num(.data[[cons_var]])
      ) %>%
      mutate(
        quintile_conso = make_quintile_safe(consommation_pc)
      )
  }
}

# ---------------------------------------------------------
# Jointure
# ---------------------------------------------------------
analysis_w4 <- health_core %>%
  left_join(indiv_core, by = c("hhid", "indiv"))

if (!is.null(cons_core)) {
  analysis_w4 <- analysis_w4 %>%
    left_join(cons_core, by = "hhid")
}

# ---------------------------------------------------------
# Nettoyage léger des libellés
# ---------------------------------------------------------
analysis_w4 <- analysis_w4 %>%
  mutate(
    type_maladie_1 = clean_text(type_maladie_1),
    type_maladie_2 = clean_text(type_maladie_2),
    prestataire_1  = clean_text(prestataire_1),
    prestataire_2  = clean_text(prestataire_2),
    lieu_consult   = clean_text(lieu_consult),
    type_gestion   = clean_text(type_gestion)
  )

# ---------------------------------------------------------
# Tableau de diagnostic des valeurs manquantes
# ---------------------------------------------------------
missing_summary <- tibble::tibble(
  variable = names(analysis_w4),
  n_missing = sapply(analysis_w4, function(x) sum(is.na(x))),
  pct_missing = round(100 * n_missing / nrow(analysis_w4), 2),
  n_non_missing = nrow(analysis_w4) - n_missing
) %>%
  arrange(desc(pct_missing))

readr::write_csv(
  missing_summary,
  file.path(path_tables, "table_missing_summary.csv")
)

# ---------------------------------------------------------
# Tableau de suivi des effectifs utiles par analyse
# ---------------------------------------------------------
analysis_counts <- tibble::tibble(
  analyse = c(
    "Base totale",
    "Morbidité (malade + sexe + âge_groupe)",
    "Types de maladies",
    "Prestataires consultés",
    "Dépenses de santé",
    "Dépenses par milieu",
    "Consultation x quintile"
  ),
  n_utilisable = c(
    nrow(analysis_w4),
    sum(!is.na(analysis_w4$malade) & !is.na(analysis_w4$sexe) & !is.na(analysis_w4$age_groupe)),
    sum(!is.na(analysis_w4$type_maladie_1) | !is.na(analysis_w4$type_maladie_2)),
    sum(!is.na(analysis_w4$prestataire_1)),
    sum(!is.na(analysis_w4$dep_sante_totale) & analysis_w4$dep_sante_totale > 0),
    sum(!is.na(analysis_w4$dep_sante_totale) & analysis_w4$dep_sante_totale > 0 & !is.na(analysis_w4$milieu)),
    sum(!is.na(analysis_w4$consulte) & !is.na(analysis_w4$quintile_conso))
  )
)

readr::write_csv(
  analysis_counts,
  file.path(path_tables, "table_effectifs_par_analyse.csv")
)

# ---------------------------------------------------------
# Contrôles de qualité
# ---------------------------------------------------------
cat("\nRésumé rapide de la base analytique :\n")
cat("N lignes :", nrow(analysis_w4), "\n")
cat("N colonnes :", ncol(analysis_w4), "\n\n")

cat("Répartition sexe :\n")
print(table(analysis_w4$sexe, useNA = "ifany"))

cat("\nRépartition malade :\n")
print(table(analysis_w4$malade, useNA = "ifany"))

cat("\nRépartition consulte :\n")
print(table(analysis_w4$consulte, useNA = "ifany"))

cat("\nRépartition milieu :\n")
print(table(analysis_w4$milieu, useNA = "ifany"))

cat("\nNombre de dépenses santé non manquantes :\n")
print(sum(!is.na(analysis_w4$dep_sante_totale)))

if (all(is.na(analysis_w4$malade))) {
  stop("La variable 'malade' est entièrement manquante.")
}

if (all(is.na(analysis_w4$consulte))) {
  stop("La variable 'consulte' est entièrement manquante.")
}

readr::write_csv(
  analysis_w4,
  file.path(path_clean, "analysis_w4.csv")
)

cat("\nBase analytique W4 construite.\n")
cat("Dimensions :", nrow(analysis_w4), "lignes et", ncol(analysis_w4), "colonnes.\n")