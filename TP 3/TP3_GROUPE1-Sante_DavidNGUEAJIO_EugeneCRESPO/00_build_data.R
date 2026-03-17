# =============================================================================
# TP3 — Accès aux Soins et Dépenses de Santé | Nigeria GHS Panel (W1-W4)
# Script : 00_build_data.R
# Auteurs : David NGUEAJIO · Yemeli Crespo
# ENSAE Dakar — ISE1 2025-2026
# Description : Construit les bases de travail harmonisées pour l'analyse
#               santé à partir de sect4a_harvestwX (4 vagues).
# =============================================================================

library(haven)
library(dplyr)
library(tidyr)
library(forcats)

# =============================================================================
# 0. CHEMINS
# =============================================================================

ROOT <- dirname(dirname(rstudioapi::getSourceEditorContext()$path))
# ROOT <- "/chemin/vers/projet_GHS_Nigeria"

DATA_RAW  <- file.path(ROOT, "data", "raw")
DATA_PROC <- file.path(ROOT, "data", "processed")
dir.create(DATA_PROC, showWarnings = FALSE, recursive = TRUE)

W1_HH  <- file.path(DATA_RAW, "Post Harvest Wave 1", "Household")
W2_HH  <- file.path(DATA_RAW, "Post Harvest Wave 2", "Household")
W3_DIR <- file.path(DATA_RAW, "w3")
W4_DIR <- file.path(DATA_RAW, "w4")

load_dta <- function(path, label = "") {
  if (!file.exists(path)) stop(paste("Fichier introuvable :", path))
  cat(sprintf("  [OK] %s\n", label))
  read_dta(path)
}

miss_info <- function(x) {
  n_miss <- sum(is.na(x))
  sprintf("%d NA (%.1f%%)", n_miss, 100 * n_miss / length(x))
}

# =============================================================================
# 1. CHARGEMENT SECT1 (démographie — toutes vagues)
#    Note : si les RDS de TP2 existent déjà, on peut les réutiliser
# =============================================================================
cat("\n>>> Chargement sect1 (démographie)...\n")

sect1_w1 <- load_dta(file.path(W1_HH, "sect1_harvestw1.dta"), "sect1 W1")
sect1_w2 <- load_dta(file.path(W2_HH, "sect1_harvestw2.dta"), "sect1 W2")
sect1_w3 <- load_dta(file.path(W3_DIR, "sect1_harvestw3.dta"), "sect1 W3")
sect1_w4 <- load_dta(file.path(W4_DIR, "sect1_harvestw4.dta"), "sect1 W4")

harmonise_sect1 <- function(df, wave) {
  df %>%
    select(hhid, indiv, zone, state,
           any_of(c("s1q1", "s1q2", "s1q4"))) %>%
    rename_with(~ case_when(
      . == "s1q1" ~ "lien_parente",
      . == "s1q2" ~ "sexe",
      . == "s1q4" ~ "age",
      TRUE ~ .
    )) %>%
    mutate(
      wave = wave,
      sexe_label = case_when(
        as.integer(as_factor(sexe)) == 1 ~ "Homme",
        as.integer(as_factor(sexe)) == 2 ~ "Femme",
        TRUE ~ NA_character_
      ),
      zone_label = case_when(
        as.integer(zone) == 1 ~ "Urbain",
        as.integer(zone) == 2 ~ "Rural",
        TRUE ~ NA_character_
      ),
      groupe_age = case_when(
        age >= 0  & age <= 5  ~ "0-5 ans",
        age >= 6  & age <= 17 ~ "6-17 ans",
        age >= 18 & age <= 30 ~ "18-30 ans",
        age >= 31 & age <= 45 ~ "31-45 ans",
        age >= 46 & age <= 60 ~ "46-60 ans",
        age > 60              ~ "60+ ans",
        TRUE ~ NA_character_
      )
    )
}

demo_w1 <- harmonise_sect1(sect1_w1, 1)
demo_w2 <- harmonise_sect1(sect1_w2, 2)
demo_w3 <- harmonise_sect1(sect1_w3, 3)
demo_w4 <- harmonise_sect1(sect1_w4, 4)

# =============================================================================
# 2. CHARGEMENT CONS_AGG (quintiles de consommation — W4)
# =============================================================================
cat("\n>>> Chargement cons_agg W4...\n")

cons_agg_w4_path <- file.path(DATA_RAW, "cons_agg_wave4_visit2.dta")

if (file.exists(cons_agg_w4_path)) {
  cons_w4 <- load_dta(cons_agg_w4_path, "cons_agg W4")
  cons_w4 <- cons_w4 %>%
    select(hhid, any_of(c("total_cons_lcu", "pcexp", "welfare"))) %>%
    rename_with(~ "consommation", .cols = any_of(c("total_cons_lcu", "pcexp", "welfare"))) %>%
    mutate(quintile_conso = ntile(consommation, 5),
           quintile_label = paste0("Q", quintile_conso))
  cat("  -> cons_agg W4 chargé.\n")
} else {
  cat("  /!\\ cons_agg_wave4_visit2.dta introuvable — quintile non disponible.\n")
  cons_w4 <- tibble(hhid = character(), consommation = numeric(),
                    quintile_conso = integer(), quintile_label = character())
}

# =============================================================================
# 3. CHARGEMENT SECT4A — MODULE SANTÉ (4 vagues)
# =============================================================================
cat("\n>>> Chargement sect4a (santé)...\n")

sect4a_w1 <- load_dta(file.path(DATA_RAW, "sect4a_harvestw1.dta"), "sect4a W1")
sect4a_w2 <- load_dta(file.path(DATA_RAW, "sect4a_harvestw2.dta"), "sect4a W2")
sect4a_w3 <- load_dta(file.path(W3_DIR,   "sect4a_harvestw3.dta"), "sect4a W3")
sect4a_w4 <- load_dta(file.path(W4_DIR,   "sect4a_harvestw4.dta"), "sect4a W4")

# =============================================================================
# 4. HARMONISATION DES VARIABLES SANTÉ
#
# Variables ciblées par vague :
#   s4aq3    : maladie/blessure 4 semaines (toutes vagues)
#   s4aq3b   : type maladie (W1 : simple, W3 : s4aq3b, W4 : s4aq3b_1)
#   s4aq1    : consulté praticien santé
#   s4aq6a   : type 1er prestataire consulté
#   s4aq9    : montant payé 1ère consultation (Naira)
#   s4aq10   : montant transport (Naira)
# =============================================================================

harmonise_sante <- function(df, wave, demo_df) {

  # ---- Sélection robuste des colonnes disponibles -------------------------
  cols_base <- c("hhid", "indiv", "zone", "state")

  df_sel <- df %>%
    select(all_of(cols_base),
           any_of(c(
             "s4aq1",                         # consulté praticien
             "s4aq3",                         # maladie / blessure
             "s4aq3b",  "s4aq3b_1",           # type maladie (W1/W3 vs W4)
             "s4aq3b_os",                     # other specify type maladie
             "s4aq4",                         # a dû arrêter activités
             "s4aq5",                         # nb jours arrêt
             "s4aq6a",                        # prestataire 1
             "s4aq6a_os",                     # prestataire other specify
             "s4aq9",                         # dépense consultation (Naira)
             "s4aq10"                         # dépense transport
           ))
    )

  # ---- Harmonisation des noms -----------------------------------------------
  df_harm <- df_sel %>%
    # Maladie/blessure -> 1 = oui, 2 = non, harmonisé en 0/1
    mutate(
      wave = wave,
      malade = case_when(
        as.integer(as_factor(s4aq3)) == 1 ~ 1L,
        as.integer(as_factor(s4aq3)) == 2 ~ 0L,
        TRUE ~ NA_integer_
      ),
      # Type maladie : W4 utilise s4aq3b_1, W1/W3 utilisent s4aq3b
      type_maladie = as_factor(
        if ("s4aq3b_1" %in% names(.))  s4aq3b_1
        else if ("s4aq3b" %in% names(.)) s4aq3b
        else NA_real_
      ),
      # Consulté praticien
      consulte = case_when(
        as.integer(as_factor(s4aq1)) == 1 ~ 1L,
        as.integer(as_factor(s4aq1)) == 2 ~ 0L,
        TRUE ~ NA_integer_
      ),
      # Prestataire
      prestataire = as_factor(if ("s4aq6a" %in% names(.)) s4aq6a else NA_real_),
      # Dépenses santé (Naira)
      depense_consult = as.numeric(if ("s4aq9" %in% names(.)) s4aq9 else NA_real_),
      depense_transp  = as.numeric(if ("s4aq10" %in% names(.)) s4aq10 else NA_real_),
      depense_totale  = rowSums(cbind(depense_consult, depense_transp), na.rm = TRUE),
      depense_totale  = if_else(depense_totale == 0 & is.na(depense_consult), NA_real_,
                                depense_totale),
      # Nb jours d'arrêt
      jours_arret = as.numeric(if ("s4aq5" %in% names(.)) s4aq5 else NA_real_)
    ) %>%
    select(hhid, indiv, zone, state, wave,
           malade, type_maladie, consulte, prestataire,
           depense_consult, depense_transp, depense_totale, jours_arret) %>%
    # Jointure données démographiques
    left_join(
      demo_df %>% select(hhid, indiv, sexe_label, age, groupe_age, zone_label),
      by = c("hhid", "indiv")
    )

  df_harm
}

cat("\n>>> Harmonisation des 4 vagues santé...\n")
sante_w1 <- harmonise_sante(sect4a_w1, 1, demo_w1)
sante_w2 <- harmonise_sante(sect4a_w2, 2, demo_w2)
sante_w3 <- harmonise_sante(sect4a_w3, 3, demo_w3)
sante_w4 <- harmonise_sante(sect4a_w4, 4, demo_w4)

# Panel toutes vagues
sante_panel <- bind_rows(sante_w1, sante_w2, sante_w3, sante_w4)
cat(sprintf("  -> Panel santé : %d observations, %d vagues.\n",
            nrow(sante_panel), n_distinct(sante_panel$wave)))

# =============================================================================
# 5. BASES SPÉCIFIQUES PAR TÂCHE
# =============================================================================

# --- Tâche 13 : taux de morbidité W4
morbidite_w4 <- sante_w4 %>%
  filter(!is.na(malade), !is.na(sexe_label), !is.na(groupe_age)) %>%
  group_by(sexe_label, groupe_age) %>%
  summarise(
    n_total  = n(),
    n_malade = sum(malade == 1L, na.rm = TRUE),
    taux_morb = n_malade / n_total,
    .groups = "drop"
  ) %>%
  rowwise() %>%
  mutate(
    test  = list(binom.test(n_malade, n_total)),
    ic_lo = test$conf.int[1],
    ic_hi = test$conf.int[2]
  ) %>%
  select(-test)

# --- Tâche 14 : types de maladies
type_maladie_w4 <- sante_w4 %>%
  filter(!is.na(type_maladie), malade == 1L) %>%
  count(type_maladie) %>%
  arrange(desc(n)) %>%
  slice_head(n = 10) %>%
  mutate(
    pct = n / sum(n),
    type_maladie = fct_reorder(as.character(type_maladie), n)
  )

# --- Tâche 15 : prestataires
prestataires_w4 <- sante_w4 %>%
  filter(!is.na(prestataire), consulte == 1L) %>%
  count(prestataire) %>%
  arrange(desc(n)) %>%
  mutate(
    pct = n / sum(n),
    prestataire = fct_reorder(as.character(prestataire), n)
  )

# --- Tâche 16 : dépenses santé (W4, parmi ceux qui ont consulté)
depenses_w4 <- sante_w4 %>%
  filter(consulte == 1L, !is.na(depense_consult), depense_consult > 0) %>%
  mutate(
    log_depense = log1p(depense_consult),
    # Outliers : au-delà de Q3 + 3*IQR
    q1 = quantile(depense_consult, 0.25, na.rm = TRUE),
    q3 = quantile(depense_consult, 0.75, na.rm = TRUE),
    iqr = q3 - q1,
    outlier = depense_consult > q3 + 3 * iqr
  )

# --- Tâche 17 : recours × quintile (W4)
recours_quintile <- sante_w4 %>%
  filter(!is.na(consulte)) %>%
  left_join(cons_w4 %>% select(hhid, quintile_label), by = "hhid") %>%
  filter(!is.na(quintile_label))

# --- Tâche 18 : dépenses rural/urbain (W4)
depenses_zone <- sante_w4 %>%
  filter(!is.na(depense_consult), depense_consult > 0, !is.na(zone_label))

# =============================================================================
# 6. EXPORT
# =============================================================================
cat("\n>>> Export des bases...\n")

saveRDS(sante_panel,      file.path(DATA_PROC, "sante_panel_w1w4.rds"))
saveRDS(sante_w4,         file.path(DATA_PROC, "sante_w4.rds"))
saveRDS(morbidite_w4,     file.path(DATA_PROC, "morbidite_w4.rds"))
saveRDS(type_maladie_w4,  file.path(DATA_PROC, "type_maladie_w4.rds"))
saveRDS(prestataires_w4,  file.path(DATA_PROC, "prestataires_w4.rds"))
saveRDS(depenses_w4,      file.path(DATA_PROC, "depenses_w4.rds"))
saveRDS(recours_quintile, file.path(DATA_PROC, "recours_quintile_w4.rds"))
saveRDS(depenses_zone,    file.path(DATA_PROC, "depenses_zone_w4.rds"))

cat("  -> 8 fichiers RDS exportés dans data/processed/\n")

# =============================================================================
# 7. RAPPORT QUALITÉ
# =============================================================================
cat("\n>>> Rapport qualité — base santé W4 :\n")
cat(sprintf("  malade      : %s\n", miss_info(sante_w4$malade)))
cat(sprintf("  consulte    : %s\n", miss_info(sante_w4$consulte)))
cat(sprintf("  prestataire : %s\n", miss_info(sante_w4$prestataire)))
cat(sprintf("  depense     : %s\n", miss_info(sante_w4$depense_consult)))
cat(sprintf("  sexe_label  : %s\n", miss_info(sante_w4$sexe_label)))
cat(sprintf("  zone_label  : %s\n", miss_info(sante_w4$zone_label)))

cat("\n=== 00_build_data.R (TP3) terminé avec succès ===\n")
