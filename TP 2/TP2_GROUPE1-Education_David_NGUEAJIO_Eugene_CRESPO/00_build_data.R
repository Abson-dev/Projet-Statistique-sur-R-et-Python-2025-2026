# =============================================================================
# TP2 — Éducation et Alphabétisation | Nigeria GHS Panel (W1-W4)
# Script : 00_build_data.R
# Auteurs : David NGUEAJIO · Yemeli Crespo
# ENSAE Dakar — ISE1 2025-2026
# Description : Construit les bases de travail harmonisées pour l'analyse
#               éducation à partir des fichiers .dta bruts (4 vagues).
# =============================================================================

library(haven)
library(dplyr)
library(tidyr)
library(forcats)

# =============================================================================
# 0. CHEMINS — À ADAPTER SELON VOTRE ARBORESCENCE LOCALE
# =============================================================================

# Racine du projet (dossier contenant TP2_Education/, TP3_Sante/, data/)
ROOT <- dirname(dirname(rstudioapi::getSourceEditorContext()$path))
# Si vous n'utilisez pas RStudio, commentez la ligne ci-dessus et définissez :
# ROOT <- "/chemin/vers/projet_GHS_Nigeria"

DATA_RAW  <- file.path(ROOT, "data", "raw")
DATA_PROC <- file.path(ROOT, "data", "processed")
dir.create(DATA_PROC, showWarnings = FALSE, recursive = TRUE)

# Chemins vers les dossiers bruts par vague
W1_HH  <- file.path(DATA_RAW, "Post Harvest Wave 1", "Household")
W2_HH  <- file.path(DATA_RAW, "Post Harvest Wave 2", "Household")
W3_DIR <- file.path(DATA_RAW, "w3")
W4_DIR <- file.path(DATA_RAW, "w4")

# =============================================================================
# FONCTIONS UTILITAIRES
# =============================================================================

#' Charge un fichier .dta et affiche un message
load_dta <- function(path, label = "") {
  if (!file.exists(path)) stop(paste("Fichier introuvable :", path))
  cat(sprintf("  [OK] %s (%s)\n", label, basename(path)))
  read_dta(path)
}

#' Retourne le nombre et % de NA pour un vecteur
miss_info <- function(x) {
  n_miss <- sum(is.na(x))
  sprintf("%d NA (%.1f%%)", n_miss, 100 * n_miss / length(x))
}

# =============================================================================
# 1. CHARGEMENT DES FICHIERS SECT1 (démographie — toutes vagues)
# =============================================================================
cat("\n>>> Chargement sect1 (démographie)...\n")

sect1_w1 <- load_dta(file.path(W1_HH, "sect1_harvestw1.dta"), "sect1 W1")
sect1_w2 <- load_dta(file.path(W2_HH, "sect1_harvestw2.dta"), "sect1 W2")
sect1_w3 <- load_dta(file.path(W3_DIR, "sect1_harvestw3.dta"), "sect1 W3")
sect1_w4 <- load_dta(file.path(W4_DIR, "sect1_harvestw4.dta"), "sect1 W4")

# Harmonisation sect1 : on garde hhid, indiv, sexe (s1q2), âge (s1q4), lien parenté (s1q1)
harmonise_sect1 <- function(df, wave) {
  df %>%
    select(
      hhid,
      indiv,
      zone,
      state,
      any_of(c("s1q1", "s1q2", "s1q4"))
    ) %>%
    rename_with(~ case_when(
      . == "s1q1" ~ "lien_parente",
      . == "s1q2" ~ "sexe",
      . == "s1q4" ~ "age",
      TRUE ~ .
    )) %>%
    mutate(wave = wave,
           sexe = as_factor(sexe),
           lien_parente = as_factor(lien_parente))
}

demo_w1 <- harmonise_sect1(sect1_w1, 1)
demo_w2 <- harmonise_sect1(sect1_w2, 2)
demo_w3 <- harmonise_sect1(sect1_w3, 3)
demo_w4 <- harmonise_sect1(sect1_w4, 4)

cat("  -> sect1 harmonisé pour 4 vagues.\n")

# =============================================================================
# 2. CHARGEMENT SECT2 — MODULE ÉDUCATION
# =============================================================================

# -----------------------------------------------------------------------------
# 2A. WAVE 1 & 2 : deux fichiers distincts (sect2a = ex-scolarisés, sect2b = scolarisés)
# -----------------------------------------------------------------------------
cat("\n>>> Chargement modules éducation W1-W2...\n")

sect2a_w1 <- load_dta(file.path(W1_HH, "sect2a_harvestw1.dta"), "sect2a W1 (ex-scolarisés)")
sect2b_w1 <- load_dta(file.path(W1_HH, "sect2b_harvestw1.dta"), "sect2b W1 (scolarisés)")
sect2a_w2 <- load_dta(file.path(W2_HH, "sect2a_harvestw2.dta"), "sect2a W2 (ex-scolarisés)")
sect2b_w2 <- load_dta(file.path(W2_HH, "sect2b_harvestw2.dta"), "sect2b W2 (scolarisés)")

# sect2a W1/W2 : individus ayant quitté l'école
# s2aq9 = niveau d'éducation atteint | s2aq5 = alphabétisation
harmonise_2a <- function(df, wave) {
  df %>%
    select(hhid, indiv,
           any_of(c("s2aq5", "s2aq6", "s2aq9", "s2aq10", "s2aq11"))) %>%
    mutate(
      wave = wave,
      statut_scolaire = "ex_scolarise",
      alphab    = as_factor(if ("s2aq5" %in% names(.)) s2aq5 else NA_real_),
      a_fqte_ecole = as_factor(if ("s2aq6" %in% names(.)) s2aq6 else NA_real_),
      niv_educ_raw = as_factor(if ("s2aq9" %in% names(.)) s2aq9 else NA_real_),
      # scolarisé actuellement ? -> non pour ce fichier
      scolarise_actuel = 0L
    ) %>%
    select(hhid, indiv, wave, statut_scolaire, alphab, a_fqte_ecole,
           niv_educ_raw, scolarise_actuel)
}

# sect2b W1/W2 : individus actuellement scolarisés
# s2bq1 = actuellement en école | s2bq3 = niveau en cours
harmonise_2b <- function(df, wave) {
  df %>%
    select(hhid, indiv,
           any_of(c("s2bq1", "s2bq3"))) %>%
    mutate(
      wave = wave,
      statut_scolaire = "scolarise_actuel",
      alphab    = NA_character_,
      a_fqte_ecole = NA_character_,
      # pour sect2b, pas de niveau "atteint" (toujours en cours)
      niv_educ_raw = as_factor(if ("s2bq3" %in% names(.)) s2bq3 else NA_real_),
      scolarise_actuel = 1L
    ) %>%
    select(hhid, indiv, wave, statut_scolaire, alphab, a_fqte_ecole,
           niv_educ_raw, scolarise_actuel)
}

educ_w1 <- bind_rows(
  harmonise_2a(sect2a_w1, 1),
  harmonise_2b(sect2b_w1, 1)
)
educ_w2 <- bind_rows(
  harmonise_2a(sect2a_w2, 2),
  harmonise_2b(sect2b_w2, 2)
)

# -----------------------------------------------------------------------------
# 2B. WAVE 3 : fichier unifié sect2_harvestw3
# -----------------------------------------------------------------------------
cat("\n>>> Chargement module éducation W3...\n")

sect2_w3 <- load_dta(file.path(W3_DIR, "sect2_harvestw3.dta"), "sect2 W3 (unifié)")

educ_w3 <- sect2_w3 %>%
  select(hhid, indiv,
         any_of(c("s2aq5", "s2aq6", "s2aq9", "s2aq13"))) %>%
  mutate(
    wave = 3L,
    statut_scolaire = "unifie_w3",
    alphab    = as_factor(if ("s2aq5" %in% names(.)) s2aq5 else NA_real_),
    a_fqte_ecole = as_factor(if ("s2aq6" %in% names(.)) s2aq6 else NA_real_),
    niv_educ_raw = as_factor(if ("s2aq9" %in% names(.)) s2aq9 else NA_real_),
    # s2aq13 : présent en école 2015-2016 (1=oui, 2=non)
    scolarise_actuel = if_else(as.integer(s2aq13) == 1L, 1L, 0L, missing = NA_integer_)
  ) %>%
  select(hhid, indiv, wave, statut_scolaire, alphab, a_fqte_ecole,
         niv_educ_raw, scolarise_actuel)

# -----------------------------------------------------------------------------
# 2C. WAVE 4 : fichier unifié sect2_harvestw4
# -----------------------------------------------------------------------------
cat("\n>>> Chargement module éducation W4...\n")

sect2_w4 <- load_dta(file.path(W4_DIR, "sect2_harvestw4.dta"), "sect2 W4 (unifié)")

educ_w4 <- sect2_w4 %>%
  select(hhid, indiv,
         any_of(c("s2aq5", "s2aq6", "s2aq9", "s2aq13", "s2aq13a"))) %>%
  mutate(
    wave = 4L,
    statut_scolaire = "unifie_w4",
    alphab    = as_factor(if ("s2aq5" %in% names(.)) s2aq5 else NA_real_),
    a_fqte_ecole = as_factor(if ("s2aq6" %in% names(.)) s2aq6 else NA_real_),
    niv_educ_raw = as_factor(if ("s2aq9" %in% names(.)) s2aq9 else NA_real_),
    # W4 : s2aq13a = scolarisé 2018/2019 (1=oui, 2=non) ; si NA, utiliser s2aq13
    scolarise_actuel = case_when(
      !is.na(s2aq13a) & as.integer(s2aq13a) == 1L ~ 1L,
      !is.na(s2aq13a) & as.integer(s2aq13a) == 2L ~ 0L,
      !is.na(s2aq13)  & as.integer(s2aq13)  == 1L ~ 1L,
      !is.na(s2aq13)  & as.integer(s2aq13)  == 2L ~ 0L,
      TRUE ~ NA_integer_
    )
  ) %>%
  select(hhid, indiv, wave, statut_scolaire, alphab, a_fqte_ecole,
         niv_educ_raw, scolarise_actuel)

# =============================================================================
# 3. CONSTRUCTION DE LA VARIABLE NIVEAU_EDUC (5 catégories harmonisées)
# =============================================================================
cat("\n>>> Construction variable niveau_educ...\n")

# Mapping numérique → 5 catégories
# 0       -> Aucun
# 1-2     -> Pré-primaire/Nursery (classé dans Aucun pour les adultes)
# 11-16   -> Primaire
# 21-23   -> Junior Secondary
# 24-28, 31-35, 321 -> Senior Secondary
# 41-43, 411-424, 322 -> Tertiaire
# 51-61   -> Éducation informelle/coranique (assimilée à Aucun/Primaire)

categorise_educ <- function(niv_raw) {
  code <- suppressWarnings(as.integer(as.character(niv_raw)))
  case_when(
    is.na(code)                              ~ NA_character_,
    code == 0 | code %in% c(1, 2, 3)        ~ "Aucun",
    code %in% 11:16                          ~ "Primaire",
    code %in% 21:23                          ~ "Junior Secondary",
    code %in% c(24:28, 31:35, 321)          ~ "Senior Secondary",
    code %in% c(41:43, 321, 322, 411:424)   ~ "Tertiaire",
    code %in% c(51, 52, 61)                 ~ "Aucun",  # informel
    TRUE                                     ~ "Aucun"
  )
}

build_educ <- function(educ_df, demo_df) {
  educ_df %>%
    mutate(
      niveau_educ = categorise_educ(niv_educ_raw),
      niveau_educ = factor(niveau_educ,
                           levels = c("Aucun", "Primaire", "Junior Secondary",
                                      "Senior Secondary", "Tertiaire"),
                           ordered = TRUE)
    ) %>%
    left_join(demo_df %>% select(hhid, indiv, sexe, age, zone, state),
              by = c("hhid", "indiv")) %>%
    mutate(
      sexe_label = case_when(
        as.integer(sexe) == 1 ~ "Homme",
        as.integer(sexe) == 2 ~ "Femme",
        TRUE ~ NA_character_
      ),
      groupe_age = case_when(
        age >= 6  & age <= 17 ~ "6-17 ans",
        age >= 18 & age <= 30 ~ "18-30 ans",
        age >= 31 & age <= 45 ~ "31-45 ans",
        age >= 46 & age <= 60 ~ "46-60 ans",
        age > 60              ~ "60+ ans",
        TRUE                  ~ NA_character_
      ),
      zone_label = case_when(
        as.integer(zone) == 1 ~ "Urbain",
        as.integer(zone) == 2 ~ "Rural",
        TRUE ~ NA_character_
      )
    )
}

educ_w1_final <- build_educ(educ_w1, demo_w1)
educ_w2_final <- build_educ(educ_w2, demo_w2)
educ_w3_final <- build_educ(educ_w3, demo_w3)
educ_w4_final <- build_educ(educ_w4, demo_w4)

# Base panel toutes vagues
educ_panel <- bind_rows(educ_w1_final, educ_w2_final, educ_w3_final, educ_w4_final)

cat(sprintf("  -> Panel éducation : %d observations, %d vagues.\n",
            nrow(educ_panel), n_distinct(educ_panel$wave)))

# =============================================================================
# 4. BASES SPÉCIFIQUES POUR LES TÂCHES DU TP
# =============================================================================

# --- Tâches 7-10 : Adultes 18+ (niveau éducation atteint, W4 prioritaire)
educ_adultes_w4 <- educ_w4_final %>%
  filter(!is.na(age) & age >= 18) %>%
  filter(!is.na(niveau_educ))

cat(sprintf("  -> Base adultes 18+ W4 : %d individus.\n", nrow(educ_adultes_w4)))

# --- Tâche 11 : Enfants 6-17 ans (scolarisation, toutes vagues)
scol_enfants <- educ_panel %>%
  filter(!is.na(age) & age >= 6 & age <= 17) %>%
  filter(!is.na(scolarise_actuel)) %>%
  filter(!is.na(zone_label))

cat(sprintf("  -> Base enfants 6-17 ans : %d individus.\n", nrow(scol_enfants)))

# --- Tâche 12 : Adultes 18+ toutes vagues (heatmap État)
educ_heatmap <- educ_panel %>%
  filter(!is.na(age) & age >= 18) %>%
  filter(!is.na(state)) %>%
  group_by(wave, state) %>%
  summarise(
    n_total       = n(),
    n_aucun       = sum(niveau_educ == "Aucun", na.rm = TRUE),
    part_aucun    = n_aucun / n_total,
    .groups = "drop"
  )

# =============================================================================
# 5. EXPORT DES BASES TRAITÉES
# =============================================================================
cat("\n>>> Export des bases de travail dans data/processed/...\n")

saveRDS(educ_panel,       file.path(DATA_PROC, "educ_panel_w1w4.rds"))
saveRDS(educ_w4_final,    file.path(DATA_PROC, "educ_w4.rds"))
saveRDS(educ_adultes_w4,  file.path(DATA_PROC, "educ_adultes_w4.rds"))
saveRDS(scol_enfants,     file.path(DATA_PROC, "scol_enfants_panel.rds"))
saveRDS(educ_heatmap,     file.path(DATA_PROC, "educ_heatmap_etat.rds"))
saveRDS(demo_w4,          file.path(DATA_PROC, "demo_w4.rds"))

cat("  -> Fichiers exportés :\n")
cat("     educ_panel_w1w4.rds   — Panel toutes vagues\n")
cat("     educ_w4.rds           — Éducation complète W4\n")
cat("     educ_adultes_w4.rds   — Adultes 18+ W4\n")
cat("     scol_enfants_panel.rds— Scolarisation 6-17 ans panel\n")
cat("     educ_heatmap_etat.rds — Agrégats État pour heatmap\n")
cat("     demo_w4.rds           — Démographie W4\n")

# =============================================================================
# 6. RAPPORT RAPIDE DE QUALITÉ DES DONNÉES
# =============================================================================
cat("\n>>> Rapport qualité :\n")
cat(sprintf("  niveau_educ  NA (W4 adultes) : %s\n",
            miss_info(educ_adultes_w4$niveau_educ)))
cat(sprintf("  sexe_label   NA (W4 adultes) : %s\n",
            miss_info(educ_adultes_w4$sexe_label)))
cat(sprintf("  age          NA (W4 adultes) : %s\n",
            miss_info(educ_adultes_w4$age)))
cat(sprintf("  zone_label   NA (W4 adultes) : %s\n",
            miss_info(educ_adultes_w4$zone_label)))

cat("\n=== 00_build_data.R terminé avec succès ===\n")
