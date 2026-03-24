# =============================================================================
# 02_nettoyage.R — Nettoyage et préparation des données
# TP3 : Accès aux services de santé et chocs sanitaires
# GHS Panel Nigeria | Wave 4 | ENSAE ISE 1 — 2025-2026
# =============================================================================
#
# NOMS DE VARIABLES CONFIRMÉS (names() vérifié) :
#
# sect3a_harvestw4.dta  (individus) :
#   zone, state, lga, sector, ea, hhid, indiv
#   s3q1  = maladie/blessure (1=YES, 2=NO)
#   s3q3  = type de maladie (code numérique, labels via as_factor)
#
# sect3b_harvestw4.dta  (ménage — PAS d'identifiant individu) :
#   zone, state, lga, sector, ea, hhid
#   s3q50    = traitement médical cherché ? (1=YES, 2=NO)
#   s3q51_1  = dépenses hôpital gouvernemental (Naira)
#   s3q51_2  = dépenses clinique/hôpital privé (Naira)
#   s3q51_3  = dépenses pharmacie (Naira)
#   s3q51_4  = dépenses tradipraticien (Naira)
#   s3q51_5  = dépenses autre prestataire (Naira)
#   s3q51_6  = dépenses automédication (Naira)
#   s3q51_7  = total dépenses santé (Naira)
#
# sect1_harvestw4.dta  (individus) :
#   zone, state, lga, sector, ea, hhid, indiv
#   s1q2  = sexe (1=MALE, 2=FEMALE)
#   s1q4  = âge (années)
#
# totcons_final.dta  (ménage) :
#   hhid, zone, sector, ea, hhsize
#   wt_wave4         = poids d'enquête
#   totcons_pc       = consommation totale per capita
#   totcons_adj      = consommation totale ajustée
#   totcons_adj_norm = consommation totale ajustée normalisée
#
# NOTE IMPORTANTE — Tâche 18 :
#   "zone" dans la consigne = MILIEU DE RÉSIDENCE (Rural / Urbain)
#   Variable : sector (1=Urban, 2=Rural) dans sect3b_raw / cons_raw
# =============================================================================

library(haven)
library(dplyr)
library(forcats)

# --------------------------------------------------------------------------
# 0. Répertoires
# --------------------------------------------------------------------------
dir.create("data/processed", recursive = TRUE, showWarnings = FALSE)
dir.create("output/tables",  recursive = TRUE, showWarnings = FALSE)
dir.create("output/figures", recursive = TRUE, showWarnings = FALSE)

# --------------------------------------------------------------------------
# 1. Import des fichiers .dta
# --------------------------------------------------------------------------
cat("--- Import des fichiers bruts ---\n")

sect3a_raw <- read_dta("data/raw/sect3a_harvestw4.dta")
sect3b_raw <- read_dta("data/raw/sect3b_harvestw4.dta")
sect1_raw  <- read_dta("data/raw/sect1_harvestw4.dta")
cons_raw   <- read_dta("data/raw/totcons_final.dta")

cat("sect3a_raw :", nrow(sect3a_raw), "lignes x", ncol(sect3a_raw), "colonnes\n")
cat("sect3b_raw :", nrow(sect3b_raw), "lignes x", ncol(sect3b_raw), "colonnes\n")
cat("sect1_raw  :", nrow(sect1_raw),  "lignes x", ncol(sect1_raw),  "colonnes\n")
cat("cons_raw   :", nrow(cons_raw),   "lignes x", ncol(cons_raw),   "colonnes\n")

# --------------------------------------------------------------------------
# 2. Nettoyage sect3a — morbidité (niveau individu)
# --------------------------------------------------------------------------
cat("\n--- Nettoyage sect3a ---\n")

sect3a_clean <- sect3a_raw |>
  rename(
    indiv_id         = indiv,
    malade_raw       = s3q1,
    type_maladie_raw = s3q3
  ) |>
  mutate(
    malade = case_when(
      malade_raw == 1 ~ 1L,
      malade_raw == 2 ~ 0L,
      TRUE            ~ NA_integer_
    ),
    type_maladie = case_when(
      is.na(type_maladie_raw) ~ NA_character_,
      TRUE ~ as.character(as_factor(type_maladie_raw))
    )
  ) |>
  select(hhid, indiv_id, malade, type_maladie)

cat("sect3a_clean :", nrow(sect3a_clean), "individus\n")
cat("Taux morbidite brut :", round(mean(sect3a_clean$malade, na.rm = TRUE) * 100, 2), "%\n")

# --------------------------------------------------------------------------
# 3. Nettoyage sect3b — dépenses santé (niveau MÉNAGE)
# --------------------------------------------------------------------------
# CORRECTION erreur "." introuvable :
# rowSums() calculé AVANT le mutate dans un vecteur séparé
# --------------------------------------------------------------------------
cat("\n--- Nettoyage sect3b ---\n")

# Étape A : nettoyage des colonnes de dépenses
sect3b_step1 <- sect3b_raw |>
  rename(consulte_raw = s3q50) |>
  mutate(
    consulte = case_when(
      consulte_raw == 1 ~ 1L,
      consulte_raw == 2 ~ 0L,
      TRUE              ~ NA_integer_
    ),
    s3q51_1 = if_else(as.numeric(s3q51_1) < 0 | as.numeric(s3q51_1) > 5000000,
                      NA_real_, as.numeric(s3q51_1)),
    s3q51_2 = if_else(as.numeric(s3q51_2) < 0 | as.numeric(s3q51_2) > 5000000,
                      NA_real_, as.numeric(s3q51_2)),
    s3q51_3 = if_else(as.numeric(s3q51_3) < 0 | as.numeric(s3q51_3) > 5000000,
                      NA_real_, as.numeric(s3q51_3)),
    s3q51_4 = if_else(as.numeric(s3q51_4) < 0 | as.numeric(s3q51_4) > 5000000,
                      NA_real_, as.numeric(s3q51_4)),
    s3q51_5 = if_else(as.numeric(s3q51_5) < 0 | as.numeric(s3q51_5) > 5000000,
                      NA_real_, as.numeric(s3q51_5)),
    s3q51_6 = if_else(as.numeric(s3q51_6) < 0 | as.numeric(s3q51_6) > 5000000,
                      NA_real_, as.numeric(s3q51_6)),
    s3q51_7 = if_else(as.numeric(s3q51_7) < 0 | as.numeric(s3q51_7) > 5000000,
                      NA_real_, as.numeric(s3q51_7))
  )

# Étape B : dépense totale (calculée HORS pipeline pour éviter l'erreur ".")
depense_totale <- rowSums(
  sect3b_step1[, c("s3q51_1","s3q51_2","s3q51_3",
                   "s3q51_4","s3q51_5","s3q51_6")],
  na.rm = TRUE
)

sect3b_step2 <- sect3b_step1 |>
  mutate(
    depense = case_when(
      !is.na(s3q51_7) & s3q51_7 > 0 ~ s3q51_7,
      depense_totale > 0              ~ depense_totale,
      TRUE                            ~ NA_real_
    )
  )

# Étape C : prestataire principal (montant le plus élevé parmi s3q51_1..6)
sect3b_clean <- sect3b_step2 |>
  mutate(
    dep_1   = if_else(!is.na(s3q51_1) & s3q51_1 > 0, s3q51_1, 0),
    dep_2   = if_else(!is.na(s3q51_2) & s3q51_2 > 0, s3q51_2, 0),
    dep_3   = if_else(!is.na(s3q51_3) & s3q51_3 > 0, s3q51_3, 0),
    dep_4   = if_else(!is.na(s3q51_4) & s3q51_4 > 0, s3q51_4, 0),
    dep_5   = if_else(!is.na(s3q51_5) & s3q51_5 > 0, s3q51_5, 0),
    dep_6   = if_else(!is.na(s3q51_6) & s3q51_6 > 0, s3q51_6, 0),
    max_dep = pmax(dep_1, dep_2, dep_3, dep_4, dep_5, dep_6, na.rm = TRUE),
    prestataire = case_when(
      is.na(consulte) | consulte == 0  ~ "Aucun",
      dep_1 > 0 & dep_1 == max_dep     ~ "Hopital public",
      dep_2 > 0 & dep_2 == max_dep     ~ "Clinique privee",
      dep_3 > 0 & dep_3 == max_dep     ~ "Pharmacie",
      dep_4 > 0 & dep_4 == max_dep     ~ "Tradipraticien",
      dep_5 > 0 | dep_6 > 0           ~ "Autre",
      consulte == 1                    ~ "Aucun",
      TRUE                             ~ "Aucun"
    ) |> factor(levels = c("Aucun", "Hopital public", "Clinique privee",
                           "Pharmacie", "Tradipraticien", "Autre")),
    # ── MILIEU DE RÉSIDENCE (sector : 1=Urban, 2=Rural) ──────────────────
    milieu = case_when(
      as.integer(sector) == 1 ~ "Urbain",
      as.integer(sector) == 2 ~ "Rural",
      TRUE                    ~ NA_character_
    ) |> factor(levels = c("Urbain", "Rural"))
  ) |>
  select(hhid, consulte, prestataire, depense, milieu)

cat("sect3b_clean :", nrow(sect3b_clean), "menages\n")
cat("Repartition prestataire :\n")
print(table(sect3b_clean$prestataire, useNA = "always"))
cat("Repartition milieu :\n")
print(table(sect3b_clean$milieu, useNA = "always"))

# --------------------------------------------------------------------------
# 4. Nettoyage sect1 — démographie (niveau individu)
# --------------------------------------------------------------------------
cat("\n--- Nettoyage sect1 ---\n")

sect1_clean <- sect1_raw |>
  rename(
    indiv_id = indiv,
    sexe_raw = s1q2,
    age_raw  = s1q4
  ) |>
  mutate(
    sexe = case_when(
      sexe_raw == 1 ~ "Homme",
      sexe_raw == 2 ~ "Femme",
      TRUE          ~ NA_character_
    ) |> factor(levels = c("Homme", "Femme")),
    groupe_age = case_when(
      age_raw >=  0 & age_raw <=  4 ~ "0-4 ans",
      age_raw >=  5 & age_raw <= 14 ~ "5-14 ans",
      age_raw >= 15 & age_raw <= 29 ~ "15-29 ans",
      age_raw >= 30 & age_raw <= 44 ~ "30-44 ans",
      age_raw >= 45 & age_raw <= 59 ~ "45-59 ans",
      age_raw >= 60                 ~ "60 ans et +",
      TRUE                          ~ NA_character_
    ) |> factor(levels = c("0-4 ans", "5-14 ans", "15-29 ans",
                           "30-44 ans", "45-59 ans", "60 ans et +"))
  ) |>
  select(hhid, indiv_id, sexe, groupe_age)

cat("sect1_clean  :", nrow(sect1_clean), "individus\n")

# --------------------------------------------------------------------------
# 5. Consommation, poids et quintiles (totcons_final.dta)
# --------------------------------------------------------------------------
cat("\n--- Consommation, poids et quintiles ---\n")

conso_var <- intersect(
  c("totcons_pc", "totcons_adj", "totcons_adj_norm"),
  names(cons_raw)
)[1]
cat("Variable consommation utilisee :", conso_var, "\n")

cons_clean <- cons_raw |>
  mutate(
    # Milieu de résidence du ménage (depuis cons_raw aussi)
    milieu_cons = case_when(
      as.integer(sector) == 1 ~ "Urbain",
      as.integer(sector) == 2 ~ "Rural",
      TRUE                    ~ NA_character_
    ) |> factor(levels = c("Urbain", "Rural"))
  ) |>
  select(hhid,
         poids   = wt_wave4,
         hhsize,
         conso   = all_of(conso_var),
         milieu_cons) |>
  mutate(
    poids    = as.numeric(poids),
    hhsize   = as.integer(hhsize),
    conso    = as.numeric(conso),
    quintile = ntile(conso, 5)
  )

cat("cons_clean   :", nrow(cons_clean), "menages\n")
cat("Quintiles    :\n"); print(table(cons_clean$quintile, useNA = "always"))
cat("Milieu       :\n"); print(table(cons_clean$milieu_cons, useNA = "always"))

# --------------------------------------------------------------------------
# 6. Assemblage — data_morbid (individus)
# --------------------------------------------------------------------------
cat("\n--- Assemblage data_morbid ---\n")

data_morbid <- sect3a_clean |>
  left_join(sect1_clean, by = c("hhid", "indiv_id")) |>
  left_join(
    cons_clean |> select(hhid, poids, hhsize, quintile, milieu = milieu_cons),
    by = "hhid"
  )

cat("data_morbid :", nrow(data_morbid), "x", ncol(data_morbid), "\n")
cat("Taux morbidite :", round(mean(data_morbid$malade, na.rm = TRUE)*100, 2), "%\n")
cat("Taux morbidite pondere :",
    round(weighted.mean(data_morbid$malade,
                        w = replace_na(data_morbid$poids, 1),
                        na.rm = TRUE)*100, 2), "%\n")

# --------------------------------------------------------------------------
# 7. Assemblage — data_recours (ménages)
# --------------------------------------------------------------------------
cat("\n--- Assemblage data_recours ---\n")

data_recours <- sect3b_clean |>
  left_join(cons_clean, by = "hhid") |>
  # Si milieu de sect3b_clean et milieu_cons sont identiques, on garde un seul
  mutate(
    milieu = case_when(
      !is.na(milieu)           ~ milieu,
      !is.na(milieu_cons)      ~ milieu_cons,
      TRUE                     ~ NA_character_
    ) |> factor(levels = c("Urbain", "Rural"))
  ) |>
  select(hhid, consulte, prestataire, depense, milieu,
         poids, hhsize, conso, quintile)

cat("data_recours :", nrow(data_recours), "x", ncol(data_recours), "\n")
cat("NA quintile  :", sum(is.na(data_recours$quintile)), "\n")
cat("Repartition milieu (data_recours) :\n")
print(table(data_recours$milieu, useNA = "always"))

# --------------------------------------------------------------------------
# 8. Assemblage — data_depenses
# --------------------------------------------------------------------------
cat("\n--- Assemblage data_depenses ---\n")

data_depenses <- data_recours |>
  select(hhid, depense, prestataire, consulte, milieu, quintile, poids, hhsize)

dep_pos <- data_depenses |>
  filter(!is.na(depense) & depense > 0 & is.finite(depense))

cat("Menages avec depenses > 0 :", nrow(dep_pos), "\n")
if (nrow(dep_pos) > 0) {
  cat("Mediane :", round(median(dep_pos$depense), 0), "Naira\n")
  cat("Moyenne :", round(mean(dep_pos$depense),   0), "Naira\n")
  cat("Deciles :\n")
  print(round(quantile(dep_pos$depense, probs = seq(0.1, 1, 0.1)), 0))
  cat("Repartition milieu (depenses > 0) :\n")
  print(table(dep_pos$milieu, useNA = "always"))
}

# --------------------------------------------------------------------------
# 9. Sauvegarde
# --------------------------------------------------------------------------
cat("\n--- Sauvegarde ---\n")

saveRDS(data_morbid,   "data/processed/data_morbid.rds")
saveRDS(data_depenses, "data/processed/data_depenses.rds")
saveRDS(data_recours,  "data/processed/data_recours.rds")
saveRDS(sect3a_clean,  "data/processed/sect3a_clean.rds")
saveRDS(sect3b_clean,  "data/processed/sect3b_clean.rds")

cat("\n=== SUCCES ===\n")
cat("data_morbid.rds   :", nrow(data_morbid),   "lignes |",
    ncol(data_morbid),   "colonnes :", paste(names(data_morbid),   collapse=", "), "\n")
cat("data_depenses.rds :", nrow(data_depenses),  "lignes |",
    ncol(data_depenses),  "colonnes :", paste(names(data_depenses), collapse=", "), "\n")
cat("data_recours.rds  :", nrow(data_recours),   "lignes |",
    ncol(data_recours),   "colonnes :", paste(names(data_recours),  collapse=", "), "\n")