# Script de nettoyage
# Import, vérifications, construction des variables analytiques

# 1. Chargement des données brutes
sect2 <- read_dta(here("data", "raw", "sect2_harvestw4.dta"))
sect1 <- read_dta(here("data", "raw", "sect1_harvestw4.dta"))
secta <- read_dta(here("data", "raw", "secta_harvestw4.dta"))

cat("sect2 brut :", nrow(sect2), "lignes x", ncol(sect2), "colonnes\n")
cat("sect1      :", nrow(sect1), "lignes x", ncol(sect1), "colonnes\n")
cat("secta      :", nrow(secta), "lignes x", ncol(secta), "colonnes\n")

# 2. Vérifications de base
doublons_s2 <- sect2 |> group_by(hhid, indiv) |> filter(n() > 1) |> nrow()
cat("Doublons hhid+indiv (sect2) :", doublons_s2, "\n")

menages_sect2 <- n_distinct(sect2$hhid)
menages_secta <- n_distinct(secta$hhid)
cat("Ménages sect2 :", menages_sect2, "| Ménages secta :", menages_secta, "\n")
cat("Ménages secta absents de sect2 :",
    sum(!unique(secta$hhid) %in% unique(sect2$hhid)), "\n")

# 3. Extraction des poids et variables de sondage
poids <- secta |>
  select(hhid, wt_wave4, wt_longpanel, old_new, strata, cluster = ea)

# 4. Filtre membres résidents actifs (même logique que TP1)
# sect1 sert de filtre de référence : on garde s1q4a == 1 | is.na(s1q4a)
sect1_actifs <- sect1 |>
  filter(s1q4a == 1 | is.na(s1q4a)) |>
  select(hhid, indiv, s1q2, s1q4, s1q3, sector, zone, state)

n_retires <- nrow(sect1) - nrow(sect1_actifs)
cat("Membres retirés (non résidents) :", n_retires, "sur", nrow(sect1), "\n")
cat("Membres actifs retenus :", nrow(sect1_actifs), "\n")

# 5. Jointure sect1_actifs + sect2 + poids
# left_join depuis sect1_actifs : tous les membres actifs conservés
# On retire zone/state/sector/lga/ea de sect2 (identiques, vérifiés)
df_educ <- sect1_actifs |>
  left_join(
    sect2 |> select(-zone, -state, -sector, -lga, -ea),
    by = c("hhid", "indiv")
  ) |>
  left_join(poids, by = "hhid")

cat("df_educ après jointure :", nrow(df_educ), "x", ncol(df_educ), "\n")

# 6. Diagnostic des NA sur s2aq9 (niveau d'éducation)
na_filtre  <- sum(df_educ$s2aq2 == 2, na.rm = TRUE)
na_jamais  <- sum(df_educ$s2aq6 == 2, na.rm = TRUE)
na_absents <- sum(is.na(df_educ$s2aq2))
na_total   <- sum(is.na(df_educ$s2aq9))
na_vraies  <- na_total - na_filtre - na_jamais - na_absents
cat("Décomposition NA s2aq9 :\n")
cat("  Moins de 3 ans     :", na_filtre, "\n")
cat("  Jamais scolarisés  :", na_jamais, "\n")
cat("  Absents de sect2   :", na_absents, "\n")
cat("  Vraies non-réponses:", na_vraies, "\n")

# 7. Construction des variables analytiques
df_educ <- df_educ |>
  mutate(
    sexe = factor(s1q2, levels = c(1, 2), labels = c("Homme", "Femme")),
    milieu = factor(sector, levels = c(1, 2), labels = c("Urbain", "Rural")),
    age = if_else(s1q4 > 100, NA_real_, as.numeric(s1q4)),
    # Alphabétisation
    alphabetise = case_when(
      s2aq5 == 1 ~ 1L,
      s2aq5 == 2 ~ 0L,
      TRUE       ~ NA_integer_
    ),
    # Niveau d'éducation — 6 catégories
    # Aucun : jamais scolarisé (s2aq6==2) + code 0
    # Primaire : préscolaire (1-3) + Primary 1-6 (11-16)
    # Secondaire : JSS (21-23) + SSS (24-26) + Lower/Upper 6 (27-28)
    # Technique/Prof : NCE, OND, HND, vocational (31-35, 321-322, 411-412)
    # Tertiaire : université + postgrad (41, 43, 421-424)
    # Coranique/Adulte : éducation islamique + alphabétisation adulte (51-52, 61)
    niveau_educ = case_when(
      s2aq6 == 2                              ~ "Aucun",
      s2aq9 == 0                              ~ "Aucun",
      s2aq9 %in% c(1:3, 11:16)               ~ "Primaire",
      s2aq9 %in% c(21:28)                     ~ "Secondaire",
      s2aq9 %in% c(31:35, 321, 322, 411, 412) ~ "Technique/Prof",
      s2aq9 %in% c(41, 43, 421:424)           ~ "Tertiaire",
      s2aq9 %in% c(51, 52, 61)               ~ "Coranique/Adulte",
      TRUE                                    ~ NA_character_
    ) |> factor(
      levels  = c("Aucun", "Primaire", "Secondaire",
                   "Technique/Prof", "Tertiaire", "Coranique/Adulte"),
      ordered = TRUE
    ),
    # Scolarisation actuelle (2018-2019)
    scolarise_actuel = case_when(
      s2aq13a == 1 ~ 1L,
      s2aq13a == 2 ~ 0L,
      TRUE         ~ NA_integer_
    ),
    # Groupes d'âge pour la tâche 10
    groupe_age_large = cut(
      s1q4,
      breaks = c(0, 18, 31, 46, 61, Inf),
      labels = c("0-17", "18-30", "31-45", "46-60", "61+"),
      right  = FALSE, include.lowest = TRUE
    ),
    adulte_18plus = if_else(s1q4 >= 18, TRUE, FALSE, missing = NA),
    enfant_6_17   = if_else(s1q4 >= 6 & s1q4 <= 17, TRUE, FALSE, missing = NA)
  )

# 8. Plan de sondage pondéré (niveau individu)
plan_educ <- df_educ |>
  filter(!is.na(wt_wave4)) |>
  as_survey_design(
    ids     = cluster,
    strata  = strata,
    weights = wt_wave4,
    nest    = TRUE
  )

# 9. Diagnostic valeurs manquantes (figure 0)
vars_diag <- df_educ |>
  select(zone, state, sector, s1q2, s1q4, s2aq5, s2aq6, s2aq9, s2aq13a) |>
  rename(
    Zone = zone, État = state, Milieu = sector,
    Sexe = s1q2, Âge = s1q4,
    Alphabétisation = s2aq5, `Déjà scolarisé` = s2aq6,
    `Niveau éduc.` = s2aq9, `Scolarisé 2018` = s2aq13a
  )

fig00 <- vis_miss(vars_diag, sort_miss = TRUE) +
  labs(
    title    = "Diagnostic des valeurs manquantes — variables clés TP2",
    subtitle = "GHS Panel W4 — membres résidents actifs",
    caption  = source_ghs
  ) +
  theme_minimal(base_size = 10)

ggsave(here("outputs", "figures", "fig00_diagnostic_NA.png"),
       fig00, width = 10, height = 5, dpi = 300)
cat("fig00 exportée\n")

# 10. Sauvegarde
saveRDS(df_educ,   here("data", "processed", "df_educ_clean.rds"))
saveRDS(plan_educ, here("data", "processed", "plan_educ.rds"))

cat(sprintf("df_educ : %d obs x %d vars\n", nrow(df_educ), ncol(df_educ)))
cat(sprintf("Poids wt_wave4 : %.0f individus représentés\n",
            sum(df_educ$wt_wave4, na.rm = TRUE)))
# Script nettoyage terminé
