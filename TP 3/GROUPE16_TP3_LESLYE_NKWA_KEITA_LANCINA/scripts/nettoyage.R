# Script de nettoyage
# Import, vérifications, jointures, construction des variables analytiques
#
# Notons quil ya eu une restructuration du module santé en W4 :
# En W4, le contenu santé (morbidité, consultations, dépenses) est dans sect4a_harvestw4 (69 vars).
# sect3a_harvestw4 = emploi, sect3b_harvestw4 = enrôlement NHIS (National Health Insurance Scheme) uniquement.
# Pour les quintiles de consommation : cons_agg_wave4_visit2 n'existe pas en W4
# On utilise totcons_final.dta (variable totcons_adj).

# 1. Chargement des données brutes
sect4a  <- read_dta(here("data", "raw", "sect4a_harvestw4.dta"))
sect3b  <- read_dta(here("data", "raw", "sect3b_harvestw4.dta"))
sect1   <- read_dta(here("data", "raw", "sect1_harvestw4.dta"))
secta   <- read_dta(here("data", "raw", "secta_harvestw4.dta"))
totcons <- read_dta(here("data", "raw", "totcons_final.dta"))

cat("sect4a  :", nrow(sect4a),  "x", ncol(sect4a),  "\n")
cat("sect3b  :", nrow(sect3b),  "x", ncol(sect3b),  "\n")
cat("sect1   :", nrow(sect1),   "x", ncol(sect1),   "\n")
cat("secta   :", nrow(secta),   "x", ncol(secta),   "\n")
cat("totcons :", nrow(totcons), "x", ncol(totcons),  "\n")

# 2. Vérifications de base
doublons <- sect4a |> group_by(hhid, indiv) |> filter(n() > 1) |> nrow()
cat("Doublons hhid+indiv (sect4a) :", doublons, "\n")

menages_sect4a <- n_distinct(sect4a$hhid)
menages_secta  <- n_distinct(secta$hhid)
menages_totcons <- n_distinct(totcons$hhid)
cat("Ménages sect4a :", menages_sect4a,
    "| secta :", menages_secta,
    "| totcons :", menages_totcons, "\n")
cat("Ménages secta absents de sect4a :",
    sum(!unique(secta$hhid) %in% unique(sect4a$hhid)), "\n")

# 3. Extraction des poids et variables de sondage
poids <- secta |>
  select(hhid, wt_wave4, wt_longpanel, old_new, strata, cluster = ea)

# 4. Filtre membres résidents actifs (cohérent TP1/TP2)
sect1_actifs <- sect1 |>
  filter(s1q4a == 1 | is.na(s1q4a)) |>
  select(hhid, indiv, s1q2, s1q4, s1q3, sector, zone, state)

cat("Membres actifs retenus :", nrow(sect1_actifs), "\n")
cat("Membres retirés :", nrow(sect1) - nrow(sect1_actifs), "\n")

# 5. Construction des quintiles de consommation
# totcons_adj = consommation annuelle par tête, ajustée spatialement
quintiles <- totcons |>
  select(hhid, totcons_adj) |>
  mutate(quintile = ntile(totcons_adj, 5))

# 6. Extraction NHIS (niveau ménage, depuis sect3b)
nhis_menage <- sect3b |>
  select(hhid, nhis = s3q50)

# 7. Jointure principale : sect1_actifs + sect4a + poids + quintiles + NHIS
df_sante <- sect1_actifs |>
  left_join(
    sect4a |> select(-zone, -state, -sector, -lga, -ea),
    by = c("hhid", "indiv")
  ) |>
  left_join(poids, by = "hhid") |>
  left_join(quintiles, by = "hhid") |>
  left_join(nhis_menage, by = "hhid")

cat("df_sante après jointure :", nrow(df_sante), "x", ncol(df_sante), "\n")

# 8. Diagnostic des NA sur variables clés
cat("NA s4aq3 (morbidité)    :", sum(is.na(df_sante$s4aq3)), "\n")
cat("NA s4aq1 (consultation) :", sum(is.na(df_sante$s4aq1)), "\n")
cat("NA s4aq8 (prestataire)  :", sum(is.na(df_sante$s4aq8)), "\n")
cat("NA quintile             :", sum(is.na(df_sante$quintile)), "\n")
cat("NA nhis                 :", sum(is.na(df_sante$nhis)), "\n")

# Les NA sur s4aq3 correspondent aux enfants <5 ans (s4aq51 == 1) pour
# qui la question de morbidité n'est pas posée en W4, plus les membres
# de sect1_actifs absents de sect4a.

# 9. Construction des variables analytiques
df_sante <- df_sante |>
  mutate(
    sexe = factor(s1q2, levels = c(1, 2), labels = c("Homme", "Femme")),
    milieu = factor(sector, levels = c(1, 2), labels = c("Urbain", "Rural")),
    age = if_else(s1q4 > 100, NA_real_, as.numeric(s1q4)),
    # Morbidité : maladie/blessure dans les 4 dernières semaines
    malade = case_when(
      s4aq3 == 1 ~ 1L,
      s4aq3 == 2 ~ 0L,
      TRUE       ~ NA_integer_
    ),
    # Consultation d'un praticien (curatives ET préventives)
    a_consulte = case_when(
      s4aq1 == 1 ~ 1L,
      s4aq1 == 2 ~ 0L,
      TRUE       ~ NA_integer_
    ),
    # Type de prestataire (gestionnaire de l'établissement)
    # Public : Fédéral (1), État (2), Local (3)
    # Privé : Privé (7)
    # Autre : Communauté (4), Religieux (5), ONG (6), Autre (8)
    type_presta = case_when(
      s4aq8 %in% c(1, 2, 3)    ~ "Public",
      s4aq8 == 7                ~ "Privé",
      s4aq8 %in% c(4, 5, 6, 8) ~ "Autre",
      TRUE                      ~ NA_character_
    ) |> factor(levels = c("Public", "Privé", "Autre")),
    # Dépenses de santé (nairas courants)
    # depense_totale : NA si un poste manque (pour les analyses de montants)
    depense_totale = rowSums(cbind(s4aq9, s4aq14, s4aq17), na.rm = FALSE),
    # depense_med : médicaments seuls (le poste le plus universel)
    depense_med = s4aq14,
    # Groupes d'âge selon l'OMS
    groupe_age = cut(
      s1q4,
      breaks = c(0, 15, 35, 60, Inf),
      labels = c("0-14", "15-34", "35-59", "60+"),
      right  = FALSE, include.lowest = TRUE
    ),
    # NHIS
    nhis_couvert = case_when(
      nhis == 1 ~ 1L,
      nhis == 2 ~ 0L,
      TRUE      ~ NA_integer_
    ),
    # Quintile en factor
    quintile_f = factor(quintile, levels = 1:5,
                        labels = c("Q1 (plus pauvre)", "Q2", "Q3", "Q4",
                                   "Q5 (plus riche)"))
  )

# 10. Plan de sondage pondéré
plan_sante <- df_sante |>
  filter(!is.na(wt_wave4)) |>
  as_survey_design(
    ids     = cluster,
    strata  = strata,
    weights = wt_wave4,
    nest    = TRUE
  )

# 11. Diagnostic valeurs manquantes
vars_diag <- df_sante |>
  select(zone, state, sector, s1q2, s1q4,
         s4aq3, s4aq1, s4aq6a, s4aq9, s4aq14, quintile) |>
  rename(
    Zone = zone, État = state, Milieu = sector,
    Sexe = s1q2, Âge = s1q4,
    Morbidité = s4aq3, Consultation = s4aq1,
    `Type praticien` = s4aq6a, `Dép. consult.` = s4aq9,
    `Dép. médicam.` = s4aq14, Quintile = quintile
  )

fig00 <- vis_miss(vars_diag, sort_miss = TRUE) +
  labs(
    title    = "Diagnostic des valeurs manquantes — variables clés TP3",
    subtitle = "GHS Panel W4 — membres résidents actifs",
    caption  = source_ghs
  ) +
  theme_minimal(base_size = 10)

ggsave(here("outputs", "figures", "fig00_diagnostic_NA.png"),
       fig00, width = 10, height = 5, dpi = 300)
cat("fig00 exportée\n")

# 12. Sauvegarde
saveRDS(df_sante,   here("data", "processed", "df_sante_clean.rds"))
saveRDS(plan_sante, here("data", "processed", "plan_sante.rds"))

cat(sprintf("df_sante : %d obs x %d vars\n", nrow(df_sante), ncol(df_sante)))
cat(sprintf("Poids wt_wave4 : %.0f individus représentés\n",
            sum(df_sante$wt_wave4, na.rm = TRUE)))
# Script nettoyage terminé
