# scriptsde nettoyage
# Import, vérifications, construction des variables analytiques

# 1. Chargement des données brutes
sect1 <- read_dta(here("data", "raw", "sect1_harvestw4.dta"))
secta <- read_dta(here("data", "raw", "secta_harvestw4.dta"))

cat("sect1 brut :", nrow(sect1), "lignes x", ncol(sect1), "colonnes\n")
cat("secta      :", nrow(secta), "lignes x", ncol(secta), "colonnes\n")

# 2. Vérifications de base
doublons <- sect1 |> group_by(hhid, indiv) |> filter(n() > 1) |> nrow()
cat("Doublons hhid+indiv :", doublons, "\n")

menages_sect1 <- n_distinct(sect1$hhid)
menages_secta <- n_distinct(secta$hhid)
cat("Ménages sect1 :", menages_sect1, "Ménages secta :", menages_secta, "\n")
cat("Ménages secta absents de sect1 :",
    sum(!secta$hhid %in% sect1$hhid), "\n")

# 3. Extraction des poids 
poids <- secta |>
  select(hhid, wt_wave4, wt_longpanel, old_new, strata, cluster = ea)

# 4. Filtre sur les membres résidents actifs 
n_total  <- nrow(sect1)
sect1_ok <- sect1 |> filter(s1q4a == 1 | is.na(s1q4a))
cat("Membres retirés (non résidents) :", n_total - nrow(sect1_ok),
    "sur", n_total, "\n")

# 5. Jointure avec les poids ménage
sect1_ok <- sect1_ok |>
  left_join(poids, by = "hhid")

# 6. Construction des variables analytiques
sect1_clean <- sect1_ok |>
  mutate(
    # Sexe
    sexe = factor(
      to_character(s1q2),
      levels = c("1. MALE", "2. FEMALE"),
      labels = c("Homme", "Femme")
    ),
    # Milieu de résidence
    milieu = factor(
      to_character(sector),
      levels = c("1. Urban", "2. Rural"),
      labels = c("Urbain", "Rural")
    ),
    # Lien de parenté (4 catégories)
    parente = case_when(
      s1q3 == 1              ~ "Chef de ménage",
      s1q3 == 2              ~ "Conjoint(e)",
      s1q3 %in% c(3, 4, 5)  ~ "Enfant",
      !is.na(s1q3)           ~ "Autre"
    ) |> factor(levels = c("Chef de ménage", "Conjoint(e)", "Enfant", "Autre")),
    # Groupe d'âge quinquennal
    groupe_age = cut(
      s1q4,
      breaks = c(0, 5, 10, 15, 20, 25, 30, 35, 40, 45,
                 50, 55, 60, 65, 70, 75, 80, 85, 90, 95, Inf),
      labels = c("[0,5)", "[5,10)", "[10,15)", "[15,20)", "[20,25)",
                 "[25,30)", "[30,35)", "[35,40)", "[40,45)", "[45,50)",
                 "[50,55)", "[55,60)", "[60,65)", "[65,70)", "[70,75)",
                 "[75,80)", "[80,85)", "[85,90)", "[90,95)", "95+"),
      right = FALSE, include.lowest = TRUE
    ),
    # Âge corrigé . ici, tous les ages>100 devienent des NA
    age = if_else(s1q4 > 100, NA_real_, as.numeric(s1q4))
  )

# 7. Taille des ménages 
taille_menage <- sect1_clean |>
  group_by(hhid) |>
  summarise(taille_menage = n(), .groups = "drop")

sect1_clean <- sect1_clean |>
  left_join(taille_menage, by = "hhid")

# 8. le Plan de sondage pondéré 
plan_sondage <- sect1_clean |>
  filter(!is.na(wt_wave4)) |>
  as_survey_design(
    ids     = cluster,
    strata  = strata,
    weights = wt_wave4,
    nest    = TRUE
  )

# 9. creation de  l'Objet ménage avec 1 ligne par ménage, pour les analyses de taille
menages <- sect1_clean |>
  distinct(hhid, .keep_all = TRUE) |>
  filter(!is.na(milieu), !is.na(wt_wave4))

plan_menages <- menages |>
  as_survey_design(
    ids     = cluster,
    strata  = strata,
    weights = wt_wave4,
    nest    = TRUE
  )

# 10. Diagnostic valeurs manquantes (la figure 0)
vars_diag <- sect1_clean |>
  select(zone, state, sector, hhid, indiv, s1q2, s1q3, age)|>
  rename(
    Zone = zone,
    Région = state,
    Milieu = sector,
    Ménage = hhid,
    Individu = indiv,
    Sexe = s1q2,
    Parenté = s1q3,
    Âge = age
  )
fig00 <- vis_miss(vars_diag, sort_miss = TRUE) +
  labs(
    title    = "Diagnostic des valeurs manquantes — variables clés",
    subtitle = "GHS Panel W4 (2018-2019), membres résidents",
    caption  = source_ghs
  ) +
  theme_minimal(base_size = 10)

ggsave(here("outputs", "figures", "fig00_diagnostic_NA.png"),
       fig00, width = 9, height = 5, dpi = 300)
cat("fig00 exportée\n")

# 11. Sauvegarde
saveRDS(sect1_clean,  here("data", "processed", "sect1_clean.rds"))
saveRDS(menages,      here("data", "processed", "menages.rds"))
saveRDS(plan_sondage, here("data", "processed", "plan_sondage.rds"))
saveRDS(plan_menages, here("data", "processed", "plan_menages.rds"))

cat(sprintf("sect1_clean : %d obs x %d vars\n", nrow(sect1_clean), ncol(sect1_clean)))
cat(sprintf("Ménages     : %d\n", nrow(menages)))
cat(sprintf("En sommant Poids wt_wave4 on obtient : %.0f (qui est approximativement le nombre de ménages au Nigeria cette année la.)\n",
            sum(menages$wt_wave4, na.rm = TRUE)))
#Script 1 terminé