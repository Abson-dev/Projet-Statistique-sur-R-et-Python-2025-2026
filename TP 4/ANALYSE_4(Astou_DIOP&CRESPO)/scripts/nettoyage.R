# scriptsde nettoyage
# Import, vérifications, construction des variables analytiques


# 1. Chargement des données brutes

sect11a1 <- read_dta(here("data", "raw", "sect11a1_plantingw4.dta"))
sect11a <- read_dta(here("data", "raw", "sect11a_plantingw4.dta"))
secta <- read_dta(here("data", "raw", "secta_plantingw4.dta"))


cat("sect11a1     :", nrow(sect11a1), "lignes x", ncol(sect11a1), "colonnes\n")
cat("sect11a      :", nrow(sect11a), "lignes x", ncol(sect11a), "colonnes\n")
cat("secta      :", nrow(secta), "lignes x", ncol(secta), "colonnes\n")

cat("On remarque que sect11a_plantingw4 possède 5 070 menages alors que secta_plantingw4 en possède 5 263.
    cette différence est du au fait que les 1 348 ménages présents dans `secta_plantingw4` 
    mais absents de `sect11a1_plantingw4` sont des **ménages qui n'ont pas cultivé de terres** (`ag1a = 2` : Non).
    Ils n'ont donc pas de parcelles à renseigner et n'apparaissent pas dans la base parcelles.
    qu'en a sect11a1_plantingw4 il est au niveau parcelle(11076 ména):un menage peut avoir plusieurs parcelles")

# 2. Vérifications de base
doublons <- sect11a1 |> group_by(hhid, plotid) |> filter(n() > 1) |> nrow()
cat("Doublons hhid+plotid :", doublons, "\n")

cat(" la base sect11a1_planting ne possède aucun doublons")

# Compte le nombre de ménages uniques dans sect11a1 et secta en comptant les valeurs distinctes de hhid
menages_sect11a1 <- n_distinct(sect11a1$hhid)
menages_secta <- n_distinct(secta$hhid)
cat("Les ménages de  sect11a1 sont au nombre de :", menages_sect11a1, "et ceux de  secta sont :", menages_secta, "\n")
cat("Ménages secta absents de sect11a1 :",
    sum(!secta$hhid %in% sect11a1$hhid), "\n")

cat("ce nombre de menage abscent était prévisible car nous avons plus haut que 1 348 ménages n'avaient pas cultivés de parcelles")
# 3. Extraction des poids 
poids <- secta |>
  select(hhid, wt_wave4, wt_longpanel, old_new, strata, cluster = ea)

# 4. Filtre sur les membres résidents actifs 
n_total  <- nrow(sect11a1)
sect11a1_ok <- sect11a1 %>%
  filter(
    s11aq4aa == 1,          #La personne est un résident habituel du ménage (1 = Oui)
    s11aq6a %in% c(1, NA),  #La personne était présente dans le ménage pendant les 12 derniers mois
                            # (1 = Oui ou valeur manquante)
    s11b1q27 == 1,           #La personne a participé aux activités agricoles sur la parcelle
                              # (s11b1q27 == 1 signifie qu'elle a travaillé sur la parcelle)
    between(s11aq4a, 15, 65)  # La personne est en âge actif (entre 15 et 65 ans inclus)
  )

cat("Membres retirés (non résidents) :", n_total - nrow(sect11a1_ok),
    "sur", n_total, "\n")
cat("Donc tous les résidents sont actifs on va donc continuer avec la base sect11a1")

# 5. Jointure avec les poids parcels
sect11a1_ok <- sect11a1 |>
  left_join(poids, by = "hhid")

# 6. Construction des variables analytiques

sect11a1_clean <- sect11a1_ok %>%
  mutate(
    # Sexe
    sexe = case_when(
      s11aq4b1 == 1 ~ "Homme",
      s11aq4b1 == 2 ~ "Femme",
      TRUE ~ NA_character_
    ) %>% 
      factor(levels = c("Homme", "Femme")),
    
    # Milieu de résidence
    milieu = case_when(
      sector == 1 ~ "Urbain",
      sector == 2 ~ "Rural",
      TRUE ~ NA_character_
    ) %>% 
      factor(levels = c("Urbain", "Rural")),
    
    # Lien de parenté (simplifié en 4 catégories)
    parente = case_when(
      s11aq4b == 1 ~ "Chef de ménage",
      s11aq4b == 2 ~ "Conjoint(e)",
      s11aq4b %in% c(3, 4, 5) ~ "Enfant",
      !is.na(s11aq4b) ~ "Autre",
      TRUE ~ NA_character_
    ) %>% 
      factor(levels = c("Chef de ménage", "Conjoint(e)", "Enfant", "Autre")),
    
    # Âge corrigé (âges > 100 deviennent NA)
    age = if_else(s11aq4a > 100 | s11aq4a < 0, NA_real_, as.numeric(s11aq4a)),
    
    # Groupe d'âge quinquennal
    groupe_age = cut(
      age,
      breaks = c(0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 
                 55, 60, 65, 70, 75, 80, 85, 90, 95, Inf),
      labels = c("[0-5)", "[5-10)", "[10-15)", "[15-20)", "[20-25)", 
                 "[25-30)", "[30-35)", "[35-40)", "[40-45)", "[45-50)", 
                 "[50-55)", "[55-60)", "[60-65)", "[65-70)", "[70-75)", 
                 "[75-80)", "[80-85)", "[85-90)", "[90-95)", "95+"),
      right = FALSE,
      include.lowest = TRUE
    )
  )
# 7. Taille des ménages
taille_menage <- sect11a1_clean %>%
  group_by(hhid) %>%
  summarise(taille_menage = n(), .groups = "drop")

# Ajout de la taille du ménage dans la base
sect11a1_clean <- sect11a1_clean %>%
  left_join(taille_menage, by = "hhid")

# 8. Plan de sondage pondéré (au niveau individu/parcelle)
plan_sondage <- sect11a1_clean %>%
  filter(!is.na(wt_wave4)) %>%
  as_survey_design(
    ids = cluster,
    strata = strata,
    weights = wt_wave4,
    nest = TRUE
  )

# 9. Création de l'objet ménage (1 ligne par ménage)
menages <- sect11a1_clean %>%
  distinct(hhid, .keep_all = TRUE) %>%
  filter(!is.na(milieu), !is.na(wt_wave4))

plan_menages <- menages %>%
  as_survey_design(
    ids = cluster,
    strata = strata,
    weights = wt_wave4,
    nest = TRUE
  )

# 10. Diagnostic des valeurs manquantes
vars_diag <- sect11a1_clean %>%
  select(zone, state, sector, hhid, s11aq4b1, s11aq4b, age) %>%
  rename(
    Zone     = zone,
    Région   = state,
    Milieu   = sector,
    Ménage   = hhid,
    Sexe     = s11aq4b1,
    Parenté  = s11aq4b,
    Âge      = age
  )

fig00 <- vis_miss(vars_diag, sort_miss = TRUE) +
  labs(
    title = "Diagnostique des valeurs manquantes - variables clés",
    subtitle = "GHS Panel W4 (2018-2019), membres résidents actifs",
    caption = source_ghs
  ) +
  theme_minimal(base_size = 10)

ggsave(here("outputs", "figures", "fig00_diagnostic_NA.png"),
       fig00, width = 9, height = 5, dpi = 300)

cat("fig00 exportée\n")
# 11. Sauvegarde
saveRDS(sect11a1_clean,  here("data", "processed", "sect11a1_clean.rds"))
saveRDS(menages,      here("data", "processed", "menages.rds"))
saveRDS(plan_sondage, here("data", "processed", "plan_sondage.rds"))
saveRDS(plan_menages, here("data", "processed", "plan_menages.rds"))

cat(sprintf("sect11a1_clean : %d obs x %d vars\n", nrow(sect11a1_clean), ncol(sect11a1_clean)))
cat(sprintf("Ménages     : %d\n", nrow(menages)))
cat(sprintf("En sommant Poids wt_wave4 on obtient : %.0f (qui est approximativement le nombre de ménages au Nigeria cette année la.)\n",
            sum(menages$wt_wave4, na.rm = TRUE)))

#saveRDS(stats_age, here("data", "processed", "stats_age.rds"))
#saveRDS(stats_taille, here("data", "processed", "stats_taille.rds"))
#Script 1 terminé
