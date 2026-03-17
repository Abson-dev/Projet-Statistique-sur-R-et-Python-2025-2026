###############################################################################
# 02_nettoyage.R -- Nettoyage, recodage et creation de variables
# Projet : TP3 -- Acces aux soins et depenses de sante (GHS Panel W4)
###############################################################################

cat("\n========== 02_nettoyage : Nettoyage et recodage ==========\n")

# ---- Jointure sect4a + sect1 (sexe, age) ----
# On recupere le sexe et l'age depuis sect1 pour enrichir les donnees sante
sect1_demo <- sect1 %>%
  select(hhid, indiv, s1q2, s1q4) %>%
  mutate(
    sexe = factor(s1q2, levels = c(1, 2), labels = c("Homme", "Femme")),
    age  = as.numeric(as.character(s1q4))
  ) %>%
  select(hhid, indiv, sexe, age)

sante <- sect4a %>%
  left_join(sect1_demo, by = c("hhid", "indiv"))

cat("Jointure sect4a + sect1 :", nrow(sante), "observations\n")

# ---- Recodage de la zone de residence ----
sante <- sante %>%
  mutate(zone = factor(sector, levels = c(1, 2), labels = c("Urbain", "Rural")))

# ---- Variable de morbidite (maladie/blessure dans les 4 dernieres semaines) ----
sante <- sante %>%
  mutate(malade = ifelse(s4aq3 == 1, 1, 0))

# ---- Recodage du type de maladie ----
sante <- sante %>%
  mutate(
    type_maladie_code = as.numeric(as.character(s4aq3b_1)),
    type_maladie      = labelliser_maladie(type_maladie_code),
    cat_maladie       = categoriser_maladie(type_maladie_code)
  )

# ---- Recodage du prestataire consulte ----
sante <- sante %>%
  mutate(
    prestataire_code = as.numeric(as.character(s4aq6a)),
    prestataire      = labelliser_prestataire(prestataire_code)
  )

# ---- Variable de recours aux soins (a consulte = oui/non) ----
sante <- sante %>%
  mutate(a_consulte = ifelse(s4aq1 == 1, "Oui", "Non"))

# ---- Construction de la depense totale de sante ----
# Somme des couts : consultation + transport + medicaments OTC + hospitalisation
sante <- sante %>%
  mutate(
    cout_consult  = ifelse(is.na(s4aq9)  | s4aq9  < 0, 0, s4aq9),
    cout_transp   = ifelse(is.na(s4aq10) | s4aq10 < 0, 0, s4aq10),
    cout_medic    = ifelse(is.na(s4aq14) | s4aq14 < 0, 0, s4aq14),
    cout_hosp     = ifelse(is.na(s4aq17) | s4aq17 < 0, 0, s4aq17),
    depense_sante = cout_consult + cout_transp + cout_medic + cout_hosp
  )

# ---- Groupes d'age ----
sante <- sante %>%
  mutate(
    groupe_age = cut(age,
                     breaks = c(0, 5, 15, 30, 45, 60, Inf),
                     labels = c("0-4", "5-14", "15-29", "30-44", "45-59", "60+"),
                     right = FALSE,
                     include.lowest = TRUE)
  )

# ---- Construction des quintiles de consommation ----
# A partir de totcons_final (consommation ajustee par tete)
quintiles_menage <- totcons %>%
  select(hhid, totcons_adj) %>%
  mutate(
    quintile = cut(totcons_adj,
                   breaks = quantile(totcons_adj, probs = seq(0, 1, 0.2),
                                     na.rm = TRUE),
                   labels = c("Q1 (pauvre)", "Q2", "Q3", "Q4", "Q5 (riche)"),
                   include.lowest = TRUE)
  )

# Jointure des quintiles sur les donnees sante (niveau menage)
sante <- sante %>%
  left_join(quintiles_menage %>% select(hhid, quintile), by = "hhid")

cat("Variables creees : zone, malade, type_maladie, cat_maladie,\n")
cat("  prestataire, a_consulte, depense_sante, groupe_age, quintile\n")
cat("Quintiles disponibles pour", sum(!is.na(sante$quintile)), "individus\n")
cat("[02_nettoyage] Nettoyage termine.\n")
