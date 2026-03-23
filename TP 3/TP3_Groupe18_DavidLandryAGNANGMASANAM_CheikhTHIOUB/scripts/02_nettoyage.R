###############################################################################
# 02_nettoyage.R -- Nettoyage, recodage et integration de la ponderation
# Projet : TP3 -- Acces aux soins et depenses de sante (GHS Panel W4)
###############################################################################

cat("\n========== 02_nettoyage : Nettoyage et recodage ==========\n")

# Jointure sect4a + sect1 (sexe, age)
sect1_demo <- sect1 %>%
  select(hhid, indiv, s1q2, s1q4) %>%
  mutate(
    sexe = factor(s1q2, levels = c(1, 2), labels = c("Homme", "Femme")),
    age  = as.numeric(as.character(s1q4))
  ) %>%
  select(hhid, indiv, sexe, age)

sante <- sect4a %>%
  left_join(sect1_demo, by = c("hhid", "indiv"))

# Integration de la ponderation wt_wave4
poids_menage <- secta %>%
  select(hhid, wt_wave4) %>%
  distinct(hhid, .keep_all = TRUE)

sante <- sante %>%
  left_join(poids_menage, by = "hhid")

cat("Ponderation wt_wave4 integree :",
    sum(!is.na(sante$wt_wave4)), "/", nrow(sante), "individus\n")

# Zone de residence
sante <- sante %>%
  mutate(zone = factor(sector, levels = c(1, 2), labels = c("Urbain", "Rural")))

# Morbidite
sante <- sante %>%
  mutate(malade = ifelse(s4aq3 == 1, 1, 0))

# Type de maladie
sante <- sante %>%
  mutate(
    type_maladie_code = as.numeric(as.character(s4aq3b_1)),
    type_maladie      = labelliser_maladie(type_maladie_code),
    cat_maladie       = categoriser_maladie(type_maladie_code)
  )

# Prestataire consulte
sante <- sante %>%
  mutate(
    prestataire_code = as.numeric(as.character(s4aq6a)),
    prestataire      = labelliser_prestataire(prestataire_code)
  )

# Recours aux soins
sante <- sante %>%
  mutate(a_consulte = ifelse(s4aq1 == 1, "Oui", "Non"))

# Depense totale de sante
sante <- sante %>%
  mutate(
    cout_consult = ifelse(is.na(s4aq9)  | s4aq9  < 0, 0, s4aq9),
    cout_transp  = ifelse(is.na(s4aq10) | s4aq10 < 0, 0, s4aq10),
    cout_medic   = ifelse(is.na(s4aq14) | s4aq14 < 0, 0, s4aq14),
    cout_hosp    = ifelse(is.na(s4aq17) | s4aq17 < 0, 0, s4aq17),
    depense_sante = cout_consult + cout_transp + cout_medic + cout_hosp
  )

# Groupes d'age
sante <- sante %>%
  mutate(
    groupe_age = cut(age, breaks = c(0, 5, 15, 30, 45, 60, Inf),
                     labels = c("0-4", "5-14", "15-29", "30-44", "45-59", "60+"),
                     right = FALSE, include.lowest = TRUE)
  )

# Quintiles de consommation
quintiles_menage <- totcons %>%
  select(hhid, totcons_adj) %>%
  mutate(
    quintile = cut(totcons_adj,
                   breaks = quantile(totcons_adj, probs = seq(0, 1, 0.2), na.rm = TRUE),
                   labels = c("Q1 (pauvre)", "Q2", "Q3", "Q4", "Q5 (riche)"),
                   include.lowest = TRUE)
  )

sante <- sante %>%
  left_join(quintiles_menage %>% select(hhid, quintile), by = "hhid")

# Sous-ensemble individus avec depenses > 0
sante_dep <- sante %>% filter(depense_sante > 0)

cat("[02_nettoyage] Nettoyage termine.\n")
