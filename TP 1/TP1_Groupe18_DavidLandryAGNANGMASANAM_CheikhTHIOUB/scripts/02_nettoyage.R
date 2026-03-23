###############################################################################
# 02_nettoyage.R -- Nettoyage, recodage et integration de la ponderation
# Projet : TP1 -- Profil demographique des menages nigerians
###############################################################################

cat("\n========== 02_nettoyage : Nettoyage et recodage ==========\n")

# Conversion de l'age en numerique et recodage du sexe
sect1_hw4 <- sect1_hw4 %>%
  mutate(
    age  = as.numeric(as.character(s1q4)),
    sexe = factor(s1q2, levels = c(1, 2), labels = c("Homme", "Femme"))
  )

# Recodage du lien de parente
sect1_hw4 <- sect1_hw4 %>%
  mutate(
    lien_parente = case_when(
      s1q3 == 1  ~ "Chef de menage",
      s1q3 == 2  ~ "Conjoint(e)",
      s1q3 == 3  ~ "Enfant biologique",
      s1q3 == 4  ~ "Bel-enfant",
      s1q3 == 5  ~ "Enfant adopte",
      s1q3 == 6  ~ "Petit-enfant",
      s1q3 == 7  ~ "Frere/Soeur",
      s1q3 == 8  ~ "Neveu/Niece",
      s1q3 == 9  ~ "Beau-frere/Belle-soeur",
      s1q3 == 10 ~ "Parent",
      s1q3 == 11 ~ "Beau-parent",
      s1q3 == 12 ~ "Domestique",
      s1q3 %in% c(14, 15) ~ "Autre",
      TRUE ~ NA_character_
    )
  )

# Zone de residence
sect1_hw4 <- sect1_hw4 %>%
  mutate(zone_label = factor(sector, levels = c(1, 2),
                              labels = c("Urbain", "Rural")))

# --- Integration de la ponderation wt_wave4 ---
# La ponderation est au niveau menage dans secta_harvestw4
poids_menage <- secta_hw4 %>%
  select(hhid, wt_wave4) %>%
  distinct(hhid, .keep_all = TRUE)

sect1_hw4 <- sect1_hw4 %>%
  left_join(poids_menage, by = "hhid")

cat("Ponderation wt_wave4 integree :",
    sum(!is.na(sect1_hw4$wt_wave4)), "/", nrow(sect1_hw4), "individus\n")

# Taille des menages
taille_menage <- sect1_hw4 %>%
  group_by(hhid) %>%
  summarise(taille = n(), .groups = "drop")

sector_menage <- sect1_hw4 %>%
  distinct(hhid, sector, wt_wave4)

taille_zone <- taille_menage %>%
  inner_join(sector_menage, by = "hhid") %>%
  mutate(zone = factor(sector, levels = c(1, 2), labels = c("Urbain", "Rural")))

# 4 vagues empilees
donnees_4vagues <- bind_rows(
  charger_vague(1), charger_vague(2),
  charger_vague(3), charger_vague(4)
)

cat("[02_nettoyage] Nettoyage termine.\n")
