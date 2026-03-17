###############################################################################
# 02_nettoyage.R — Nettoyage et recodage des variables (Tâche 1 suite)
# Projet : TP1 — Profil démographique des ménages nigérians
###############################################################################

cat("\n========== 02_nettoyage : Nettoyage et recodage ==========\n")

# Conversion de l'âge en numérique
sect1_hw4 <- sect1_hw4 %>%
  mutate(age = as.numeric(as.character(s1q4)))

# Recodage du sexe
sect1_hw4 <- sect1_hw4 %>%
  mutate(sexe = factor(s1q2, levels = c(1, 2), labels = c("Homme", "Femme")))

# Recodage du lien de parenté
sect1_hw4 <- sect1_hw4 %>%
  mutate(
    lien_parente = case_when(
      s1q3 == 1  ~ "Chef de ménage",
      s1q3 == 2  ~ "Conjoint(e)",
      s1q3 == 3  ~ "Enfant biologique",
      s1q3 == 4  ~ "Bel-enfant",
      s1q3 == 5  ~ "Enfant adopté",
      s1q3 == 6  ~ "Petit-enfant",
      s1q3 == 7  ~ "Frère/Sœur",
      s1q3 == 8  ~ "Neveu/Nièce",
      s1q3 == 9  ~ "Beau-frère/Belle-sœur",
      s1q3 == 10 ~ "Parent",
      s1q3 == 11 ~ "Beau-parent",
      s1q3 == 12 ~ "Domestique",
      s1q3 %in% c(14, 15) ~ "Autre",
      TRUE ~ NA_character_
    )
  )

# Recodage de la zone de résidence
sect1_hw4 <- sect1_hw4 %>%
  mutate(zone_label = factor(sector, levels = c(1, 2),
                              labels = c("Urbain", "Rural")))

# Calcul de la taille des ménages
taille_menage <- sect1_hw4 %>%
  group_by(hhid) %>%
  summarise(taille = n(), .groups = "drop")

# Table de correspondance ménage → zone
sector_menage <- sect1_hw4 %>%
  distinct(hhid, sector)

# Taille avec zone
taille_zone <- taille_menage %>%
  inner_join(sector_menage, by = "hhid") %>%
  mutate(zone = factor(sector, levels = c(1, 2),
                       labels = c("Urbain", "Rural")))

# Chargement des 4 vagues
donnees_4vagues <- bind_rows(
  charger_vague(1), charger_vague(2),
  charger_vague(3), charger_vague(4)
)

cat("Variables créées : age, sexe, lien_parente, zone_label\n")
cat("Taille ménage calculée :", nrow(taille_menage), "ménages\n")
cat("4 vagues empilées :", nrow(donnees_4vagues), "observations\n")
cat("[02_nettoyage] Nettoyage terminé.\n")
