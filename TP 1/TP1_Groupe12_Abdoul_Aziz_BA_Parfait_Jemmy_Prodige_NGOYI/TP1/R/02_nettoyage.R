
# =========================
#  1. Gestion des valeurs manquantes
# =========================

data_clean <- data_combined %>%
  drop_na(s1q2, s1q4,s1q3)

# Verification
sum(is.na(data_clean$s1q2))
sum(is.na(data_clean$s1q4))
sum(is.na(data_clean$s1q3))

# =========================
#  2. Recodage des variables
# =========================

data_clean <- data_clean %>%
  mutate(
    sexe = recode(as.numeric(s1q2),
                  `1` = "Homme",
                  `2` = "Femme",
                  )  # pour gérer d'éventuelles autres valeurs
  )
#voir les modalités de s1q3(lien de parente avec le CM)
unique(data_clean$s1q3)

data_clean <- data_clean %>%
  mutate(
    lien_parente = case_when(
      s1q3 == 1 ~ "Chef",
      s1q3 == 2 ~ "Conjoint",
      s1q3 == 3 ~ "Enfant",
      TRUE ~ "Autres"
    )
  )

data_clean <- data_clean %>%
  mutate(
    lien_parente = factor(lien_parente,
                          levels = c("Chef", "Conjoint", "Enfant", "Autres"))
  )

# groupe d'age

data_clean <- data_clean %>%
  mutate(
    # Groupes d'âge (5 ans)
    age_group = cut(
      s1q4,
      breaks = seq(0, 100, by = 5),
      right = FALSE
    )
  )

View(data_clean$age_group)
sum(is.na(data_clean$age_group))

data_clean <- data_clean %>%
  drop_na(age_group)

individu_130 <- data_clean %>%
  filter(s1q4 >=100)

# Afficher les informations
print(individu_130)


# =========================
# Sauvegarde des données propres
# =========================
saveRDS(data_clean, "data/processed/data_clean.rds")