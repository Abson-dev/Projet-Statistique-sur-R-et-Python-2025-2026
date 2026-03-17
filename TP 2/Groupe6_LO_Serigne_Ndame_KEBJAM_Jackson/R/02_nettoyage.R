# ==========================================
# SCRIPT 02 : NETTOYAGE ET VARIABLES
# ==========================================

library(dplyr)
library(forcats)

# 1. Charger les données fusionnées
df <- readRDS("data/processed/data_w4_merged.rds")

# 2. Création de la variable 'niveau_educ' à 5 catégories
# On utilise s2aq13b selon les codes officiels 
df_clean <- df %>%
  mutate(niveau_educ = case_when(
    is.na(s2aq13b) ~ "Aucun", # Si vide, on considère souvent "pas d'école"
    s2aq13b %in% c(1, 2, 3) ~ "Aucun", # Nursery/Maternelle = Pas encore au primaire
    s2aq13b >= 11 & s2aq13b <= 16 ~ "Primaire",
    s2aq13b >= 21 & s2aq13b <= 23 ~ "Junior Secondary",
    s2aq13b >= 24 & s2aq13b <= 26 ~ "Senior Secondary",
    # Tertiaire : Université (421+), Polytechnic (41, 411), Nursing (35), etc.
    s2aq13b %in% c(31, 34, 35, 41, 43, 321, 322, 411, 412, 421, 422, 423, 424) ~ "Tertiaire",
    # Autres (Coranique, Education adulte) : on peut les mettre dans "Aucun" ou NA
    s2aq13b %in% c(51, 52, 61) ~ "Aucun",
    TRUE ~ "Aucun"
  )) %>%
  # Transformation en facteur ordonné
  mutate(niveau_educ = fct_relevel(niveau_educ, 
                                   "Aucun", "Primaire", "Junior Secondary", 
                                   "Senior Secondary", "Tertiaire"))

# 3. Calcul des fréquences et proportions (Tâche 8)
table_freq <- df_clean %>%
  count(niveau_educ) %>%
  mutate(proportion = round(n / sum(n) * 100, 2))

print("--- TABLEAU DES FRÉQUENCES (Tâche 8) ---")
print(table_freq)

# 4. Sauvegarder les données propres
saveRDS(df_clean, "data/processed/data_ready.rds")



