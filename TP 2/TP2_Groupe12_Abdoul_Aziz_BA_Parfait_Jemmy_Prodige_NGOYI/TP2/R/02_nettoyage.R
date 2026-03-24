
#-----------------------------------#
# 1. Traitement des valeurs manquantes #
#---------------------------------#

# Inspection des valeurs manquantes
sum(is.na(data_combined$wt_wave4))

sum(is.na(data_combined$s2aq9))
## [1] 11066
mean(is.na(data_combined$s2aq9)*100)
## [1] 36.47691%

sum(is.na(data_combined$s2aq6))
#[1] 5548

# Suppression des enregistrements inutiles pour cette etude
 data_clean <- data_combined %>%
   drop_na(s2aq9,wt_wave4)
 
sum(is.na(data_clean$s2aq9))
#AUcune valeur manquante

# Construction d’une variable ordonnée niveau_educ à 5 catégories
data_clean <- data_clean %>%
  mutate(
    niveau_educ = case_when(
      s2aq9 %in% c(0, 1, 2, 3, 51, 52, 61)                     ~ "Aucun",
      s2aq9 %in% c(11, 12, 13, 14, 15, 16)                    ~ "Primaire",
      s2aq9 %in% c(21, 22, 23)                                ~ "Junior Secondary",
      s2aq9 %in% c(24, 25, 26, 27, 28, 31, 33, 34, 35, 321)   ~ "Senior Secondary",
      s2aq9 %in% c(41, 43, 322, 411, 412, 421, 422, 423, 424) ~ "Tertiaire",
      TRUE                                                    ~ NA_character_
    ),
    niveau_educ = factor(niveau_educ,
                         levels = c("Aucun", "Primaire", "Junior Secondary", 
                                    "Senior Secondary", "Tertiaire"),
                         ordered = TRUE)
  )

# Création de la variable sexe avec labels
data_clean <- data_clean %>%
  mutate(
    sexe = recode(as.numeric(s1q2),
                  `1` = "Homme",
                  `2` = "Femme",
    )  # pour gérer d'éventuelles autres valeurs
  )

# --- Ajout de la variable tranche_age dans data_clean ---

data_clean <- data_clean %>%
  mutate(
    # Création des groupes d'âge basés sur la variable s1q4
    tranche_age = case_when(
      s1q4 >= 18 & s1q4 <= 30 ~ "18-30",
      s1q4 >= 31 & s1q4 <= 45 ~ "31-45",
      s1q4 >= 46 & s1q4 <= 60 ~ "46-60",
      s1q4 > 60               ~ "60+",
      TRUE                    ~ "Moins de 18 / Non renseigné"
    ),
    
    # Transformation en facteur pour garder l'ordre dans les tableaux/graphiques
    tranche_age = factor(tranche_age, 
                         levels = c("18-30", "31-45", "46-60", "60+", "Moins de 18 / Non renseigné"),
                         ordered = TRUE)
  )


#=========================
# Sauvegarde des données propres
# =========================
saveRDS(data_clean, "data/processed/data_clean.rds")
