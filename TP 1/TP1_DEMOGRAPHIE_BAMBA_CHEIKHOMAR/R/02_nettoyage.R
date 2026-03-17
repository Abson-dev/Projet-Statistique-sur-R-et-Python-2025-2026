# =============================================================
# 02_nettoyage.R
# Tâche 1 : Chargement et exploration de sect1_harvestw4
# =============================================================


# -- Chargement de sect1_harvestw4
sect1_w4 <- charger_dta("sect1_harvest", 4)

# -- Explorer la structure
str(sect1_w4)
glimpse(sect1_w4)
summary(sect1_w4)

# Afficher toutes les colonnes de sect1_w4
names(sect1_w4)
# -- Vérifier les doublons sur hhid + indiv_id
doublons <- sect1_w4 %>%
  group_by(hhid, indiv) %>%
  filter(n() > 1)

message("Nombre de lignes dupliquées : ", nrow(doublons))

# -- Afficher le nombre de valeurs manquantes par variable
colSums(is.na(sect1_w4))

# -- Supprimer les doublons et âges aberrants
sect1_w4_clean <- sect1_w4 %>%
  distinct(hhid, indiv, .keep_all = TRUE) %>%
  filter(is.na(s1q4) | (s1q4 >= 0 & s1q4 <= 120))

message("Lignes avant nettoyage : ", nrow(sect1_w4))
message("Lignes après nettoyage : ", nrow(sect1_w4_clean))

# -- Sauvegarder les données nettoyées
saveRDS(sect1_w4_clean, here("data", "processed", "sect1_w4_clean.rds"))
message("Données nettoyées sauvegardées")