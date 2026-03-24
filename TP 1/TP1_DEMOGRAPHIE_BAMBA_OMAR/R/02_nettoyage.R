# =============================================================
# 02_nettoyage.R
# Tâche 1 : Chargement, exploration et jointure des poids
# =============================================================

sect1_w4  <- charger_dta("sect1_harvest", 4)
secta_w4  <- charger_dta("secta_harvest", 4)

str(sect1_w4)
glimpse(sect1_w4)
summary(sect1_w4)
names(sect1_w4)

doublons <- sect1_w4 %>%
  group_by(hhid, indiv) %>%
  filter(n() > 1)

message("Nombre de lignes dupliquées : ", nrow(doublons))

colSums(is.na(sect1_w4))

poids_hh <- secta_w4 %>%
  select(hhid, sector, state, wt_wave4) %>%
  distinct(hhid, .keep_all = TRUE)

sect1_w4_clean <- sect1_w4 %>%
  distinct(hhid, indiv, .keep_all = TRUE) %>%
  filter(is.na(s1q4) | (s1q4 >= 0 & s1q4 <= 120)) %>%
  left_join(poids_hh, by = "hhid") %>%
  filter(!is.na(wt_wave4))

message("Lignes avant nettoyage : ", nrow(sect1_w4))
message("Lignes après nettoyage : ", nrow(sect1_w4_clean))

svy_indiv <- sect1_w4_clean %>%
  as_survey_design(ids = 1, weights = wt_wave4)

saveRDS(sect1_w4_clean, here("data", "processed", "sect1_w4_clean.rds"))
saveRDS(svy_indiv,      here("data", "processed", "svy_indiv_w4.rds"))
message("Données nettoyées sauvegardées")