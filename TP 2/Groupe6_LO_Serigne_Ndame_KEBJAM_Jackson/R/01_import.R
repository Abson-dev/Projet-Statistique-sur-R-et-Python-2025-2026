# ==========================================
# SCRIPT 01 : IMPORTATION ET JOINTURE (W4)
# ==========================================

# 1. Charger les bibliothèques nécessaires
library(haven)
library(dplyr)

# 2. Lire les fichiers .dta (Vague 4) depuis le dossier raw

sect1_w4 <- read_dta("data/raw/sect1_harvestw4.dta")
sect2_w4 <- read_dta("data/raw/sect2_harvestw4.dta")

# 3. Joindre les deux fichiers sur hhid et indiv_id
# On fusionne les infos individuelles (sect1) avec l'éducation (sect2)
data_brute_w4 <- sect2_w4 %>%
  left_join(sect1_w4, by = c("hhid", "indiv"))

# 4. Inspecter les valeurs manquantes pour l'éducation (variable s2aq13b)
# s2aq13b est la variable qui contient le niveau d'éducation
print("Résumé des valeurs manquantes pour s2aq13b :")
print(sum(is.na(data_brute_w4$s2aq13b)))

# 5. Sauvegarder l'objet fusionné dans 'processed' pour la suite
saveRDS(data_brute_w4, "data/processed/data_w4_merged.rds")
