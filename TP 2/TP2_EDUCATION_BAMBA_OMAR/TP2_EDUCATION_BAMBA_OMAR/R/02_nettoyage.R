# -- fichier éducation W4
sect2_w4 <- charger_dta("sect2_harvest", 4)
sect1_w4 <- charger_dta("sect1_harvest", 4)

# -- Jointure sur hhid + indiv
df_education <- sect2_w4 %>%
  left_join(sect1_w4, by = c("hhid", "indiv"))

# -- Inspecter les valeurs manquantes
colSums(is.na(df_education))

# -- Sauvegarder
file.remove(here("data", "processed", "df_education_w4.rds"))  # supprimer l'ancien
saveRDS(df_education, here("data", "processed", "df_education_w4.rds"))
message("✓ Données jointes sauvegardées")