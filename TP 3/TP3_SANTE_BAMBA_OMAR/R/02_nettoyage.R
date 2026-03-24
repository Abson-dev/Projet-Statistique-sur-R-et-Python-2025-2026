# =============================================================
# 02_nettoyage.R — Chargement et nettoyage TP3 Santé
# =============================================================

# -- Chargement des fichiers W4
sect3a_w4    <- charger_dta("sect3a_harvest", 4)
sect3b_w4    <- charger_dta("sect3b_harvest", 4)
sect1_w4     <- charger_dta("sect1_harvest",  4)
secta_w4     <- charger_dta("secta_harvest",  4)
cons_agg_w4 <- haven::read_dta(here("data", "raw", "w3",
                                    "cons_agg_wave3_visit2.dta"))

# -- Explorer
glimpse(sect3a_w4)
glimpse(sect3b_w4)
colSums(is.na(sect3a_w4))
colSums(is.na(sect3b_w4))

# -- Jointure sect3a + sect1 sur hhid + indiv
df_sante_a <- sect3a_w4 %>%
  left_join(sect1_w4 %>% select(hhid, indiv, s1q2, s1q4),
            by = c("hhid", "indiv"))

# -- Jointure sect3b + sect1 sur hhid uniquement (pas d'indiv dans sect3b)
df_sante_b <- sect3b_w4 %>%
  left_join(sect1_w4 %>% select(hhid, s1q2, s1q4) %>% distinct(hhid, .keep_all = TRUE),
            by = "hhid")

# -- Sauvegarder
saveRDS(df_sante_a,   here("data", "processed", "df_sante_a_w4.rds"))
saveRDS(df_sante_b,   here("data", "processed", "df_sante_b_w4.rds"))
saveRDS(secta_w4,     here("data", "processed", "secta_w4.rds"))
saveRDS(cons_agg_w4,  here("data", "processed", "cons_agg_w4.rds"))

message("Données sauvegardées dans data/processed/")