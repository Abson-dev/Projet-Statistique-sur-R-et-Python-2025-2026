library(haven)
library(dplyr)

# Chargement
cat("Chargement des fichiers bruts ...\n")
sect4a <- read_dta("data/raw/sect4a_harvestw4.dta")
sect1  <- read_dta("data/raw/sect1_harvestw4.dta")
cons   <- read_dta("data/raw/totcons_final.dta")

cat("  sect4a :", nrow(sect4a), "obs\n")
cat("  sect1  :", nrow(sect1),  "obs\n")
cat("  cons   :", nrow(cons),   "obs\n")

# Démographie (sect1)
sect1_clean <- sect1 %>%
  select(hhid, indiv, s1q2, s1q4, sector) %>%
  rename(sexe = s1q2, age = s1q4, milieu = sector) %>%
  mutate(
    sexe_label   = factor(sexe,   levels = c(1, 2), labels = c("Homme", "Femme")),
    milieu_label = factor(milieu, levels = c(1, 2), labels = c("Urbain", "Rural")),
    groupe_age   = cut(age,
                       breaks = c(0, 14, 24, 34, 44, 54, 64, Inf),
                       labels = c("0-14","15-24","25-34","35-44","45-54","55-64","65+"),
                       right = TRUE, include.lowest = TRUE)
  )

# Santé (sect4a) + fusion avec sect1
# malade = 1 si déclaration maladie/blessure dans les 4 dernières semaines

df_health <- sect4a %>%
  select(hhid, indiv, s4aq3, s4aq1, s4aq3b_1, s4aq6a,
         s4aq9, s4aq14, s4aq17) %>%
  mutate(
    malade   = if_else(s4aq3 == 1, 1L, 0L, missing = NA_integer_),
    consulte = if_else(s4aq1 == 1, 1L, 0L, missing = NA_integer_)
  ) %>%
  left_join(sect1_clean, by = c("hhid", "indiv"))

cat("\n  df_health :", nrow(df_health), "individus\n")
cat("  Malades   :", sum(df_health$malade == 1, na.rm = TRUE), "\n")

# Quintiles de consommation
cons_clean <- cons %>%
  select(hhid, totcons_adj) %>%
  mutate(
    quintile       = ntile(totcons_adj, 5),
    quintile_label = factor(quintile, levels = 1:5,
                            labels = c("Q1 (plus pauvres)", "Q2", "Q3",
                                       "Q4", "Q5 (plus riches)"))
  )

df_health <- df_health %>%
  left_join(cons_clean, by = "hhid")

# Sauvegarde

dir.create("data/processed", showWarnings = FALSE, recursive = TRUE)
saveRDS(df_health, "data/processed/df_health_base.rds")

cat("\n Données sauvegardées dans data/processed/\n")