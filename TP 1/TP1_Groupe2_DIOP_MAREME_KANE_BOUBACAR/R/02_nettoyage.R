# ============================================================
#  Nettoyage et contrôle qualité — sect1_harvestw4
#  Étapes : exploration, doublons, valeurs manquantes, filtrage
# ============================================================

library(dplyr)
library(naniar)
library(ggplot2)

# ============================================================
#  1. Chargement
# ============================================================

sect1_w4 <- readRDS("data/processed/sect1_w4.rds")

# ============================================================
#  2. Exploration de la structure
# ============================================================

str(sect1_w4)
glimpse(sect1_w4)
summary(sect1_w4)

# Résultat : 30 337 individus pour 59 variables

# ============================================================
#  3. Vérification des doublons
# ============================================================

# La paire (hhid, indiv) doit identifier chaque ligne de manière unique
# hhid = identifiant du ménage | indiv = identifiant de l'individu

doublons <- sect1_w4 %>%
  group_by(hhid, indiv) %>%
  filter(n() > 1) %>%
  ungroup()

cat("Nombre de doublons détectés :", nrow(doublons), "\n")
# Résultat attendu : 0

# ============================================================
#  4. Analyse des valeurs manquantes
# ============================================================

# ---  Résumé global ---
miss_var_summary(sect1_w4)
# s1q12_4 et s1q13a_4 ont 100% de manquant

sect1_w4 %>%
  mutate(est_parti = s1q4a == 2) %>%
  group_by(est_parti) %>%
  summarise(
    n          = n(),
    na_age     = sum(is.na(s1q4)),
    na_sexe    = sum(is.na(s1q2)),
    na_parente = sum(is.na(s1q3)),
    .groups    = "drop"
  )

# --- 4.3 Visualisation des NA sur les variables clés (avant filtrage) ---

vars_cles <- c("hhid", "indiv", "s1q2", "s1q3", "s1q4", "s1q4a", "sector", "state")

p_miss <- vis_miss(
  sect1_w4 %>% select(all_of(vars_cles)),
  warn_large_data = FALSE
) +
  labs(
    title    = "Valeurs manquantes sur les variables clés ",
  ) +
  theme_minimal(base_size = 12)

ggsave(
  filename = "output/figures/valeurs_manquantes_toutes_variables.png",
  plot     = p_miss,
  width    = 12,
  height   = 6,
  dpi      = 150
)


# ============================================================
#  5. Filtrage — conservation des membres présents uniquement
# ============================================================

sect1_w4_clean <- sect1_w4 %>%
  filter(s1q4a == 1)   # s1q4a == 1 : membre encore présent dans le ménage


# ============================================================
#  6. Sauvegarde
# ============================================================

saveRDS(sect1_w4_clean, "data/processed/sect1_w4_clean.rds")
