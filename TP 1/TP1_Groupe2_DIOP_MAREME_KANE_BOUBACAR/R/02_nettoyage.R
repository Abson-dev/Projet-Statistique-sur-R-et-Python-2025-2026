# ============================================================
#  Nettoyage et contrôle qualité — sect1_harvestw4
<<<<<<< HEAD
#  Étapes : exploration, doublons, valeurs manquantes, filtrage
=======
>>>>>>> 664df72ee5b586307c349b237310d9487e119661
# ============================================================

library(dplyr)
library(naniar)
library(ggplot2)
<<<<<<< HEAD
=======
library(sjlabelled)
>>>>>>> 664df72ee5b586307c349b237310d9487e119661

# ============================================================
#  1. Chargement
# ============================================================

sect1_w4 <- readRDS("data/processed/sect1_w4.rds")
<<<<<<< HEAD
=======
secta_w4 <- readRDS("data/processed/secta_w4.rds")
>>>>>>> 664df72ee5b586307c349b237310d9487e119661

# ============================================================
#  2. Exploration de la structure
# ============================================================

str(sect1_w4)
glimpse(sect1_w4)
summary(sect1_w4)
<<<<<<< HEAD

# Résultat : 30 337 individus pour 59 variables
=======
get_label(sect1_w4)

# Résultat : 30 337 individus, 59 variables
n_distinct(sect1_w4$hhid)  # 4 980 ménages

str(secta_w4)
glimpse(secta_w4)
summary(secta_w4)
get_label(secta_w4)

# Résultat : 5 025 ménages, 22 variables
# wt_wave4 = poids de sondage transversal du ménage
>>>>>>> 664df72ee5b586307c349b237310d9487e119661

# ============================================================
#  3. Vérification des doublons
# ============================================================
<<<<<<< HEAD

# La paire (hhid, indiv) doit identifier chaque ligne de manière unique
# hhid = identifiant du ménage | indiv = identifiant de l'individu
=======
# La paire (hhid, indiv) doit identifier chaque ligne de façon unique.
>>>>>>> 664df72ee5b586307c349b237310d9487e119661

doublons <- sect1_w4 %>%
  group_by(hhid, indiv) %>%
  filter(n() > 1) %>%
  ungroup()

cat("Nombre de doublons détectés :", nrow(doublons), "\n")
# Résultat attendu : 0

# ============================================================
#  4. Analyse des valeurs manquantes
# ============================================================

<<<<<<< HEAD
# ---  Résumé global ---
miss_var_summary(sect1_w4)
# s1q12_4 et s1q13a_4 ont 100% de manquant

sect1_w4 %>%
  mutate(est_parti = s1q4a == 2) %>%
  group_by(est_parti) %>%
=======
# 4.1 Résumé global
miss_var_summary(sect1_w4)
# → s1q12_4 et s1q13a_4 : 100 % de NA (inutilisables)

# 4.2 Diagnostic de s1q4a
# s1q4a == 1 → encore membre | s1q4a == 2 → parti | NA → non renseigné
table(sect1_w4$s1q4a, useNA = "ifany")

sect1_w4 %>%
  mutate(statut = case_when(
    s1q4a == 1   ~ "Présent",
    s1q4a == 2   ~ "Parti",
    is.na(s1q4a) ~ "NA s1q4a"
  )) %>%
  group_by(statut) %>%
>>>>>>> 664df72ee5b586307c349b237310d9487e119661
  summarise(
    n          = n(),
    na_age     = sum(is.na(s1q4)),
    na_sexe    = sum(is.na(s1q2)),
    na_parente = sum(is.na(s1q3)),
    .groups    = "drop"
  )
<<<<<<< HEAD

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
=======
# Interprétation : les 3 780 NA sur âge/sexe/parenté correspondent
# exactement aux membres partis → NA structurels, non aléatoires.
# Les 790 NA sur s1q4a n'ont pas de NA sur les variables analytiques
# → conservés dans la base filtrée.

# 4.3 Visualisation des NA avec VRAIS LABELS (pas les noms s1q...)
# On renomme les colonnes avant vis_miss pour afficher des libellés lisibles.

vars_cles_labeled <- sect1_w4 %>%
  select(hhid, indiv, s1q2, s1q3, s1q4, s1q4a, sector, state) %>%
  rename(
    "Identifiant ménage"    = hhid,
    "Identifiant individu"  = indiv,
    "Sexe"                  = s1q2,
    "Lien de parenté"       = s1q3,
    "Âge (années révolues)" = s1q4,
    "Statut de résidence"   = s1q4a,
    "Milieu (urbain/rural)" = sector,
    "État (State)"          = state
  )

p_miss <- vis_miss(vars_cles_labeled, warn_large_data = FALSE) +
  labs(
    title    = "Valeurs manquantes sur les variables clés — sect1_harvestw4 (avant filtrage)",
    subtitle = "Les NA sur Âge, Sexe et Lien de parenté correspondent aux membres partis"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x  = element_text(angle = 45, hjust = 1),
    plot.title   = element_text(size = 12, face = "bold"),
    plot.subtitle = element_text(size = 10, color = "gray40")
  )

ggsave(
  filename = "output/figures/valeurs_manquantes_variables_cles.png",
  plot     = p_miss,
  width    = 12, height = 6, dpi = 150
)

# ============================================================
#  5. Filtrage
# ============================================================
# On conserve : s1q4a == 1 (présent) ET NA sur s1q4a (statut inconnu).
# On exclut  : s1q4a == 2 (membres partis — aucune info démographique).

sect1_w4_clean <- sect1_w4 %>%
  filter(is.na(s1q4a) | s1q4a == 1)

cat("Avant filtrage  :", nrow(sect1_w4), "\n")
cat("Après filtrage  :", nrow(sect1_w4_clean), "\n")
cat("Exclus (partis) :", nrow(sect1_w4) - nrow(sect1_w4_clean), "\n")
# Attendu : 26 557 individus conservés (25 767 présents + 790 NA statut)

# ============================================================
#  6. Merge avec secta_w4 (poids de sondage)
# ============================================================
# On joint APRÈS filtrage pour ne pas propager les membres exclus.
# On ne prend que les colonnes utiles de secta pour éviter les doublons
# de colonnes (zone, sector, state existent déjà dans sect1).

base_mergee <- sect1_w4_clean %>%
  left_join(
    secta_w4 %>% select(hhid, wt_wave4, wt_longpanel),
    by = "hhid"
  )

cat("Individus sans poids (wt_wave4 manquant) :",
    sum(is.na(base_mergee$wt_wave4)), "\n")
# → 24 individus sans poids

# ============================================================
#  7. Calcul du poids individuel
# ============================================================
# Méthode standard LSMS : poids individuel = poids ménage / taille ménage.

base_mergee <- base_mergee %>%
  group_by(hhid) %>%
  mutate(
    taille_menage  = n(),
    poids_individu = wt_wave4 / taille_menage
  ) %>%
  ungroup()

summary(base_mergee$taille_menage)
summary(base_mergee$poids_individu)

# Diagnostic des ménages sans poids
menages_sans_poids <- base_mergee %>%
  filter(is.na(wt_wave4)) %>%
  distinct(hhid)

cat("Ménages sans poids :", nrow(menages_sans_poids), "\n")  # → 4

menages_sans_poids %>%
  left_join(secta_w4 %>% select(hhid, interview_result), by = "hhid")
# → interview_result == 2 (PARTIALLY COMPLETE - REFUSED) pour les 4 ménages
# Ces ménages ont refusé l'interview : aucun poids valide ne leur a été
# attribué par la Banque Mondiale. Exclusion justifiée.

base_mergee <- base_mergee %>%
  filter(!is.na(wt_wave4))

cat("Vérification poids manquants restants :", sum(is.na(base_mergee$wt_wave4)), "\n")

# ============================================================
#  8. Sauvegarde
# ============================================================

saveRDS(base_mergee, "data/processed/base_individus_w4.rds")

cat("Base finale sauvegardée :",
    nrow(base_mergee), "individus —",
    n_distinct(base_mergee$hhid), "ménages.\n")
# Attendu : 26 533 individus — 4 972 ménages
>>>>>>> 664df72ee5b586307c349b237310d9487e119661
