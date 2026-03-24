#==================================================
# TP - ANALYSE 1 (AVEC PONDERATION) - VERSION CORRIGEE
# Profil démographique des ménages nigérians
# Wave 4 (2018) - Nigeria GHS Panel
# Poids : wt_wave4 (sectaa_harvestw4.dta)
#==================================================

# install.packages("patchwork")
# install.packages("coin")
# install.packages("survey")
# install.packages("srvyr")

setwd("D:/ENSAE/ISE_1_SEMESTRE_2/R/mon_projet")

library(haven)
library(dplyr)
library(ggplot2)
library(apyramid)
library(naniar)
library(gtsummary)
library(rstatix)
library(PropCIs)
library(patchwork)
library(survey)    # analyses pondérées
library(srvyr)     # version tidyverse de survey

# ── CHARGEMENT DES DONNÉES ───────────────────────────────────────────────────

sect1_harvestw4 <- read_dta(
  "data/raw/NGA_2018_GHSP-W4_v03_M_Stata12/sect1_harvestw4.dta"
)

sectaa_harvestw4 <- read_dta(
  "data/raw/NGA_2018_GHSP-W4_v03_M_Stata12/sectaa_harvestw4.dta"
)

# ── EXPLORATION RAPIDE ───────────────────────────────────────────────────────
str(sect1_harvestw4)
glimpse(sect1_harvestw4)
summary(sect1_harvestw4)

# Variables de pondération disponibles dans sectaa
names(sectaa_harvestw4)
# wt_wave4 = poids de sondage Wave 4
# strata   = strates
# cluster  = grappes (EA)
summary(sectaa_harvestw4$wt_wave4)

# ── TÂCHE 1 : VÉRIFICATION DOUBLONS ET VALEURS MANQUANTES ───────────────────

duplicates <- sect1_harvestw4 %>%
  group_by(hhid, indiv) %>%
  filter(n() > 1) %>%
  ungroup()

cat("Nombre de doublons sur (hhid, indiv) :", nrow(duplicates), "\n")

# Carte des valeurs manquantes
naniar::vis_miss(
  sect1_harvestw4 %>% select(hhid, indiv, s1q2, s1q3, s1q4),
  warn_large_data = FALSE
) +
  labs(title = "Valeurs manquantes -- Variables demographiques cles",
       subtitle = "sect1_harvestw4 | Wave 4 (2018)")

# ── JOINTURE AVEC LES POIDS ──────────────────────────────────────────────────

# sectaa contient les poids au niveau ménage (une ligne par ménage)
# On joint sur hhid pour attribuer le poids à chaque individu

poids <- sectaa_harvestw4 %>%
  select(hhid, wt_wave4, strata, cluster) %>%
  distinct(hhid, .keep_all = TRUE)   # garder une ligne par ménage

sect1_w <- sect1_harvestw4 %>%
  left_join(poids, by = "hhid") %>%
  mutate(
    sexe_fct = as_factor(s1q2),
    lien_fct = as_factor(s1q3),
    age      = as.numeric(s1q4),
    zone     = case_when(
      sector == 1 ~ "Rural",
      sector == 2 ~ "Urbain",
      TRUE        ~ NA_character_
    ),
    age_group = cut(
      age,
      breaks = c(0,4,9,14,19,24,29,34,39,44,49,54,59,64,Inf),
      labels = c("0-4","5-9","10-14","15-19","20-24","25-29",
                 "30-34","35-39","40-44","45-49","50-54","55-59","60-64","65+"),
      right = TRUE
    )
  ) %>%
  group_by(hhid) %>%
  mutate(taille_menage = n()) %>%
  ungroup()

# Vérification des poids manquants
cat("Individus sans poids :", sum(is.na(sect1_w$wt_wave4)), "\n")

# ── CRÉATION DU PLAN DE SONDAGE ──────────────────────────────────────────────

plan_sondage <- sect1_w %>%
  filter(!is.na(wt_wave4)) %>%
  as_survey_design(
    ids     = cluster,
    strata  = strata,
    weights = wt_wave4,
    nest    = TRUE
  )

cat("Plan de sondage créé :\n")
print(plan_sondage)

# ── TÂCHE 2 : STATISTIQUES DESCRIPTIVES DE L'ÂGE (PONDÉRÉES) ───────────────

age_stats_w <- plan_sondage %>%
  summarise(
    moyenne = survey_mean(age, na.rm = TRUE),
    mediane = survey_quantile(age, quantiles = 0.5, na.rm = TRUE),
    Q1      = survey_quantile(age, quantiles = 0.25, na.rm = TRUE),
    Q3      = survey_quantile(age, quantiles = 0.75, na.rm = TRUE),
    sd      = survey_sd(age, na.rm = TRUE)
  )

# Statistiques non pondérées pour comparaison
age_stats_nw <- sect1_w %>%
  summarise(
    moyenne   = mean(age, na.rm = TRUE),
    mediane   = median(age, na.rm = TRUE),
    Q1        = quantile(age, 0.25, na.rm = TRUE),
    Q3        = quantile(age, 0.75, na.rm = TRUE),
    sd        = sd(age, na.rm = TRUE),
    cv        = sd / moyenne * 100,
    asymetrie = (3 * (moyenne - mediane)) / sd
  )

cat("=== Statistiques de l'âge (non pondérées) ===\n")
print(age_stats_nw)

cat("=== Statistiques de l'âge (pondérées) ===\n")
print(age_stats_w)

# Histogramme pondéré
p1 <- ggplot(sect1_w %>% filter(!is.na(age), !is.na(wt_wave4)),
             aes(x = age, weight = wt_wave4)) +
  geom_histogram(binwidth = 5, fill = "steelblue", color = "white", alpha = 0.7) +
  geom_vline(xintercept = age_stats_nw$moyenne,
             color = "red", linetype = "dashed", linewidth = 1) +
  geom_vline(xintercept = age_stats_nw$mediane,
             color = "darkgreen", linetype = "dashed", linewidth = 1) +
  annotate("text", x = age_stats_nw$moyenne + 3, y = Inf,
           label = "Moyenne", vjust = 2, color = "red", size = 3.5) +
  annotate("text", x = age_stats_nw$mediane - 3, y = Inf,
           label = "Mediane", vjust = 4, color = "darkgreen", size = 3.5) +
  labs(title = "Distribution ponderee de l'age des membres",
       subtitle = "Poids : wt_wave4",
       x = "Age (ans)", y = "Effectif pondere") +
  theme_minimal(base_size = 12)

p2 <- ggplot(sect1_w %>% filter(!is.na(age), !is.na(wt_wave4)),
             aes(y = age)) +
  geom_boxplot(fill = "steelblue", alpha = 0.7,
               outlier.colour = "red", outlier.alpha = 0.4) +
  labs(title = "Boxplot de l'age", y = "Age (ans)") +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())

p1 + p2 + plot_layout(widths = c(3, 1))

# Test de Shapiro-Wilk (sur échantillon)
set.seed(123)
age_sample <- sample(
  sect1_w$age[!is.na(sect1_w$age)],
  min(5000, sum(!is.na(sect1_w$age)))
)
shapiro_test <- shapiro.test(age_sample)
print(shapiro_test)

# ── TÂCHE 3 : PYRAMIDE DES ÂGES PONDÉRÉE ────────────────────────────────────

pyramide_data <- plan_sondage %>%
  filter(!is.na(age_group), !is.na(sexe_fct)) %>%
  group_by(age_group, sexe_fct) %>%
  summarise(
    effectif_pondere = survey_total(na.rm = TRUE),
    .groups = "drop"
  )

cat("=== Effectifs pondérés par groupe d'âge et sexe ===\n")
print(pyramide_data)

sect1_w_pyr <- sect1_w %>%
  filter(!is.na(age_group), !is.na(sexe_fct), !is.na(wt_wave4))

pyramide_apy <- age_pyramid(
  data      = sect1_w_pyr,
  age_group = "age_group",
  split_by  = "sexe_fct",
  count     = "wt_wave4"
) +
  labs(
    title    = "Pyramide des ages ponderee -- Wave 4 (2018)",
    subtitle = "Poids : wt_wave4 | Nigeria GHS Panel",
    x        = "Effectif pondere",
    y        = "Groupe d'age",
    fill     = "Sexe"
  ) +
  theme_minimal(base_size = 12)

pyramide_apy

# ── TÂCHE 4 : LIEN DE PARENTÉ PONDÉRÉ ───────────────────────────────────────

freq_lien_w <- plan_sondage %>%
  filter(!is.na(lien_fct)) %>%
  group_by(lien_fct) %>%
  summarise(
    n_pondere = survey_total(na.rm = TRUE),
    prop      = survey_prop(na.rm = TRUE, vartype = "ci")
  ) %>%
  arrange(desc(prop))

cat("=== Fréquences pondérées du lien de parenté ===\n")
print(freq_lien_w)

ggplot(freq_lien_w,
       aes(x = prop * 100, y = reorder(lien_fct, prop))) +
  geom_col(fill = "steelblue", alpha = 0.75, width = 0.7) +
  geom_errorbar(
    aes(xmin = prop_low * 100, xmax = prop_upp * 100),
    width = 0.3, color = "#1a5276", linewidth = 0.8
  ) +
  geom_text(
    aes(label = paste0(round(prop * 100, 1), "%")),
    hjust = -0.2, size = 3.5, color = "#1a5276"
  ) +
  scale_x_continuous(
    limits = c(0, max(freq_lien_w$prop_upp) * 100 * 1.15),
    labels = function(x) paste0(x, "%")
  ) +
  labs(
    title    = "Distribution ponderee du lien de parente",
    subtitle = "IC a 95% (plan de sondage complexe) -- Wave 4",
    x        = "Proportion ponderee (%)",
    y        = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold"),
        panel.grid.major.y = element_blank())

# ── TÂCHE 5 : TAILLE DES MÉNAGES RURAL VS URBAIN (PONDÉRÉE) ─────────────────

taille_zone_w <- plan_sondage %>%
  filter(!is.na(zone)) %>%
  group_by(zone) %>%
  summarise(
    moy_ponderee = survey_mean(taille_menage, na.rm = TRUE, vartype = "ci"),
    med_ponderee = survey_quantile(taille_menage, quantiles = 0.5, na.rm = TRUE)
  )

cat("=== Taille moyenne pondérée des ménages par zone ===\n")
print(taille_zone_w)

ggplot(sect1_w %>% filter(!is.na(zone)),
       aes(x = zone, y = taille_menage, fill = zone)) +
  geom_boxplot(alpha = 0.7,
               outlier.colour = "red", outlier.alpha = 0.4) +
  scale_fill_manual(
    values = c("Rural" = "forestgreen", "Urbain" = "orange"),
    guide  = "none"
  ) +
  labs(
    title    = "Taille des menages par zone de residence",
    subtitle = "Wave 4 (2018) -- Nigeria GHS Panel",
    x        = "Zone",
    y        = "Nombre de membres du menage"
  ) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold"))

# Test de Wilcoxon non pondéré
wilcox_result <- wilcox.test(
  taille_menage ~ zone,
  data  = sect1_w %>% filter(!is.na(zone)),
  exact = FALSE
)
print(wilcox_result)

# Taille d'effet
effet <- sect1_w %>%
  filter(!is.na(zone)) %>%
  wilcox_effsize(taille_menage ~ zone)
print(effet)

# Test de Student pondéré (svyttest)
svyt_taille <- svyttest(
  taille_menage ~ zone,
  design = plan_sondage %>% filter(!is.na(zone))
)
cat("\n=== Test de Student pondéré (svyttest) : Taille ménage Rural vs Urbain ===\n")
print(svyt_taille)

# ── TÂCHE 6 : TABLEAU GTSUMMARY PONDÉRÉ ─────────────────────────────────────
# CORRECTION : tbl_summary() avec weight = ~wt_wave4
# (tbl_svysummary nécessite un objet survey.design, pas un tibble)

tbl_w <- sect1_w %>%
  filter(!is.na(zone), !is.na(wt_wave4)) %>%
  mutate(
    sexe = factor(as.numeric(s1q2),
                  levels = c(1, 2),
                  labels = c("Homme", "Femme"))
  ) %>%
  select(zone, age, sexe, taille_menage, wt_wave4) %>%
  tbl_summary(
    by        = zone,
    include   = c(age, sexe, taille_menage),
    statistic = list(
      all_continuous()  ~ "{mean} ({sd})",
      all_categorical() ~ "{n} ({p}%)"
    ),
    digits    = all_continuous() ~ 1,
    label     = list(
      age           ~ "Age (annees)",
      sexe          ~ "Sexe",
      taille_menage ~ "Taille du menage"
    ),
    weight    = ~wt_wave4   # ← poids directement, sans plan complexe
  ) %>%
  add_p() %>%
  add_overall() %>%
  bold_labels() %>%
  modify_caption(
    "**Tableau 1 -- Caracteristiques demographiques ponderees par zone (Wave 4)**"
  ) %>%
  modify_header(label ~ "**Variable**")

print(tbl_w)
