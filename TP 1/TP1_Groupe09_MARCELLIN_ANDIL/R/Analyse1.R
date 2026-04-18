#==================================================
# TP ISE1 - ANALYSE 1
# Profil démographique des ménages nigérians
# Nigeria GHS Panel - Wave 4 (2018)
# Poids : wt_wave4 issu de sectaa_harvestw4.dta
#==================================================

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
library(scales)
library(survey)

# ── CHARGEMENT DES POIDS ─────────────────────────────────────────────────────
sectaa_harvestw4 <- read_dta(
  "data/raw/NGA_2018_GHSP-W4_v03_M_Stata12/sectaa_harvestw4.dta"
)

poids <- sectaa_harvestw4 %>%
  select(hhid, wt_wave4) %>%
  distinct(hhid, .keep_all = TRUE)

cat("Poids chargés :", nrow(poids), "ménages\n")
summary(poids$wt_wave4)

# ── CHARGEMENT DES DONNÉES ────────────────────────────────────────────────────
sect1_harvestw4 <- read_dta(
  "data/raw/NGA_2018_GHSP-W4_v03_M_Stata12/sect1_harvestw4.dta"
)

# ── MERGE avec les poids ──────────────────────────────────────────────────────
sect1_w <- sect1_harvestw4 %>%
  left_join(poids, by = "hhid") %>%
  mutate(
    lien_fct  = as_factor(s1q3),
    age       = as.numeric(s1q4),
    zone      = case_when(
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
    ),
    sexe_label = case_when(
      as.numeric(s1q2) == 1 ~ "Homme",
      as.numeric(s1q2) == 2 ~ "Femme",
      TRUE ~ NA_character_
    ),
    sexe_label = factor(sexe_label, levels = c("Homme", "Femme"))
  ) %>%
  group_by(hhid) %>%
  mutate(taille_menage = n()) %>%
  ungroup()

cat("Individus sans poids après merge :", sum(is.na(sect1_w$wt_wave4)), "\n")

# ── TÂCHE 1 : Doublons et valeurs manquantes ──────────────────────────────────
duplicates <- sect1_harvestw4 %>%
  group_by(hhid, indiv) %>%
  filter(n() > 1) %>%
  ungroup()
cat("Nombre de doublons :", nrow(duplicates), "\n")

naniar::vis_miss(
  sect1_harvestw4 %>% select(hhid, indiv, s1q2, s1q3, s1q4),
  warn_large_data = FALSE
) +
  labs(title = "Valeurs manquantes -- Variables demographiques cles",
       subtitle = "sect1_harvestw4 | Wave 4 (2018)")

# ── TÂCHE 2 : Statistiques descriptives de l'âge ─────────────────────────────

# Stats non pondérées
age_stats <- sect1_w %>%
  summarise(
    moyenne   = mean(age, na.rm = TRUE),
    mediane   = median(age, na.rm = TRUE),
    Q1        = quantile(age, 0.25, na.rm = TRUE),
    Q3        = quantile(age, 0.75, na.rm = TRUE),
    sd        = sd(age, na.rm = TRUE),
    cv        = sd / moyenne * 100,
    asymetrie = (3 * (moyenne - mediane)) / sd
  )
print(age_stats)

# Stats pondérées
age_stats_w <- sect1_w %>%
  filter(!is.na(age), !is.na(wt_wave4)) %>%
  summarise(
    moyenne_w = weighted.mean(age, wt_wave4, na.rm = TRUE),
    sd_w      = sqrt(sum(wt_wave4 * (age - weighted.mean(age, wt_wave4, na.rm = TRUE))^2,
                         na.rm = TRUE) / sum(wt_wave4, na.rm = TRUE))
  )
cat("=== Statistiques pondérées de l'âge ===\n")
print(age_stats_w)

# Histogramme pondéré
p1 <- ggplot(sect1_w %>% filter(!is.na(age), !is.na(wt_wave4)),
             aes(x = age, weight = wt_wave4)) +
  geom_histogram(binwidth = 5, fill = "steelblue", color = "white", alpha = 0.7) +
  geom_vline(xintercept = age_stats$moyenne,
             color = "red", linetype = "dashed", linewidth = 1) +
  geom_vline(xintercept = age_stats$mediane,
             color = "green", linetype = "dashed", linewidth = 1) +
  annotate("text", x = age_stats$moyenne + 2, y = Inf,
           label = "Moyenne", vjust = 2, color = "red") +
  annotate("text", x = age_stats$mediane + 2, y = Inf,
           label = "Médiane", vjust = 4, color = "green") +
  labs(title = "Distribution pondérée de l'âge des membres",
       subtitle = "Poids : wt_wave4",
       x = "Âge (ans)", y = "Effectif pondéré") +
  theme_minimal(base_size = 12)

p2 <- ggplot(sect1_w %>% filter(!is.na(age), !is.na(wt_wave4)),
             aes(y = age)) +
  geom_boxplot(fill = "steelblue", alpha = 0.7,
               outlier.colour = "red", outlier.alpha = 0.4) +
  labs(title = "Boxplot de l'âge", y = "Âge (ans)") +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())

p1 + p2 + plot_layout(widths = c(3, 1))

# Test de Shapiro-Wilk
set.seed(123)
age_sample <- sample(sect1_w$age[!is.na(sect1_w$age)],
                     min(5000, sum(!is.na(sect1_w$age))))
shapiro_test <- shapiro.test(age_sample)
print(shapiro_test)

# ── TÂCHE 3 : Pyramide des âges pondérée (CORRIGÉE) ──────────────────────────
sect1_w_pyr <- sect1_w %>%
  filter(!is.na(age_group), !is.na(sexe_label), !is.na(wt_wave4))

age_pyramid(
  data      = sect1_w_pyr,
  age_group = "age_group",
  split_by  = "sexe_label",
  count     = "wt_wave4"
) +
  scale_fill_manual(
    values = c("Homme" = "#2874a6", "Femme" = "#e74c3c"),
    name   = "Sexe"
  ) +
  labs(
    title    = "Pyramide des âges pondérée -- Wave 4 (2018)",
    subtitle = "Poids : wt_wave4 | Nigeria GHS Panel",
    x        = "Effectif pondéré",
    y        = "Groupe d'âge",
    fill     = "Sexe"
  ) +
  theme_minimal(base_size = 12)

cat("Commentaire : La pyramide pondérée montre une base large (0-14 ans),
caractéristique d'une population jeune en croissance. Les effectifs pondérés
représentent la population réelle nigériane et non l'échantillon brut.\n")

# ── TÂCHE 4 : Lien de parenté pondéré ────────────────────────────────────────
freq_lien <- sect1_w %>%
  filter(!is.na(lien_fct), !is.na(wt_wave4)) %>%
  group_by(lien_fct) %>%
  summarise(n_pondere = sum(wt_wave4), .groups = "drop") %>%
  mutate(proportion = n_pondere / sum(n_pondere) * 100) %>%
  arrange(desc(proportion))

n_total <- sect1_w %>% filter(!is.na(lien_fct)) %>% nrow()
freq_lien_ci <- sect1_w %>%
  filter(!is.na(lien_fct)) %>%
  count(lien_fct) %>%
  mutate(
    ci_lower = sapply(n, function(x) binom.test(x, n_total)$conf.int[1]) * 100,
    ci_upper = sapply(n, function(x) binom.test(x, n_total)$conf.int[2]) * 100
  )

freq_lien <- freq_lien %>%
  left_join(freq_lien_ci %>% select(lien_fct, ci_lower, ci_upper), by = "lien_fct")

print(freq_lien)

ggplot(freq_lien,
       aes(x = proportion, y = reorder(lien_fct, proportion))) +
  geom_col(fill = "steelblue", alpha = 0.75, width = 0.7) +
  geom_errorbar(aes(xmin = ci_lower, xmax = ci_upper),
                width = 0.3, color = "#1a5276", linewidth = 0.8) +
  geom_text(aes(label = paste0(round(proportion, 1), "%")),
            hjust = -0.2, size = 3.5, color = "#1a5276") +
  scale_x_continuous(limits = c(0, max(freq_lien$ci_upper) * 1.15),
                     labels = function(x) paste0(x, "%")) +
  labs(
    title    = "Distribution pondérée du lien de parenté",
    subtitle = "IC à 95% -- Wave 4 (2018)",
    x        = "Proportion pondérée (%)",
    y        = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold"),
        panel.grid.major.y = element_blank())

# ── TÂCHE 5 : Taille des ménages rural vs urbain ──────────────────────────────
taille_zone_w <- sect1_w %>%
  filter(!is.na(zone), !is.na(wt_wave4)) %>%
  group_by(zone) %>%
  summarise(
    moy_ponderee = weighted.mean(taille_menage, wt_wave4, na.rm = TRUE),
    .groups      = "drop"
  )
print(taille_zone_w)

ggplot(sect1_w %>% filter(!is.na(zone)),
       aes(x = zone, y = taille_menage, fill = zone)) +
  geom_boxplot(alpha = 0.7, outlier.colour = "red", outlier.alpha = 0.4) +
  scale_fill_manual(values = c("Rural" = "forestgreen", "Urbain" = "orange"),
                    guide = "none") +
  labs(
    title    = "Taille des ménages par zone de résidence",
    subtitle = "Wave 4 (2018) -- Nigeria GHS Panel",
    x        = "Zone",
    y        = "Nombre de membres du ménage"
  ) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold"))

wilcox_result <- wilcox.test(taille_menage ~ zone,
                             data  = sect1_w %>% filter(!is.na(zone)),
                             exact = FALSE)
print(wilcox_result)

effet <- sect1_w %>%
  filter(!is.na(zone)) %>%
  wilcox_effsize(taille_menage ~ zone)
print(effet)

# ── TÂCHE 6 : Tableau gtsummary pondéré ──────────────────────────────────────
plan_tbl1 <- sect1_w %>%
  filter(!is.na(zone), !is.na(wt_wave4)) %>%
  mutate(
    sexe = factor(as.numeric(s1q2),
                  levels = c(1, 2),
                  labels = c("Homme", "Femme"))
  ) %>%
  svydesign(
    ids     = ~1,
    weights = ~wt_wave4,
    data    = .
  )

tbl_1 <- tbl_svysummary(
  data      = plan_tbl1,
  by        = zone,
  include   = c(age, sexe, taille_menage),
  statistic = list(
    all_continuous()  ~ "{mean} ({sd})",
    all_categorical() ~ "{n_unweighted} ({p}%)"
  ),
  digits    = all_continuous() ~ 1,
  label     = list(
    age           ~ "Âge (années)",
    sexe          ~ "Sexe",
    taille_menage ~ "Taille du ménage"
  )
) %>%
  add_p() %>%
  add_overall() %>%
  bold_labels() %>%
  modify_caption(
    "**Tableau 1 -- Caractéristiques démographiques pondérées par zone (Wave 4)**"
  ) %>%
  modify_header(label ~ "**Variable**")

print(tbl_1)
