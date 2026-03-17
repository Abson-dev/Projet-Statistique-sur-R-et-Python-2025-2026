# =============================================================
# 03_analyse.R — Analyses Santé
# Questions 13 à 18
# =============================================================

# -- Chargement des données nettoyées
df_sante_a  <- readRDS(here("data", "processed", "df_sante_a_w4.rds"))
df_sante_b  <- readRDS(here("data", "processed", "df_sante_b_w4.rds"))
secta_w4    <- readRDS(here("data", "processed", "secta_w4.rds"))
cons_agg_w4 <- readRDS(here("data", "processed", "cons_agg_w4.rds"))


# ── QUESTION 13 ───────────────────────────────────────────────
# Taux de morbidité par sexe et groupe d'âge

df_q13 <- df_sante_a %>%
  filter(!is.na(s1q2), !is.na(s1q4)) %>%
  mutate(
    sexe = case_when(
      as.numeric(s1q2) == 1 ~ "Masculin",
      as.numeric(s1q2) == 2 ~ "Féminin"
    ),
    groupe_age = case_when(
      s1q4 <  15              ~ "0-14",
      s1q4 >= 15 & s1q4 < 30 ~ "15-29",
      s1q4 >= 30 & s1q4 < 45 ~ "30-44",
      s1q4 >= 45 & s1q4 < 60 ~ "45-59",
      s1q4 >= 60              ~ "60+"
    ),
    groupe_age = factor(groupe_age,
                        levels = c("0-14", "15-29", "30-44", "45-59", "60+")),
    # s3q1 : 1 = malade/blessé, 2 = non
    malade = as.numeric(s3q1) == 1
  ) %>%
  filter(!is.na(sexe), !is.na(groupe_age), !is.na(malade))

# -- Taux de morbidité par sexe
freq_sexe <- df_q13 %>%
  group_by(sexe) %>%
  summarise(
    n       = n(),
    n_malade = sum(malade),
    prop    = mean(malade),
    ic_bas  = binom.test(sum(malade), n())$conf.int[1],
    ic_haut = binom.test(sum(malade), n())$conf.int[2],
    .groups = "drop"
  )
sauvegarder_tab(freq_sexe, "Q13_morbidite_sexe.csv")

p_morbidite_sexe <- ggplot(freq_sexe,
                           aes(x = sexe, y = prop, fill = sexe)) +
  geom_col(alpha = 0.85) +
  geom_errorbar(aes(ymin = ic_bas, ymax = ic_haut), width = 0.2) +
  scale_y_continuous(labels = percent_format()) +
  labs(title   = "Taux de morbidité par sexe – W4",
       x = NULL, y = "Proportion (%)",
       caption = "Source : Nigeria GHS Panel W4") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "none")

sauvegarder_fig(p_morbidite_sexe, "Q13_morbidite_sexe.png")

# -- Taux de morbidité par groupe d'âge
freq_age <- df_q13 %>%
  group_by(groupe_age) %>%
  summarise(
    n        = n(),
    n_malade = sum(malade),
    prop     = mean(malade),
    ic_bas   = binom.test(sum(malade), n())$conf.int[1],
    ic_haut  = binom.test(sum(malade), n())$conf.int[2],
    .groups  = "drop"
  )

p_morbidite_age <- ggplot(freq_age,
                          aes(x = groupe_age, y = prop, fill = groupe_age)) +
  geom_col(alpha = 0.85) +
  geom_errorbar(aes(ymin = ic_bas, ymax = ic_haut), width = 0.2) +
  scale_y_continuous(labels = percent_format()) +
  labs(title   = "Taux de morbidité par groupe d'âge – W4",
       x = "Groupe d'âge", y = "Proportion (%)",
       caption = "Source : Nigeria GHS Panel W4") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "none")

sauvegarder_fig(p_morbidite_age, "Q13_morbidite_age.png")


# ── QUESTION 14 ───────────────────────────────────────────────
# Types de maladies déclarées (s3q3) — top 10

df_q14 <- df_sante_a %>%
  filter(!is.na(s3q3)) %>%
  mutate(
    categorie = case_when(
      as.numeric(s3q3) %in% c(1, 2, 3, 4, 5)    ~ "Infectieuse",
      as.numeric(s3q3) %in% c(6, 7, 8)            ~ "Traumatique",
      as.numeric(s3q3) %in% c(9, 10, 11, 12, 13)  ~ "Chronique",
      TRUE                                          ~ "Autre"
    )
  )

top10_maladies <- df_q14 %>%
  count(s3q3, categorie) %>%
  slice_max(n, n = 10) %>%
  mutate(s3q3 = fct_reorder(as.factor(s3q3), n))

sauvegarder_tab(as.data.frame(top10_maladies), "Q14_top10_maladies.csv")

p_maladies <- ggplot(top10_maladies,
                     aes(x = n, y = s3q3, fill = categorie)) +
  geom_col(alpha = 0.85) +
  geom_text(aes(label = n), hjust = -0.2, size = 3.5) +
  labs(title   = "10 affections les plus fréquentes – W4",
       x = "Effectif", y = "Code maladie",
       fill    = "Catégorie",
       caption = "Source : Nigeria GHS Panel W4") +
  theme_minimal(base_size = 12)

sauvegarder_fig(p_maladies, "Q14_types_maladies.png")


# ── QUESTION 15 ───────────────────────────────────────────────
# Recours aux soins par type de prestataire (s3q50)

df_q15 <- df_sante_b %>%
  filter(!is.na(s3q50)) %>%
  mutate(
    prestataire = case_when(
      as.numeric(s3q50) == 1 ~ "Hôpital public",
      as.numeric(s3q50) == 2 ~ "Clinique privée",
      as.numeric(s3q50) == 3 ~ "Pharmacie",
      as.numeric(s3q50) == 4 ~ "Tradipraticien",
      as.numeric(s3q50) == 5 ~ "Aucun",
      TRUE                   ~ "Autre"
    )
  )

freq_presta <- df_q15 %>%
  count(prestataire) %>%
  mutate(prestataire = fct_reorder(prestataire, n))

sauvegarder_tab(freq_presta, "Q15_prestataires.csv")

p_presta <- ggplot(freq_presta, aes(x = n, y = prestataire)) +
  geom_col(fill = "#26A69A", alpha = 0.85) +
  geom_text(aes(label = n), hjust = -0.2, size = 3.5) +
  labs(title   = "Recours aux soins par type de prestataire – W4",
       x = "Effectif", y = NULL,
       caption = "Source : Nigeria GHS Panel W4") +
  theme_minimal(base_size = 12)

sauvegarder_fig(p_presta, "Q15_prestataires.png")


# ── QUESTION 16 ───────────────────────────────────────────────
# Distribution des dépenses de santé
# s3q51_1 à s3q51_7 = dépenses par type de prestataire
# on prend s3q51_1 comme dépense principale

df_q16 <- df_sante_b %>%
  mutate(
    depense = rowSums(across(starts_with("s3q51_")), na.rm = TRUE),
    prestataire = case_when(
      as.numeric(s3q50) == 1 ~ "Hôpital public",
      as.numeric(s3q50) == 2 ~ "Clinique privée",
      as.numeric(s3q50) == 3 ~ "Pharmacie",
      as.numeric(s3q50) == 4 ~ "Tradipraticien",
      as.numeric(s3q50) == 5 ~ "Aucun",
      TRUE                   ~ "Autre"
    )
  ) %>%
  filter(depense > 0)

# -- Statistiques par décile
stats_decile <- df_q16 %>%
  reframe(
    decile = paste0("D", 1:10),
    valeur = quantile(depense, probs = seq(0.1, 1, 0.1), na.rm = TRUE)
  )
sauvegarder_tab(stats_decile, "Q16_stats_decile_depenses.csv")

# -- Histogramme échelle log
p_hist_dep <- ggplot(df_q16, aes(x = depense)) +
  geom_histogram(bins = 40, fill = "#42A5F5", color = "white", alpha = 0.85) +
  scale_x_log10(labels = comma_format()) +
  labs(title   = "Distribution des dépenses de santé (échelle log) – W4",
       x = "Dépenses (log)", y = "Effectif",
       caption = "Source : Nigeria GHS Panel W4") +
  theme_minimal(base_size = 12)

sauvegarder_fig(p_hist_dep, "Q16_histogramme_depenses.png")

# -- Boxplot par prestataire
p_box_dep <- ggplot(df_q16, aes(x = prestataire, y = depense,
                                fill = prestataire)) +
  geom_boxplot(alpha = 0.8, outlier.color = "grey50") +
  scale_y_log10(labels = comma_format()) +
  labs(title   = "Dépenses de santé par prestataire (échelle log) – W4",
       x = NULL, y = "Dépenses (log)",
       caption = "Source : Nigeria GHS Panel W4") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 30, hjust = 1))

sauvegarder_fig(p_box_dep, "Q16_boxplot_depenses_presta.png")


# ── QUESTION 17 ───────────────────────────────────────────────
# Recours aux soins × quintile de consommation

df_recours <- df_sante_b %>%
  mutate(
    consulte = case_when(
      as.numeric(s3q50) != 5 ~ "Consulté",
      as.numeric(s3q50) == 5 ~ "Non consulté",
      TRUE                   ~ NA_character_
    )
  ) %>%
  filter(!is.na(consulte))

df_q17 <- df_recours %>%
  left_join(cons_agg_w4 %>% select(hhid, totcons), by = "hhid") %>%
  filter(!is.na(totcons)) %>%
  mutate(quintile = paste0("Q", ntile(totcons, 5)))

# -- Tableau de contingence
tab_q17 <- table(df_q17$quintile, df_q17$consulte)
print(tab_q17)

# -- Chi-deux ou Fisher si effectifs < 5
if (any(tab_q17 < 5)) {
  print(fisher.test(tab_q17, simulate.p.value = TRUE))
} else {
  chi2_q17 <- chisq.test(tab_q17)
  print(chi2_q17)
  v_cramer <- sqrt(chi2_q17$statistic /
                     (sum(tab_q17) * (min(dim(tab_q17)) - 1)))
  message("V de Cramér = ", round(v_cramer, 3))
}

sauvegarder_tab(as.data.frame(tab_q17), "Q17_contingence_recours_quintile.csv")

# -- Barplot 100% empilé
p_recours <- df_q17 %>%
  count(quintile, consulte) %>%
  group_by(quintile) %>%
  mutate(prop = n / sum(n)) %>%
  ggplot(aes(x = quintile, y = prop, fill = consulte)) +
  geom_col(position = "fill", alpha = 0.85) +
  scale_y_continuous(labels = percent_format()) +
  labs(title   = "Recours aux soins par quintile de consommation – W4",
       x = "Quintile", y = "Proportion (%)",
       fill    = NULL,
       caption = "Source : Nigeria GHS Panel W4") +
  theme_minimal(base_size = 12)

sauvegarder_fig(p_recours, "Q17_recours_quintile.png")


# ── QUESTION 18 ───────────────────────────────────────────────
df_q18 <- df_q16 %>%
  mutate(zone_label = case_when(
    as.numeric(zone) == 1 ~ "Urbain",
    as.numeric(zone) == 2 ~ "Rural"
  )) %>%
  filter(!is.na(zone_label))

# -- Test de Wilcoxon
wilcox_q18 <- wilcox.test(depense ~ zone_label, data = df_q18)
print(wilcox_q18)

# -- Violin plot + boxplot superposé
p_violin <- ggplot(df_q18, aes(x = zone_label, y = depense,
                               fill = zone_label)) +
  geom_violin(alpha = 0.6, trim = FALSE) +
  geom_boxplot(width = 0.15, alpha = 0.9, outlier.size = 0.5) +
  scale_y_log10(labels = comma_format()) +
  scale_fill_manual(values = c("Rural" = "#66BB6A", "Urbain" = "#42A5F5")) +
  annotate("text", x = 1.5, y = max(df_q18$depense) * 0.8,
           label = paste0("p = ", round(wilcox_q18$p.value, 4)),
           size = 4) +
  labs(title   = "Dépenses de santé par zone (échelle log) – W4",
       x = NULL, y = "Dépenses (log)",
       caption = "Source : Nigeria GHS Panel W4") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "none")

sauvegarder_fig(p_violin, "Q18_violin_depenses_zone.png")