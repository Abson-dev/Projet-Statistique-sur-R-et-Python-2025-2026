# =============================================================
# 03_analyse.R — Analyses démographiques pondérées
# =============================================================

sect1_w4_clean <- readRDS(here("data", "processed", "sect1_w4_clean.rds"))
svy_indiv      <- readRDS(here("data", "processed", "svy_indiv_w4.rds"))
secta_w4       <- charger_dta("secta_harvest", 4)


# ── TÂCHE 2 : Analyse univariée de l'âge ─────────────────────

df_age <- sect1_w4_clean %>% filter(!is.na(s1q4))

stats_age <- data.frame(
  moyenne   = wtd.mean(df_age$s1q4, weights = df_age$wt_wave4),
  mediane   = wtd.quantile(df_age$s1q4, weights = df_age$wt_wave4, probs = 0.50),
  Q1        = wtd.quantile(df_age$s1q4, weights = df_age$wt_wave4, probs = 0.25),
  Q3        = wtd.quantile(df_age$s1q4, weights = df_age$wt_wave4, probs = 0.75),
  CV        = sqrt(wtd.var(df_age$s1q4, weights = df_age$wt_wave4)) /
    wtd.mean(df_age$s1q4, weights = df_age$wt_wave4) * 100,
  asymetrie = skewness(df_age$s1q4)
)
sauvegarder_tab(stats_age, "T2_stats_age.csv")

df_age_hist <- df_age %>%
  mutate(age_bin = cut(s1q4, breaks = seq(0, 120, by = 5), right = FALSE)) %>%
  group_by(age_bin) %>%
  summarise(effectif_pondere = sum(wt_wave4), .groups = "drop")

p_hist <- ggplot(df_age_hist, aes(x = age_bin, y = effectif_pondere)) +
  geom_col(fill = "#2196F3", color = "white") +
  labs(title   = "Distribution pondérée de l'âge des membres – W4",
       x       = "Âge (années)",
       y       = "Effectif pondéré",
       caption = "Source : Nigeria GHS Panel W4 | Pondérations wt_wave4") +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))
sauvegarder_fig(p_hist, "T2_histogramme_age.png")

p_box <- ggplot(df_age, aes(y = s1q4, weight = wt_wave4)) +
  geom_boxplot(fill = "#42A5F5") +
  labs(title   = "Boîte à moustaches pondérée de l'âge – W4",
       y       = "Âge (années)",
       caption = "Source : Nigeria GHS Panel W4 | Pondérations wt_wave4") +
  theme_minimal(base_size = 12)
sauvegarder_fig(p_box, "T2_boxplot_age.png")

set.seed(123)
shapiro_sample <- if (nrow(df_age) > 5000) sample(df_age$s1q4, 5000) else df_age$s1q4
shapiro.test(shapiro_sample)


# ── TÂCHE 3 : Pyramide des âges pondérée ─────────────────────

df_pyramide <- sect1_w4_clean %>%
  filter(!is.na(s1q4), !is.na(s1q2)) %>%
  mutate(
    groupe_age = cut(s1q4,
                     breaks = seq(0, 100, by = 5),
                     right  = FALSE,
                     labels = paste(seq(0, 95, by = 5),
                                    seq(4, 99, by = 5), sep = "-")),
    sexe = case_when(
      as.numeric(s1q2) == 1 ~ "Masculin",
      as.numeric(s1q2) == 2 ~ "Féminin"
    )
  ) %>%
  filter(!is.na(groupe_age), !is.na(sexe)) %>%
  group_by(groupe_age, sexe) %>%
  summarise(n = sum(wt_wave4), .groups = "drop") %>%
  tidyr::uncount(weights = round(n))

p_pyramide <- age_pyramid(
  data         = df_pyramide,
  age_group    = "groupe_age",
  split_by     = "sexe",
  proportional = TRUE
) +
  labs(title    = "Pyramide des âges pondérée – Nigeria GHS Panel W4 (2018)",
       subtitle = "Groupes quinquennaux par sexe",
       x        = "Proportion pondérée (%)",
       y        = "Groupe d'âge",
       fill     = "Sexe",
       caption  = "Source : Nigeria GHS Panel W4 | Pondérations wt_wave4")
sauvegarder_fig(p_pyramide, "T3_pyramide_ages_W4.png",
                largeur = 10, hauteur = 12)


# ── TÂCHE 4 : Fréquences du lien de parenté ──────────────────

df_parente <- sect1_w4_clean %>%
  mutate(
    parente = case_when(
      as.numeric(s1q3) == 1          ~ "Chef de ménage",
      as.numeric(s1q3) == 2          ~ "Conjoint(e)",
      as.numeric(s1q3) %in% c(3,4,5) ~ "Enfant",
      TRUE                            ~ "Autre"
    )
  ) %>%
  filter(!is.na(parente))

n_pond_total <- sum(df_parente$wt_wave4)
n_brut_total <- nrow(df_parente)

freq_parente <- df_parente %>%
  group_by(parente) %>%
  summarise(
    n_pondere = sum(wt_wave4),
    n_brut    = n(),
    .groups   = "drop"
  ) %>%
  mutate(
    prop    = n_pondere / n_pond_total,
    ic_bas  = mapply(function(x) binom.test(x, n_brut_total)$conf.int[1], n_brut),
    ic_haut = mapply(function(x) binom.test(x, n_brut_total)$conf.int[2], n_brut),
    parente = fct_reorder(parente, prop)
  )
sauvegarder_tab(freq_parente, "T4_frequences_parente.csv")

p_parente <- ggplot(freq_parente, aes(x = prop, y = parente)) +
  geom_col(fill = "#26A69A", alpha = 0.85) +
  geom_errorbar(aes(xmin = ic_bas, xmax = ic_haut),
                width = 0.2, color = "#004D40") +
  geom_text(aes(label = paste0(round(prop * 100, 1), "%")),
            hjust = -0.2, size = 3.8) +
  scale_x_continuous(labels = percent_format(),
                     expand = expansion(mult = c(0, 0.1))) +
  labs(title   = "Lien de parenté des membres – W4 (pondéré)",
       x       = "Proportion pondérée (%)",
       y       = NULL,
       caption = "Source : Nigeria GHS Panel W4 | Pondérations wt_wave4")
sauvegarder_fig(p_parente, "T4_lien_parente.png")


# ── TÂCHE 5 : Taille des ménages rural/urbain + Wilcoxon ─────

taille_menage <- sect1_w4_clean %>%
  group_by(hhid, wt_wave4) %>%
  summarise(taille = n(), .groups = "drop")

poids_hh <- secta_w4 %>%
  select(hhid, sector, wt_wave4) %>%
  distinct(hhid, .keep_all = TRUE)

df_taille <- taille_menage %>%
  left_join(poids_hh %>% select(hhid, sector), by = "hhid") %>%
  mutate(zone_label = case_when(
    as.numeric(sector) == 1 ~ "Urbain",
    as.numeric(sector) == 2 ~ "Rural",
    TRUE                    ~ NA_character_
  )) %>%
  filter(!is.na(zone_label))

svy_taille <- svydesign(ids = ~1, weights = ~wt_wave4, data = df_taille)

wilcox_res <- svyranktest(taille ~ zone_label, design = svy_taille)
print(wilcox_res)

r_effet <- abs(qnorm(wilcox_res$p.value / 2)) / sqrt(nrow(df_taille))
message("Taille d'effet r = ", round(r_effet, 3))

p_taille <- ggplot(df_taille, aes(x = zone_label, y = taille,
                                  fill = zone_label, weight = wt_wave4)) +
  geom_boxplot(alpha = 0.8) +
  annotate("text", x = 1.5, y = max(df_taille$taille) * 0.95,
           label = paste0("p = ", round(wilcox_res$p.value, 4)),
           size = 4) +
  scale_fill_manual(values = c("Rural" = "#66BB6A", "Urbain" = "#42A5F5")) +
  labs(title   = "Taille des ménages par zone – W4 (pondéré)",
       x       = NULL,
       y       = "Nombre de membres",
       fill    = "Zone",
       caption = "Source : Nigeria GHS Panel W4 | Pondérations wt_wave4")
sauvegarder_fig(p_taille, "T5_boxplot_taille_zone.png")


# ── TÂCHE 6 ───────────────────────────────────────────────────

df_summary <- sect1_w4_clean %>%
  mutate(
    sexe_label = case_when(
      as.numeric(s1q2) == 1 ~ "Masculin",
      as.numeric(s1q2) == 2 ~ "Féminin"
    )
  ) %>%
  left_join(taille_menage %>% select(hhid, taille), by = "hhid") %>%
  mutate(zone_label = case_when(
    as.numeric(sector) == 1 ~ "Urbain",
    as.numeric(sector) == 2 ~ "Rural"
  )) %>%
  filter(!is.na(zone_label)) %>%
  select(s1q4, sexe_label, taille, zone_label, wt_wave4)

svy_summary <- svydesign(ids = ~1, weights = ~wt_wave4, data = df_summary)

tableau <- tbl_svysummary(
  data      = svy_summary,
  by        = "zone_label",
  label     = list(
    s1q4       ~ "Âge (années)",
    sexe_label ~ "Sexe",
    taille     ~ "Taille du ménage"
  ),
  statistic = list(
    all_continuous()  ~ "{mean} ({sd})",
    all_categorical() ~ "{n_unweighted} ({p}%)"
  ),
  include   = c("s1q4", "sexe_label", "taille")
) %>%
  add_p() %>%
  add_overall() %>%
  bold_labels()

print(tableau)

tableau %>%
  as_gt() %>%
  gt::gtsave(here("output", "tables", "T6_tableau_gtsummary.png"))

message("Tableau sauvegardé : T6_tableau_gtsummary.png")