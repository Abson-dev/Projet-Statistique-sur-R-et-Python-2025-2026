# Script des analyses
# Analyses statistiques pondérées et visualisations , TP3 Santé

# Palette TP3 — couleur OMS (WHO World Health Statistics)
col_oms       <- "#009EDB"
col_cyan      <- "#00ACC1"
col_urgence   <- "#E53935"
col_public    <- "#1565C0"
col_prive     <- "#2E7D32"
col_ocre      <- "#F9A825"
col_orange    <- "#EF6C00"
col_homme     <- "#0058AB"
col_femme     <- "#E8416F"
col_rural     <- "#80BD41"
col_urbain    <- "#6C757D"

# Palette prestataires (public/privé/autre)
pal_presta <- c("Public" = col_public, "Privé" = col_prive, "Autre" = col_ocre)

# Palette groupes d'âge (séquentielle bleue)
pal_age <- c("0-14" = "#B2EBF2", "15-34" = "#4DD0E1",
             "35-59" = "#0097A7", "60+" = "#006064")

# Palette catégories de maladies
pal_cat <- c("Infectieuse" = col_urgence, "Respiratoire" = col_cyan,
             "Traumatique" = col_orange, "Chronique" = col_public,
             "Autre" = "grey60")

theme_tp3 <- theme_minimal(base_size = 10) +
  theme(
    plot.title    = element_text(size = 11, face = "bold", hjust = 0),
    plot.subtitle = element_text(size = 9, color = "grey40"),
    plot.caption  = element_text(size = 7, color = "grey50", hjust = 1),
    axis.title    = element_text(size = 9),
    legend.position = "bottom"
  )
theme_set(theme_tp3)

# Chargement
df_sante   <- readRDS(here("data", "processed", "df_sante_clean.rds"))
plan_sante <- readRDS(here("data", "processed", "plan_sante.rds"))

# Fonction de mise en forme flextable selon le thème couleur OMS
style_lsms <- function(ft) {
  oms_header <- "#00ACC1"
  oms_clair  <- "#E0F4FD"
  blanc            <- "#FFFFFF"
  gris_bord        <- "#A0A0A0"
  nrow_ft          <- nrow(ft$body$dataset)
  ft |>
    flextable::font(fontname = "Garamond", part = "all") |>
    flextable::fontsize(size = 9, part = "body") |>
    flextable::fontsize(size = 9.5, part = "header") |>
    flextable::bg(bg = oms_header, part = "header") |>
    flextable::color(color = blanc, part = "header") |>
    flextable::bold(part = "header") |>
    flextable::align(align = "center", part = "header") |>
    flextable::align(j = 1, align = "left", part = "header") |>
    flextable::bg(i = seq(1, nrow_ft, 2), bg = blanc, part = "body") |>
    flextable::bg(i = seq(2, nrow_ft, 2), bg = oms_clair, part = "body") |>
    flextable::align(j = 1, align = "left", part = "body") |>
    flextable::bold(j = 1, part = "body") |>
    flextable::border_remove() |>
    flextable::border_outer(part = "all",
      border = officer::fp_border(color = oms_header, width = 1.5)) |>
    flextable::border_inner_h(part = "body",
      border = officer::fp_border(color = gris_bord, width = 0.4)) |>
    flextable::hline(part = "header",
      border = officer::fp_border(color = blanc, width = 0.6)) |>
    flextable::hline_bottom(part = "header",
      border = officer::fp_border(color = oms_header, width = 2)) |>
    flextable::padding(padding.left = 6, padding.right = 6,
                       padding.top = 3, padding.bottom = 3, part = "all")
}

# 13.  Taux de morbidité par sexe et groupe d'âge (pondéré)

plan_morb <- plan_sante |>
  filter(!is.na(malade))

# Taux global pondéré
taux_global <- plan_morb |>
  summarise(taux = survey_mean(malade, vartype = "ci", na.rm = TRUE))
cat(" Taux de morbidité pondéré global :",
    round(taux_global$taux * 100, 1), "%\n")

# Par sexe
taux_sexe <- plan_morb |>
  filter(!is.na(sexe)) |>
  group_by(sexe) |>
  summarise(
    n    = unweighted(n()),
    taux = survey_mean(malade, vartype = "ci", na.rm = TRUE)
  ) |>
  mutate(across(c(taux, taux_low, taux_upp), ~ . * 100))

cat("Taux de morbidité par sexe :\n")
print(taux_sexe)

fig01a <- ggplot(taux_sexe, aes(x = sexe, y = taux, fill = sexe)) +
  geom_col(alpha = 0.85, width = 0.5) +
  geom_errorbar(aes(ymin = taux_low, ymax = taux_upp),
                width = 0.12, linewidth = 0.7, color = col_oms) +
  geom_text(aes(label = paste0(round(taux, 1), "%")),
            vjust = -2, size = 4, fontface = "bold", color = col_oms) +
  scale_fill_manual(values = c(Homme = col_homme, Femme = col_femme),
                    guide = "none") +
  scale_y_continuous(limits = c(0, 18),
                     labels = percent_format(scale = 1)) +
  labs(
    title    = "Taux de morbidité déclarée par sexe (pondéré)",
    subtitle = "4 semaines de référence | IC 95% (plan de sondage)",
    x = NULL, y = "Taux de morbidité (%)",
    caption = source_ghs
  )

ggsave(here("outputs", "figures", "fig01a_morbidite_sexe.png"),
       fig01a, width = 7, height = 5, dpi = 300)
cat("fig01a exportée\n")

# Par groupe d'âge
taux_age <- plan_morb |>
  filter(!is.na(groupe_age)) |>
  group_by(groupe_age) |>
  summarise(
    n    = unweighted(n()),
    taux = survey_mean(malade, vartype = "ci", na.rm = TRUE)
  ) |>
  mutate(across(c(taux, taux_low, taux_upp), ~ . * 100))

cat("Taux de morbidité par groupe d'âge :\n")
print(taux_age)

fig01b <- ggplot(taux_age, aes(x = groupe_age, y = taux, fill = groupe_age)) +
  geom_col(alpha = 0.85, width = 0.6) +
  geom_errorbar(aes(ymin = taux_low, ymax = taux_upp),
                width = 0.15, linewidth = 0.7, color = col_oms) +
  geom_text(aes(label = paste0(round(taux, 1), "%")),
            vjust = -2, size = 3.8, fontface = "bold", color = col_oms) +
  scale_fill_manual(values = pal_age, guide = "none") +
  scale_y_continuous(limits = c(0, 35),
                     labels = percent_format(scale = 1)) +
  labs(
    title    = "Taux de morbidité déclarée par groupe d'âge (pondéré)",
    subtitle = "4 semaines de référence | IC 95% (plan de sondage)",
    x = "Groupe d'âge", y = "Taux de morbidité (%)",
    caption = source_ghs
  )

ggsave(here("outputs", "figures", "fig01b_morbidite_age.png"),
       fig01b, width = 8, height = 5.5, dpi = 300)
cat("fig01b exportée\n")


fig01_combined <- (fig01a | fig01b) +
  plot_annotation(
    title   = "Taux de morbidité déclarée par groupe d'âge et par groupe d'âge (pondéré)",
    caption = source_ghs,
    theme   = theme(
      plot.title   = element_text(size = 12, face = "bold"),
      plot.caption = element_text(size = 7, color = "grey50", hjust = 1)
    )
  )

ggsave(here("outputs", "figures", "fig01_morbidite_combined.png"),
       fig01_combined, width = 14, height = 5.5, dpi = 300)
cat("fig01 combinée exportée\n")

#  14. Types de maladies déclarées (top 10)
# La population est celle des membres malades avec s4aq3b_1 renseigné

df_t14 <- df_sante |>
  filter(malade == 1, !is.na(s4aq3b_1)) |>
  mutate(
    type_maladie = as_factor(s4aq3b_1),
    categorie = case_when(
      s4aq3b_1 %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9, 19, 20, 23) ~ "Infectieuse",
      s4aq3b_1 %in% c(10, 14, 15, 16)                          ~ "Respiratoire",
      s4aq3b_1 == 11                                            ~ "Traumatique",
      s4aq3b_1 %in% c(13, 18, 24)                              ~ "Chronique",
      TRUE                                                      ~ "Autre"
    )
  )

top10 <- df_t14 |>
  count(type_maladie, categorie, wt = wt_wave4, name = "n_pond") |>
  mutate(pct = round(n_pond / sum(n_pond) * 100, 1)) |>
  slice_max(n_pond, n = 10)

cat(" Top 10 maladies (pondéré) :\n")
print(top10 |> select(type_maladie, categorie, pct))

fig02 <- ggplot(top10,
                aes(x = pct, y = fct_reorder(type_maladie, pct),
                    fill = categorie)) +
  geom_col(alpha = 0.87) +
  geom_text(aes(label = paste0(pct, "%")),
            hjust = -0.15, size = 3.2, color = "grey30") +
  scale_fill_manual(values = pal_cat, name = "Catégorie") +
  scale_x_continuous(limits = c(0, 55)) +
  labs(
    title    = "Les 10 affections les plus fréquentes (pondéré)",
    subtitle = "Parmi les membres ayant déclaré une maladie/blessure (4 semaines)",
    x = "Proportion (%)", y = NULL,
    caption = source_ghs
  )

ggsave(here("outputs", "figures", "fig02_types_maladies.png"),
       fig02, width = 10, height = 6, dpi = 300)
cat("fig02 exportée\n")

#  15. Recours aux soins par type de prestataire (pondéré)

plan_t15 <- plan_sante |>
  filter(a_consulte == 1, !is.na(type_presta))

freq_presta <- plan_t15 |>
  group_by(type_presta) |>
  summarise(
    n_pond = survey_total(vartype = NULL),
    prop   = survey_mean(vartype = "ci")
  ) |>
  mutate(pct = round(prop * 100, 1))

cat(" Prestataires (pondéré) :\n")
print(freq_presta |> select(type_presta, pct))

fig03 <- ggplot(freq_presta,
                aes(x = pct, y = fct_reorder(type_presta, pct),
                    fill = type_presta)) +
  geom_col(alpha = 0.87, width = 0.55) +
  geom_errorbarh(aes(xmin = prop_low * 100, xmax = prop_upp * 100),
                 height = 0.2, linewidth = 0.5, color = "grey40") +
  geom_text(aes(label = paste0(pct, "%")),
            hjust = -0.15, size = 4, fontface = "bold", color = "grey30") +
  scale_fill_manual(values = pal_presta, guide = "none") +
  scale_x_continuous(limits = c(0, 85)) +
  labs(
    title    = "Recours aux soins selon le type de gestionnaire (pondéré)",
    subtitle = "Parmi les consultants avec établissement identifié | poids wt_wave4",
    x = "Proportion (%)", y = NULL,
    caption = source_ghs
  )

ggsave(here("outputs", "figures", "fig03_recours_prestataire.png"),
       fig03, width = 9, height = 5, dpi = 300)
cat("fig03 exportée\n")

# 16. Distribution des dépenses de santé

# s4aq14 (médicaments) est le poste le mieux renseigné et aussi,
# depense_totale (s4aq9 + s4aq14 + s4aq17) exige les 3 postes non-NA

df_t16 <- df_sante |>
  filter(!is.na(depense_med), depense_med > 0)

cat(" Dépenses médicaments (s4aq14 > 0) :", nrow(df_t16), "\n")

# Déciles
deciles <- quantile(df_t16$depense_med, probs = seq(0, 1, 0.1))
cat("Déciles :\n"); print(round(deciles))

# Outliers (Q3 + 3*IQR)
q3 <- quantile(df_t16$depense_med, 0.75)
seuil_outlier <- q3 + 3 * IQR(df_t16$depense_med)
cat("Seuil outliers :", round(seuil_outlier), "nairas\n")
cat("Nb outliers :", sum(df_t16$depense_med > seuil_outlier), "\n")

fig04a <- ggplot(df_t16, aes(x = log10(depense_med))) +
  geom_histogram(bins = 40, fill = col_cyan, color = "white", alpha = 0.85) +
  scale_x_continuous(
    breaks = c(1, 2, 3, 4, 5),
    labels = c("10", "100", "1 000", "10 000", "100 000")
  ) +
  labs(
    title    = "Distribution des dépenses en médicaments (échelle log)",
    subtitle = paste0("n = ", format(nrow(df_t16), big.mark = " "),
                      " membres avec s4aq14 > 0"),
    x = "Dépenses médicaments (nairas, échelle log)", y = "Effectif",
    caption = source_ghs
  )

ggsave(here("outputs", "figures", "fig04a_depenses_distribution.png"),
       fig04a, width = 9, height = 5, dpi = 300)
cat("fig04a exportée\n")

# Boxplot par prestataire
df_t16b <- df_sante |>
  filter(!is.na(depense_med), depense_med > 0, !is.na(type_presta))

fig04b <- ggplot(df_t16b,
                 aes(x = type_presta, y = log10(depense_med),
                     fill = type_presta)) +
  geom_boxplot(alpha = 0.82, outlier.size = 0.6, outlier.color = "grey60",
               width = 0.5) +
  scale_fill_manual(values = pal_presta, guide = "none") +
  scale_y_continuous(
    breaks = c(1, 2, 3, 4, 5),
    labels = c("10", "100", "1 000", "10 000", "100 000")
  ) +
  labs(
    title    = "Dépenses médicaments par type de prestataire",
    subtitle = "Échelle log | couleurs : bleu public, vert privé, ocre autre",
    x = NULL, y = "Dépenses médicaments (nairas, échelle log)",
    caption = source_ghs
  )

ggsave(here("outputs", "figures", "fig04b_depenses_prestataire.png"),
       fig04b, width = 8, height = 5.5, dpi = 300)
cat("fig04b exportée\n")

# 17. Recours aux soins x quintile de richesse (pondéré)

plan_t17 <- plan_sante |>
  filter(!is.na(a_consulte), !is.na(quintile_f))

# Chi-deux Rao-Scott
chi2_quint <- svychisq(~ quintile_f + a_consulte, design = plan_t17)
cat(" Chi-deux Rao-Scott consultation x quintile :\n")
print(chi2_quint)

# V de Cramér (non pondéré)
df_t17 <- df_sante |> filter(!is.na(a_consulte), !is.na(quintile))
v_quint <- rstatix::cramer_v(df_t17$a_consulte, as.factor(df_t17$quintile))
cat("V de Cramér :", round(v_quint, 4), "\n")

# Taux de consultation pondéré par quintile
taux_quint <- plan_t17 |>
  group_by(quintile_f) |>
  summarise(taux = survey_mean(a_consulte, vartype = "ci", na.rm = TRUE)) |>
  mutate(across(c(taux, taux_low, taux_upp), ~ . * 100))

fig05 <- ggplot(taux_quint,
                aes(x = quintile_f, y = taux, fill = taux)) +
  geom_col(alpha = 0.85, width = 0.6) +
  geom_errorbar(aes(ymin = taux_low, ymax = taux_upp),
                width = 0.15, linewidth = 0.5, color = "grey40") +
  geom_text(aes(label = paste0(round(taux, 1), "%")),
            vjust = -1.5, size = 3.5, fontface = "bold", color = col_oms) +
  scale_fill_gradient(low = "#E0F2F1", high = col_oms, guide = "none") +
  scale_y_continuous(limits = c(0, 35),
                     labels = percent_format(scale = 1)) +
  labs(
    title = "Taux de consultation par quintile de consommation (pondéré)",
    subtitle = paste0(
      "Chi-deux Rao-Scott : p ",
      ifelse(chi2_quint$p.value < 0.001, "< 0,001",
             round(chi2_quint$p.value, 3)),
      "  et  V de Cramér = ", round(v_quint, 3)
    ),
    x = "Quintile de consommation", y = "Taux de consultation (%)",
    caption = source_ghs
  ) +
  theme(axis.text.x = element_text(size = 8))

ggsave(here("outputs", "figures", "fig05_recours_quintile.png"),
       fig05, width = 9, height = 5.5, dpi = 300)
cat("fig05 exportée\n")

# 18. Dépenses médianes rural/urbain (violin + boxplot)
# Winsorisation au 99e percentile pour robustesse
p99 <- quantile(df_sante$depense_med, 0.99, na.rm = TRUE)
df_sante <- df_sante |>
  mutate(depense_med_w = if_else(!is.na(depense_med) & depense_med > p99,
                                 p99, depense_med))

df_t18 <- df_sante |>
  filter(!is.na(depense_med_w), depense_med_w > 0, !is.na(milieu))

cat("Tâche 18 — n =", nrow(df_t18), "(dépenses > 0, winsorisées)\n")

# Statistiques par milieu
stats_milieu <- df_t18 |>
  group_by(milieu) |>
  summarise(
    n = n(),
    mediane = round(median(depense_med_w)),
    moyenne = round(mean(depense_med_w)),
    q1 = round(quantile(depense_med_w, 0.25)),
    q3 = round(quantile(depense_med_w, 0.75)),
    .groups = "drop"
  )
cat("Stats par milieu :\n"); print(stats_milieu)

# Test de Wilcoxon
wx_t18 <- rstatix::wilcox_test(df_t18, depense_med_w ~ milieu)
eff_t18 <- rstatix::wilcox_effsize(df_t18, depense_med_w ~ milieu)
cat("Wilcoxon p =", wx_t18$p, "| r =", round(eff_t18$effsize, 4), "\n")

fig06 <- ggplot(df_t18,
                aes(x = milieu, y = log10(depense_med_w), fill = milieu)) +
  geom_violin(alpha = 0.5, trim = TRUE) +
  geom_boxplot(width = 0.15, alpha = 0.9,
               outlier.size = 0.5, outlier.color = "grey60") +
  scale_fill_manual(values = c(Urbain = col_urbain, Rural = col_rural),
                    guide = "none") +
  scale_y_continuous(
    breaks = c(1, 2, 3, 4, 5),
    labels = c("10", "100", "1 000", "10 000", "100 000")
  ) +
  annotate("text", x = 1.5, y = 5,
           label = sprintf("Wilcoxon p %s\nr = %.3f (%s)",
                           ifelse(wx_t18$p < 0.001, "< 0,001",
                                  paste0("= ", round(wx_t18$p, 3))),
                           eff_t18$effsize, eff_t18$magnitude),
           size = 3, color = "grey30") +
  labs(
    title = "Dépenses de médicaments par milieu de résidence",
    subtitle = "Violin plot et boxplot , échelle log , winsorisé au 99e pctile",
    x = NULL, y = "Dépenses médicaments (nairas, échelle log)",
    caption = source_ghs
  )

ggsave(here("outputs", "figures", "fig06_depenses_milieu.png"),
       fig06, width = 8, height = 5.5, dpi = 300)
cat("fig06 exportée\n")

# Tableau gtsummary pondéré stratifié par milieu

tab_sante <- plan_sante |>
  srvyr::filter(!is.na(milieu)) |>
  tbl_svysummary(
    by      = milieu,
    include = c(sexe, groupe_age, malade, a_consulte,
                type_presta, nhis_couvert, quintile_f),
    label   = list(
      sexe         ~ "Sexe",
      groupe_age   ~ "Groupe d'âge",
      malade       ~ "Maladie/blessure (4 sem.)",
      a_consulte   ~ "A consulté un praticien",
      type_presta  ~ "Type de prestataire",
      nhis_couvert ~ "Couvert par le NHIS",
      quintile_f   ~ "Quintile de consommation"
    ),
    statistic = list(all_categorical() ~ "{n} ({p}%)"),
    digits    = list(all_categorical() ~ 0),
    missing   = "ifany",
    missing_text = "Non renseigné"
  ) |>
  add_overall(last = FALSE) |>
  add_p() |>
  modify_header(
    label      ~ "Variable",
    all_stat_cols() ~ "**{level}**  \nn = {n}"
  ) |>
  modify_caption(
    "Caractéristiques de santé par milieu de résidence (pondérées, wt_wave4)."
  )

# Export HTML
tab_sante |>
  as_gt() |>
  gt::gtsave(here("outputs", "tables", "tab_gtsummary_sante.html"))

# Export Word via flextable
tab_word <- tab_sante |>
  as_flex_table() |>
  style_lsms()

saveRDS(tab_word, here("data", "processed", "tab_sante_word.rds"))

# Export CSV
tab_sante |>
  as_tibble() |>
  write.csv(here("outputs", "tables", "tab_gtsummary_sante.csv"),
            row.names = FALSE)

cat("Tableau gtsummary exporté\n")

# Sauvegarde des objets pour le rapport
saveRDS(taux_global,  here("data", "processed", "taux_global.rds"))
saveRDS(taux_sexe,    here("data", "processed", "taux_sexe.rds"))
saveRDS(taux_age,     here("data", "processed", "taux_age.rds"))
saveRDS(top10,        here("data", "processed", "top10_maladies.rds"))
saveRDS(freq_presta,  here("data", "processed", "freq_presta.rds"))
saveRDS(taux_quint,   here("data", "processed", "taux_quint.rds"))
saveRDS(stats_milieu, here("data", "processed", "stats_milieu.rds"))
saveRDS(list(chi2 = chi2_quint, v = v_quint),
        here("data", "processed", "tests_quintile.rds"))
saveRDS(list(wx = wx_t18, eff = eff_t18),
        here("data", "processed", "tests_depenses.rds"))

cat("Script analyses terminé\n")
