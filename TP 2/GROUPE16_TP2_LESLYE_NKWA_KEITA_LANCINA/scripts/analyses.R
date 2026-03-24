# Script des analyses
# Analyses statistiques pondérées et visualisations , tp2

# Palette TP2 — violet UNESCO (SDG4 / Education for All)
col_violet  <- "#5C2D91"
col_lavande <- "#9B59B6"
col_indigo  <- "#3D3B8E"
col_lilas   <- "#D7BDE2"
col_homme   <- "#0058AB"
col_femme   <- "#E8416F"
col_rural   <- "#80BD41"
col_urbain  <- "#6C757D"

# Palette séquentielle pour niveau_educ (plus foncé = plus élevé)
pal_niveaux <- c(
  "Aucun"            = "#D7BDE2",
  "Primaire"         = "#C39BD3",
  "Secondaire"       = "#9B59B6",
  "Technique/Prof"   = "#7D3C98",
  "Tertiaire"        = "#5C2D91",
  "Coranique/Adulte" = "#7D6608"
)

theme_tp2 <- theme_minimal(base_size = 10) +
  theme(
    plot.title    = element_text(size = 11, face = "bold", hjust = 0),
    plot.subtitle = element_text(size = 9, color = "grey40"),
    plot.caption  = element_text(size = 7, color = "grey50", hjust = 1),
    axis.title    = element_text(size = 9),
    legend.position = "bottom"
  )
theme_set(theme_tp2)

# Chargement
df_educ   <- readRDS(here("data", "processed", "df_educ_clean.rds"))
plan_educ <- readRDS(here("data", "processed", "plan_educ.rds"))

# Fonction de mise en forme flextable (identique TP1)
style_lsms <- function(ft) {
  violet_header <- "#5C2D91"
  violet_clair  <- "#EDE3F5"
  blanc         <- "#FFFFFF"
  gris_bord     <- "#A0A0A0"
  nrow_ft       <- nrow(ft$body$dataset)
  ft |>
    flextable::font(fontname = "Garamond", part = "all") |>
    flextable::fontsize(size = 9, part = "body") |>
    flextable::fontsize(size = 9.5, part = "header") |>
    flextable::bg(bg = violet_header, part = "header") |>
    flextable::color(color = blanc, part = "header") |>
    flextable::bold(part = "header") |>
    flextable::align(align = "center", part = "header") |>
    flextable::align(j = 1, align = "left", part = "header") |>
    flextable::bg(i = seq(1, nrow_ft, 2), bg = blanc, part = "body") |>
    flextable::bg(i = seq(2, nrow_ft, 2), bg = violet_clair, part = "body") |>
    flextable::align(j = 1, align = "left", part = "body") |>
    flextable::bold(j = 1, part = "body") |>
    flextable::border_remove() |>
    flextable::border_outer(part = "all",
                            border = officer::fp_border(color = violet_header, width = 1.5)) |>
    flextable::border_inner_h(part = "body",
                              border = officer::fp_border(color = gris_bord, width = 0.4)) |>
    flextable::hline(part = "header",
                     border = officer::fp_border(color = blanc, width = 0.6)) |>
    flextable::hline_bottom(part = "header",
                            border = officer::fp_border(color = violet_header, width = 2)) |>
    flextable::padding(padding.left = 6, padding.right = 6,
                       padding.top = 3, padding.bottom = 3, part = "all")
}

# 8. Distribution du niveau d'éducation (pondérée)

# La population est celle des membres actifs éligibles (s2aq2 == 1) car niveau_educ renseigné

freq_niv_pond <- plan_educ |>
  filter(!is.na(niveau_educ)) |>
  group_by(niveau_educ) |>
  summarise(
    n_pond = survey_total(vartype = "ci"),
    prop   = survey_mean(vartype = "ci")
  ) |>
  mutate(pct = round(prop * 100, 1))

cat("Distribution pondérée niveau_educ\n")
print(freq_niv_pond |> select(niveau_educ, pct))

fig01 <- ggplot(freq_niv_pond,
                aes(x = pct, y = fct_reorder(niveau_educ, pct),
                    fill = niveau_educ)) +
  geom_col(alpha = 0.88) +
  geom_errorbarh(aes(xmin = prop_low * 100, xmax = prop_upp * 100),
                 height = 0.25, linewidth = 0.5, color = "grey40") +
  geom_text(aes(label = paste0(pct, "%")),
            hjust = -0.3, size = 3.2, color = col_indigo) +
  scale_fill_manual(values = pal_niveaux, guide = "none") +
  scale_x_continuous(limits = c(0, 45)) +
  labs(
    title    = "Distribution du niveau d'éducation complété (pondérée)",
    subtitle = "Nigeria GHS Panel W4 | Membres 3 ans et plus, poids wt_wave4",
    x = "Proportion (%)", y = NULL,
    caption = source_ghs
  )

ggsave(here("outputs", "figures", "fig01_niveau_educ_distribution.png"),
       fig01, width = 9, height = 6, dpi = 300)
cat("fig01 exportée\n")

# 9. Niveau d'éducation par sexe, adultes de plus de 18 ans (pondéré)
# Barplot 100% empilé avec le  chi-deux pondéré et le V de Cramér

plan_t9 <- plan_educ |>
  filter(adulte_18plus == TRUE, !is.na(niveau_educ), !is.na(sexe))

# Chi-deux sur le plan de sondage
tab_svychi2 <- svychisq(~ niveau_educ + sexe, design = plan_t9)
cat(" Chi-deux pondéré niveau_educ x sexe\n")
print(tab_svychi2)

# V de Cramér non pondéré (une approximation , ici on ne prends pas de V pondéré standard)
df_t9 <- df_educ |>
  filter(adulte_18plus == TRUE, !is.na(niveau_educ), !is.na(sexe))
v_cramer <- rstatix::cramer_v(df_t9$niveau_educ, df_t9$sexe)
cat("V de Cramér (non pondéré) :", round(v_cramer, 4), "\n")

# Proportions pondérées pour le graphique
prop_t9 <- plan_t9 |>
  group_by(sexe, niveau_educ) |>
  summarise(prop = survey_mean(vartype = NULL)) |>
  ungroup()

fig02 <- ggplot(prop_t9, aes(x = sexe, y = prop, fill = niveau_educ)) +
  geom_col(position = "fill", alpha = 0.88, width = 0.6) +
  scale_y_continuous(labels = percent_format()) +
  scale_fill_manual(values = pal_niveaux, name = "Niveau") +
  labs(
    title = "Niveau d'éducation par sexe — adultes 18+ (pondéré)",
    subtitle = paste0(
      "Chi-deux Rao-Scott : p ",
      ifelse(tab_svychi2$p.value < 0.001, "< 0,001",
             paste0("= ", round(tab_svychi2$p.value, 3))),
      "  |  V de Cramér = ", round(v_cramer, 3)
    ),
    x = NULL, y = "Proportion",
    caption = source_ghs
  )

ggsave(here("outputs", "figures", "fig02_niveau_educ_sexe.png"),
       fig02, width = 8, height = 6, dpi = 300)
cat("fig02 exportée\n")


# 10. Âge x niveau d'éducation, adultes de plus de 18ans (pondéré)

# Kruskal-Wallis non paramétrique ainsi que le post-hoc Dunn

df_t10 <- df_educ |>
  filter(adulte_18plus == TRUE, !is.na(niveau_educ), !is.na(age))

kw_t10 <- rstatix::kruskal_test(df_t10, age ~ niveau_educ)
cat(" Kruskal-Wallis âge croisé ave  niveau_educ\n")
print(kw_t10)

dunn_t10 <- NULL
if (kw_t10$p < 0.05) {
  dunn_t10 <- rstatix::dunn_test(df_t10, age ~ niveau_educ,
                                  p.adjust.method = "bonferroni")
  cat("Post-hoc Dunn (Bonferroni) :\n")
  print(dunn_t10)
}

fig03 <- ggplot(df_t10,
                aes(x = fct_reorder(niveau_educ, age, .fun = median),
                    y = age, fill = niveau_educ)) +
  geom_boxplot(alpha = 0.82, outlier.size = 0.6, outlier.color = "grey60",
               width = 0.55) +
  scale_fill_manual(values = pal_niveaux, guide = "none") +
  labs(
    title = "Distribution de l'âge par niveau d'éducation (adultes 18+)",
    subtitle = paste0(
      "Kruskal-Wallis : H(", kw_t10$df, ") = ", round(kw_t10$statistic, 1),
      ", p ", ifelse(kw_t10$p < 0.001, "< 0,001", round(kw_t10$p, 3))
    ),
    x = "Niveau d'éducation", y = "Âge (années)",
    caption = source_ghs
  ) +
  theme(axis.text.x = element_text(angle = 20, hjust = 1))

ggsave(here("outputs", "figures", "fig03_age_niveau_educ.png"),
       fig03, width = 9, height = 6, dpi = 300)
cat("fig03 exportée\n")


# 11. Taux de scolarisation 6-17 ans par milieu (pondéré)


plan_t11 <- plan_educ |>
  filter(enfant_6_17 == TRUE, !is.na(scolarise_actuel), !is.na(milieu))

# Taux pondéré avec IC
taux_scol <- plan_t11 |>
  group_by(milieu) |>
  summarise(
    taux = survey_mean(scolarise_actuel, vartype = "ci", na.rm = TRUE)
  ) |>
  mutate(across(c(taux, taux_low, taux_upp), ~ . * 100))

cat(" Taux de scolarisation pondéré 6-17 ans\n")
print(taux_scol)

# Chi-deux pondéré
chi2_scol <- svychisq(~ milieu + scolarise_actuel, design = plan_t11)
cat("Chi-deux Rao-Scott :\n")
print(chi2_scol)

fig04 <- ggplot(taux_scol, aes(x = milieu, y = taux, fill = milieu)) +
  geom_col(alpha = 0.85, width = 0.5) +
  geom_errorbar(aes(ymin = taux_low, ymax = taux_upp),
                width = 0.12, linewidth = 0.7, color = col_indigo) +
  geom_text(aes(label = paste0(round(taux, 1), "%")),
            vjust = -2, size = 4, fontface = "bold", color = col_indigo) +
  scale_fill_manual(values = c(Urbain = col_urbain, Rural = col_rural),
                    guide = "none") +
  scale_y_continuous(limits = c(0, 105),
                     labels = percent_format(scale = 1)) +
  labs(
    title = "Taux de scolarisation des 6-17 ans par milieu (pondéré)",
    subtitle = paste0(
      "Chi-deux Rao-Scott : p ",
      ifelse(chi2_scol$p.value < 0.001, "< 0,001",
             round(chi2_scol$p.value, 3)),
      "  |  IC 95% (plan de sondage)"
    ),
    x = NULL, y = "Taux de scolarisation (%)",
    caption = source_ghs
  )

ggsave(here("outputs", "figures", "fig04_scolarisation_milieu.png"),
       fig04, width = 7, height = 5.5, dpi = 300)
cat("fig04 exportée\n")


# 12. Carte choroplèthe du taux de sans instruction par État

# Elle est colorée par taux d'adultes sans instruction.


plan_t12 <- plan_educ |>
  filter(adulte_18plus == TRUE, !is.na(niveau_educ)) |>
  mutate(sans_instruction = as.integer(niveau_educ == "Aucun"),
         state_label = as_factor(state))

heatmap_data <- plan_t12 |>
  group_by(state_label) |>
  summarise(
    n_total = unweighted(n()),
    taux    = survey_mean(sans_instruction, vartype = NULL, na.rm = TRUE)
  ) |>
  mutate(
    taux_pct = round(taux * 100, 1),
    instable = n_total < 30,
    label    = if_else(instable, paste0(taux_pct, "%*"), paste0(taux_pct, "%"))
  )

# Top 10 États
tab_top10 <- heatmap_data |>
  arrange(desc(taux_pct)) |>
  select(state_label, n_total, taux_pct) |>
  head(10)
cat("Top 10 États selon le taux d'analphabétisme pondéré\n")
print(tab_top10)

write.csv(tab_top10,
          here("outputs", "tables", "tab_top10_analphabetisme.csv"),
          row.names = FALSE)

# Chargement du shapefile du Nigeria (37 États + FCT)

# au cas ou
if (requireNamespace("rnaturalearth", quietly = TRUE) &
    requireNamespace("rnaturalearthdata", quietly = TRUE)) {
  nga_sf <- rnaturalearth::ne_states(country = "Nigeria", returnclass = "sf")
} else if (file.exists(here("data", "raw", "nigeria_states.geojson"))) {
  nga_sf <- sf::st_read(here("data", "raw", "nigeria_states.geojson"), quiet = TRUE)
} else {
  stop("Installez rnaturalearth + rnaturalearthdata ou placez nigeria_states.geojson dans data/raw/")
}

# Nettoyage des noms pour jointure
# rnaturalearth utilise la colonne 'name' pour les noms d'États
nga_sf <- nga_sf |>
  mutate(state_clean = tolower(trimws(name)))

heatmap_data <- heatmap_data |>
  mutate(
    state_clean = state_label |>
      as.character() |>
      str_remove("^\\d+\\.\\s*") |>
      trimws() |>
      tolower()
  )

# Correction des noms connus qui diffèrent entre GHS et le shapefile
heatmap_data <- heatmap_data |>
  mutate(state_clean = case_when(
    state_clean == "fct"        ~ "federal capital territory",
    state_clean == "nasarawa"  ~ "nassarawa",
    TRUE                        ~ state_clean
  ))

nga_map <- nga_sf |>
  left_join(heatmap_data, by = "state_clean")

# Vérification de la jointure
n_join <- sum(!is.na(nga_map$taux_pct))
cat("États joints :", n_join, "sur", nrow(nga_sf), "\n")
if (n_join < 30) {
  cat("Attention : jointure incomplète. Noms non joints :\n")
  print(nga_sf$state_clean[is.na(nga_map$taux_pct)])
}


# Carte choroplèthe avec noms d'États
fig05 <- ggplot(nga_map) +
  geom_sf(aes(fill = taux_pct), color = "white", linewidth = 0.3) +
  geom_sf_text(
    aes(label = paste0(name, "\n", round(taux_pct, 0), "%")),
    size = 4.7, color = "white", lineheight = 0.85,,
    check_overlap = TRUE
  ) +
  scale_fill_gradient(
    low  = col_lilas,
    high = col_violet,
    name = "% sans instruction",
    na.value = "grey90",
    labels = function(x) paste0(x, "%")
  ) +
  labs(
    title    = "Part pondérée d'adultes sans instruction par État nigérian",
    subtitle = "Adultes 18+ | poids wt_wave4 | palette UNESCO",
    caption  = source_ghs
  ) +
  theme_void(base_size = 10) +
  theme(
    plot.title    = element_text(size = 12, face = "bold", hjust = 0),
    plot.subtitle = element_text(size = 9, color = "grey40"),
    plot.caption  = element_text(size = 7, color = "grey50", hjust = 1),
    legend.position = "right"
  )

ggsave(here("outputs", "figures", "fig05_carte_analphabetisme.png"),
       fig05, width = 12, height = 9, dpi = 300)
cat("fig05 (carte choroplèthe) exportée\n")


# TABLEAU  gtsummary pondéré stratifié par milieu


tab_educ <- plan_educ |>
  srvyr::filter(!is.na(milieu)) |>
  tbl_svysummary(
    by      = milieu,
    include = c(sexe, alphabetise, niveau_educ, scolarise_actuel),
    label   = list(
      sexe             ~ "Sexe",
      alphabetise      ~ "Alphabétisé(e)",
      niveau_educ      ~ "Niveau d'éducation",
      scolarise_actuel ~ "Scolarisé(e) en 2018/2019"
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
    "Caractéristiques éducatives par milieu de résidence (pondérées, wt_wave4)."
  )

# Export HTML
tab_educ |>
  as_gt() |>
  gt::gtsave(here("outputs", "tables", "tab_gtsummary_education.html"))

# Export Word via flextable
tab_word <- tab_educ |>
  as_flex_table() |>
  style_lsms()

saveRDS(tab_word, here("data", "processed", "tab_educ_word.rds"))

# Export CSV
tab_educ |>
  as_tibble() |>
  write.csv(here("outputs", "tables", "tab_gtsummary_education.csv"),
            row.names = FALSE)

cat("Tableau gtsummary exporté\n")


# Sauvegarde des objets pour le rapport

saveRDS(freq_niv_pond, here("data", "processed", "freq_niv_pond.rds"))
saveRDS(taux_scol,     here("data", "processed", "taux_scol.rds"))
saveRDS(heatmap_data,  here("data", "processed", "heatmap_data.rds"))
saveRDS(list(chi2 = tab_svychi2, v_cramer = v_cramer),
        here("data", "processed", "tests_sexe.rds"))
saveRDS(list(kw = kw_t10, dunn = dunn_t10),
        here("data", "processed", "tests_age_educ.rds"))
saveRDS(chi2_scol, here("data", "processed", "chi2_scol.rds"))

cat("Script analyses terminé\n")
