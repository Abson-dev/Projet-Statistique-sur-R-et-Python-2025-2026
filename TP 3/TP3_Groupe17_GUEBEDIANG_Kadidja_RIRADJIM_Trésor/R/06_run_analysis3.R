# =========================================================
# 06_run_analysis3.R
# Analyses pondérées de l'Analyse 3
# =========================================================

analysis_w4 <- readr::read_csv(
  file.path(path_clean, "analysis_w4.csv"),
  show_col_types = FALSE
) %>%
  mutate(
    poids_menage = as.numeric(poids_menage),
    ea = as.numeric(ea)
  )

theme_tp <- function() {
  theme_minimal(base_size = 13) +
    theme(
      plot.title = element_text(face = "bold", size = 17, hjust = 0),
      plot.subtitle = element_text(size = 11, color = "grey30"),
      axis.title = element_text(face = "bold"),
      axis.text = element_text(color = "grey20"),
      legend.title = element_text(face = "bold"),
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_line(color = "grey86"),
      panel.grid.major.x = element_line(color = "grey92")
    )
}

col_femme <- "#C76D6D"
col_homme <- "#4C8DAE"
col_barre <- "#6F7D8C"
col_infectieuse <- "#4C8DAE"
col_traumatique <- "#8B6FB3"
col_chronique <- "#7A9A3A"
col_autre <- "#B88A6A"

clean_prefix <- function(x) stringr::str_remove(as.character(x), "^\\d+\\.?\\s*")

trad_prestataire <- function(x) {
  x <- clean_prefix(x)
  dplyr::case_when(
    x == "DOCTOR" ~ "Médecin",
    x == "NURSE" ~ "Infirmier(ère)",
    x == "MIDWIFE" ~ "Sage-femme",
    x == "PHARMACIST" ~ "Pharmacien",
    x == "CHEMIST" ~ "Pharmacie / dépôt",
    x == "TRADITIONAL HEALER" ~ "Tradipraticien",
    x == "SPIRITUALIST" ~ "Soignant spirituel",
    x == "DENTIST" ~ "Dentiste",
    x == "MEDICAL ASST" ~ "Assistant médical",
    x == "PATENT MEDICINE VENDOR (PMV)" ~ "Vendeur de médicaments (PMV)",
    x == "COMMUNITY HEALTH EXTENSION WORKER (CHEW)" ~ "Agent de santé communautaire (CHEW)",
    x == "JUNIOR COMMUNITY HEALTH EXTENSION WORKER (JCHEW)" ~ "Agent de santé communautaire junior",
    x == "NO ONE" ~ "Aucun",
    x == "TBA" ~ "Accoucheuse traditionnelle",
    x == "OTHER(SPECIFY)" ~ "Autre",
    TRUE ~ stringr::str_to_sentence(stringr::str_to_lower(x))
  )
}

trad_maladie <- function(x) {
  x <- clean_prefix(x)
  dplyr::case_when(
    x == "MALARIA" ~ "Paludisme",
    x == "BODY PAINS" ~ "Douleurs corporelles",
    x == "HEADACHE" ~ "Mal de tête",
    x == "COUGH" ~ "Toux",
    x == "CATARRH" ~ "Rhume / catarrhe",
    x == "COMMON COLD" ~ "Rhume",
    x == "TYPHOID" ~ "Typhoïde",
    x == "ULCER/STOMACH PAIN" ~ "Ulcère / douleur d'estomac",
    x == "INJURY" ~ "Blessure",
    x == "OTHER(SPECIFY)" ~ "Autre",
    TRUE ~ stringr::str_to_sentence(stringr::str_to_lower(x))
  )
}

cat_maladie <- function(x) {
  x_low <- stringr::str_to_lower(x)
  dplyr::case_when(
    stringr::str_detect(x_low, "paludisme|typho|toux|rhume|catarrhe|cold|cough|malaria") ~ "Infectieuse",
    stringr::str_detect(x_low, "blessure|injury|fracture|brûlure|burn|wound") ~ "Traumatique",
    stringr::str_detect(x_low, "ulcère|ulcer|diab|hypertension|asthma|asthme") ~ "Chronique",
    TRUE ~ "Autre"
  )
}

fmt_n <- scales::label_number(big.mark = " ", decimal.mark = ",")
fmt_pct <- scales::label_percent(accuracy = 0.1, decimal.mark = ",")

valid_weight <- function(df) {
  df %>% filter(!is.na(poids_menage), poids_menage > 0)
}

make_design <- function(df) {
  df <- valid_weight(df)
  if (nrow(df) == 0) stop("Aucune observation pondérable.")
  id_formula <- if ("ea" %in% names(df) && sum(!is.na(df$ea)) > 0) stats::as.formula("~ea") else stats::as.formula("~1")
  survey::svydesign(
    ids = id_formula,
    weights = ~poids_menage,
    data = df,
    nest = TRUE
  )
}

weighted_total <- function(x, w) sum(w[!is.na(x) & !is.na(w)], na.rm = TRUE)

# ---------------------------------------------------------
# 1. Morbidité par sexe et groupe d'âge
# ---------------------------------------------------------
morbidity_data <- analysis_w4 %>%
  filter(!is.na(malade), !is.na(sexe), !is.na(age_groupe)) %>%
  mutate(
    malade_bin = ifelse(malade == "Oui", 1, 0),
    age_groupe = factor(age_groupe, levels = c("0-4", "5-14", "15-24", "25-44", "45-64", "65+")),
    sexe = factor(sexe, levels = c("Femme", "Homme"))
  ) %>%
  valid_weight()

design_morb <- make_design(morbidity_data)

morbidity_table <- survey::svyby(
  ~malade_bin,
  ~sexe + age_groupe,
  design = design_morb,
  FUN = survey::svymean,
  vartype = c("se", "ci"),
  na.rm = TRUE,
  keep.names = FALSE
) %>%
  as.data.frame() %>%
  transmute(
    sexe = as.character(sexe),
    age_groupe = as.character(age_groupe),
    prop = malade_bin,
    se = se,
    low = ci_l,
    high = ci_u
  )

readr::write_csv(morbidity_table, file.path(path_tables, "table_morbidite_sexe_age.csv"))

fig_morbidite <- ggplot(
  morbidity_table,
  aes(x = age_groupe, y = prop, fill = sexe)
) +
  geom_col(position = position_dodge(width = 0.8), width = 0.72) +
  geom_errorbar(
    aes(ymin = low, ymax = high),
    position = position_dodge(width = 0.8),
    width = 0.18,
    linewidth = 0.7
  ) +
  geom_text(
    aes(label = fmt_pct(prop)),
    position = position_dodge(width = 0.8),
    vjust = -0.35,
    size = 3.6
  ) +
  scale_fill_manual(values = c("Femme" = col_femme, "Homme" = col_homme)) +
  scale_y_continuous(labels = fmt_pct, limits = c(0, max(morbidity_table$high, na.rm = TRUE) + 0.05)) +
  labs(
    title = "Morbidité pondérée par sexe et groupe d'âge",
    subtitle = "Estimations pondérées avec intervalles de confiance à 95 %",
    x = "Groupe d'âge",
    y = "Proportion pondérée",
    fill = "Sexe"
  ) +
  theme_tp()

ggsave(file.path(path_figures, "fig_morbidite_sexe_age.png"), fig_morbidite, width = 11, height = 7, dpi = 320)

# ---------------------------------------------------------
# 2. Top 10 des maladies déclarées
# ---------------------------------------------------------
maladies_long <- analysis_w4 %>%
  select(type_maladie_1, type_maladie_2, poids_menage) %>%
  tidyr::pivot_longer(cols = c(type_maladie_1, type_maladie_2), values_to = "maladie") %>%
  filter(!is.na(maladie), maladie != "", !is.na(poids_menage), poids_menage > 0) %>%
  mutate(
    maladie = trad_maladie(maladie),
    categorie = cat_maladie(maladie)
  ) %>%
  group_by(maladie, categorie) %>%
  summarise(effectif_pondere = sum(poids_menage, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(effectif_pondere)) %>%
  slice_head(n = 10) %>%
  mutate(maladie = forcats::fct_reorder(maladie, effectif_pondere))

readr::write_csv(maladies_long, file.path(path_tables, "table_top10_maladies.csv"))

fig_maladies <- ggplot(maladies_long, aes(x = maladie, y = effectif_pondere, fill = categorie)) +
  geom_col(width = 0.72) +
  geom_text(aes(label = fmt_n(effectif_pondere)), hjust = -0.12, size = 3.8) +
  coord_flip(clip = "off") +
  scale_fill_manual(values = c(
    "Infectieuse" = col_infectieuse,
    "Traumatique" = col_traumatique,
    "Chronique" = col_chronique,
    "Autre" = col_autre
  )) +
  scale_y_continuous(labels = fmt_n, expand = expansion(mult = c(0, 0.14))) +
  labs(
    title = "Top 10 des maladies déclarées",
    subtitle = "Effectifs pondérés des mentions de maladie",
    x = NULL,
    y = "Effectif pondéré",
    fill = "Catégorie"
  ) +
  theme_tp()

ggsave(file.path(path_figures, "fig_top10_maladies.png"), fig_maladies, width = 12, height = 8, dpi = 320)

# ---------------------------------------------------------
# 3. Prestataires consultés
# ---------------------------------------------------------
prestataire_table <- analysis_w4 %>%
  filter(!is.na(prestataire_1), prestataire_1 != "", !is.na(poids_menage), poids_menage > 0) %>%
  mutate(prestataire = trad_prestataire(prestataire_1)) %>%
  group_by(prestataire) %>%
  summarise(effectif_pondere = sum(poids_menage, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(effectif_pondere)) %>%
  mutate(prestataire = forcats::fct_reorder(prestataire, effectif_pondere))

readr::write_csv(prestataire_table, file.path(path_tables, "table_prestataires.csv"))

fig_prestataires <- ggplot(prestataire_table, aes(x = prestataire, y = effectif_pondere)) +
  geom_col(fill = col_barre, width = 0.72) +
  geom_text(aes(label = fmt_n(effectif_pondere)), hjust = -0.10, size = 3.8) +
  coord_flip(clip = "off") +
  scale_y_continuous(labels = fmt_n, expand = expansion(mult = c(0, 0.10))) +
  labs(
    title = "Prestataires consultés",
    subtitle = "Répartition pondérée des consultations",
    x = NULL,
    y = "Effectif pondéré"
  ) +
  theme_tp()

ggsave(file.path(path_figures, "fig_prestataires.png"), fig_prestataires, width = 12, height = 8, dpi = 320)

# ---------------------------------------------------------
# 4. Dépenses de santé
# ---------------------------------------------------------
dep_data <- analysis_w4 %>%
  filter(!is.na(dep_sante_totale), dep_sante_totale > 0) %>%
  valid_weight()

design_dep <- make_design(dep_data)

q_obj <- survey::svyquantile(
  ~dep_sante_totale,
  design = design_dep,
  quantiles = c(0.10, 0.20, 0.30, 0.40, 0.50, 0.60, 0.70, 0.80, 0.90),
  ci = FALSE,
  na.rm = TRUE
)

deciles_dep <- tibble::tibble(
  d1 = as.numeric(q_obj[[1]][1]),
  d2 = as.numeric(q_obj[[1]][2]),
  d3 = as.numeric(q_obj[[1]][3]),
  d4 = as.numeric(q_obj[[1]][4]),
  d5 = as.numeric(q_obj[[1]][5]),
  d6 = as.numeric(q_obj[[1]][6]),
  d7 = as.numeric(q_obj[[1]][7]),
  d8 = as.numeric(q_obj[[1]][8]),
  d9 = as.numeric(q_obj[[1]][9])
)

readr::write_csv(deciles_dep, file.path(path_tables, "table_deciles_depenses_sante.csv"))

fig_hist_dep <- ggplot(dep_data, aes(x = dep_sante_totale, weight = poids_menage)) +
  geom_histogram(
    bins = 28,
    fill = col_barre,
    color = "white",
    linewidth = 0.2
  ) +
  scale_x_log10(labels = fmt_n) +
  labs(
    title = "Distribution pondérée des dépenses de santé",
    subtitle = "Histogramme en échelle logarithmique",
    x = "Dépenses de santé totale (naira, échelle log)",
    y = "Effectif pondéré"
  ) +
  theme_tp()

ggsave(file.path(path_figures, "fig_hist_depenses_log.png"), fig_hist_dep, width = 11, height = 7, dpi = 320)

dep_prest <- analysis_w4 %>%
  filter(!is.na(dep_sante_totale), dep_sante_totale > 0, !is.na(prestataire_1), prestataire_1 != "") %>%
  mutate(prestataire = trad_prestataire(prestataire_1)) %>%
  valid_weight() %>%
  group_by(prestataire) %>%
  mutate(effectif_pondere_prest = sum(poids_menage, na.rm = TRUE)) %>%
  ungroup() %>%
  filter(effectif_pondere_prest >= 30) %>%
  mutate(prestataire = forcats::fct_reorder(prestataire, dep_sante_totale, .fun = median, na.rm = TRUE))

fig_box_prest <- ggplot(dep_prest, aes(x = prestataire, y = dep_sante_totale, weight = poids_menage)) +
  geom_boxplot(fill = "#D9E2EC", color = "grey25", outlier.alpha = 0.6, width = 0.7) +
  coord_flip() +
  scale_y_log10(labels = fmt_n) +
  labs(
    title = "Dépenses de santé par type de prestataire",
    subtitle = "Visualisation descriptive pondérée en échelle logarithmique",
    x = NULL,
    y = "Dépenses de santé totale (naira, échelle log)"
  ) +
  theme_tp()

ggsave(file.path(path_figures, "fig_boxplot_depenses_prestataire.png"), fig_box_prest, width = 13, height = 9, dpi = 320)

q_iqr <- survey::svyquantile(~dep_sante_totale, design_dep, quantiles = c(0.25, 0.75), ci = FALSE, na.rm = TRUE)
q1 <- as.numeric(q_iqr[[1]][1])
q3 <- as.numeric(q_iqr[[1]][2])
iqr_val <- q3 - q1
upper_limit <- q3 + 1.5 * iqr_val

outliers_dep <- dep_data %>%
  filter(dep_sante_totale > upper_limit) %>%
  arrange(desc(dep_sante_totale))

readr::write_csv(outliers_dep, file.path(path_tables, "table_outliers_depenses_sante.csv"))

dep_quintile <- analysis_w4 %>%
  filter(!is.na(dep_sante_totale), dep_sante_totale > 0, !is.na(quintile_conso)) %>%
  valid_weight() %>%
  mutate(quintile_conso = factor(quintile_conso, levels = c("Q1", "Q2", "Q3", "Q4", "Q5")))

if (nrow(dep_quintile) > 0) {
  fig_violin_quintile <- ggplot(
    dep_quintile,
    aes(x = quintile_conso, y = dep_sante_totale, fill = quintile_conso, weight = poids_menage)
  ) +
    geom_violin(trim = FALSE, alpha = 0.55, color = "grey25") +
    geom_boxplot(width = 0.16, outlier.shape = NA, fill = "white", color = "grey25") +
    scale_fill_manual(values = c(
      "Q1" = "#D9C2A7",
      "Q2" = "#C9B29A",
      "Q3" = "#B9C7B0",
      "Q4" = "#9FC2C9",
      "Q5" = "#8BA9C9"
    )) +
    scale_y_log10(labels = fmt_n) +
    labs(
      title = "Dépenses de santé selon le quintile de consommation",
      subtitle = "Visualisation descriptive pondérée",
      x = "Quintile de consommation",
      y = "Dépenses de santé totale (naira, échelle log)"
    ) +
    theme_tp() +
    theme(legend.position = "none")
  
  ggsave(file.path(path_figures, "fig_violin_depenses_quintile.png"), fig_violin_quintile, width = 11, height = 7, dpi = 320)
}

# ---------------------------------------------------------
# 5. Recours aux soins et quintile de consommation
# ---------------------------------------------------------
consult_quint <- analysis_w4 %>%
  filter(!is.na(consulte), !is.na(quintile_conso)) %>%
  valid_weight() %>%
  mutate(
    consulte = factor(consulte, levels = c("Non", "Oui")),
    quintile_conso = factor(quintile_conso, levels = c("Q1", "Q2", "Q3", "Q4", "Q5"))
  )

design_consult <- make_design(consult_quint)

tab_pond <- survey::svytable(~consulte + quintile_conso, design = design_consult)
tab_pond_df <- as.data.frame(tab_pond)
names(tab_pond_df) <- c("consulte", "quintile_conso", "effectif_pondere")
readr::write_csv(tab_pond_df, file.path(path_tables, "table_contingence_consultation_quintile.csv"))

prop_pond <- prop.table(tab_pond, margin = 2)
prop_pond_df <- as.data.frame(prop_pond)
names(prop_pond_df) <- c("consulte", "quintile_conso", "proportion")
readr::write_csv(prop_pond_df, file.path(path_tables, "table_proportions_consultation_quintile.csv"))

chi_rs <- survey::svychisq(~consulte + quintile_conso, design = design_consult, statistic = "F")
test_summary <- tibble::tibble(
  test = "Chi-deux de Rao-Scott",
  statistic = unname(chi_rs$statistic),
  ddl_num = ifelse(!is.null(chi_rs$parameter) && length(chi_rs$parameter) >= 1, unname(chi_rs$parameter[1]), NA_real_),
  ddl_denom = ifelse(!is.null(chi_rs$parameter) && length(chi_rs$parameter) >= 2, unname(chi_rs$parameter[2]), NA_real_),
  p_value = chi_rs$p.value,
  note = "Test ajusté au plan de sondage"
)
readr::write_csv(test_summary, file.path(path_tables, "table_tests_consultation_quintile.csv"))

bar100_df <- prop_pond_df %>%
  mutate(consulte = factor(consulte, levels = c("Non", "Oui")))

bar100 <- ggplot(bar100_df, aes(x = quintile_conso, y = proportion, fill = consulte)) +
  geom_col(width = 0.72) +
  scale_fill_manual(values = c("Non" = "#B0B7BF", "Oui" = "#4C8DAE")) +
  scale_y_continuous(labels = fmt_pct, limits = c(0, 1)) +
  labs(
    title = "Recours aux soins selon le quintile de consommation",
    subtitle = "Proportions pondérées par quintile",
    x = "Quintile de consommation",
    y = "Pourcentage pondéré",
    fill = "A consulté"
  ) +
  theme_tp()

ggsave(file.path(path_figures, "fig_barplot100_consultation_quintile.png"), bar100, width = 10, height = 6.5, dpi = 320)

# ---------------------------------------------------------
# 6. Dépenses de santé : rural vs urbain
# ---------------------------------------------------------
zone_dep <- analysis_w4 %>%
  filter(!is.na(dep_sante_totale), dep_sante_totale > 0, !is.na(milieu)) %>%
  valid_weight() %>%
  mutate(milieu = factor(milieu, levels = c("Rural", "Urbain")))

design_zone <- make_design(zone_dep)

rank_res <- survey::svyranktest(dep_sante_totale ~ milieu, design = design_zone, test = "wilcoxon")
wilcox_table <- tibble::tibble(
  test = "Wilcoxon pondéré (svyranktest)",
  statistic = unname(rank_res$statistic),
  p_value = rank_res$p.value,
  note = "Comparaison ajustée au plan de sondage"
)
readr::write_csv(wilcox_table, file.path(path_tables, "table_wilcoxon_urbain_rural.csv"))

med_zone <- survey::svyby(
  ~dep_sante_totale,
  ~milieu,
  design = design_zone,
  FUN = survey::svyquantile,
  quantiles = 0.5,
  ci = FALSE,
  na.rm = TRUE,
  keep.var = FALSE
) %>%
  as.data.frame()

names(med_zone)[names(med_zone) == "dep_sante_totale"] <- "mediane_ponderee"
readr::write_csv(med_zone, file.path(path_tables, "table_medianes_depenses_milieu.csv"))

fig_violin <- ggplot(zone_dep, aes(x = milieu, y = dep_sante_totale, fill = milieu, weight = poids_menage)) +
  geom_violin(trim = FALSE, alpha = 0.55, color = "grey25") +
  geom_boxplot(width = 0.18, outlier.shape = NA, fill = "white", color = "grey25") +
  scale_fill_manual(values = c("Rural" = "#DDB0AA", "Urbain" = "#8AC6CF")) +
  scale_y_log10(labels = fmt_n) +
  labs(
    title = "Dépenses de santé : rural vs urbain",
    subtitle = "Visualisation descriptive pondérée",
    x = "Milieu de résidence",
    y = "Dépenses de santé totale (naira, échelle log)"
  ) +
  theme_tp() +
  theme(legend.position = "none")

ggsave(file.path(path_figures, "fig_violin_depenses_urbain_rural.png"), fig_violin, width = 10, height = 7, dpi = 320)

cat("Toutes les analyses pondérées ont été produites.\n")
