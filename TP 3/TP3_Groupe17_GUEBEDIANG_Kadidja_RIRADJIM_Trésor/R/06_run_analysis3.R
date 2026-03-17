# =========================================================
# 06_run_analysis3.R
# Analyses, tableaux et figures de l'Analyse 3
# Version soignée, lisible, professionnelle, en français
# =========================================================

analysis_w4 <- readr::read_csv(
  file.path(path_clean, "analysis_w4.csv"),
  show_col_types = FALSE
)

# ---------------------------------------------------------
# Thème graphique sobre et professionnel
# ---------------------------------------------------------
theme_tp <- function() {
  theme_minimal(base_size = 13) +
    theme(
      plot.title = element_text(face = "bold", size = 18, hjust = 0),
      plot.subtitle = element_text(size = 11, color = "grey30"),
      axis.title = element_text(face = "bold"),
      axis.text = element_text(color = "grey20"),
      legend.title = element_text(face = "bold"),
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_line(color = "grey85"),
      panel.grid.major.x = element_line(color = "grey90")
    )
}

col_femme <- "#D77A7A"
col_homme <- "#4C8DAE"
col_barre <- "#6F7D8C"
col_infectieuse <- "#4C8DAE"
col_traumatique <- "#8B6FB3"
col_chronique <- "#7A9A3A"
col_autre <- "#B88A6A"

# ---------------------------------------------------------
# Fonctions de nettoyage de libellés
# ---------------------------------------------------------
clean_prefix <- function(x) {
  x <- as.character(x)
  stringr::str_remove(x, "^\\d+\\.?\\s*")
}

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

# ---------------------------------------------------------
# 1. Taux de morbidité par sexe et groupe d'âge
# ---------------------------------------------------------
morbidity_table <- analysis_w4 %>%
  filter(!is.na(malade), !is.na(sexe), !is.na(age_groupe)) %>%
  mutate(malade_bin = ifelse(malade == "Oui", 1, 0)) %>%
  group_by(sexe, age_groupe) %>%
  summarise(
    n = n(),
    cas = sum(malade_bin, na.rm = TRUE),
    prop = cas / n,
    se = sqrt((prop * (1 - prop)) / n),
    low = pmax(0, prop - 1.96 * se),
    high = pmin(1, prop + 1.96 * se),
    .groups = "drop"
  ) %>%
  mutate(
    age_groupe = factor(age_groupe, levels = c("0-4", "5-14", "15-24", "25-44", "45-64", "65+")),
    sexe = factor(sexe, levels = c("Femme", "Homme"))
  ) %>%
  arrange(age_groupe, sexe)

readr::write_csv(
  morbidity_table,
  file.path(path_tables, "table_morbidite_sexe_age.csv")
)

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
    size = 3.7
  ) +
  scale_fill_manual(values = c("Femme" = col_femme, "Homme" = col_homme)) +
  scale_y_continuous(
    labels = fmt_pct,
    limits = c(0, max(morbidity_table$high, na.rm = TRUE) + 0.04)
  ) +
  labs(
    title = "Taux de morbidité par sexe et groupe d'âge",
    subtitle = "Proportion de personnes ayant déclaré une maladie ou une blessure",
    x = "Groupe d'âge",
    y = "Proportion",
    fill = "Sexe"
  ) +
  theme_tp()

ggsave(
  filename = file.path(path_figures, "fig_morbidite_sexe_age.png"),
  plot = fig_morbidite,
  width = 11, height = 7, dpi = 320
)

# ---------------------------------------------------------
# 2. Top 10 des maladies
# ---------------------------------------------------------
maladies_long <- analysis_w4 %>%
  select(type_maladie_1, type_maladie_2) %>%
  tidyr::pivot_longer(cols = everything(), values_to = "maladie") %>%
  filter(!is.na(maladie), maladie != "") %>%
  mutate(
    maladie = trad_maladie(maladie),
    categorie = cat_maladie(maladie)
  ) %>%
  count(maladie, categorie, sort = TRUE) %>%
  slice_head(n = 10) %>%
  mutate(
    maladie = forcats::fct_reorder(maladie, n)
  )

readr::write_csv(
  maladies_long,
  file.path(path_tables, "table_top10_maladies.csv")
)

fig_maladies <- ggplot(
  maladies_long,
  aes(x = maladie, y = n, fill = categorie)
) +
  geom_col(width = 0.72) +
  geom_text(
    aes(label = fmt_n(n)),
    hjust = -0.12,
    size = 3.8
  ) +
  coord_flip(clip = "off") +
  scale_fill_manual(
    values = c(
      "Infectieuse" = col_infectieuse,
      "Traumatique" = col_traumatique,
      "Chronique" = col_chronique,
      "Autre" = col_autre
    )
  ) +
  scale_y_continuous(
    labels = fmt_n,
    expand = expansion(mult = c(0, 0.12))
  ) +
  labs(
    title = "Top 10 des types de maladies déclarées",
    subtitle = "Classement des affections les plus fréquemment citées",
    x = NULL,
    y = "Effectif",
    fill = "Catégorie"
  ) +
  theme_tp()

ggsave(
  filename = file.path(path_figures, "fig_top10_maladies.png"),
  plot = fig_maladies,
  width = 12, height = 8, dpi = 320
)

# ---------------------------------------------------------
# 3. Prestataires consultés
# ---------------------------------------------------------
prestataire_table <- analysis_w4 %>%
  filter(!is.na(prestataire_1), prestataire_1 != "") %>%
  mutate(prestataire = trad_prestataire(prestataire_1)) %>%
  count(prestataire, sort = TRUE) %>%
  mutate(
    prestataire = forcats::fct_reorder(prestataire, n)
  )

readr::write_csv(
  prestataire_table,
  file.path(path_tables, "table_prestataires.csv")
)

fig_prestataires <- ggplot(
  prestataire_table,
  aes(x = prestataire, y = n)
) +
  geom_col(fill = col_barre, width = 0.72) +
  geom_text(
    aes(label = fmt_n(n)),
    hjust = -0.10,
    size = 3.8
  ) +
  coord_flip(clip = "off") +
  scale_y_continuous(
    labels = fmt_n,
    expand = expansion(mult = c(0, 0.10))
  ) +
  labs(
    title = "Prestataires consultés",
    subtitle = "Répartition des consultations selon le type de prestataire",
    x = NULL,
    y = "Nombre de consultations"
  ) +
  theme_tp()

ggsave(
  filename = file.path(path_figures, "fig_prestataires.png"),
  plot = fig_prestataires,
  width = 12, height = 8, dpi = 320
)

# ---------------------------------------------------------
# 4. Dépenses de santé
# ---------------------------------------------------------
dep_data <- analysis_w4 %>%
  filter(!is.na(dep_sante_totale), dep_sante_totale > 0)

deciles_dep <- dep_data %>%
  summarise(
    d1 = quantile(dep_sante_totale, 0.10, na.rm = TRUE),
    d2 = quantile(dep_sante_totale, 0.20, na.rm = TRUE),
    d3 = quantile(dep_sante_totale, 0.30, na.rm = TRUE),
    d4 = quantile(dep_sante_totale, 0.40, na.rm = TRUE),
    d5 = quantile(dep_sante_totale, 0.50, na.rm = TRUE),
    d6 = quantile(dep_sante_totale, 0.60, na.rm = TRUE),
    d7 = quantile(dep_sante_totale, 0.70, na.rm = TRUE),
    d8 = quantile(dep_sante_totale, 0.80, na.rm = TRUE),
    d9 = quantile(dep_sante_totale, 0.90, na.rm = TRUE)
  )

readr::write_csv(
  deciles_dep,
  file.path(path_tables, "table_deciles_depenses_sante.csv")
)

fig_hist_dep <- ggplot(dep_data, aes(x = dep_sante_totale)) +
  geom_histogram(
    bins = 28,
    fill = col_barre,
    color = "white",
    linewidth = 0.2
  ) +
  scale_x_log10(labels = fmt_n) +
  labs(
    title = "Distribution des dépenses de santé",
    subtitle = "Histogramme en échelle logarithmique",
    x = "Dépenses de santé totale (naira, échelle log)",
    y = "Effectif"
  ) +
  theme_tp()

ggsave(
  filename = file.path(path_figures, "fig_hist_depenses_log.png"),
  plot = fig_hist_dep,
  width = 11, height = 7, dpi = 320
)

dep_prest <- analysis_w4 %>%
  filter(!is.na(dep_sante_totale), dep_sante_totale > 0, !is.na(prestataire_1), prestataire_1 != "") %>%
  mutate(prestataire = trad_prestataire(prestataire_1)) %>%
  group_by(prestataire) %>%
  mutate(n_prest = n()) %>%
  ungroup() %>%
  filter(n_prest >= 15) %>%
  mutate(
    prestataire = forcats::fct_reorder(prestataire, dep_sante_totale, .fun = median)
  )

fig_box_prest <- ggplot(
  dep_prest,
  aes(x = prestataire, y = dep_sante_totale)
) +
  geom_boxplot(
    fill = "#D9E2EC",
    color = "grey25",
    outlier.alpha = 0.7,
    width = 0.7
  ) +
  coord_flip() +
  scale_y_log10(labels = fmt_n) +
  labs(
    title = "Dépenses de santé par type de prestataire",
    subtitle = "Boxplots en échelle logarithmique (prestataires avec au moins 15 observations)",
    x = NULL,
    y = "Dépenses de santé totale (naira, échelle log)"
  ) +
  theme_tp()

ggsave(
  filename = file.path(path_figures, "fig_boxplot_depenses_prestataire.png"),
  plot = fig_box_prest,
  width = 13, height = 9, dpi = 320
)

q1 <- quantile(dep_data$dep_sante_totale, 0.25, na.rm = TRUE)
q3 <- quantile(dep_data$dep_sante_totale, 0.75, na.rm = TRUE)
iqr_val <- q3 - q1
upper_limit <- q3 + 1.5 * iqr_val

outliers_dep <- dep_data %>%
  filter(dep_sante_totale > upper_limit) %>%
  arrange(desc(dep_sante_totale))

readr::write_csv(
  outliers_dep,
  file.path(path_tables, "table_outliers_depenses_sante.csv")
)

# ---------------------------------------------------------
# 4b. Violin plot des dépenses par quintile
# ---------------------------------------------------------
dep_quintile <- analysis_w4 %>%
  filter(
    !is.na(dep_sante_totale),
    dep_sante_totale > 0,
    !is.na(quintile_conso),
    quintile_conso %in% c("Q1", "Q2", "Q3", "Q4", "Q5")
  ) %>%
  mutate(
    quintile_conso = factor(quintile_conso, levels = c("Q1", "Q2", "Q3", "Q4", "Q5"))
  )

if (nrow(dep_quintile) > 0) {
  fig_violin_quintile <- ggplot(
    dep_quintile,
    aes(x = quintile_conso, y = dep_sante_totale, fill = quintile_conso)
  ) +
    geom_violin(trim = FALSE, alpha = 0.55, color = "grey25") +
    geom_boxplot(
      width = 0.16,
      outlier.shape = NA,
      fill = "white",
      color = "grey25"
    ) +
    scale_fill_manual(
      values = c(
        "Q1" = "#D9C2A7",
        "Q2" = "#C9B29A",
        "Q3" = "#B9C7B0",
        "Q4" = "#9FC2C9",
        "Q5" = "#8BA9C9"
      )
    ) +
    scale_y_log10(labels = fmt_n) +
    labs(
      title = "Dépenses de santé selon le quintile de consommation",
      subtitle = "Violon + boxplot en échelle logarithmique",
      x = "Quintile de consommation",
      y = "Dépenses de santé totale (naira, échelle log)"
    ) +
    theme_tp() +
    theme(legend.position = "none")
  
  ggsave(
    filename = file.path(path_figures, "fig_violin_depenses_quintile.png"),
    plot = fig_violin_quintile,
    width = 11, height = 7, dpi = 320
  )
}

# ---------------------------------------------------------
# 5. Test d'indépendance : consultation x quintile
# ---------------------------------------------------------
if ("quintile_conso" %in% names(analysis_w4)) {
  
  test_df <- analysis_w4 %>%
    mutate(
      consulte = as.character(consulte),
      quintile_conso = as.character(quintile_conso)
    ) %>%
    filter(
      !is.na(consulte),
      !is.na(quintile_conso),
      consulte %in% c("Non", "Oui"),
      quintile_conso %in% c("Q1", "Q2", "Q3", "Q4", "Q5")
    ) %>%
    mutate(
      consulte_bin = factor(consulte, levels = c("Non", "Oui")),
      quintile_conso = factor(quintile_conso, levels = c("Q1", "Q2", "Q3", "Q4", "Q5"))
    )
  
  if (nrow(test_df) > 0) {
    
    tab_df <- test_df %>%
      count(consulte_bin, quintile_conso, .drop = FALSE) %>%
      mutate(n = ifelse(is.na(n), 0, n))
    
    tab <- xtabs(n ~ consulte_bin + quintile_conso, data = tab_df)
    storage.mode(tab) <- "double"
    
    write.csv(
      as.data.frame.matrix(tab),
      file.path(path_tables, "table_contingence_consultation_quintile.csv"),
      row.names = TRUE
    )
    
    if (all(is.finite(tab)) && all(tab >= 0) && sum(tab) > 0) {
      
      chi_res <- suppressWarnings(chisq.test(tab))
      
      fisher_needed <- any(chi_res$expected < 5)
      fisher_res <- NULL
      
      if (fisher_needed) {
        fisher_res <- fisher.test(tab)
      }
      
      n_total <- sum(tab)
      r_dim <- nrow(tab)
      c_dim <- ncol(tab)
      k_dim <- min(r_dim - 1, c_dim - 1)
      
      cramer_v <- if (k_dim > 0 && n_total > 0) {
        sqrt(as.numeric(chi_res$statistic) / (n_total * k_dim))
      } else {
        NA_real_
      }
      
      test_summary <- tibble::tibble(
        test = c("Chi-deux", "Fisher exact", "V de Cramér"),
        statistic = c(
          unname(chi_res$statistic),
          NA_real_,
          cramer_v
        ),
        p_value = c(
          chi_res$p.value,
          ifelse(is.null(fisher_res), NA_real_, fisher_res$p.value),
          NA_real_
        ),
        note = c(
          "Toujours calculé",
          ifelse(fisher_needed, "Calculé car certains effectifs attendus sont < 5", "Non nécessaire"),
          "Mesure de l'intensité de l'association"
        )
      )
      
      readr::write_csv(
        test_summary,
        file.path(path_tables, "table_tests_consultation_quintile.csv")
      )
      
      bar100_df <- test_df %>%
        count(quintile_conso, consulte_bin) %>%
        group_by(quintile_conso) %>%
        mutate(pct = n / sum(n)) %>%
        ungroup()
      
      bar100 <- ggplot(
        bar100_df,
        aes(x = quintile_conso, y = pct, fill = consulte_bin)
      ) +
        geom_col(width = 0.72) +
        scale_fill_manual(values = c("Non" = "#B0B7BF", "Oui" = "#4C8DAE")) +
        scale_y_continuous(labels = fmt_pct, limits = c(0, 1)) +
        labs(
          title = "Recours aux soins selon le quintile de consommation",
          subtitle = "Barres empilées à 100 %",
          x = "Quintile de consommation",
          y = "Pourcentage",
          fill = "A consulté"
        ) +
        theme_tp()
      
      ggsave(
        filename = file.path(path_figures, "fig_barplot100_consultation_quintile.png"),
        plot = bar100,
        width = 10, height = 6.5, dpi = 320
      )
      
    } else {
      warning("Le tableau de contingence consultation x quintile n'est pas exploitable pour le test du chi-deux.")
    }
  } else {
    warning("Aucune observation exploitable pour le test consultation x quintile.")
  }
}

# ---------------------------------------------------------
# 6. Comparaison des dépenses rurales et urbaines
# ---------------------------------------------------------
zone_dep <- analysis_w4 %>%
  filter(!is.na(dep_sante_totale), dep_sante_totale > 0, !is.na(milieu)) %>%
  mutate(milieu = factor(milieu, levels = c("Rural", "Urbain")))

if (nrow(zone_dep) > 0) {
  wilcox_res <- wilcox.test(dep_sante_totale ~ milieu, data = zone_dep)
  
  wilcox_table <- tibble::tibble(
    test = "Wilcoxon rang-sum",
    statistic = unname(wilcox_res$statistic),
    p_value = wilcox_res$p.value
  )
  
  readr::write_csv(
    wilcox_table,
    file.path(path_tables, "table_wilcoxon_urbain_rural.csv")
  )
  
  fig_violin <- ggplot(
    zone_dep,
    aes(x = milieu, y = dep_sante_totale, fill = milieu)
  ) +
    geom_violin(trim = FALSE, alpha = 0.55, color = "grey25") +
    geom_boxplot(
      width = 0.18,
      outlier.shape = NA,
      fill = "white",
      color = "grey25"
    ) +
    scale_fill_manual(values = c("Rural" = "#DDB0AA", "Urbain" = "#8AC6CF")) +
    scale_y_log10(labels = fmt_n) +
    labs(
      title = "Dépenses de santé : rural vs urbain",
      subtitle = "Violon + boxplot en échelle logarithmique",
      x = "Milieu de résidence",
      y = "Dépenses de santé totale (naira, échelle log)"
    ) +
    theme_tp() +
    theme(legend.position = "none")
  
  ggsave(
    filename = file.path(path_figures, "fig_violin_depenses_urbain_rural.png"),
    plot = fig_violin,
    width = 10, height = 7, dpi = 320
  )
}

cat("Toutes les analyses ont été produites.\n")