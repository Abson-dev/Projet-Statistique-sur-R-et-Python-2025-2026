source("R/01_packages.R")
source("R/02_paths.R")

# =========================================================
# ANALYSE 1 - PROFIL DEMOGRAPHIQUE DES MENAGES NIGERIANS
# Vague 4 (2018)
# =========================================================

# ---------------------------------------------------------
# 0. Créer les dossiers de sortie si besoin
# ---------------------------------------------------------
dir.create(here::here("output", "tables", "analyse1"), recursive = TRUE, showWarnings = FALSE)
dir.create(here::here("output", "figures", "analyse1"), recursive = TRUE, showWarnings = FALSE)
dir.create(here::here("data", "processed", "clean"), recursive = TRUE, showWarnings = FALSE)

# ---------------------------------------------------------
# 0b. Fonctions utilitaires
# ---------------------------------------------------------

# Asymétrie sans package externe
calc_skewness <- function(x) {
  x <- x[!is.na(x)]
  n <- length(x)
  
  if (n < 3) return(NA_real_)
  
  m <- mean(x)
  s <- stats::sd(x)
  
  if (is.na(s) || s == 0) return(NA_real_)
  
  n / ((n - 1) * (n - 2)) * sum(((x - m) / s)^3)
}

# IC binomial exact à 95 %
exact_prop_ci <- function(x, n, conf.level = 0.95) {
  bt <- stats::binom.test(x, n, conf.level = conf.level)
  c(lower = unname(bt$conf.int[1]), upper = unname(bt$conf.int[2]))
}

# Taille d'effet r pour Wilcoxon-Mann-Whitney
# r = |z| / sqrt(n1 + n2)
calc_wilcox_r <- function(x, g) {
  df <- data.frame(x = x, g = g)
  df <- df[stats::complete.cases(df), , drop = FALSE]
  
  groups <- unique(df$g)
  if (length(groups) != 2) {
    return(data.frame(
      n1 = NA_integer_,
      n2 = NA_integer_,
      W = NA_real_,
      U = NA_real_,
      z = NA_real_,
      r = NA_real_
    ))
  }
  
  g1 <- groups[1]
  g2 <- groups[2]
  
  x1 <- df$x[df$g == g1]
  x2 <- df$x[df$g == g2]
  
  n1 <- length(x1)
  n2 <- length(x2)
  
  wt <- suppressWarnings(
    stats::wilcox.test(x1, x2, exact = FALSE, correct = FALSE)
  )
  
  W <- as.numeric(wt$statistic)
  
  # Conversion W -> U pour le premier groupe
  U1 <- W - n1 * (n1 + 1) / 2
  
  mu_U <- n1 * n2 / 2
  sd_U <- sqrt(n1 * n2 * (n1 + n2 + 1) / 12)
  
  if (sd_U == 0) {
    z <- NA_real_
    r <- NA_real_
  } else {
    z <- (U1 - mu_U) / sd_U
    r <- abs(z) / sqrt(n1 + n2)
  }
  
  data.frame(
    groupe_1 = as.character(g1),
    groupe_2 = as.character(g2),
    n1 = n1,
    n2 = n2,
    W = W,
    U = U1,
    z = z,
    r = r
  )
}

# ---------------------------------------------------------
# 1. Localiser les fichiers
# ---------------------------------------------------------
file_sect1_w4 <- fs::dir_ls(
  path_wave4,
  recurse = TRUE,
  regexp = "sect1_harvestw4\\.dta$"
)

file_secta_w4 <- fs::dir_ls(
  path_wave4,
  recurse = TRUE,
  regexp = "secta_harvestw4\\.dta$"
)

cat("Fichier sect1 trouvé :", file_sect1_w4, "\n")
cat("Fichier secta trouvé :", file_secta_w4, "\n")

# ---------------------------------------------------------
# 2. Lire les données
# ---------------------------------------------------------
demo_raw <- haven::read_dta(file_sect1_w4) |>
  janitor::clean_names()

cover_raw <- haven::read_dta(file_secta_w4) |>
  janitor::clean_names()

# ---------------------------------------------------------
# 3. Exploration de structure
# ---------------------------------------------------------
cat("\n==================== STRUCTURE DEMO_RAW ====================\n")
str(demo_raw)

cat("\n==================== GLIMPSE DEMO_RAW ======================\n")
dplyr::glimpse(demo_raw)

cat("\n==================== SUMMARY DEMO_RAW ======================\n")
print(summary(demo_raw))

capture.output(
  str(demo_raw),
  file = here::here("output", "tables", "analyse1", "structure_demo_raw.txt")
)

capture.output(
  dplyr::glimpse(demo_raw),
  file = here::here("output", "tables", "analyse1", "glimpse_demo_raw.txt")
)

capture.output(
  summary(demo_raw),
  file = here::here("output", "tables", "analyse1", "summary_demo_raw.txt")
)

# ---------------------------------------------------------
# 4. Tableau + graphique des valeurs manquantes
# ---------------------------------------------------------
missing_table <- demo_raw |>
  naniar::miss_var_summary() |>
  dplyr::arrange(dplyr::desc(pct_miss), dplyr::desc(n_miss))

print(missing_table, n = 20)

readr::write_csv(
  missing_table,
  here::here("output", "tables", "analyse1", "missing_summary_demo_raw.csv")
)

set.seed(123)

n_vis <- min(2000, nrow(demo_raw))

demo_sample <- demo_raw |>
  dplyr::slice_sample(n = n_vis)

png(
  filename = here::here("output", "figures", "analyse1", "vis_miss_demo_raw_sample.png"),
  width = 1400,
  height = 900,
  res = 150
)

naniar::vis_miss(
  demo_sample,
  warn_large_data = FALSE
)

dev.off()

# ---------------------------------------------------------
# 5. Vérifier les variables attendues
# ---------------------------------------------------------
label_s1q2 <- attr(demo_raw$s1q2, "label")
label_s1q3 <- attr(demo_raw$s1q3, "label")
label_s1q4 <- attr(demo_raw$s1q4, "label")

cat("Label s1q2 :", label_s1q2, "\n")
cat("Label s1q3 :", label_s1q3, "\n")
cat("Label s1q4 :", label_s1q4, "\n")

# ---------------------------------------------------------
# 6. Vérifier les doublons
# ---------------------------------------------------------
duplicates_demo <- demo_raw |>
  dplyr::count(hhid, indiv, name = "n_id") |>
  dplyr::filter(n_id > 1)

duplicates_cover <- cover_raw |>
  dplyr::count(hhid, name = "n_hh") |>
  dplyr::filter(n_hh > 1)

cat("Nombre de doublons hhid-indiv dans demo_raw :", nrow(duplicates_demo), "\n")
cat("Nombre de doublons hhid dans cover_raw :", nrow(duplicates_cover), "\n")

readr::write_csv(
  duplicates_demo,
  here::here("output", "tables", "analyse1", "duplicates_demo_w4.csv")
)

readr::write_csv(
  duplicates_cover,
  here::here("output", "tables", "analyse1", "duplicates_cover_w4.csv")
)

# ---------------------------------------------------------
# 7. Base ménage pour jointure poids
# ---------------------------------------------------------
cover_w4 <- cover_raw |>
  dplyr::select(
    hhid, zone, state, lga, sector, ea,
    wt_wave4, wt_longpanel, old_new
  ) |>
  dplyr::distinct()

# ---------------------------------------------------------
# 8. Base individuelle démographique
# ---------------------------------------------------------
demo_w4 <- demo_raw |>
  dplyr::mutate(
    sex = haven::as_factor(s1q2),
    relation_head = haven::as_factor(s1q3),
    age = as.numeric(s1q4),
    sector_label = haven::as_factor(sector),
    zone_label = haven::as_factor(zone),
    state_label = haven::as_factor(state)
  ) |>
  dplyr::select(
    hhid, indiv, zone, state, lga, sector, ea,
    sex, relation_head, age, zone_label, state_label, sector_label
  )

# ---------------------------------------------------------
# 9. Jointure avec les poids ménage
# ---------------------------------------------------------
demo_w4_full <- demo_w4 |>
  dplyr::left_join(
    cover_w4 |>
      dplyr::select(hhid, wt_wave4, wt_longpanel, old_new),
    by = "hhid"
  )

# ---------------------------------------------------------
# 10. Contrôles qualité
# ---------------------------------------------------------
cat("Dimensions demo_w4_full :", nrow(demo_w4_full), "x", ncol(demo_w4_full), "\n")

missing_core <- demo_w4_full |>
  dplyr::summarise(
    n_missing_age = sum(is.na(age)),
    pct_missing_age = mean(is.na(age)) * 100,
    n_missing_sex = sum(is.na(sex)),
    pct_missing_sex = mean(is.na(sex)) * 100,
    n_missing_relation = sum(is.na(relation_head)),
    pct_missing_relation = mean(is.na(relation_head)) * 100
  )

print(missing_core)

readr::write_csv(
  missing_core,
  here::here("output", "tables", "analyse1", "missing_core_w4.csv")
)

# ---------------------------------------------------------
# 11. Base analytique propre
# ---------------------------------------------------------
demo_w4_analytic <- demo_w4_full |>
  dplyr::filter(!is.na(age), !is.na(sex), !is.na(relation_head)) |>
  dplyr::mutate(
    sex = forcats::fct_drop(sex),
    relation_head = forcats::fct_drop(relation_head),
    sector_label = forcats::fct_drop(sector_label),
    age_group = cut(
      age,
      breaks = c(-Inf, 4, 9, 14, 19, 24, 34, 44, 54, 64, Inf),
      labels = c(
        "0-4", "5-9", "10-14", "15-19", "20-24",
        "25-34", "35-44", "45-54", "55-64", "65+"
      ),
      right = TRUE
    )
  )

# ---------------------------------------------------------
# 12. Variables françaises pour tableaux/rapport
# ---------------------------------------------------------
demo_w4_fr <- demo_w4_analytic |>
  dplyr::mutate(
    sexe = dplyr::case_when(
      as.character(sex) %in% c("1. MALE", "MALE", "male", "Male") ~ "Hommes",
      as.character(sex) %in% c("2. FEMALE", "FEMALE", "female", "Female") ~ "Femmes",
      TRUE ~ as.character(sex)
    ),
    secteur_residence = dplyr::case_when(
      stringr::str_detect(stringr::str_to_lower(as.character(sector_label)), "urban") ~ "Urbain",
      stringr::str_detect(stringr::str_to_lower(as.character(sector_label)), "rural") ~ "Rural",
      TRUE ~ as.character(sector_label)
    ),
    lien_chef = dplyr::case_when(
      stringr::str_detect(as.character(relation_head), "HEAD") ~ "Chef de ménage",
      stringr::str_detect(as.character(relation_head), "SPOUSE") ~ "Conjoint(e)",
      stringr::str_detect(as.character(relation_head), "OWN CHILD") ~ "Enfant du chef",
      stringr::str_detect(as.character(relation_head), "STEP CHILD") ~ "Beau-fils / Belle-fille",
      stringr::str_detect(as.character(relation_head), "ADOPTED CHILD") ~ "Enfant adopté",
      stringr::str_detect(as.character(relation_head), "GRANDCHILD") ~ "Petit-enfant",
      stringr::str_detect(as.character(relation_head), "BROTHER/SISTER-IN-LAW") ~ "Beau-frère / Belle-sœur",
      stringr::str_detect(as.character(relation_head), "BROTHER/SISTER") ~ "Frère / Sœur",
      stringr::str_detect(as.character(relation_head), "NIECE/NEPHEW") ~ "Nièce / Neveu",
      stringr::str_detect(as.character(relation_head), "PARENT-IN-LAW") ~ "Beau-parent",
      stringr::str_detect(as.character(relation_head), "PARENT") ~ "Parent",
      stringr::str_detect(as.character(relation_head), "DOMESTIC HELP") ~ "Aide domestique",
      stringr::str_detect(as.character(relation_head), "OTHER RELATION") ~ "Autre lien de parenté",
      stringr::str_detect(as.character(relation_head), "OTHER NON-RELATION") ~ "Autre personne sans lien de parenté",
      TRUE ~ as.character(relation_head)
    ),
    sexe = factor(sexe, levels = c("Hommes", "Femmes")),
    secteur_residence = factor(secteur_residence, levels = c("Urbain", "Rural"))
  )

print(table(demo_w4_fr$sexe, useNA = "ifany"))
print(table(demo_w4_fr$secteur_residence, useNA = "ifany"))

# ---------------------------------------------------------
# 13. Statistiques descriptives de l'âge
# ---------------------------------------------------------
age_stats <- demo_w4_fr |>
  dplyr::summarise(
    n = dplyr::n(),
    moyenne_age = mean(age, na.rm = TRUE),
    ecart_type_age = sd(age, na.rm = TRUE),
    mediane_age = median(age, na.rm = TRUE),
    q1_age = quantile(age, 0.25, na.rm = TRUE),
    q3_age = quantile(age, 0.75, na.rm = TRUE),
    min_age = min(age, na.rm = TRUE),
    max_age = max(age, na.rm = TRUE),
    cv_age = sd(age, na.rm = TRUE) / mean(age, na.rm = TRUE),
    asymetrie_age = calc_skewness(age)
  )

print(age_stats)

readr::write_csv(
  age_stats,
  here::here("output", "tables", "analyse1", "age_stats_w4.csv")
)

# ---------------------------------------------------------
# 14. Test de normalité Shapiro-Wilk
# ---------------------------------------------------------
set.seed(123)

age_non_missing <- demo_w4_fr |>
  dplyr::filter(!is.na(age)) |>
  dplyr::pull(age)

age_for_shapiro <- if (length(age_non_missing) > 5000) {
  sample(age_non_missing, 5000)
} else {
  age_non_missing
}

shapiro_res <- shapiro.test(age_for_shapiro)
print(shapiro_res)

capture.output(
  shapiro_res,
  file = here::here("output", "tables", "analyse1", "shapiro_age_w4.txt")
)

# ---------------------------------------------------------
# 15. Tableau sexe
# ---------------------------------------------------------
tab_sex <- demo_w4_fr |>
  dplyr::count(sexe) |>
  dplyr::mutate(
    pct = 100 * n / sum(n)
  )

print(tab_sex)

readr::write_csv(
  tab_sex,
  here::here("output", "tables", "analyse1", "tab_sex_w4.csv")
)

# ---------------------------------------------------------
# 16. Tableau relation au chef + IC 95%
# ---------------------------------------------------------
total_n <- nrow(demo_w4_fr)

tab_relation <- demo_w4_fr |>
  dplyr::count(lien_chef, sort = TRUE) |>
  dplyr::mutate(
    pct = 100 * n / sum(n)
  ) |>
  dplyr::rowwise() |>
  dplyr::mutate(
    ci_lower = exact_prop_ci(n, total_n)[1],
    ci_upper = exact_prop_ci(n, total_n)[2],
    ic_inf = 100 * ci_lower,
    ic_sup = 100 * ci_upper
  ) |>
  dplyr::ungroup() |>
  dplyr::select(-ci_lower, -ci_upper)

print(tab_relation)

readr::write_csv(
  tab_relation,
  here::here("output", "tables", "analyse1", "tab_relation_head_w4.csv")
)

# ---------------------------------------------------------
# 17. Tableau secteur urbain/rural
# ---------------------------------------------------------
tab_sector <- demo_w4_fr |>
  dplyr::count(secteur_residence) |>
  dplyr::mutate(
    pct = 100 * n / sum(n)
  )

print(tab_sector)

readr::write_csv(
  tab_sector,
  here::here("output", "tables", "analyse1", "tab_sector_w4.csv")
)

# ---------------------------------------------------------
# 18. Statistiques âge par sexe
# ---------------------------------------------------------
tab_age_sex <- demo_w4_fr |>
  dplyr::group_by(sexe) |>
  dplyr::summarise(
    n = dplyr::n(),
    moyenne_age = mean(age, na.rm = TRUE),
    mediane_age = median(age, na.rm = TRUE),
    ecart_type_age = sd(age, na.rm = TRUE),
    .groups = "drop"
  )

print(tab_age_sex)

readr::write_csv(
  tab_age_sex,
  here::here("output", "tables", "analyse1", "tab_age_by_sex_w4.csv")
)

# ---------------------------------------------------------
# 19. Taille des ménages
# ---------------------------------------------------------
hh_size <- demo_w4_fr |>
  dplyr::count(hhid, secteur_residence, name = "taille_menage")

print(hh_size)

readr::write_csv(
  hh_size,
  here::here("output", "tables", "analyse1", "hh_size_w4.csv")
)

hh_size_stats <- hh_size |>
  dplyr::group_by(secteur_residence) |>
  dplyr::summarise(
    n_menages = dplyr::n(),
    moyenne_taille = mean(taille_menage, na.rm = TRUE),
    mediane_taille = median(taille_menage, na.rm = TRUE),
    ecart_type_taille = sd(taille_menage, na.rm = TRUE),
    .groups = "drop"
  )

print(hh_size_stats)

readr::write_csv(
  hh_size_stats,
  here::here("output", "tables", "analyse1", "hh_size_stats_by_sector_w4.csv")
)

# ---------------------------------------------------------
# 20. Test de Wilcoxon + taille d'effet manuelle
# ---------------------------------------------------------
wilcox_res <- wilcox.test(
  taille_menage ~ secteur_residence,
  data = hh_size,
  exact = FALSE,
  correct = FALSE
)

print(wilcox_res)

capture.output(
  wilcox_res,
  file = here::here("output", "tables", "analyse1", "wilcox_hh_size_urban_rural.txt")
)

effet_wilcox <- calc_wilcox_r(
  x = hh_size$taille_menage,
  g = hh_size$secteur_residence
)

print(effet_wilcox)

readr::write_csv(
  effet_wilcox,
  here::here("output", "tables", "analyse1", "wilcox_effsize_hh_size_urban_rural.csv")
)

# ---------------------------------------------------------
# 21. Figure boxplot taille ménage par zone
# ---------------------------------------------------------
p_hh_size <- ggplot2::ggplot(
  hh_size,
  ggplot2::aes(x = secteur_residence, y = taille_menage)
) +
  ggplot2::geom_boxplot(outlier.alpha = 0.25) +
  ggplot2::labs(
    title = "Figure 6. Distribution de la taille des ménages selon le secteur de résidence",
    subtitle = "Nigeria GHS-Panel, vague 4 (2018)",
    x = "Secteur de résidence",
    y = "Taille du ménage",
    caption = "Source : calculs de l'auteur à partir des données GHS-Panel."
  ) +
  ggplot2::theme_minimal(base_size = 13) +
  ggplot2::theme(
    plot.title = ggplot2::element_text(face = "bold", hjust = 0.5),
    plot.subtitle = ggplot2::element_text(hjust = 0.5),
    axis.title = ggplot2::element_text(face = "bold")
  )

ggplot2::ggsave(
  filename = here::here("output", "figures", "analyse1", "figure6_boxplot_taille_menage_zone_w4.png"),
  plot = p_hh_size,
  width = 8,
  height = 5,
  dpi = 300
)

# ---------------------------------------------------------
# 22. Tableau gtsummary stratifié par zone
# ---------------------------------------------------------
tableau_zone <- hh_size |>
  dplyr::select(hhid, secteur_residence, taille_menage) |>
  dplyr::left_join(
    demo_w4_fr |>
      dplyr::group_by(hhid) |>
      dplyr::summarise(
        age_moyen_menage = mean(age, na.rm = TRUE),
        part_hommes = mean(sexe == "Hommes", na.rm = TRUE),
        .groups = "drop"
      ),
    by = "hhid"
  ) |>
  dplyr::mutate(
    sexe_majoritaire = dplyr::if_else(part_hommes >= 0.5, "Hommes", "Femmes")
  )

tbl_zone <- tableau_zone |>
  dplyr::select(secteur_residence, age_moyen_menage, sexe_majoritaire, taille_menage) |>
  gtsummary::tbl_summary(
    by = secteur_residence,
    statistic = list(
      gtsummary::all_continuous() ~ "{mean} ({sd})",
      gtsummary::all_categorical() ~ "{n} ({p}%)"
    ),
    label = list(
      age_moyen_menage ~ "Âge moyen des membres du ménage",
      sexe_majoritaire ~ "Sexe majoritaire dans le ménage",
      taille_menage ~ "Taille du ménage"
    ),
    missing = "no"
  ) |>
  gtsummary::add_p() |>
  gtsummary::bold_labels()

gtsummary::as_gt(tbl_zone) |>
  gt::gtsave(
    filename = here::here("output", "tables", "analyse1", "tbl_summary_zone_w4.html")
  )

# ---------------------------------------------------------
# 23. Tableau gtsummary descriptif initial
# ---------------------------------------------------------
tbl_demo <- demo_w4_fr |>
  dplyr::select(age, sexe, lien_chef, secteur_residence) |>
  gtsummary::tbl_summary(
    by = sexe,
    statistic = list(
      age ~ "{mean} ({sd})"
    ),
    missing = "ifany"
  )

gtsummary::as_gt(tbl_demo) |>
  gt::gtsave(
    filename = here::here("output", "tables", "analyse1", "tbl_summary_demo_w4.html")
  )

# ---------------------------------------------------------
# 24. Sauvegarder les bases analytiques
# ---------------------------------------------------------
saveRDS(
  demo_w4_analytic,
  here::here("data", "processed", "clean", "demo_w4_analytic.rds")
)

saveRDS(
  demo_w4_fr,
  here::here("data", "processed", "clean", "demo_w4_fr.rds")
)

cat(
  "\nExports réalisés :\n",
  "- output/tables/analyse1/missing_summary_demo_raw.csv\n",
  "- output/figures/analyse1/vis_miss_demo_raw_sample.png\n",
  "- output/tables/analyse1/age_stats_w4.csv\n",
  "- output/tables/analyse1/tab_relation_head_w4.csv\n",
  "- output/tables/analyse1/wilcox_hh_size_urban_rural.txt\n",
  "- output/tables/analyse1/wilcox_effsize_hh_size_urban_rural.csv\n",
  "- output/tables/analyse1/tbl_summary_zone_w4.html\n",
  "- data/processed/clean/demo_w4_analytic.rds\n",
  "- data/processed/clean/demo_w4_fr.rds\n",
  sep = ""
)

cat("\nAnalyse 1 - préparation terminée avec succès.\n")