rm(list = ls())
packages <- c(
  "haven", "labelled", "dplyr", "Hmisc", "stringr",
  "ggplot2", "PropCIs", "forcats", "rcompanion",
  "survey", "gt", "gtsummary", "naniar", "visdat",
  "flextable", "officer", "scales"
)

packages_manquants <- packages[!packages %in% installed.packages()[, "Package"]]
if (length(packages_manquants) > 0) {
  cat("Installation des packages manquants :", paste(packages_manquants, collapse = ", "), "\n")
  install.packages(packages_manquants)
} else {
  cat("Tous les packages sont déjà installés\n")
}
invisible(lapply(packages, library, character.only = TRUE))

# ─────────────────────────────────────────────
# FONCTION UTILITAIRE : sauvegarder flextable
# ─────────────────────────────────────────────
save_flextable <- function(ft, path_html, path_png) {
  # HTML
  save_as_html(ft, path = path_html)
  # PNG via docx temporaire
  tmp_docx <- tempfile(fileext = ".docx")
  save_as_docx(ft, path = tmp_docx)
  # PNG via officer
  doc <- officer::read_docx(tmp_docx)
  officer::docx_summary(doc)
  cat("✅ Sauvegardé :", path_html, "\n")
}

# ─────────────────────────────────────────────
# CREATION DES DOSSIERS OUTPUT
# ─────────────────────────────────────────────
dir.create("output/figures", recursive = TRUE, showWarnings = FALSE)
dir.create("output/tables",  recursive = TRUE, showWarnings = FALSE)

# ─────────────────────────────────────────────
# CHARGEMENT DES DONNÉES
# ─────────────────────────────────────────────
processed_w4 <- read_dta("data/processed/processed_w4.dta") %>%
  mutate(across(-c(hhid, indiv, starts_with("wt_wave"), ends_with("s1q4")), ~ as_factor(.x)))

sect1_harvestw4 <- processed_w4 %>%
  select(hhid, indiv, strata, ea, starts_with("harvest_"), wt_wave4)

# ─────────────────────────────────────────────
# PLAN DE SONDAGE
# ─────────────────────────────────────────────
plan_sondage <- svydesign(
  ids     = ~ea,
  strata  = ~strata,
  weights = ~wt_wave4,
  data    = processed_w4,
  nest    = TRUE
)

# ─────────────────────────────────────────────
# 1. VALEURS MANQUANTES
# ─────────────────────────────────────────────
tab_miss <- sect1_harvestw4 %>%
  reframe(
    variable                                  = names(.),
    `Nombre de valeurs manquantes`            = sapply(., function(x) sum(is.na(x))),
    `Proportion de valeurs manquantes (en %)` = sapply(., function(x) round(mean(is.na(x)) * 100, 2))
  ) %>%
  mutate(variable = recode(variable,
                           "hhid"         = "ID Ménage",
                           "indiv"        = "ID Membre du ménage",
                           "strata"       = "Strate",
                           "ea"           = "Zone de dénombrement (Enumeration Area)",
                           "harvest_s1q2" = "Sexe",
                           "harvest_s1q3" = "Lien de parenté avec le chef de ménage",
                           "harvest_s1q4" = "Age du membre",
                           "wt_wave4"     = "Pondération (vague 4)"
  ))

write.csv(tab_miss, "output/tables/01_valeurs_manquantes.csv", row.names = FALSE)

# Flextable valeurs manquantes
ft_miss <- tab_miss %>%
  flextable() %>%
  set_header_labels(
    variable                                  = "Variable",
    `Nombre de valeurs manquantes`            = "Nombre de valeurs manquantes",
    `Proportion de valeurs manquantes (en %)` = "Proportion (%)"
  ) %>%
  bold(part = "header") %>%
  autofit() %>%
  set_caption("Résumé des valeurs manquantes")

save_as_html(ft_miss, path = "output/tables/01_valeurs_manquantes.html")
save_as_image(ft_miss, path = "output/tables/01_valeurs_manquantes.png")

# Graphique valeurs manquantes
p_miss <- visdat::vis_miss(
  sect1_harvestw4 %>%
    mutate(across(where(is.factor), as.character)) %>%
    as.data.frame() %>%
    setNames(c("ID Ménage", "ID Membre du ménage", "Strate",
               "Zone de dénombrement (Enumeration Area)", "Sexe",
               "Lien de parenté avec le chef de ménage",
               "Age du membre", "Pondération (vague 4)"))
) +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, size = 14, face = "bold"),
    axis.text.y = element_text(size = 16),
    axis.title  = element_text(size = 16),
    plot.title  = element_text(size = 18, face = "bold"),
    legend.text = element_text(size = 16)
  ) +
  labs(
    title   = "Valeurs manquantes pour la Section 1 - Période de soudure (Harvest) - Vague 4",
    caption = "Source : World Bank LSMS-ISA | Section 1 (Vague 4)"
  )

ggsave("output/figures/01_valeurs_manquantes.png",
       plot = p_miss, width = 12, height = 7, dpi = 150)

# ─────────────────────────────────────────────
# 2. COEFFICIENT D'ASYMÉTRIE PONDÉRÉ
# ─────────────────────────────────────────────
x <- sect1_harvestw4$harvest_s1q4
w <- sect1_harvestw4$wt_wave4

ok      <- !is.na(x) & !is.na(w)
x_clean <- x[ok]
w_clean <- w[ok] / sum(w[ok])

x_bar_w <- sum(w_clean * x_clean)
sigma_w <- sqrt(sum(w_clean * (x_clean - x_bar_w)^2))
mu3_w   <- sum(w_clean * (x_clean - x_bar_w)^3)
g1_w    <- mu3_w / sigma_w^3

cat("Coefficient d'asymétrie pondéré :", round(g1_w, 4), "\n")

# ─────────────────────────────────────────────
# 3. HISTOGRAMME DE L'ÂGE
# ─────────────────────────────────────────────
p_hist <- ggplot(sect1_harvestw4,
                 aes(x = harvest_s1q4,
                     weight = wt_wave4 / sum(wt_wave4, na.rm = TRUE))) +
  geom_histogram(binwidth = 5, fill = "steelblue", color = "black") +
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(breaks = seq(0, 130, by = 10)) +
  labs(
    title   = "Distribution de l'âge des membres des ménages",
    x       = "Âge",
    y       = "Proportion",
    caption = "Source : World Bank LSMS-ISA | Section 1 (Vague 4)"
  ) +
  theme_minimal()

ggsave("output/figures/02_histogramme_age.png",
       plot = p_hist, width = 8, height = 5, dpi = 150)

# ─────────────────────────────────────────────
# 4. BOXPLOT DE L'ÂGE PONDÉRÉ
# ─────────────────────────────────────────────
q_age     <- svyquantile(~harvest_s1q4, plan_sondage,
                         quantiles = c(0.25, 0.5, 0.75), na.rm = TRUE)
Q1        <- coef(q_age)[1]
Med       <- coef(q_age)[2]
Q3        <- coef(q_age)[3]
IQR_pond  <- Q3 - Q1
borne_inf <- Q1 - 1.5 * IQR_pond
borne_sup <- Q3 + 1.5 * IQR_pond

df_box <- data.frame(
  x      = "Âge",
  ymin   = borne_inf,
  lower  = Q1,
  middle = Med,
  upper  = Q3,
  ymax   = borne_sup
)

p_box_age <- ggplot(df_box,
                    aes(x = x, ymin = ymin, lower = lower,
                        middle = middle, upper = upper, ymax = ymax)) +
  geom_boxplot(stat = "identity", fill = "steelblue",
               color = "black", width = 0.4) +
  labs(
    title   = "Boxplot pondéré de l'âge des membres des ménages",
    x       = "",
    y       = "Âge",
    caption = "Source : World Bank LSMS-ISA | Section 1 (Vague 4)"
  ) +
  theme_minimal()

ggsave("output/figures/03_boxplot_age.png",
       plot = p_box_age, width = 6, height = 5, dpi = 150)

# ─────────────────────────────────────────────
# 5. STATISTIQUES DESCRIPTIVES PONDÉRÉES
# ─────────────────────────────────────────────
moy_pond <- coef(svymean(~harvest_s1q4, plan_sondage, na.rm = TRUE))
sd_pond  <- sqrt(coef(svyvar(~harvest_s1q4, plan_sondage, na.rm = TRUE)))
cv       <- sd_pond / moy_pond

stats_desc <- data.frame(
  Statistique = c("Moyenne", "Ecart-type", "CV", "Q1", "Médiane", "Q3",
                  "Borne inf boxplot", "Borne sup boxplot", "Asymétrie"),
  Valeur      = round(c(moy_pond, sd_pond, cv, Q1, Med, Q3,
                        borne_inf, borne_sup, g1_w), 4)
)

write.csv(stats_desc, "output/tables/02_stats_descriptives_age.csv", row.names = FALSE)

ft_stats <- stats_desc %>%
  flextable() %>%
  bold(part = "header") %>%
  autofit() %>%
  set_caption("Statistiques descriptives pondérées de l'âge")

save_as_html(ft_stats, path = "output/tables/02_stats_descriptives_age.html")
save_as_image(ft_stats, path = "output/tables/02_stats_descriptives_age.png")

# Tableau quartiles
tab_quartiles <- data.frame(
  Quartile = c("Q1 (25%)", "Médiane (50%)", "Q3 (75%)"),
  Valeur   = round(c(Q1, Med, Q3), 2)
)

ft_quartiles <- tab_quartiles %>%
  flextable() %>%
  bold(part = "header") %>%
  autofit() %>%
  set_caption("Quartiles pondérés de l'âge")

save_as_html(ft_quartiles, path = "output/tables/02b_quartiles_age.html")
save_as_image(ft_quartiles, path = "output/tables/02b_quartiles_age.png")

# ─────────────────────────────────────────────
# 6. QQ-PLOT ET SHAPIRO-WILK
# ─────────────────────────────────────────────
p_qq <- ggplot(sect1_harvestw4, aes(sample = harvest_s1q4)) +
  stat_qq(color = "steelblue") +
  stat_qq_line(color = "red") +
  labs(
    title   = "QQ-plot de l'âge des membres des ménages",
    x       = "Quantiles théoriques",
    y       = "Quantiles observés",
    caption = "Source : World Bank LSMS-ISA | Section 1 (Vague 4)"
  ) +
  theme_minimal()

ggsave("output/figures/04_qqplot_age.png",
       plot = p_qq, width = 7, height = 5, dpi = 150)

set.seed(123)
age_clean  <- sect1_harvestw4$harvest_s1q4[!is.na(sect1_harvestw4$harvest_s1q4)]
age_sample <- sample(age_clean, size = 5000)
test_sw    <- shapiro.test(age_sample)

tab_shapiro <- data.frame(
  N             = length(age_sample),
  Statistique_W = round(test_sw$statistic, 4),
  p_value       = format(test_sw$p.value, scientific = TRUE, digits = 3)
) %>%
  setNames(c("Taille de l'échantillon (N)", "Statistique de test", "p-value"))

write.csv(tab_shapiro, "output/tables/03_shapiro_wilk.csv", row.names = FALSE)

ft_shapiro <- tab_shapiro %>%
  flextable() %>%
  bold(part = "header") %>%
  autofit() %>%
  set_caption("Test de Shapiro-Wilk")

save_as_html(ft_shapiro, path = "output/tables/03_shapiro_wilk.html")
save_as_image(ft_shapiro, path = "output/tables/03_shapiro_wilk.png")

# ─────────────────────────────────────────────
# 7. PYRAMIDE DES ÂGES
# ─────────────────────────────────────────────
pyramid_data <- processed_w4 %>%
  filter(!is.na(harvest_s1q4), !is.na(harvest_s1q2),
         harvest_s1q4 >= 0, harvest_s1q4 <= 110) %>%
  mutate(
    groupe_age = cut(harvest_s1q4,
                     breaks = c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,Inf),
                     labels = c("0-4","5-9","10-14","15-19","20-24","25-29","30-34",
                                "35-39","40-44","45-49","50-54","55-59","60-64","65-69",
                                "70-74","75-79","80+"),
                     right = FALSE)
  ) %>%
  filter(!is.na(groupe_age))

n_total_pyr <- sum(pyramid_data$wt_wave4, na.rm = TRUE)

pyr_count <- pyramid_data %>%
  group_by(groupe_age, harvest_s1q2) %>%
  summarise(n = sum(wt_wave4, na.rm = TRUE), .groups = "drop") %>%
  mutate(
    pct_plot  = ifelse(harvest_s1q2 == "Homme",
                       -n / n_total_pyr * 100,
                       n / n_total_pyr * 100),
    label_pct = format(abs(round(pct_plot, 1)), decimal.mark = ",")
  )

y_lim <- max(abs(pyr_count$pct_plot)) * 1.28

p_pyramid <- ggplot(pyr_count,
                    aes(x = groupe_age, y = pct_plot, fill = harvest_s1q2)) +
  geom_col(width = 0.88, alpha = 0.92) +
  geom_text(
    aes(label = label_pct),
    hjust = ifelse(pyr_count$harvest_s1q2 == "Homme", 1.18, -0.18),
    size  = 3, color = "#2D3436", fontface = "bold"
  ) +
  scale_fill_manual(values = c("Homme" = "steelblue", "Femme" = "#FF6B6B")) +
  scale_y_continuous(
    labels = function(x) abs(x),
    limits = c(-y_lim, y_lim),
    breaks = seq(-8, 8, 2)
  ) +
  coord_flip() +
  geom_hline(yintercept = 0, color = "#2D3436", linewidth = 0.9) +
  annotate("text", x = 17.5, y = -y_lim * 0.75,
           label = "◄  HOMMES", fontface = "bold",
           color = "steelblue", size = 4.5) +
  annotate("text", x = 17.5, y = y_lim * 0.75,
           label = "FEMMES  ►", fontface = "bold",
           color = "#FF6B6B", size = 4.5) +
  labs(
    title    = "Pyramide des âges des membres des ménages",
    subtitle = paste0("Nigeria GHS Panel — Wave 4 (2018) | N = ",
                      format(nrow(pyramid_data), big.mark = " "), " individus"),
    x        = "Groupe d'âge (années)",
    y        = "Part de la population (%)",
    caption  = "Source : World Bank LSMS-ISA | Section 1 (Vague 4)"
  ) +
  theme(
    legend.position    = "none",
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(color = "#EEEEEE", linewidth = 0.5)
  )

ggsave("output/figures/05_pyramide_ages.png",
       plot = p_pyramid, width = 10, height = 7, dpi = 150)

# ─────────────────────────────────────────────
# 8. DISTRIBUTION DU LIEN DE PARENTÉ
# ─────────────────────────────────────────────
df_lien <- processed_w4 %>%
  mutate(harvest_s1q3 = fct_collapse(harvest_s1q3,
                                     "Chef de ménage"        = "Chef de ménage",
                                     "Conjoint(e)"           = "Conjoint(e)",
                                     "Enfant"                = "Enfant",
                                     "Petit-enfant"          = "Petit-enfant",
                                     "Frère/Sœur"            = "Frère/Sœur",
                                     "Père/Mère"             = "Père/Mère",
                                     "Autres apparentés"     = c("Beau-fils/Belle-fille", "Enfant adopté(e)",
                                                                 "Neveu/Nièce", "Beau-frère/Belle-sœur",
                                                                 "Beau-père/Belle-mère", "Autre membre de la famille"),
                                     "Autres non apparentés" = c("Employé(e) domestique", "Autre personne non apparentée")
  ))

modalites <- levels(droplevels(df_lien$harvest_s1q3))

freq_table <- do.call(rbind, lapply(modalites, function(m) {
  df_lien$indicatrice <- as.integer(df_lien$harvest_s1q3 == m)
  plan_tmp <- svydesign(ids = ~ea, strata = ~strata,
                        weights = ~wt_wave4, data = df_lien, nest = TRUE)
  res <- svyciprop(~indicatrice, plan_tmp, method = "logit")
  data.frame(
    harvest_s1q3 = m,
    prop         = as.numeric(coef(res)),
    prop_inf     = as.numeric(confint(res)[1]),
    prop_sup     = as.numeric(confint(res)[2]),
    n            = sum(df_lien$harvest_s1q3 == m)
  )
}))

write.csv(freq_table, "output/tables/04_freq_lien_parente.csv", row.names = FALSE)

ft_lien <- freq_table %>%
  mutate(
    prop     = round(prop * 100, 1),
    prop_inf = round(prop_inf * 100, 1),
    prop_sup = round(prop_sup * 100, 1)
  ) %>%
  flextable() %>%
  set_header_labels(
    harvest_s1q3 = "Lien de parenté",
    prop         = "Proportion (%)",
    prop_inf     = "Borne inf IC 95% (%)",
    prop_sup     = "Borne sup IC 95% (%)",
    n            = "Effectif"
  ) %>%
  bold(part = "header") %>%
  autofit() %>%
  set_caption("Distribution du lien de parenté (Vague 4)")

save_as_html(ft_lien, path = "output/tables/04_freq_lien_parente.html")
save_as_image(ft_lien, path = "output/tables/04_freq_lien_parente.png")

p_lien <- ggplot(freq_table, aes(x = reorder(harvest_s1q3, prop), y = prop)) +
  geom_bar(stat = "identity", fill = "steelblue", color = "black") +
  geom_errorbar(aes(ymin = prop_inf, ymax = prop_sup), width = 0.2) +
  geom_text(
    aes(label = round(prop * 100, 1)),
    hjust = -0.5, size = 3, color = "#2D3436", fontface = "bold"
  ) +
  coord_flip() +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1),
    limits = c(0, max(freq_table$prop_sup) * 1.25)
  ) +
  labs(
    title   = "Distribution du lien de parenté (Vague 4)",
    x       = "Lien de parenté",
    y       = "Proportion pondérée en % (IC 95%)",
    caption = "Source : World Bank LSMS-ISA | Section 1 (Vague 4)"
  ) +
  theme_minimal()

ggsave("output/figures/06_lien_parente.png",
       plot = p_lien, width = 9, height = 6, dpi = 150)

# ─────────────────────────────────────────────
# 9. BOXPLOT TAILLE MÉNAGE + WILCOXON
# ─────────────────────────────────────────────
menages <- processed_w4 %>%
  filter(!is.na(hhid), !is.na(sector)) %>%
  group_by(hhid, sector, ea, strata) %>%
  summarise(
    taille_menage = n(),
    poids         = first(wt_wave4),
    .groups       = "drop"
  )

plan_menage <- svydesign(
  ids     = ~ea,
  strata  = ~strata,
  weights = ~poids,
  data    = menages,
  nest    = TRUE
)

get_stats_secteur <- function(secteur) {
  plan_s <- subset(plan_menage, sector == secteur)
  q      <- svyquantile(~taille_menage, plan_s,
                        quantiles = c(0.25, 0.5, 0.75), na.rm = TRUE)
  Q1  <- coef(q)[1]; Med <- coef(q)[2]; Q3 <- coef(q)[3]
  IQR_s <- Q3 - Q1
  data.frame(
    sector    = secteur,
    Q1        = Q1,
    mediane   = Med,
    Q3        = Q3,
    borne_inf = pmax(Q1 - 1.5 * IQR_s, 0),
    borne_sup = Q3 + 1.5 * IQR_s
  )
}

stats_sector <- rbind(get_stats_secteur("Rural"), get_stats_secteur("Urbain"))

write.csv(stats_sector, "output/tables/05_stats_taille_menage.csv", row.names = FALSE)

ft_stats_menage <- stats_sector %>%
  flextable() %>%
  set_header_labels(
    sector    = "Milieu",
    Q1        = "Q1",
    mediane   = "Médiane",
    Q3        = "Q3",
    borne_inf = "Borne inf",
    borne_sup = "Borne sup"
  ) %>%
  bold(part = "header") %>%
  autofit() %>%
  set_caption("Statistiques pondérées de la taille des ménages par milieu")

save_as_html(ft_stats_menage, path = "output/tables/05_stats_taille_menage.html")
save_as_image(ft_stats_menage, path = "output/tables/05_stats_taille_menage.png")

p_box_menage <- ggplot(stats_sector,
                       aes(x = sector, ymin = borne_inf, lower = Q1,
                           middle = mediane, upper = Q3, ymax = borne_sup,
                           fill = sector)) +
  geom_boxplot(stat = "identity") +
  scale_fill_manual(values = c("Rural" = "lightgreen", "Urbain" = "steelblue")) +
  labs(
    title   = "Taille des ménages selon le milieu de résidence",
    x       = "Milieu de résidence",
    y       = "Taille du ménage",
    caption = "Source : World Bank LSMS-ISA | Section 1 (Vague 4)"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

ggsave("output/figures/07_boxplot_taille_menage.png",
       plot = p_box_menage, width = 7, height = 5, dpi = 150)

wilcox_res     <- wilcox.test(taille_menage ~ sector, data = menages, exact = FALSE)
wilcox_effsize <- wilcoxonR(x = menages$taille_menage, g = menages$sector)

tab_wilcox <- data.frame(
  W       = round(wilcox_res$statistic, 0),
  p_value = format(wilcox_res$p.value, scientific = TRUE, digits = 3),
  r       = round(abs(wilcox_effsize), 3)
) %>%
  setNames(c("Statistique W", "p-value", "Taille d'effet (r)"))

write.csv(tab_wilcox, "output/tables/06_wilcoxon_taille_menage.csv", row.names = FALSE)

ft_wilcox <- tab_wilcox %>%
  flextable() %>%
  bold(part = "header") %>%
  autofit() %>%
  set_caption("Test de Mann-Whitney-Wilcoxon")

save_as_html(ft_wilcox, path = "output/tables/06_wilcoxon_taille_menage.html")
save_as_image(ft_wilcox, path = "output/tables/06_wilcoxon_taille_menage.png")

# ─────────────────────────────────────────────
# 10. TABLEAU GTSUMMARY
# ─────────────────────────────────────────────
plan_recap <- svydesign(
  ids     = ~ea,
  strata  = ~strata,
  weights = ~wt_wave4,
  data    = processed_w4 %>%
    group_by(hhid) %>%
    mutate(taille_menage = n()) %>%
    ungroup(),
  nest    = TRUE
)

tableau_recap <- tbl_svysummary(
  data    = plan_recap,
  by      = sector,
  include = c(harvest_s1q2, harvest_s1q4, taille_menage),
  statistic = list(
    all_continuous()  ~ "{mean}",
    all_categorical() ~ "{n} ({p}%)"
  ),
  digits = list(
    all_continuous()  ~ 1,
    all_categorical() ~ c(0, 1)
  ),
  label = list(
    harvest_s1q4  ~ "Âge (années)",
    harvest_s1q2  ~ "Sexe",
    taille_menage ~ "Taille du ménage"
  ),
  missing = "no"
) %>%
  add_p(
    test = list(
      harvest_s1q4  ~ "svy.t.test",
      harvest_s1q2  ~ "svy.chisq.test",
      taille_menage ~ "svy.t.test"
    )
  ) %>%
  add_overall() %>%
  add_n() %>%
  bold_labels() %>%
  italicize_levels() %>%
  modify_header(label ~ "**Variable**") %>%
  modify_caption("**Tableau 1. Caractéristiques sociodémographiques selon le milieu de résidence**")

# Export CSV
tableau_recap %>%
  as_tibble() %>%
  write.csv("output/tables/07_tableau_recap.csv", row.names = FALSE)

# Export via flextable
ft_recap <- tableau_recap %>%
  as_flex_table() %>%
  autofit()

save_as_html(ft_recap, path = "output/tables/07_tableau_recap.html")
save_as_image(ft_recap, path = "output/tables/07_tableau_recap.png")
