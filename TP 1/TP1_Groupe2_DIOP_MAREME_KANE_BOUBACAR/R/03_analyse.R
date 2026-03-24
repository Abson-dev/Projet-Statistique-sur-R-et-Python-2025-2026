# ============================================================
#  Analyses statistiques — GHS Panel Nigeria W4
# ============================================================

library(dplyr)
library(ggplot2)
library(gtsummary)
library(rstatix)
library(patchwork)
library(haven)
library(scales)

# ============================================================
#  1. Chargement
# ============================================================

base_w4 <- readRDS("data/processed/base_individus_w4.rds")

# ============================================================
#  2. Analyse univariée de l'âge (pondérée)
# ============================================================

age_data <- base_w4 %>%
  filter(!is.na(s1q4), s1q4 <= 110)

cat("Observations retenues :", nrow(age_data), "\n")
cat("Observations exclues  :", nrow(base_w4) - nrow(age_data), "\n\n")

moy <- weighted.mean(age_data$s1q4, age_data$poids_individu, na.rm = TRUE)
med <- median(age_data$s1q4)

stats_age <- age_data %>%
  summarise(
    n          = n(),
    moyenne    = round(moy, 1),
    mediane    = round(med, 1),
    Q1         = round(quantile(s1q4, 0.25), 1),
    Q3         = round(quantile(s1q4, 0.75), 1),
    ecart_type = round(sqrt(weighted.mean(
      (s1q4 - weighted.mean(s1q4, poids_individu))^2,
      poids_individu)), 1),
    CV        = round(ecart_type / moyenne * 100, 1),
    asymetrie = round(mean(((s1q4 - mean(s1q4)) / sd(s1q4))^3), 3)
  )

print(stats_age)

set.seed(42)
shapiro_result <- shapiro.test(sample(age_data$s1q4, 5000))
cat("\nTest de Shapiro-Wilk :\n")
cat("  W =", round(shapiro_result$statistic, 4),
    "| p-value =", format.pval(shapiro_result$p.value, digits = 3), "\n")
cat("  → Distribution non normale (p < 0,001)\n\n")

# --- Histogramme ---
p_hist <- ggplot(age_data, aes(x = s1q4)) +
  geom_histogram(
    aes(weight = poids_individu),
    binwidth = 5, fill = "#4E7CB8", color = "white", alpha = 0.88
  ) +
  geom_vline(xintercept = moy, color = "#C0392B",
             linetype = "dashed", linewidth = 0.9) +
  geom_vline(xintercept = med, color = "#D4880A",
             linetype = "dashed", linewidth = 0.9) +
  annotate("text", x = moy + 5, y = Inf, vjust = 1.5,
           label = paste0("Moyenne = ", round(moy, 1)),
           color = "#C0392B", size = 3.5) +
  annotate("text", x = med - 7, y = Inf, vjust = 3.5,
           label = paste0("Médiane = ", round(med, 1)),
           color = "#D4880A", size = 3.5) +
  scale_y_continuous(labels = label_number(scale = 1e-6, suffix = " M")) +
  labs(
    title    = "Distribution de l'âge des membres des ménages — Nigeria GHS W4",
    subtitle = "Effectifs pondérés | Classes de 5 ans",
    x        = "Âge (années révolues)",
    y        = "Effectif pondéré (millions)",
    caption  = "Source : Nigeria GHS Panel W4, NBS & World Bank (2019)"
  ) +
  theme_minimal(base_size = 13) +
  theme(plot.caption = element_text(size = 9, color = "gray50"))

# --- Boîte à moustaches ---
p_box <- ggplot(age_data, aes(x = "", y = s1q4)) +
  geom_boxplot(
    fill = "#4E7CB8", color = "#2C4F7A", alpha = 0.75,
    outlier.color = "#C0392B", outlier.size = 0.7, outlier.alpha = 0.5
  ) +
  labs(title = "Boîte à moustaches\nde l'âge", x = NULL,
       y = "Âge (années révolues)") +
  theme_minimal(base_size = 13)

p_age <- p_hist + p_box + plot_layout(widths = c(3, 1))

ggsave("output/figures/analyse_age.png", p_age,
       width = 13, height = 6, dpi = 150)

# ============================================================
#  3. Pyramide des âges (pondérée) — Hommes gauche, Femmes droite
# ============================================================

pyramide_data <- base_w4 %>%
  filter(!is.na(s1q4), !is.na(s1q2), s1q4 <= 110) %>%
  mutate(
    groupe_age = cut(
      s1q4,
      breaks = c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,Inf),
      labels = c("0-4","5-9","10-14","15-19","20-24","25-29","30-34",
                 "35-39","40-44","45-49","50-54","55-59","60-64",
                 "65-69","70-74","75-79","80-84","85+"),
      right = FALSE
    ),
    sexe = case_when(
      as.numeric(s1q2) == 1 ~ "Homme",
      as.numeric(s1q2) == 2 ~ "Femme"
    )
  ) %>%
  filter(!is.na(groupe_age), !is.na(sexe))

pyramide_gg <- pyramide_data %>%
  group_by(groupe_age, sexe) %>%
  summarise(effectif = sum(poids_individu, na.rm = TRUE), .groups = "drop") %>%
  mutate(
    effectif_plot = ifelse(sexe == "Homme", -effectif, effectif)
  )

max_eff <- max(abs(pyramide_gg$effectif_plot))
limite  <- ceiling(max_eff / 1e5) * 1e5

p_pyramide <- ggplot(pyramide_gg,
                     aes(x = effectif_plot, y = groupe_age, fill = sexe)) +
  geom_col(width = 0.85, alpha = 0.9) +
  scale_x_continuous(
    limits = c(-limite, limite),
    labels = function(x) format(abs(x) / 1000, big.mark = " ", suffix = "k"),
    breaks = seq(-limite, limite, by = limite / 4)
  ) +
  scale_fill_manual(values = c("Homme" = "#2E6FA3", "Femme" = "#B5471B")) +
  geom_vline(xintercept = 0, linewidth = 0.5, color = "gray20") +
  labs(
    title    = "Pyramide des âges — Nigeria GHS Panel (Vague 4, 2018-2019)",
    subtitle = "Effectifs pondérés par groupe d'âge quinquennal",
    x        = "Effectif pondéré (en milliers)",
    y        = "Groupe d'âge",
    fill     = "Sexe",
    caption  = "Source : Nigeria GHS Panel W4, NBS & World Bank (2019)"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position    = "bottom",
    plot.caption       = element_text(size = 9, color = "gray50"),
    panel.grid.major.y = element_blank()
  )

ggsave("output/figures/pyramide_ages_w4.png", p_pyramide,
       width = 10, height = 10, dpi = 150)

# ============================================================
#  4. Lien de parenté (pondéré )
# ============================================================
library(survey)
library(scales)

# --- Recodage du lien de parenté ---
base_lien <- base_w4 %>%
  filter(!is.na(s1q3), !is.na(poids_individu)) %>%
  mutate(
    lien = case_when(
      s1q3 == 1            ~ "Chef de ménage",
      s1q3 == 2            ~ "Conjoint(e)",
      s1q3 %in% c(3,4,5)  ~ "Enfant du chef",
      TRUE                 ~ "Autre membre"
    )
  )

# --- Plan de sondage pondéré ---
design_lien <- svydesign(
  ids     = ~1,
  weights = ~poids_individu,
  data    = base_lien
)

# --- Proportions pondérées + IC à 95 % cohérents ---
res_lien <- svymean(~factor(lien), design_lien)
ci_lien  <- confint(res_lien, level = 0.95)

# --- Construction du tableau ---
lien_data <- data.frame(
  lien    = gsub("factor\\(lien\\)", "", rownames(ci_lien)),
  prop    = as.numeric(res_lien),
  ic_low  = ci_lien[, 1],
  ic_high = ci_lien[, 2],
  n_brut  = as.numeric(table(base_lien$lien)[
    gsub("factor\\(lien\\)", "", rownames(ci_lien))
  ])
) %>%
  arrange(desc(prop)) %>%
  mutate(lien = factor(lien, levels = lien))

# --- Vérification : prop doit être dans [ic_low, ic_high] ---
cat("Vérification cohérence proportions / IC :\n")
print(lien_data %>%
        mutate(
          coherent = ic_low <= prop & prop <= ic_high,
          prop_pct = paste0(round(prop*100, 1), " %"),
          ic       = paste0("[", round(ic_low*100,1), " % ; ", round(ic_high*100,1), " %]")
        ) %>%
        select(lien, n_brut, prop_pct, ic, coherent))

# --- Calcul dynamique de la limite X ---
limite_x <- max(lien_data$ic_high) + 0.03

cat("\nProportion max      :", round(max(lien_data$prop),    3), "\n")
cat("IC max              :", round(max(lien_data$ic_high),   3), "\n")
cat("Limite axe X fixée  :", round(limite_x,                3), "\n")

# --- Graphique ---
p_lien <- ggplot(lien_data, aes(x = prop, y = lien)) +
  geom_col(fill = "#5B4DA0", color = "#3D3370",
           alpha = 0.85, width = 0.65) +
  geom_errorbar(
    aes(xmin = ic_low, xmax = ic_high),
    width = 0.25, color = "#2A1F6B",
    linewidth = 0.7, orientation = "y"
  ) +
  geom_text(
    aes(label = paste0(round(prop * 100, 1), " %")),
    hjust = -0.2, size = 4, color = "#2C2C2A"
  ) +
  scale_x_continuous(
    labels = percent_format(accuracy = 1),
    limits = c(0, limite_x),
    expand = expansion(mult = c(0, 0))
  ) +
  labs(
    title   = "Lien de parenté avec le chef de ménage — Nigeria GHS W4",
    subtitle = "Proportions pondérées avec intervalles de confiance à 95 %",
    x       = "Proportion (%)",
    y       = NULL,
    caption = "Source : Nigeria GHS Panel W4, NBS & World Bank (2019)"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.caption = element_text(size = 9, color = "gray50"),
    plot.margin  = margin(t = 10, r = 20, b = 10, l = 80, unit = "pt"),
    axis.text.y  = element_text(hjust = 1, margin = margin(r = 8))
  )

ggsave("output/figures/lien_parente.png", p_lien,
       width = 10, height = 6, dpi = 150)
cat("Graphique lien_parente.png sauvegardé.\n")
# ============================================================
#  5. Taille des ménages : rural vs urbain
# ============================================================

taille_menage <- base_w4 %>%
  group_by(hhid) %>%
  summarise(
    taille     = n(),
    zone_label = ifelse(as.numeric(first(sector)) == 1, "Urbain", "Rural"),
    poids_hh   = first(wt_wave4),
    .groups    = "drop"
  ) %>%
  filter(!is.na(zone_label), !is.na(poids_hh))

cat("\nRépartition des ménages par zone :\n")
print(taille_menage %>% count(zone_label))

taille_menage %>%
  group_by(zone_label) %>%
  summarise(
    n            = n(),
    moyenne_pond = round(weighted.mean(taille, poids_hh), 2),
    mediane      = round(median(taille), 1),
    .groups      = "drop"
  ) %>%
  print()

wilcox_result <- wilcox.test(
  taille ~ zone_label, data = taille_menage,
  exact = FALSE, conf.int = TRUE
)
r_effet <- rstatix::wilcox_effsize(taille ~ zone_label, data = taille_menage)

cat("\nTest de Wilcoxon :\n")
print(wilcox_result)
cat("Taille d'effet r =", round(r_effet$effsize, 3), "\n")

p_taille <- ggplot(taille_menage,
                   aes(x = zone_label, y = taille, fill = zone_label)) +
  geom_boxplot(
    alpha = 0.78, width = 0.5,
    outlier.color = "#8B1A1A", outlier.size = 0.8, outlier.alpha = 0.4
  ) +
  scale_fill_manual(values = c("Urbain" = "#2E8B6E", "Rural" = "#A05C12")) +
  annotate(
    "text", x = 1.5, y = max(taille_menage$taille) * 0.92,
    label = paste0("W = ", format(round(wilcox_result$statistic, 0),
                                  big.mark = " "),
                   "\np < 0,001",
                   "\nr = ", round(r_effet$effsize, 3)),
    size = 3.8, color = "gray25"
  ) +
  labs(
    title    = "Taille des ménages selon le milieu de résidence — Nigeria GHS W4",
    subtitle = "Test de Wilcoxon-Mann-Whitney",
    x        = "Milieu de résidence",
    y        = "Nombre de membres du ménage",
    caption  = "Source : Nigeria GHS Panel W4, NBS & World Bank (2019)"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "none",
    plot.caption    = element_text(size = 9, color = "gray50")
  )

ggsave("output/figures/taille_menage.png", p_taille,
       width = 8, height = 6, dpi = 150)

# ============================================================
#  6. Tableau gtsummary (sauvegardé en HTML pour consultation)
# ============================================================

data_gt <- base_w4 %>%
  filter(!is.na(s1q2), !is.na(s1q4)) %>%
  left_join(taille_menage %>% select(hhid, taille, zone_label), by = "hhid") %>%
  filter(!is.na(zone_label)) %>%
  mutate(
    sexe = factor(
      case_when(as.numeric(s1q2) == 1 ~ "Homme",
                as.numeric(s1q2) == 2 ~ "Femme"),
      levels = c("Homme", "Femme")
    )
  )

tableau_gt <- data_gt %>%
  select(zone_label, s1q4, sexe, taille) %>%
  tbl_summary(
    by    = zone_label,
    label = list(
      s1q4   ~ "Âge (années révolues)",
      sexe   ~ "Sexe",
      taille ~ "Taille du ménage (membres)"
    ),
    statistic = list(
      all_continuous()  ~ "{median} [{p25} – {p75}]",
      all_categorical() ~ "{n} ({p}%)"
    ),
    digits  = all_continuous() ~ 1,
    missing = "no"
  ) %>%
  add_p(test = list(
    all_continuous()  ~ "wilcox.test",
    all_categorical() ~ "chisq.test"
  )) %>%
  add_overall() %>%
  bold_labels() %>%
  modify_header(
    label   ~ "**Caractéristique**",
    stat_0  ~ "**Ensemble**\nN = {N}",
    stat_1  ~ "**Rural**\nN = {N}",
    stat_2  ~ "**Urbain**\nN = {N}",
    p.value ~ "**p-valeur**"
  ) %>%
  modify_caption(
    "**Tableau 1. Caractéristiques démographiques par milieu de résidence — Nigeria GHS W4**"
  ) %>%
  modify_footnote(
    p.value ~ "Wilcoxon-Mann-Whitney (variables continues) ; Chi-deux (variables catégorielles)"
  )

print(tableau_gt)

tableau_gt %>%
  as_gt() %>%
  gt::gtsave("output/tables/tableau_gtsummary.html")