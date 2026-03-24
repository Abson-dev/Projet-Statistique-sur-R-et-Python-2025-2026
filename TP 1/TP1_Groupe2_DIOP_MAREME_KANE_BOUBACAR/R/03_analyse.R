# ============================================================
#  Analyses statistiques — GHS Panel Nigeria W4
#  Produit les figures dans output/figures/
#  et le tableau dans output/tables/
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

sect1_w4_clean <- readRDS("data/processed/sect1_w4_clean.rds")

# ============================================================
#  2. Analyse univariée de l'âge
# ============================================================

# Filtre unique — les âges > 110 ans sont considérés aberrants
age_data <- sect1_w4_clean %>%
  filter(!is.na(s1q4), s1q4 <= 110)

cat("Observations retenues pour l'analyse de l'âge :", nrow(age_data), "\n")
cat("Observations exclues (âge > 110) :",
    nrow(sect1_w4_clean) - nrow(age_data), "\n\n")

# --- Statistiques descriptives ---
stats_age <- age_data %>%
  summarise(
    n          = n(),
    moyenne    = round(mean(s1q4), 1),
    mediane    = round(median(s1q4), 1),
    Q1         = round(quantile(s1q4, 0.25), 1),
    Q3         = round(quantile(s1q4, 0.75), 1),
    ecart_type = round(sd(s1q4), 1),
    CV         = round((sd(s1q4) / mean(s1q4)) * 100, 1),
    asymetrie  = round(mean(((s1q4 - mean(s1q4)) / sd(s1q4))^3), 3)
  )

print(stats_age)

# --- Test de normalité (Shapiro-Wilk sur échantillon de 5000) ---
set.seed(42)
shapiro_test <- shapiro.test(sample(age_data$s1q4, 5000))
print(shapiro_test)

# --- Histogramme ---
p_hist <- ggplot(age_data, aes(x = s1q4)) +
  geom_histogram(binwidth = 5, fill = "#3B8BD4", color = "white", alpha = 0.85) +
  geom_vline(aes(xintercept = mean(s1q4)),
             color = "#E24B4A", linetype = "dashed", linewidth = 0.8) +
  geom_vline(aes(xintercept = median(s1q4)),
             color = "#EF9F27", linetype = "dashed", linewidth = 0.8) +
  labs(
    title    = "Distribution de l'âge des membres du ménage — W4",
    subtitle = "Ligne rouge = moyenne,  Ligne orange = médiane",
    x        = "Âge (années)",
    y        = "Effectif"
  ) +
  theme_minimal(base_size = 13)

# --- Boîte à moustaches ---
p_box <- ggplot(age_data, aes(x = "", y = s1q4)) +
  geom_boxplot(fill = "#3B8BD4", alpha = 0.7,
               outlier.color = "#E24B4A", outlier.size = 0.8) +
  labs(title = "Boîte à moustaches", x = NULL, y = "Âge (années)") +
  theme_minimal(base_size = 13)

# --- Combinaison et export ---
p_age <- p_hist + p_box + plot_layout(widths = c(3, 1))

ggsave("output/figures/analyse_age.png", p_age,
       width = 12, height = 6, dpi = 150)

# ============================================================
#  3. Pyramide des âges par sexe
# ============================================================

pyramide_data <- sect1_w4_clean %>%
  filter(!is.na(s1q4), !is.na(s1q2), s1q4 <= 110) %>%
  mutate(
    groupe_age = cut(
      s1q4,
      breaks = c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,Inf),
      labels = c("0-4","5-9","10-14","15-19","20-24","25-29","30-34",
                 "35-39","40-44","45-49","50-54","55-59","60-64",
                 "65-69","70-74","75-79","80-84","85+"),
      right  = FALSE
    ),
    sexe = haven::as_factor(s1q2)
  ) %>%
  filter(!is.na(groupe_age))

pyramide_gg <- pyramide_data %>%
  count(groupe_age, sexe) %>%
  mutate(
    sexe_label = as.character(sexe),
    n_plot     = ifelse(
      grepl("MALE", sexe_label) & !grepl("FEMALE", sexe_label), -n, n
    )
  )

p_pyramide <- ggplot(pyramide_gg,
                     aes(x = n_plot, y = groupe_age, fill = sexe_label)) +
  geom_col() +
  scale_x_continuous(labels = function(x) format(abs(x), big.mark = " ")) +
  scale_fill_manual(
    values = c("1. MALE" = "#3B8BD4", "2. FEMALE" = "#D85A30"),
    labels = c("1. MALE" = "Homme",   "2. FEMALE" = "Femme")
  ) +
  geom_vline(xintercept = 0, linewidth = 0.5, color = "gray30") +
  labs(
    title    = "Pyramide des âges — Nigeria GHS Panel (Vague 4, 2018-2019)",
    subtitle = "Distribution par groupe d'âge quinquennal et par sexe",
    x = "Effectif", y = "Groupe d'âge", fill = "Sexe",
    caption  = "Source : Nigeria GHS Panel W4, NBS & World Bank (2019)"
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom",
        plot.caption = element_text(size = 9, color = "gray50"))

ggsave("output/figures/pyramide_ages_w4.png", p_pyramide,
       width = 10, height = 10, dpi = 150)

# ============================================================
#  4. Fréquences du lien de parenté
# ============================================================

lien_data <- sect1_w4_clean %>%
  filter(!is.na(s1q3)) %>%
  mutate(
    lien = case_when(
      s1q3 == 1           ~ "Chef de ménage",
      s1q3 == 2           ~ "Conjoint(e)",
      s1q3 %in% c(3,4,5)  ~ "Enfant du CM",  # biologiques, adoptifs, beaux-enfants
      TRUE                ~ "Autre"
    )
  ) %>%
  count(lien) %>%
  mutate(
    prop    = n / sum(n),
    ic_low  = mapply(function(x, n) binom.test(x, n)$conf.int[1], n, sum(n)),
    ic_high = mapply(function(x, n) binom.test(x, n)$conf.int[2], n, sum(n))
  ) %>%
  arrange(desc(prop)) %>%
  mutate(lien = factor(lien, levels = lien))

# geom_errorbar() avec orientation = "y" (geom_errorbarh déprécié depuis ggplot2 4.0.0)
p_lien <- ggplot(lien_data, aes(x = prop, y = lien)) +
  geom_col(fill = "#534AB7", alpha = 0.85) +
  geom_errorbar(
    aes(xmin = ic_low, xmax = ic_high),
    width = 0.3, color = "#3C3489",
    linewidth = 0.7, orientation = "y"
  ) +
  geom_text(
    aes(label = paste0(round(prop * 100, 1), " %")),
    hjust = -0.2, size = 4, color = "#2C2C2A"
  ) +
  scale_x_continuous(
    labels = scales::percent,
    expand = expansion(mult = c(0, 0.12))
  ) +
  labs(
    title    = "Lien de parenté avec le chef de ménage — W4",
    subtitle = "Proportions avec intervalles de confiance à 95 % (test binomial exact)",
    x        = "Proportion",
    y        = NULL,
    caption  = "Source : Nigeria GHS Panel W4, NBS & World Bank (2019)"
  ) +
  theme_minimal(base_size = 13) +
  theme(plot.caption = element_text(size = 9, color = "gray50"))

ggsave("output/figures/lien_parente.png", p_lien,
       width = 10, height = 6, dpi = 150)

# ============================================================
#  5. Taille des ménages selon le milieu de résidence
# ============================================================

taille_menage <- sect1_w4_clean %>%
  group_by(hhid) %>%
  summarise(
    taille  = n(),
    sector  = first(sector),
    .groups = "drop"
  ) %>%
  filter(!is.na(sector)) %>%
  mutate(zone_label = ifelse(as.numeric(sector) == 1, "Urbain", "Rural"))

cat("\nRépartition des ménages par zone :\n")
print(taille_menage %>% count(zone_label))

# --- Test de Wilcoxon-Mann-Whitney ---
wilcox_result <- wilcox.test(
  taille ~ zone_label,
  data     = taille_menage,
  exact    = FALSE,
  conf.int = TRUE
)
print(wilcox_result)

# --- Taille d'effet ---
r_effet <- rstatix::wilcox_effsize(taille ~ zone_label, data = taille_menage)
print(r_effet)

# --- Boxplot avec annotation du test ---
p_taille <- ggplot(taille_menage,
                   aes(x = zone_label, y = taille, fill = zone_label)) +
  geom_boxplot(alpha = 0.75,
               outlier.color = "#E24B4A", outlier.size = 0.8) +
  scale_fill_manual(values = c("Urbain" = "#1D9E75", "Rural" = "#BA7517")) +
  annotate("text", x = 1.5, y = max(taille_menage$taille) * 0.93,
           label = paste0("W = ", format(round(wilcox_result$statistic, 0),
                                         big.mark = " "),
                          "\np < 0,001\nr = ", round(r_effet$effsize, 3)),
           size = 3.8, color = "gray30") +
  labs(
    title    = "Taille des ménages : zones rurales vs urbaines — W4",
    subtitle = "Test de Wilcoxon-Mann-Whitney",
    x = NULL, y = "Taille du ménage",
    caption  = "Source : Nigeria GHS Panel W4, NBS & World Bank (2019)"
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "none",
        plot.caption = element_text(size = 9, color = "gray50"))

ggsave("output/figures/taille_menage.png", p_taille,
       width = 8, height = 6, dpi = 150)

# ============================================================
#  6. Tableau gtsummary
# ============================================================

data_gt <- sect1_w4_clean %>%
  filter(!is.na(s1q2), !is.na(s1q4)) %>%
  left_join(taille_menage %>% select(hhid, taille, zone_label), by = "hhid") %>%
  filter(!is.na(zone_label)) %>%
  mutate(sexe = haven::as_factor(s1q2))

tableau_gt <- data_gt %>%
  select(zone_label, s1q4, sexe, taille) %>%
  tbl_summary(
    by        = zone_label,
    label     = list(
      s1q4   ~ "Âge (années)",
      sexe   ~ "Sexe",
      taille ~ "Taille du ménage"
    ),
    statistic = list(
      all_continuous()  ~ "{mean} ({sd})",
      all_categorical() ~ "{n} ({p}%)"
    ),
    digits = all_continuous() ~ 1
  ) %>%
  add_p(
    test = list(
      all_continuous()  ~ "wilcox.test",
      all_categorical() ~ "chisq.test"
    )
  ) %>%
  add_overall() %>%
  bold_labels() %>%
  modify_caption("**Tableau 1. Caractéristiques démographiques par zone**") %>%
  modify_footnote(
    p.value ~ "Wilcoxon-Mann-Whitney pour les variables continues ; Chi-deux pour les variables catégorielles"
  )

print(tableau_gt)

tableau_gt %>%
  as_gt() %>%
  gt::gtsave("output/tables/tableau_gtsummary.html")
