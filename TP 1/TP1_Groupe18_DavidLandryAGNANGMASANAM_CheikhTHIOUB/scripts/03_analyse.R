###############################################################################
# 03_analyse.R -- Analyses statistiques ponderees et generation des livrables
# Projet : TP1 -- Profil demographique des menages nigerians
###############################################################################

cat("\n========== 03_analyse : Analyses ponderees et livrables ==========\n")

# ===================================================================
# TACHE 2 : Analyse univariee de l'age (ponderee)
# ===================================================================

# Statistiques ponderees de l'age
sect1_w <- sect1_hw4 %>% filter(!is.na(age), !is.na(wt_wave4))
stats_age <- sect1_w %>%
  summarise(
    N           = n(),
    Moyenne_p   = round(weighted.mean(age, wt_wave4), 2),
    Mediane     = median(age),
    Q1          = quantile(age, 0.25),
    Q3          = quantile(age, 0.75),
    Ecart_type  = round(sqrt(sum(wt_wave4 * (age - weighted.mean(age, wt_wave4))^2) / sum(wt_wave4)), 2),
    Asymetrie   = round(skewness(age), 3),
    Min         = min(age),
    Max         = max(age)
  )
cat("\n--- Statistiques ponderees de l'age ---\n")
print(stats_age)

# Histogramme pondere
p_hist_age <- ggplot(sect1_w, aes(x = age, weight = wt_wave4)) +
  geom_histogram(binwidth = 5, fill = "#3C78B4", color = "white", alpha = 0.85) +
  geom_vline(xintercept = median(sect1_w$age),
             linetype = "dashed", color = "#B2182B", linewidth = 0.7) +
  annotate("text", x = median(sect1_w$age) + 5, y = Inf, vjust = 2,
           label = "Mediane = 18", color = "#B2182B", size = 3, fontface = "italic") +
  labs(title = "Distribution ponderee de l'age des membres des menages",
       subtitle = "GHS Panel -- Vague 4 (2018)",
       x = "Age (annees)", y = "Effectif pondere")
print(p_hist_age)

# Boite a moustaches
p_box_age <- ggplot(sect1_w, aes(y = age)) +
  geom_boxplot(fill = "#3C78B4", alpha = 0.6, width = 0.4,
               outlier.size = 0.6, outlier.alpha = 0.3) +
  coord_flip() +
  labs(title = "Boite a moustaches de l'age", y = "Age (annees)")
print(p_box_age)

# Test de Shapiro-Wilk
set.seed(2025)
test_shapiro <- shapiro.test(
  sect1_w %>% slice_sample(n = 5000) %>% pull(age)
)
cat("\nShapiro-Wilk : W =", round(test_shapiro$statistic, 5),
    "| p =", format(test_shapiro$p.value, scientific = TRUE), "\n")

# ===================================================================
# TACHE 3 : Pyramide des ages ponderee
# ===================================================================

pyramide_data <- sect1_w %>%
  filter(!is.na(s1q2)) %>%
  mutate(
    sexe = factor(s1q2, levels = c(1, 2), labels = c("Homme", "Femme")),
    groupe_age = cut(age, breaks = seq(0, 100, 5), right = FALSE,
                     labels = paste0(seq(0, 95, 5), "-", seq(4, 99, 5)),
                     include.lowest = TRUE)
  ) %>%
  filter(!is.na(groupe_age)) %>%
  group_by(sexe, groupe_age) %>%
  summarise(n = sum(wt_wave4), .groups = "drop") %>%
  mutate(n = ifelse(sexe == "Homme", -n, n))

p_pyramide <- ggplot(pyramide_data, aes(x = groupe_age, y = n, fill = sexe)) +
  geom_bar(stat = "identity", width = 0.85) + coord_flip() +
  scale_y_continuous(labels = function(x) format(abs(x), big.mark = " ")) +
  scale_fill_manual(values = c("Homme" = "#2166AC", "Femme" = "#B2182B"),
                    name = "Sexe") +
  labs(title = "Pyramide des ages ponderee",
       subtitle = "GHS Panel Nigeria -- Vague 4 (2018)",
       x = "Groupe d'age", y = "Effectif pondere") +
  theme(axis.text.y = element_text(size = 8))
print(p_pyramide)

# ===================================================================
# TACHE 4 : Lien de parente (proportions ponderees)
# ===================================================================

freq_parente <- sect1_w %>%
  filter(!is.na(lien_parente)) %>%
  group_by(lien_parente) %>%
  summarise(n_pond = sum(wt_wave4), .groups = "drop") %>%
  mutate(
    proportion = round(n_pond / sum(n_pond) * 100, 2),
    lien_parente = fct_reorder(lien_parente, n_pond)
  ) %>%
  arrange(desc(n_pond))

p_parente <- ggplot(freq_parente, aes(x = lien_parente, y = n_pond)) +
  geom_col(fill = "#3C78B4", alpha = 0.85) +
  geom_text(aes(label = paste0(proportion, "%")), hjust = -0.1, size = 3) +
  coord_flip() +
  scale_y_continuous(expand = expansion(mult = c(0, 0.18)),
                     labels = label_comma()) +
  labs(title = "Repartition ponderee selon le lien de parente",
       subtitle = "GHS Panel Nigeria -- Vague 4 (2018)",
       x = NULL, y = "Effectif pondere")
print(p_parente)

# IC a 95 % (non pondere, test binomial exact)
cat("\n--- Proportions avec IC a 95 % ---\n")
sect1_clean <- sect1_w %>%
  filter(!is.na(lien_parente)) %>%
  mutate(lien_4cat = ifelse(lien_parente %in%
    c("Chef de menage", "Conjoint(e)", "Enfant biologique"),
    lien_parente, "Autre"))
n_total <- nrow(sect1_clean)
for (cat_l in c("Enfant biologique", "Chef de menage", "Conjoint(e)", "Autre")) {
  n_cat <- sum(sect1_clean$lien_4cat == cat_l)
  tb <- binom.test(n_cat, n_total)
  cat(sprintf("  %s : %.2f%% [%.2f%% - %.2f%%]\n",
              cat_l, tb$estimate*100, tb$conf.int[1]*100, tb$conf.int[2]*100))
}

# ===================================================================
# TACHE 5 : Comparaison taille menage rural/urbain (ponderee)
# ===================================================================

cat("\n--- Taille des menages par zone (ponderee) ---\n")
taille_zone_w <- taille_zone %>% filter(!is.na(wt_wave4))
taille_zone_w %>%
  group_by(zone) %>%
  summarise(
    N = n(),
    Moyenne_p = round(weighted.mean(taille, wt_wave4), 2),
    Mediane = median(taille),
    .groups = "drop"
  ) %>%
  print()

p_box_zone <- ggplot(taille_zone_w, aes(x = zone, y = taille, fill = zone)) +
  geom_boxplot(alpha = 0.7, outlier.alpha = 0.3) +
  scale_fill_manual(values = c("Urbain" = "#E66101", "Rural" = "#5E3C99")) +
  labs(title = "Taille des menages selon la zone de residence",
       subtitle = "GHS Panel Nigeria -- Vague 4 (2018)",
       x = "Zone", y = "Nombre de membres") +
  theme(legend.position = "none")
print(p_box_zone)

# Test de Wilcoxon
test_wilcox <- wilcox.test(taille ~ zone, data = taille_zone_w, conf.int = TRUE)
n1 <- sum(taille_zone_w$zone == "Urbain")
n2 <- sum(taille_zone_w$zone == "Rural")
r_rang <- abs(qnorm(test_wilcox$p.value / 2)) / sqrt(n1 + n2)
cat("Wilcoxon : W =", format(test_wilcox$statistic, big.mark = " "),
    "| p =", format(test_wilcox$p.value, scientific = TRUE),
    "| r =", round(r_rang, 4), "\n")

# ===================================================================
# TACHE 6 : Tableau gtsummary pondere
# ===================================================================

# Tableau recapitulatif pondere construit manuellement (compatible dplyr 1.1.4)
donnees_gts <- sect1_hw4 %>%
  filter(!is.na(wt_wave4), !is.na(age)) %>%
  mutate(
    sexe = factor(s1q2, levels = c(1, 2), labels = c("Homme", "Femme")),
    zone = factor(sector, levels = c(1, 2), labels = c("Urbain", "Rural"))
  ) %>%
  left_join(taille_menage, by = "hhid")

# Fonction utilitaire : statistiques ponderees par zone
stats_zone <- function(df, var, w, zone_var) {
  df %>% group_by(!!sym(zone_var)) %>%
    summarise(
      moy  = round(weighted.mean(!!sym(var), !!sym(w), na.rm=TRUE), 1),
      et   = round(sqrt(sum(!!sym(w) * (!!sym(var) - weighted.mean(!!sym(var), !!sym(w), na.rm=TRUE))^2, na.rm=TRUE) / sum(!!sym(w), na.rm=TRUE)), 1),
      med  = median(!!sym(var), na.rm=TRUE),
      q25  = quantile(!!sym(var), .25, na.rm=TRUE),
      q75  = quantile(!!sym(var), .75, na.rm=TRUE),
      .groups = "drop"
    ) %>%
    mutate(stat = paste0(moy, " (", et, ") | Med: ", med, " [", q25, "-", q75, "]"))
}

# Age par zone
age_z <- stats_zone(donnees_gts, "age", "wt_wave4", "zone")
age_all <- donnees_gts %>%
  summarise(stat = paste0(
    round(weighted.mean(age, wt_wave4, na.rm=TRUE),1), " (",
    round(sqrt(sum(wt_wave4*(age-weighted.mean(age,wt_wave4,na.rm=TRUE))^2,na.rm=TRUE)/sum(wt_wave4,na.rm=TRUE)),1), ") | Med: ",
    median(age,na.rm=TRUE), " [", quantile(age,.25,na.rm=TRUE), "-", quantile(age,.75,na.rm=TRUE), "]"))

# Taille par zone
t_z <- stats_zone(donnees_gts %>% filter(!is.na(taille)), "taille", "wt_wave4", "zone")
t_all <- donnees_gts %>% filter(!is.na(taille)) %>%
  summarise(stat = paste0(
    round(weighted.mean(taille,wt_wave4,na.rm=TRUE),1), " (",
    round(sqrt(sum(wt_wave4*(taille-weighted.mean(taille,wt_wave4,na.rm=TRUE))^2,na.rm=TRUE)/sum(wt_wave4,na.rm=TRUE)),1), ") | Med: ",
    median(taille,na.rm=TRUE), " [", quantile(taille,.25,na.rm=TRUE), "-", quantile(taille,.75,na.rm=TRUE), "]"))

# Sexe par zone (proportions ponderees)
sexe_z <- donnees_gts %>% filter(!is.na(sexe)) %>%
  group_by(zone, sexe) %>%
  summarise(n_p = sum(wt_wave4), .groups="drop") %>%
  group_by(zone) %>%
  mutate(pct = round(n_p / sum(n_p) * 100, 1)) %>%
  filter(sexe == "Femme") %>%
  mutate(stat = paste0(round(n_p), " (", pct, "%)"))
sexe_all <- donnees_gts %>% filter(!is.na(sexe)) %>%
  group_by(sexe) %>%
  summarise(n_p = sum(wt_wave4), .groups="drop") %>%
  mutate(pct = round(n_p / sum(n_p) * 100, 1)) %>%
  filter(sexe == "Femme") %>%
  mutate(stat = paste0(round(n_p), " (", pct, "%)"))

# Tests
p_age <- format(wilcox.test(age ~ zone, data = donnees_gts)$p.value, digits=3)
p_taille <- format(wilcox.test(taille ~ zone, data = donnees_gts %>% filter(!is.na(taille)))$p.value, digits=3)
p_sexe <- format(chisq.test(table(donnees_gts$sexe, donnees_gts$zone))$p.value, digits=3)

# Assemblage du tableau final
tab_recap <- data.frame(
  Variable = c("Age (annees)", "Sexe = Femme", "Taille du menage"),
  Ensemble = c(age_all$stat, sexe_all$stat, t_all$stat),
  Urbain = c(
    age_z$stat[age_z$zone=="Urbain"],
    sexe_z$stat[sexe_z$zone=="Urbain"],
    t_z$stat[t_z$zone=="Urbain"]
  ),
  Rural = c(
    age_z$stat[age_z$zone=="Rural"],
    sexe_z$stat[sexe_z$zone=="Rural"],
    t_z$stat[t_z$zone=="Rural"]
  ),
  p_value = c(p_age, p_sexe, p_taille),
  stringsAsFactors = FALSE
)

cat("\n--- Tableau recapitulatif pondere ---\n")
print(tab_recap)

# Export en flextable Word
library(flextable)
ft_recap <- flextable(tab_recap) %>%
  set_header_labels(Variable="Variable", Ensemble="Ensemble",
                    Urbain="Urbain", Rural="Rural", p_value="p-value") %>%
  bold(j=1) %>% autofit() %>%
  set_caption("Tableau recapitulatif pondere par zone (W4, 2018).")
flextable::save_as_docx(ft_recap,
  path = file.path(chemin_outputs, "tableau_gtsummary_zone.docx"))
cat("Tableau exporte : outputs/tableau_gtsummary_zone.docx\n")

# ===================================================================
# ANALYSES COMPLEMENTAIRES : evolution inter-vagues
# ===================================================================

stats_par_vague <- donnees_4vagues %>%
  filter(!is.na(age)) %>%
  group_by(vague) %>%
  summarise(
    N = n(), Age_moyen = round(mean(age), 2), Age_median = median(age),
    Pct_moins_15 = round(mean(age < 15) * 100, 1),
    Pct_15_64 = round(mean(age >= 15 & age < 65) * 100, 1),
    Pct_65_plus = round(mean(age >= 65) * 100, 1),
    .groups = "drop"
  )
cat("\n--- Indicateurs demographiques par vague ---\n")
print(stats_par_vague)

pct_femmes_chef <- donnees_4vagues %>%
  filter(s1q3 == 1, !is.na(s1q2)) %>%
  group_by(vague) %>%
  summarise(N = n(), Pct_femmes = round(mean(s1q2 == 2) * 100, 1),
            .groups = "drop")
cat("\n--- Chefs de menage feminins ---\n")
print(pct_femmes_chef)

# ===================================================================
# FIGURE DE SYNTHESE
# ===================================================================

figure_synthese <- (p_hist_age | p_box_age) /
  (p_pyramide) /
  (p_parente | p_box_zone) +
  plot_annotation(
    title = "TP1 -- Profil demographique des menages nigerians (pondere)",
    subtitle = "GHS Panel -- Vague 4 (2018)",
    caption = paste0("Source : GHS Panel W4 (LSMS-ISA)\n",
                     "Auteurs : D.L. AGNANGMA SANAM & C. THIOUB | ENSAE 2025-2026")
  )

# ===================================================================
# SAUVEGARDE DES LIVRABLES
# ===================================================================

ggsave(file.path(chemin_outputs, "pyramide_ages_w4.png"),
       p_pyramide, width = 10, height = 7, dpi = 300)
ggsave(file.path(chemin_outputs, "histogramme_age_w4.png"),
       p_hist_age, width = 8, height = 5, dpi = 300)
ggsave(file.path(chemin_outputs, "boxplot_taille_zone.png"),
       p_box_zone, width = 7, height = 5, dpi = 300)
ggsave(file.path(chemin_outputs, "barplot_parente.png"),
       p_parente, width = 9, height = 6, dpi = 300)
ggsave(file.path(chemin_outputs, "figure_synthese_tp1.png"),
       figure_synthese, width = 14, height = 16, dpi = 300)

cat("\n--- Graphiques sauvegardes dans", chemin_outputs, "---\n")
cat("[03_analyse] Analyses ponderees et livrables termines.\n")
