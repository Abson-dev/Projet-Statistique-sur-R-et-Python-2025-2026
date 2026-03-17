###############################################################################
# 03_analyse.R — Analyses statistiques et génération des livrables
# Projet : TP1 — Profil démographique des ménages nigérians
###############################################################################

cat("\n========== 03_analyse : Analyses et livrables ==========\n")

# ===================================================================
# TÂCHE 2 : Analyse univariée de l'âge
# ===================================================================

stats_age <- sect1_hw4 %>%
  filter(!is.na(age)) %>%
  summarise(
    N = n(), Moyenne = round(mean(age), 2), Médiane = median(age),
    Q1 = quantile(age, 0.25), Q3 = quantile(age, 0.75),
    Écart_type = round(sd(age), 2),
    CV = round(sd(age) / mean(age) * 100, 2),
    Asymétrie = round(skewness(age), 3),
    Min = min(age), Max = max(age)
  )
cat("\n--- Statistiques descriptives de l'âge ---\n")
print(stats_age)

# Histogramme
p_hist_age <- ggplot(sect1_hw4 %>% filter(!is.na(age)), aes(x = age)) +
  geom_histogram(binwidth = 5, fill = "#3C78B4", color = "white", alpha = 0.85) +
  geom_vline(aes(xintercept = median(age, na.rm = TRUE)),
             linetype = "dashed", color = "#B2182B", linewidth = 0.7) +
  annotate("text", x = median(sect1_hw4$age, na.rm = TRUE) + 5,
           y = Inf, vjust = 2, label = "Médiane = 18",
           color = "#B2182B", size = 3, fontface = "italic") +
  labs(title = "Distribution de l'âge des membres des ménages",
       subtitle = "GHS Panel — Vague 4 (Post-Harvest 2018)",
       x = "Âge (années)", y = "Effectif")

# Boîte à moustaches
p_box_age <- ggplot(sect1_hw4 %>% filter(!is.na(age)), aes(y = age)) +
  geom_boxplot(fill = "#3C78B4", alpha = 0.6, width = 0.4) +
  coord_flip() +
  labs(title = "Boîte à moustaches de l'âge",
       subtitle = "GHS Panel — Vague 4", y = "Âge (années)")

# Test de Shapiro-Wilk
set.seed(2025)
test_shapiro <- shapiro.test(
  sect1_hw4 %>% filter(!is.na(age)) %>% slice_sample(n = 5000) %>% pull(age)
)
cat("\n--- Test de Shapiro-Wilk ---\n")
cat("W =", round(test_shapiro$statistic, 5),
    "| p-value =", format(test_shapiro$p.value, scientific = TRUE), "\n")

# ===================================================================
# TÂCHE 3 : Pyramide des âges
# ===================================================================

pyramide_data <- sect1_hw4 %>%
  filter(!is.na(age), !is.na(s1q2)) %>%
  mutate(
    sexe = factor(s1q2, levels = c(1, 2), labels = c("Homme", "Femme")),
    groupe_age = cut(age, breaks = seq(0, 100, 5), right = FALSE,
                     labels = paste0(seq(0, 95, 5), "-", seq(4, 99, 5)),
                     include.lowest = TRUE)
  ) %>%
  filter(!is.na(groupe_age)) %>%
  count(sexe, groupe_age) %>%
  mutate(n = ifelse(sexe == "Homme", -n, n))

p_pyramide <- ggplot(pyramide_data, aes(x = groupe_age, y = n, fill = sexe)) +
  geom_bar(stat = "identity", width = 0.85) + coord_flip() +
  scale_y_continuous(labels = function(x) format(abs(x), big.mark = " ")) +
  scale_fill_manual(values = c("Homme" = "#2166AC", "Femme" = "#B2182B"),
                    name = "Sexe") +
  labs(title = "Pyramide des âges des membres des ménages",
       subtitle = "GHS Panel Nigeria — Vague 4 (2018)",
       x = "Groupe d'âge", y = "Effectif") +
  theme(axis.text.y = element_text(size = 8))

# ===================================================================
# TÂCHE 4 : Lien de parenté
# ===================================================================

freq_parente <- sect1_hw4 %>%
  filter(!is.na(lien_parente)) %>%
  count(lien_parente, sort = TRUE) %>%
  mutate(
    proportion = round(n / sum(n) * 100, 2),
    lien_parente = fct_reorder(lien_parente, n)
  )

p_parente <- ggplot(freq_parente, aes(x = lien_parente, y = n)) +
  geom_col(fill = "#3C78B4", alpha = 0.85) +
  geom_text(aes(label = paste0(proportion, "%")), hjust = -0.1, size = 3) +
  coord_flip() +
  scale_y_continuous(expand = expansion(mult = c(0, 0.18))) +
  labs(title = "Répartition selon le lien de parenté",
       subtitle = "GHS Panel Nigeria — Vague 4 (2018)", x = NULL, y = "Effectif")

# IC à 95 %
cat("\n--- Proportions avec IC à 95 % ---\n")
sect1_clean <- sect1_hw4 %>%
  filter(!is.na(lien_parente)) %>%
  mutate(lien_4cat = ifelse(lien_parente %in%
    c("Chef de ménage", "Conjoint(e)", "Enfant biologique"),
    lien_parente, "Autre"))
n_total <- nrow(sect1_clean)
for (cat_l in c("Enfant biologique","Chef de ménage","Conjoint(e)","Autre")) {
  n_cat <- sum(sect1_clean$lien_4cat == cat_l)
  tb <- binom.test(n_cat, n_total)
  cat(sprintf("  %s : %.2f%% [%.2f%% - %.2f%%]\n",
              cat_l, tb$estimate*100, tb$conf.int[1]*100, tb$conf.int[2]*100))
}

# ===================================================================
# TÂCHE 5 : Comparaison rural / urbain
# ===================================================================

cat("\n--- Taille des ménages par zone ---\n")
taille_zone %>%
  group_by(zone) %>%
  summarise(N = n(), Moyenne = round(mean(taille), 2),
            Médiane = median(taille), Écart_type = round(sd(taille), 2),
            .groups = "drop") %>%
  print()

p_box_zone <- ggplot(taille_zone, aes(x = zone, y = taille, fill = zone)) +
  geom_boxplot(alpha = 0.7, outlier.alpha = 0.3) +
  scale_fill_manual(values = c("Urbain" = "#E66101", "Rural" = "#5E3C99")) +
  labs(title = "Taille des ménages selon la zone de résidence",
       subtitle = "GHS Panel Nigeria — Vague 4 (2018)",
       x = "Zone", y = "Nombre de membres") +
  theme(legend.position = "none")

# Test de Wilcoxon
test_wilcox <- wilcox.test(taille ~ zone, data = taille_zone, conf.int = TRUE)
n1 <- sum(taille_zone$zone == "Urbain")
n2 <- sum(taille_zone$zone == "Rural")
r_rang <- abs(qnorm(test_wilcox$p.value / 2)) / sqrt(n1 + n2)
cat("\n--- Test de Wilcoxon ---\n")
cat("W =", format(test_wilcox$statistic, big.mark = " "),
    "| p =", format(test_wilcox$p.value, scientific = TRUE),
    "| r =", round(r_rang, 4),
    "(", ifelse(r_rang < 0.3, "faible", "moyenne"), ")\n")

# ===================================================================
# TÂCHE 6 : Tableau gtsummary exportable
# ===================================================================

donnees_gts <- sect1_hw4 %>%
  mutate(
    sexe = factor(s1q2, levels = c(1,2), labels = c("Homme","Femme")),
    zone = factor(sector, levels = c(1,2), labels = c("Urbain","Rural"))
  ) %>%
  left_join(taille_menage, by = "hhid")

tab_gts <- donnees_gts %>%
  select(zone, age, sexe, taille) %>%
  tbl_summary(
    by = zone,
    statistic = list(
      all_continuous()  ~ "{mean} ({sd}) | Méd: {median} [{p25}-{p75}]",
      all_categorical() ~ "{n} ({p}%)"
    ),
    label = list(age ~ "Âge (années)", sexe ~ "Sexe",
                 taille ~ "Taille du ménage"),
    missing = "ifany", missing_text = "Manquantes"
  ) %>%
  add_p(test = list(all_continuous() ~ "wilcox.test",
                    all_categorical() ~ "chisq.test")) %>%
  add_overall() %>%
  modify_header(label ~ "**Variable**") %>%
  bold_labels()

# Export du tableau gtsummary en .docx (livrable demandé)
tab_gts_flex <- as_flex_table(tab_gts)
flextable::save_as_docx(tab_gts_flex,
  path = file.path(chemin_outputs, "tableau_gtsummary_zone.docx"))
cat("\nTableau gtsummary exporté :", file.path(chemin_outputs,
    "tableau_gtsummary_zone.docx"), "\n")

# ===================================================================
# ANALYSES COMPLÉMENTAIRES : évolution inter-vagues
# ===================================================================

stats_par_vague <- donnees_4vagues %>%
  filter(!is.na(age)) %>%
  group_by(vague) %>%
  summarise(
    N = n(), Âge_moyen = round(mean(age), 2), Âge_médian = median(age),
    Pct_moins_15 = round(mean(age < 15) * 100, 1),
    Pct_15_64 = round(mean(age >= 15 & age < 65) * 100, 1),
    Pct_65_plus = round(mean(age >= 65) * 100, 1),
    .groups = "drop"
  )
cat("\n--- Indicateurs démographiques par vague ---\n")
print(stats_par_vague)

pct_femmes_chef <- donnees_4vagues %>%
  filter(s1q3 == 1, !is.na(s1q2)) %>%
  group_by(vague) %>%
  summarise(N = n(), Pct_femmes = round(mean(s1q2 == 2) * 100, 1),
            .groups = "drop")
cat("\n--- Chefs de ménage féminins ---\n")
print(pct_femmes_chef)

# ===================================================================
# FIGURE DE SYNTHÈSE
# ===================================================================

figure_synthese <- (p_hist_age | p_box_age) /
  (p_pyramide) /
  (p_parente | p_box_zone) +
  plot_annotation(
    title = "TP1 — Profil démographique des ménages nigérians",
    subtitle = "GHS Panel — Vague 4 (2018)",
    caption = paste0("Source : GHS Panel (LSMS-ISA, Banque Mondiale)\n",
                     "Auteurs : D.L. AGNANGMA SANAM & C. THIOUB | ENSAE 2025-2026"),
    theme = theme(
      plot.title = element_text(face = "bold", size = 15, hjust = 0.5),
      plot.subtitle = element_text(size = 11, hjust = 0.5),
      plot.caption = element_text(size = 8, hjust = 1, color = "grey50")
    )
  )

# ===================================================================
# SAUVEGARDE DES LIVRABLES DANS outputs/
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

cat("\n--- Graphiques sauvegardés dans", chemin_outputs, "---\n")
cat("[03_analyse] Analyses et livrables terminés.\n")
