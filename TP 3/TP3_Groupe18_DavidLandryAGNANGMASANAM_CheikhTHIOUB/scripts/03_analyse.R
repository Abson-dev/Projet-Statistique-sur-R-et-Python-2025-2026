###############################################################################
# 03_analyse.R -- Analyses statistiques et generation des livrables
# Projet : TP3 -- Acces aux soins et depenses de sante (GHS Panel W4)
###############################################################################

cat("\n========== 03_analyse : Analyses et livrables ==========\n")

# ===================================================================
# TACHE 13 : Taux de morbidite par sexe et groupe d'age
# ===================================================================

taux_morb_global <- round(mean(sante$malade, na.rm = TRUE) * 100, 2)
cat("\n--- Taux de morbidite global :", taux_morb_global, "% ---\n")

# Taux par sexe
morb_sexe <- sante %>%
  filter(!is.na(sexe)) %>%
  group_by(sexe) %>%
  summarise(
    N = n(),
    N_malades = sum(malade, na.rm = TRUE),
    Taux = round(mean(malade, na.rm = TRUE) * 100, 2),
    .groups = "drop"
  )
cat("\nMorbidite par sexe :\n")
print(morb_sexe)

# Taux par groupe d'age
morb_age <- sante %>%
  filter(!is.na(groupe_age)) %>%
  group_by(groupe_age) %>%
  summarise(
    N = n(),
    Taux = round(mean(malade, na.rm = TRUE) * 100, 2),
    .groups = "drop"
  )
cat("\nMorbidite par groupe d'age :\n")
print(morb_age)

# Barplot morbidite par sexe et groupe d'age avec IC
p_morb <- sante %>%
  filter(!is.na(sexe), !is.na(groupe_age)) %>%
  group_by(sexe, groupe_age) %>%
  summarise(
    taux = mean(malade, na.rm = TRUE),
    se   = sqrt(taux * (1 - taux) / n()),
    .groups = "drop"
  ) %>%
  ggplot(aes(x = groupe_age, y = taux * 100, fill = sexe)) +
  geom_col(position = position_dodge(0.8), width = 0.7, alpha = 0.85) +
  geom_errorbar(
    aes(ymin = (taux - 1.96 * se) * 100, ymax = (taux + 1.96 * se) * 100),
    position = position_dodge(0.8), width = 0.25
  ) +
  scale_fill_manual(values = c("Homme" = "#2166AC", "Femme" = "#B2182B")) +
  labs(
    title = "Taux de morbidite par sexe et groupe d'age",
    subtitle = "GHS Panel Nigeria -- Vague 4 (2018)",
    x = "Groupe d'age", y = "Taux de morbidite (%)", fill = "Sexe"
  )
print(p_morb)

# ===================================================================
# TACHE 14 : Types de maladies declarees (top 10)
# ===================================================================

freq_maladies <- sante %>%
  filter(!is.na(type_maladie), malade == 1) %>%
  count(type_maladie, cat_maladie, sort = TRUE) %>%
  slice_head(n = 10) %>%
  mutate(type_maladie = fct_reorder(type_maladie, n))

p_maladies <- ggplot(freq_maladies, aes(x = type_maladie, y = n,
                                         fill = cat_maladie)) +
  geom_col(alpha = 0.85) +
  geom_text(aes(label = n), hjust = -0.1, size = 3) +
  coord_flip() +
  scale_fill_manual(
    values = c("Infectieuse" = "#E66101", "Chronique" = "#5E3C99",
               "Traumatique" = "#1B7837", "Symptomatique" = "#3C78B4",
               "Autre" = "#999999"),
    name = "Categorie"
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(
    title = "Les 10 affections les plus frequentes",
    subtitle = "GHS Panel Nigeria -- Vague 4 (2018)",
    x = NULL, y = "Nombre de cas"
  )
print(p_maladies)

# ===================================================================
# TACHE 15 : Recours aux soins - type de prestataire consulte
# ===================================================================

freq_prestataire <- sante %>%
  filter(!is.na(prestataire), prestataire != "Personne", malade == 1) %>%
  count(prestataire, sort = TRUE) %>%
  mutate(
    pct = round(n / sum(n) * 100, 1),
    prestataire = fct_reorder(prestataire, n)
  )

p_prestataire <- ggplot(freq_prestataire, aes(x = prestataire, y = n)) +
  geom_col(fill = "#3C78B4", alpha = 0.85) +
  geom_text(aes(label = paste0(pct, "%")), hjust = -0.1, size = 3) +
  coord_flip() +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(
    title = "Type de prestataire consulte par les malades",
    subtitle = "GHS Panel Nigeria -- Vague 4 (2018)",
    x = NULL, y = "Nombre de consultations"
  )
print(p_prestataire)

# -- Livrable 1 : Barplot maladies + prestataires (combine) --
p_barplot_combine <- p_maladies / p_prestataire +
  plot_annotation(
    title = "TP3 -- Types de maladies et prestataires consultes",
    caption = "Source : GHS Panel W4 (LSMS-ISA) | ENSAE 2025-2026"
  )
ggsave(file.path(chemin_outputs, "barplot_maladies_prestataires.png"),
       p_barplot_combine, width = 10, height = 10, dpi = 300)
cat("\nLivrable : barplot_maladies_prestataires.png sauvegarde\n")

# ===================================================================
# TACHE 16 : Distribution des depenses de sante
# ===================================================================

# Filtre sur les individus ayant eu des depenses > 0
sante_dep <- sante %>%
  filter(depense_sante > 0)

cat("\n--- Depenses de sante (> 0) ---\n")
cat("N =", nrow(sante_dep), "\n")
cat("Moyenne =", round(mean(sante_dep$depense_sante), 0), "Naira\n")
cat("Mediane =", round(median(sante_dep$depense_sante), 0), "Naira\n")
cat("Max =", round(max(sante_dep$depense_sante), 0), "Naira\n")

# Histogramme en echelle log
p_hist_dep <- ggplot(sante_dep, aes(x = depense_sante)) +
  geom_histogram(bins = 40, fill = "#3C78B4", color = "white", alpha = 0.85) +
  scale_x_log10(labels = label_comma()) +
  labs(
    title = "Distribution des depenses de sante (echelle log)",
    subtitle = "Individus ayant eu des depenses > 0 -- GHS Panel W4",
    x = "Depense totale de sante (Naira, echelle log)",
    y = "Effectif"
  )
print(p_hist_dep)

# ===================================================================
# TACHE 17 : Test d'independance recours aux soins x quintile
# ===================================================================

# Tableau de contingence : recours aux soins x quintile de consommation
tab_cont_data <- sante %>%
  filter(!is.na(a_consulte), !is.na(quintile))

tab_contingence <- table(tab_cont_data$a_consulte, tab_cont_data$quintile)
cat("\n--- Tableau de contingence : recours aux soins x quintile ---\n")
print(tab_contingence)

# Test du chi-deux
test_chi2 <- chisq.test(tab_contingence)
cat("\nTest du chi-deux :\n")
cat("  Chi2 =", round(test_chi2$statistic, 2),
    "| df =", test_chi2$parameter,
    "| p =", format(test_chi2$p.value, digits = 3), "\n")

# V de Cramer
n_obs <- sum(tab_contingence)
k <- min(nrow(tab_contingence), ncol(tab_contingence))
v_cramer <- sqrt(test_chi2$statistic / (n_obs * (k - 1)))
cat("  V de Cramer =", round(v_cramer, 4), "\n")

# Barplot 100% empile
p_cont <- tab_cont_data %>%
  count(quintile, a_consulte) %>%
  group_by(quintile) %>%
  mutate(pct = n / sum(n) * 100) %>%
  ggplot(aes(x = quintile, y = pct, fill = a_consulte)) +
  geom_col(position = "fill", alpha = 0.85) +
  scale_y_continuous(labels = percent) +
  scale_fill_manual(values = c("Oui" = "#2166AC", "Non" = "#CCCCCC"),
                    name = "A consulte") +
  labs(
    title = "Recours aux soins selon le quintile de richesse",
    subtitle = paste0("Chi2 = ", round(test_chi2$statistic, 1),
                      " | V de Cramer = ", round(v_cramer, 3)),
    x = "Quintile de consommation", y = "Proportion"
  )
print(p_cont)

# -- Livrable 3 : Tableau de contingence exportable --
# Construction d'un tableau propre avec resultats des tests
tab_export <- as.data.frame.matrix(tab_contingence)
tab_export$Total <- rowSums(tab_export)
tab_export <- rbind(tab_export, Total = colSums(tab_export))

# Ajout des resultats du test en note
tab_export_note <- paste0(
  "Chi2 = ", round(test_chi2$statistic, 2),
  " | df = ", test_chi2$parameter,
  " | p-value = ", format(test_chi2$p.value, digits = 3),
  " | V de Cramer = ", round(v_cramer, 4)
)

# Sauvegarde en CSV
write.csv(tab_export, file.path(chemin_outputs, "tableau_contingence_soins_quintile.csv"),
          row.names = TRUE)
writeLines(tab_export_note,
           file.path(chemin_outputs, "tableau_contingence_soins_quintile_tests.txt"))
cat("\nLivrable : tableau_contingence_soins_quintile.csv + tests.txt sauvegardes\n")

# ===================================================================
# TACHE 18 : Depenses medianes par zone et par quintile (violin plots)
# ===================================================================

# -- Violin plot par zone (rural/urbain) --
sante_dep_zone <- sante_dep %>% filter(!is.na(zone))

test_wilcox_zone <- wilcox.test(depense_sante ~ zone, data = sante_dep_zone)
cat("\n--- Test de Wilcoxon (depenses par zone) ---\n")
cat("  W =", format(test_wilcox_zone$statistic, big.mark = " "),
    "| p =", format(test_wilcox_zone$p.value, digits = 3), "\n")

p_violin_zone <- ggplot(sante_dep_zone, aes(x = zone, y = depense_sante, fill = zone)) +
  geom_violin(alpha = 0.5, scale = "width") +
  geom_boxplot(width = 0.15, outlier.size = 0.5, alpha = 0.8) +
  scale_y_log10(labels = label_comma()) +
  scale_fill_manual(values = c("Urbain" = "#E66101", "Rural" = "#5E3C99")) +
  labs(
    title = "Depenses de sante par zone de residence",
    subtitle = paste0("Wilcoxon : p = ", format(test_wilcox_zone$p.value, digits = 3)),
    x = "Zone", y = "Depense totale (Naira, echelle log)"
  ) +
  theme(legend.position = "none")
print(p_violin_zone)

# -- Violin plot par quintile --
sante_dep_quint <- sante_dep %>% filter(!is.na(quintile))

test_kw <- kruskal.test(depense_sante ~ quintile, data = sante_dep_quint)
cat("\n--- Test de Kruskal-Wallis (depenses par quintile) ---\n")
cat("  H =", round(test_kw$statistic, 2),
    "| df =", test_kw$parameter,
    "| p =", format(test_kw$p.value, digits = 3), "\n")

p_violin_quint <- ggplot(sante_dep_quint,
                          aes(x = quintile, y = depense_sante, fill = quintile)) +
  geom_violin(alpha = 0.5, scale = "width") +
  geom_boxplot(width = 0.12, outlier.size = 0.5, alpha = 0.8) +
  scale_y_log10(labels = label_comma()) +
  scale_fill_brewer(palette = "RdYlBu", direction = -1) +
  labs(
    title = "Depenses de sante par quintile de consommation",
    subtitle = paste0("Kruskal-Wallis : H = ", round(test_kw$statistic, 1),
                      " | p = ", format(test_kw$p.value, digits = 3)),
    x = "Quintile", y = "Depense totale (Naira, echelle log)"
  ) +
  theme(legend.position = "none",
        axis.text.x = element_text(size = 9, angle = 15, hjust = 1))
print(p_violin_quint)

# -- Livrable 2 : Violin plots combines --
p_violin_combine <- p_violin_zone / p_violin_quint +
  plot_annotation(
    title = "TP3 -- Depenses de sante par zone et par quintile",
    caption = "Source : GHS Panel W4 (LSMS-ISA) | ENSAE 2025-2026"
  )
ggsave(file.path(chemin_outputs, "violin_depenses_zone_quintile.png"),
       p_violin_combine, width = 10, height = 10, dpi = 300)
cat("\nLivrable : violin_depenses_zone_quintile.png sauvegarde\n")

# ===================================================================
# SAUVEGARDES SUPPLEMENTAIRES
# ===================================================================

ggsave(file.path(chemin_outputs, "barplot_morbidite_sexe_age.png"),
       p_morb, width = 9, height = 5, dpi = 300)
ggsave(file.path(chemin_outputs, "barplot_recours_quintile.png"),
       p_cont, width = 9, height = 5, dpi = 300)
ggsave(file.path(chemin_outputs, "histogramme_depenses_log.png"),
       p_hist_dep, width = 8, height = 5, dpi = 300)

cat("\n--- Tous les graphiques sauvegardes dans", chemin_outputs, "---\n")
cat("[03_analyse] Analyses et livrables termines.\n")
