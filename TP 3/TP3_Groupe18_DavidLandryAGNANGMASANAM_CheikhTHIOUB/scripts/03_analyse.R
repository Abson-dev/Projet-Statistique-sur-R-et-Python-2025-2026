###############################################################################
# 03_analyse.R -- Analyses statistiques ponderees et generation des livrables
# Projet : TP3 -- Acces aux soins et depenses de sante (GHS Panel W4)
###############################################################################

cat("\n========== 03_analyse : Analyses ponderees et livrables ==========\n")

library(flextable)

sante_w <- sante %>% filter(!is.na(wt_wave4))

# ===================================================================
# TACHE 13 : Taux de morbidite pondere par sexe et groupe d'age
# ===================================================================

taux_morb_global <- round(weighted.mean(sante_w$malade, sante_w$wt_wave4, na.rm=TRUE) * 100, 2)
cat("\nTaux de morbidite pondere global :", taux_morb_global, "%\n")

p_morb <- sante_w %>%
  filter(!is.na(sexe), !is.na(groupe_age)) %>%
  group_by(sexe, groupe_age) %>%
  summarise(taux = weighted.mean(malade, wt_wave4, na.rm=TRUE),
            se = sqrt(taux * (1 - taux) / n()), .groups = "drop") %>%
  ggplot(aes(x = groupe_age, y = taux * 100, fill = sexe)) +
  geom_col(position = position_dodge(0.8), width = 0.7, alpha = 0.85) +
  geom_errorbar(aes(ymin = (taux-1.96*se)*100, ymax = (taux+1.96*se)*100),
                position = position_dodge(0.8), width = 0.25) +
  scale_fill_manual(values = c("Homme" = "#2166AC", "Femme" = "#B2182B")) +
  labs(title = "Taux de morbidite pondere par sexe et groupe d'age",
       subtitle = "GHS Panel Nigeria -- W4 (2018)",
       x = "Groupe d'age", y = "Taux (%)", fill = "Sexe")
print(p_morb)

# ===================================================================
# TACHE 14 : Types de maladies (effectifs ponderes)
# ===================================================================

freq_maladies <- sante_w %>%
  filter(!is.na(type_maladie), malade == 1) %>%
  group_by(type_maladie, cat_maladie) %>%
  summarise(n_p = sum(wt_wave4), .groups = "drop") %>%
  arrange(desc(n_p)) %>% slice_head(n = 10) %>%
  mutate(type_maladie = fct_reorder(type_maladie, n_p))

p_maladies <- ggplot(freq_maladies, aes(x = type_maladie, y = n_p, fill = cat_maladie)) +
  geom_col(alpha = 0.85) +
  geom_text(aes(label = format(round(n_p), big.mark = " ")), hjust = -0.1, size = 3) +
  coord_flip() +
  scale_fill_manual(values = c("Infectieuse"="#E66101", "Chronique"="#5E3C99",
    "Traumatique"="#1B7837", "Symptomatique"="#3C78B4", "Autre"="#999999"), name = "Categorie") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15)), labels = label_comma()) +
  labs(title = "Les 10 affections les plus frequentes (pondere)",
       subtitle = "GHS Panel Nigeria -- W4 (2018)", x = NULL, y = "Effectif pondere")
print(p_maladies)

# ===================================================================
# TACHE 15 : Prestataires consultes (ponderes)
# ===================================================================

freq_prestataire <- sante_w %>%
  filter(!is.na(prestataire), prestataire != "Personne", malade == 1) %>%
  group_by(prestataire) %>%
  summarise(n_p = sum(wt_wave4), .groups = "drop") %>%
  arrange(desc(n_p)) %>%
  mutate(pct = round(n_p / sum(n_p) * 100, 1),
         prestataire = fct_reorder(prestataire, n_p))

p_prestataire <- ggplot(freq_prestataire, aes(x = prestataire, y = n_p)) +
  geom_col(fill = "#3C78B4", alpha = 0.85) +
  geom_text(aes(label = paste0(pct, "%")), hjust = -0.1, size = 3) +
  coord_flip() +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15)), labels = label_comma()) +
  labs(title = "Prestataires de sante consultes (pondere)",
       subtitle = "GHS Panel Nigeria -- W4 (2018)", x = NULL, y = "Effectif pondere")
print(p_prestataire)

# Livrable 1 : Barplot combine
p_barplot_combine <- p_maladies / p_prestataire +
  plot_annotation(title = "TP3 -- Types de maladies et prestataires consultes",
                  caption = "Source : GHS Panel W4 (LSMS-ISA) | ENSAE 2025-2026")
ggsave(file.path(chemin_outputs, "barplot_maladies_prestataires.png"),
       p_barplot_combine, width = 10, height = 10, dpi = 300)

# ===================================================================
# TACHE 16 : Distribution des depenses de sante
# ===================================================================

sante_dep_w <- sante_dep %>% filter(!is.na(wt_wave4))
cat("\nDepenses > 0 : N =", nrow(sante_dep_w),
    "| Mediane =", round(median(sante_dep_w$depense_sante), 0),
    "| Moyenne pond. =", round(weighted.mean(sante_dep_w$depense_sante, sante_dep_w$wt_wave4), 0), "\n")

p_hist_dep <- ggplot(sante_dep_w, aes(x = depense_sante, weight = wt_wave4)) +
  geom_histogram(bins = 40, fill = "#3C78B4", color = "white", alpha = 0.85) +
  scale_x_log10(labels = label_comma()) +
  labs(title = "Distribution ponderee des depenses de sante (echelle log)",
       subtitle = "GHS Panel W4 (2018)", x = "Depense totale (Naira, log)", y = "Effectif pondere")
print(p_hist_dep)

# ===================================================================
# TACHE 17 : Test d'independance recours x quintile
# ===================================================================

tab_cd <- sante_w %>% filter(!is.na(a_consulte), !is.na(quintile))
tab_contingence <- table(tab_cd$a_consulte, tab_cd$quintile)
chi2 <- chisq.test(tab_contingence)
n_obs <- sum(tab_contingence); k <- min(nrow(tab_contingence), ncol(tab_contingence))
v_cramer <- as.numeric(sqrt(chi2$statistic / (n_obs * (k - 1))))
cat("\nChi2 =", round(chi2$statistic, 2), "| p =", format(chi2$p.value, digits=3),
    "| V de Cramer =", round(v_cramer, 4), "\n")

# Export tableau de contingence
tab_export <- as.data.frame.matrix(tab_contingence)
tab_export$Total <- rowSums(tab_export)
tab_export <- rbind(tab_export, Total = colSums(tab_export))
write.csv(tab_export, file.path(chemin_outputs, "tableau_contingence_soins_quintile.csv"))
writeLines(paste0("Chi2 = ", round(chi2$statistic, 2), " | df = ", chi2$parameter,
                  " | p = ", format(chi2$p.value, digits=3),
                  " | V de Cramer = ", round(v_cramer, 4)),
           file.path(chemin_outputs, "tableau_contingence_soins_quintile_tests.txt"))

p_cont <- tab_cd %>%
  count(quintile, a_consulte) %>% group_by(quintile) %>%
  mutate(pct = n / sum(n)) %>%
  ggplot(aes(x = quintile, y = pct, fill = a_consulte)) +
  geom_col(position = "fill", alpha = 0.85) +
  scale_y_continuous(labels = percent) +
  scale_fill_manual(values = c("Oui"="#2166AC","Non"="#CCCCCC"), name = "A consulte") +
  labs(title = "Recours aux soins selon le quintile de richesse",
       subtitle = paste0("Chi2 = ", round(chi2$statistic, 1), " | V de Cramer = ", round(v_cramer, 3)),
       x = "Quintile", y = "Proportion") +
  theme(axis.text.x = element_text(size = 9, angle = 10, hjust = 1))
print(p_cont)

# ===================================================================
# TACHE 18 : Violin plots depenses par zone et quintile
# ===================================================================

sd_z <- sante_dep_w %>% filter(!is.na(zone))
tw_z <- wilcox.test(depense_sante ~ zone, data = sd_z)

p_violin_zone <- ggplot(sd_z, aes(x = zone, y = depense_sante, fill = zone)) +
  geom_violin(alpha = 0.5, scale = "width") +
  geom_boxplot(width = 0.15, outlier.size = 0.5, alpha = 0.8) +
  scale_y_log10(labels = label_comma()) +
  scale_fill_manual(values = c("Urbain"="#E66101","Rural"="#5E3C99")) +
  labs(title = "Depenses de sante par zone",
       subtitle = paste0("Wilcoxon : p = ", format(tw_z$p.value, digits=3)),
       x = "Zone", y = "Depense totale (Naira, log)") +
  theme(legend.position = "none")
print(p_violin_zone)

sd_q <- sante_dep_w %>% filter(!is.na(quintile))
kw <- kruskal.test(depense_sante ~ quintile, data = sd_q)

p_violin_quint <- ggplot(sd_q, aes(x = quintile, y = depense_sante, fill = quintile)) +
  geom_violin(alpha = 0.5, scale = "width") +
  geom_boxplot(width = 0.12, outlier.size = 0.5, alpha = 0.8) +
  scale_y_log10(labels = label_comma()) +
  scale_fill_brewer(palette = "RdYlBu", direction = -1) +
  labs(title = "Depenses de sante par quintile",
       subtitle = paste0("Kruskal-Wallis : H = ", round(kw$statistic, 1),
                         " | p = ", format(kw$p.value, digits=3)),
       x = "Quintile", y = "Depense totale (Naira, log)") +
  theme(legend.position = "none", axis.text.x = element_text(size = 9, angle = 10, hjust = 1))
print(p_violin_quint)

# Livrable 2 : Violin plots combines
p_violin_combine <- p_violin_zone / p_violin_quint +
  plot_annotation(title = "TP3 -- Depenses de sante par zone et par quintile",
                  caption = "Source : GHS Panel W4 (LSMS-ISA) | ENSAE 2025-2026")
ggsave(file.path(chemin_outputs, "violin_depenses_zone_quintile.png"),
       p_violin_combine, width = 10, height = 10, dpi = 300)

# Sauvegardes supplementaires
ggsave(file.path(chemin_outputs, "barplot_morbidite_sexe_age.png"), p_morb, width=9, height=5, dpi=300)
ggsave(file.path(chemin_outputs, "barplot_recours_quintile.png"), p_cont, width=9, height=5, dpi=300)
ggsave(file.path(chemin_outputs, "histogramme_depenses_log.png"), p_hist_dep, width=8, height=5, dpi=300)

cat("\n[03_analyse] Analyses ponderees et livrables termines.\n")
