# =============================================================================
# Analyse.R — Tâches 25 à 29 : statistiques, tests et visualisations
# ENSAE ISE 1 — 2025/2026
# =============================================================================

library(dplyr)
library(forcats)
library(ggplot2)
library(rstatix)
library(scales)
library(patchwork)
library(stringr)
library(tidyr)
library(broom)

source("R/fonctions.R")

options(warn = -1)

# Création des dossiers
dir.create("output/figures", recursive = TRUE, showWarnings = FALSE)
dir.create("output/tables", recursive = TRUE, showWarnings = FALSE)

# =============================================================================
# TÂCHE 25 — Top 15 cultures
# =============================================================================

cat(">>> Tâche 25 : Top 15 cultures W4...\n")

secta3ii <- readRDS("data/processed/secta3ii.rds")

top15 <- secta3ii %>%
  filter(!is.na(cropcode)) %>%
  distinct(hhid, cropcode, crop_name, crop_type, wt_wave4) %>%
  group_by(cropcode, crop_name, crop_type) %>%
  summarise(frequence_ponderee = sum(wt_wave4, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(frequence_ponderee)) %>%
  slice_head(n = 15) %>%
  mutate(
    proportion = frequence_ponderee / sum(frequence_ponderee) * 100,
    crop_label = clean_crop_label(crop_name),
    crop_label = fct_reorder(crop_label, frequence_ponderee)
  )

p25 <- ggplot(top15, aes(x = crop_label, y = frequence_ponderee, fill = crop_type)) +
  geom_col(width = 0.75) +
  geom_text(aes(label = paste0(round(proportion, 1), "%")), hjust = -0.1, size = 3.2) +
  coord_flip() +
  scale_fill_manual(values = palette_type, name = "Type de culture") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15)), labels = comma_format()) +
  labs(title = "15 cultures les plus fréquentes — Vague 4 (2018-2019)",
       subtitle = "Pondéré par les poids ménage", x = NULL, 
       y = "Nombre pondéré de ménages", caption = "Source : Nigeria LSMS-ISA, 2018-2019 vague 4") +
  theme_minimal(base_size = 12) +
  theme(panel.grid.major.y = element_blank(), legend.position = "bottom")
p25

ggsave("output/figures/p25_top15_cultures.png", p25, width = 9, height = 6, dpi = 150)
cat("    → output/figures/p25_top15_cultures.png\n\n")

# =============================================================================
# TÂCHE 26 — Diversification culturale
# =============================================================================

cat(">>> Tâche 26 : Diversification culturale W4...\n")

secta3 <- readRDS("data/processed/secta3.rds")

diversification <- secta3 %>%
  filter(!is.na(cropcode)) %>%
  group_by(hhid, area_type, wt_wave4) %>%
  summarise(nb_cultures = n_distinct(cropcode), .groups = "drop") %>%
  filter(nb_cultures > 0) %>%
  mutate(zone = ifelse(area_type == "urban", "Urbain", "Rural"))

stats_div <- weighted_stats(diversification, zone, nb_cultures, wt_wave4)
write.csv(stats_div, "output/tables/t26_stats_diversification.csv", row.names = FALSE)

wilcox_result <- wilcox_test(diversification, nb_cultures ~ zone)
effsize_result <- wilcox_effsize(diversification, nb_cultures ~ zone)

p26a <- ggplot(diversification, aes(x = nb_cultures, fill = zone, weight = wt_wave4)) +
  geom_histogram(binwidth = 1, position = "dodge", color = "white", alpha = 0.85) +
  scale_fill_manual(values = palette_zone, name = "Zone") +
  labs(title = "Distribution pondérée de l'indice de diversification culturale",
       x = "Nombre de cultures par ménage", y = "Nombre pondéré de ménages") +
  theme_minimal(base_size = 11)
p26a

p26b <- ggplot(diversification, aes(x = zone, y = nb_cultures, fill = zone)) +
  geom_violin(trim = FALSE, alpha = 0.7) +
  geom_boxplot(width = 0.12, fill = "white", outlier.size = 0.8, alpha = 0.9) +
  stat_summary(fun = median, geom = "point", size = 3, color = "black", shape = 18) +
  scale_fill_manual(values = palette_zone, guide = "none") +
  annotate("text", x = 1.5, y = max(diversification$nb_cultures) * 0.95,
           label = paste0("Wilcoxon\np = ", format(wilcox_result$p, digits = 3),
                          "\nr = ", round(effsize_result$effsize, 3)),
           size = 3.5, hjust = 0.5) +
  labs(title = "Comparaison Rural et Urbain", x = NULL, y = "Nombre de cultures") +
  theme_minimal(base_size = 11)
p26b

p26 <- (p26a | p26b) + plot_annotation(title = "Indice de diversification culturale", tag_levels = "A")
p26
ggsave("output/figures/p26_diversification.png", p26, width = 12, height = 5, dpi = 150)
cat("    → output/figures/p26_diversification.png\n\n")

# =============================================================================
# TÂCHE 27 — Utilisation des engrais
# =============================================================================

cat(">>> Tâche 27 : Utilisation des engrais W4...\n")

secta11c2 <- readRDS("data/processed/secta11c2.rds")

if(!"used_chemical" %in% names(secta11c2)) {
  secta11c2 <- secta11c2 %>%
    mutate(used_chemical = ifelse(used_npk == 1 | used_urea == 1, 1, 0))
}

# ----------------------------------------------------------------------------
# GRAPHIQUE 1 : 4 TYPES D'ENGRAIS PAR ZONE (avec étiquettes)
# ----------------------------------------------------------------------------

total_menages <- secta11c2 %>%
  filter(!is.na(area_type)) %>%
  group_by(area_type) %>%
  summarise(total = sum(wt_wave4, na.rm = TRUE), .groups = "drop") %>%
  mutate(zone_ru = ifelse(area_type == "urban", "Urbain", "Rural"))

calc_rate <- function(data, var_name, label) {
  data %>%
    filter(!is.na(area_type), {{ var_name }} == 1) %>%
    mutate(zone_ru = ifelse(area_type == "urban", "Urbain", "Rural")) %>%
    group_by(zone_ru) %>%
    summarise(n_menages = sum(wt_wave4, na.rm = TRUE), .groups = "drop") %>%
    left_join(total_menages, by = "zone_ru") %>%
    mutate(
      type_engrais = label,
      taux = n_menages / total,
      ic_lo = (taux + qnorm(0.025)^2/(2*total) + qnorm(0.025)*sqrt(taux*(1-taux)/total + qnorm(0.025)^2/(4*total^2))) / (1 + qnorm(0.025)^2/total),
      ic_hi = (taux + qnorm(0.975)^2/(2*total) + qnorm(0.975)*sqrt(taux*(1-taux)/total + qnorm(0.975)^2/(4*total^2))) / (1 + qnorm(0.975)^2/total)
    )
}

taux_engrais <- bind_rows(
  calc_rate(secta11c2, used_npk, "NPK"),
  calc_rate(secta11c2, used_urea, "Urée"),
  calc_rate(secta11c2, used_organic, "Organique"),
  calc_rate(secta11c2, used_chemical, "Chimique")
)

write.csv(taux_engrais, "output/tables/t27_taux_engrais.csv", row.names = FALSE)

p27 <- ggplot(taux_engrais, aes(x = type_engrais, y = taux, fill = zone_ru)) +
  geom_col(position = position_dodge(0.8), width = 0.7, alpha = 0.9) +
  geom_errorbar(aes(ymin = ic_lo, ymax = ic_hi), 
                position = position_dodge(0.8), width = 0.25, linewidth = 0.5) +
  geom_text(aes(label = paste0(round(taux * 100, 1), "%")), 
            position = position_dodge(0.8), vjust = -0.8, size = 4, fontface = "bold") +
  scale_y_continuous(labels = percent_format(accuracy = 1), 
                     expand = expansion(mult = c(0, 0.2))) +
  scale_fill_manual(values = c("Urbain" = "#2E86AB", "Rural" = "#A23B72"), name = "Zone") +
  labs(title = "Taux d'utilisation des engrais par type et par zone",
       subtitle = "IC 95% (méthode de Wilson) | Pondéré par wt_wave4",
       x = "Type d'engrais", y = "Taux d'utilisation (% ménages)") +
  theme_minimal(base_size = 13) + 
  theme(panel.grid.major.x = element_blank(), legend.position = "bottom")
p27

ggsave("output/figures/p27_utilisation_engrais.png", p27, width = 10, height = 6, dpi = 150)
cat("    → output/figures/p27_utilisation_engrais.png\n")

# ----------------------------------------------------------------------------
# GRAPHIQUE 2 : PAR ÉTAT (avec noms et étiquettes)
# ----------------------------------------------------------------------------

base_etat <- secta11c2 %>%
  filter(!is.na(area_type), !is.na(state)) %>%
  mutate(zone_ru = ifelse(area_type == "urban", "Urbain", "Rural"))

total_menages_etat <- base_etat %>%
  group_by(zone_ru, state) %>%
  summarise(total = sum(wt_wave4, na.rm = TRUE), .groups = "drop")

calc_rate_etat <- function(data, var_name, label) {
  data %>%
    filter(!is.na(area_type), !is.na(state), {{ var_name }} == 1) %>%
    mutate(zone_ru = ifelse(area_type == "urban", "Urbain", "Rural")) %>%
    group_by(zone_ru, state) %>%
    summarise(n_menages = sum(wt_wave4, na.rm = TRUE), .groups = "drop") %>%
    left_join(total_menages_etat, by = c("zone_ru", "state")) %>%
    mutate(
      type_engrais = label,
      taux = n_menages / total,
      ic_lo = (taux + qnorm(0.025)^2/(2*total) + qnorm(0.025)*sqrt(taux*(1-taux)/total + qnorm(0.025)^2/(4*total^2))) / (1 + qnorm(0.025)^2/total),
      ic_hi = (taux + qnorm(0.975)^2/(2*total) + qnorm(0.975)*sqrt(taux*(1-taux)/total + qnorm(0.975)^2/(4*total^2))) / (1 + qnorm(0.975)^2/total)
    )
}

taux_engrais_etat <- bind_rows(
  calc_rate_etat(base_etat, used_chemical, "Chimique")
)

taux_engrais_etat <- taux_engrais_etat %>%
  mutate(state_name = state_labels[as.character(state)])

p27_etat <- taux_engrais_etat %>%
  mutate(state_name = fct_reorder(state_name, taux)) %>%
  ggplot(aes(x = state_name, y = taux, fill = zone_ru)) +
  geom_col(position = position_dodge(0.8), width = 0.7, alpha = 0.9) +
  geom_errorbar(aes(ymin = ic_lo, ymax = ic_hi), position = position_dodge(0.8), width = 0.25) +
  geom_text(aes(label = paste0(round(taux * 100, 1), "%")), 
            position = position_dodge(0.8), hjust = -0.2, size = 2.5) +
  coord_flip() +
  scale_y_continuous(labels = percent_format(accuracy = 1), expand = expansion(mult = c(0, 0.15))) +
  scale_fill_manual(values = palette_zone, name = "Zone") +
  labs(title = "Taux d'utilisation des engrais chimiques par État",
       subtitle = "Définition : engrais chimique = NPK ou Urée",
       x = NULL, y = "Taux d'utilisation (% ménages)") +
  theme_minimal(base_size = 10) +
  theme(axis.text.y = element_text(size = 8))
p27_etat

ggsave("output/figures/p27_engrais_par_etat.png", p27_etat, width = 12, height = 10, dpi = 150)
cat("    → output/figures/p27_engrais_par_etat.png\n")

# ----------------------------------------------------------------------------
# TEST DU CHI-DEUX
# ----------------------------------------------------------------------------

chi2_data <- secta11c2 %>%
  filter(!is.na(area_type), !is.na(used_chemical), !is.na(wt_wave4), wt_wave4 > 0)

contingence_table <- xtabs(wt_wave4 ~ area_type + used_chemical, data = chi2_data)

if (all(contingence_table >= 0) & !any(is.na(contingence_table)) & nrow(contingence_table) > 1 & ncol(contingence_table) > 1) {
  chi2_test <- chisq.test(contingence_table, simulate.p.value = TRUE)
  
  cat("📊 T27 - Test du Chi-deux (pondéré) : χ² =", round(chi2_test$statistic, 3), 
      "- p =", chi2_test$p.value, "\n")
  
  chi2_result <- data.frame(
    chi2 = as.numeric(chi2_test$statistic),
    p_value = chi2_test$p.value,
    df = chi2_test$parameter
  )
} else {
  chi2_result <- data.frame(chi2 = NA, p_value = NA, df = NA)
}
write.csv(chi2_result, "output/tables/t27_chi2_result.csv", row.names = FALSE)

cat("    → output/tables/t27_taux_engrais.csv\n")
cat("    → output/tables/t27_taux_engrais_par_etat.csv\n")
cat("    → output/tables/t27_chi2_result.csv\n\n")

# =============================================================================
# TÂCHE 28 — Rendements par État (Maïs et Millet)
# =============================================================================

cat(">>> Tâche 28 : Rendements par État W4...\n")

secta_rendement <- readRDS("data/processed/secta_rendement.rds")

# Ajout des noms d'États
rendements_clean <- secta_rendement %>%
  filter(outlier == FALSE, !is.na(state)) %>%
  mutate(state_name = state_labels[as.character(state)],
         state_name = factor(state_name, levels = state_labels[as.character(sort(unique(state)))]))

# Graphique Maïs
mais_data <- rendements_clean %>%
  filter(culture == "mais") %>%
  mutate(state_name = fct_reorder(state_name, rendement_kg_ha, median, .na_rm = TRUE))

p28_mais <- ggplot(mais_data, aes(x = state_name, y = rendement_kg_ha, weight = wt_wave4)) +
  geom_boxplot(fill = "#2E86AB", outlier.size = 0.4, alpha = 0.85, linewidth = 0.4) +
  coord_flip() +
  scale_y_continuous(labels = comma_format(), expand = expansion(mult = c(0, 0.05))) +
  labs(title = "Distribution pondérée des rendements de Maïs par État",
       subtitle = "Outliers exclus (IQR × 3) | Pondéré par wt_wave4",
       x = NULL, y = "Rendement (kg/ha)") +
  theme_minimal(base_size = 11) +
  theme(axis.text.y = element_text(size = 8), panel.grid.major.y = element_blank())
p28_mais

# Graphique Millet
millet_data <- rendements_clean %>%
  filter(culture == "millet") %>%
  mutate(state_name = fct_reorder(state_name, rendement_kg_ha, median, .na_rm = TRUE))

p28_millet <- ggplot(millet_data, aes(x = state_name, y = rendement_kg_ha, weight = wt_wave4)) +
  geom_boxplot(fill = "#A23B72", outlier.size = 0.4, alpha = 0.85, linewidth = 0.4) +
  coord_flip() +
  scale_y_continuous(labels = comma_format(), expand = expansion(mult = c(0, 0.05))) +
  labs(title = "Distribution pondérée des rendements de Millet par État",
       subtitle = "Outliers exclus (IQR × 3) | Pondéré par wt_wave4",
       x = NULL, y = "Rendement (kg/ha)") +
  theme_minimal(base_size = 11) +
  theme(axis.text.y = element_text(size = 8), panel.grid.major.y = element_blank())
p28_millet

ggsave("output/figures/p28_rendements_mais.png", p28_mais, width = 10, height = 12, dpi = 150)
ggsave("output/figures/p28_rendements_millet.png", p28_millet, width = 10, height = 10, dpi = 150)

stats_mais <- weighted_stats(rendements_clean %>% filter(culture == "mais"), 
                             state_name, rendement_kg_ha, wt_wave4) %>%
  arrange(desc(mediane))

stats_millet <- weighted_stats(rendements_clean %>% filter(culture == "millet"), 
                               state_name, rendement_kg_ha, wt_wave4) %>%
  arrange(desc(mediane))

write.csv(stats_mais, "output/tables/t28_stats_rendements_mais.csv", row.names = FALSE)
write.csv(stats_millet, "output/tables/t28_stats_rendements_millet.csv", row.names = FALSE)

cat("    → output/figures/p28_rendements_mais.png\n")
cat("    → output/figures/p28_rendements_millet.png\n\n")

# =============================================================================
# TÂCHE 29 — Engrais chimique × rendement
# =============================================================================

cat(">>> Tâche 29 : Engrais chimique vs rendement W4...\n")

rendements_engrais <- secta_rendement %>%
  filter(outlier == FALSE, !is.na(used_chemical)) %>%
  mutate(engrais_label = ifelse(used_chemical == 1, "Avec engrais chimique", "Sans engrais chimique"))

wilcox_rdt <- wilcox_test(rendements_engrais, rendement_kg_ha ~ engrais_label)
effect_rdt <- wilcox_effsize(rendements_engrais, rendement_kg_ha ~ engrais_label)

stats_engrais_pond <- weighted_stats(rendements_engrais, engrais_label, rendement_kg_ha, wt_wave4)
write.csv(stats_engrais_pond, "output/tables/t29_stats_engrais_rendement.csv", row.names = FALSE)

mediane_avec <- stats_engrais_pond$mediane[stats_engrais_pond$engrais_label == "Avec engrais chimique"]
mediane_sans <- stats_engrais_pond$mediane[stats_engrais_pond$engrais_label == "Sans engrais chimique"]
gain_mediane <- mediane_avec - mediane_sans

label_pval <- if (wilcox_rdt$p < 0.001) "p < 0.001" else paste0("p = ", round(wilcox_rdt$p, 4))

effet_texte <- if(abs(effect_rdt$effsize) < 0.1) {
  "négligeable"
} else if(abs(effect_rdt$effsize) < 0.3) {
  "faible"
} else if(abs(effect_rdt$effsize) < 0.5) {
  "modérée"
} else {
  "forte"
}

y_annotation <- quantile(rendements_engrais$rendement_kg_ha, 0.95, na.rm = TRUE)

p29 <- ggplot(rendements_engrais, aes(x = engrais_label, y = rendement_kg_ha, fill = engrais_label, weight = wt_wave4)) +
  geom_boxplot(outlier.size = 0.5, alpha = 0.85) +
  scale_fill_manual(values = c("Avec engrais chimique" = "#2E86AB", "Sans engrais chimique" = "#E8A87C"), guide = "none") +
  scale_y_log10(labels = comma_format()) +
  annotate("text", x = 1.5, y = y_annotation,
           label = paste0("Wilcoxon\n", label_pval, "\nr = ", round(effect_rdt$effsize, 3),
                          "\n(", effet_texte, ")"),
           size = 3.5, hjust = 0.5) +
  labs(title = "Rendement selon l'utilisation d'engrais chimique",
       subtitle = paste0("Gain médian : ", round(gain_mediane, 0), " kg/ha"),
       x = NULL, y = "Rendement kg/ha (échelle log10)") +
  theme_minimal(base_size = 12)
p29

ggsave("output/figures/p29_engrais_rendement.png", p29, width = 7, height = 5, dpi = 150)
cat("    → output/figures/p29_engrais_rendement.png\n\n")

