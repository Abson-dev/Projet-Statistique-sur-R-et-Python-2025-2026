library(dplyr)
library(ggplot2)
library(forcats)
library(rstatix)
library(ggpubr)
library(gtsummary)
library(viridis)
library(patchwork)
library(scales)

dir.create("outputs", showWarnings = FALSE, recursive = TRUE)

df_adultes <- readRDS("data/processed/df_adultes.rds")
df_educ    <- readRDS("data/processed/df_educ.rds")
df_scol    <- readRDS("data/processed/df_scol.rds")

# palette ordonnée pour les niveaux d'éducation
palette_niveaux <- c(
  "Aucun"            = "#C5A882",
  "Primaire"         = "#8FB08C",
  "Junior Secondary" = "#5A9EA8",
  "Senior Secondary" = "#3A6EA5",
  "Tertiaire"        = "#1E3A6E"
)

# constantes visuelles réutilisées partout
COL_FOND   <- "#FFFFFF"         
COL_GRILLE <- "#F0F0F0"         
COL_TEXTE  <- "#1A1A1A"          
COL_MUTED  <- "#666666"
COL_ACCENT <- "#E63946"       

# thème unique pour tous les graphiques du script
theme_tp2 <- function(base_size = 12) {
  theme_minimal(base_size = base_size) %+replace%
    theme(
      plot.background   = element_rect(fill = COL_FOND,   color = NA),
      panel.background  = element_rect(fill = COL_FOND,   color = NA),
      panel.grid.major  = element_line(color = COL_GRILLE, linewidth = 0.4),
      panel.grid.minor  = element_blank(),
      plot.title        = element_text(face = "bold", size = base_size + 1,
                                       color = COL_TEXTE, margin = margin(b = 4)),
      plot.subtitle     = element_text(color = COL_MUTED, size = base_size - 1,
                                       margin = margin(b = 8)),
      plot.caption      = element_text(color = "#A09080", size = base_size - 3,
                                       hjust = 0, margin = margin(t = 6)),
      axis.title        = element_text(color = "#4A3C2F", size = base_size - 1),
      axis.text         = element_text(color = "#5C4D3E"),
      legend.background = element_rect(fill = COL_FOND, color = NA),
      legend.key        = element_rect(fill = COL_FOND, color = NA),
      legend.title      = element_text(face = "bold", color = COL_TEXTE),
      strip.background  = element_rect(fill = "#EDE6DC", color = NA),
      strip.text        = element_text(face = "bold", color = COL_TEXTE)
    )
}

# Barplot horizontal – fréquences du niveau_educ
cat("\nDistribution du niveau d'éducation \n")

freq_niveau <- df_adultes %>%
  count(niveau_educ) %>%
  mutate(prop  = n / sum(n) * 100,
         label = paste0(round(prop, 1), "%"))
print(freq_niveau)

p8 <- ggplot(freq_niveau,
             aes(x = prop, y = fct_rev(niveau_educ), fill = niveau_educ)) +
  geom_col(width = 0.58, color = COL_FOND, linewidth = 0.3) +
  geom_text(aes(label = label), hjust = -0.12,
            size = 3.5, fontface = "bold", color = COL_TEXTE) +
  scale_fill_manual(values = palette_niveaux, guide = "none") +
  scale_x_continuous(expand = expansion(mult = c(0, 0.14)),
                     labels = function(x) paste0(x, "%")) +
  labs(title    = "Distribution du niveau d'éducation des adultes (18+)",
       subtitle = "Nigeria – GHS Wave 4 (2018)",
       x        = "Proportion (%)",
       y        = NULL,
       caption  = "Source : GHS-W4 Nigeria 2018") +
  theme_tp2() +
  theme(panel.grid.major.y = element_blank())

ggsave("outputs/fig01_barplot_niveau_educ_global.png", p8,
       width = 8, height = 5, dpi = 150, bg = COL_FOND)
cat("  ✓ fig01 sauvegardé\n")

# Barplot 100% empilé par sexe + Chi2 + V de Cramer
cat("\nNiveau d'éducation par sexe \n")

df_sexe_niveau <- df_adultes %>%
  filter(!is.na(sexe_label)) %>%
  count(sexe_label, niveau_educ) %>%
  group_by(sexe_label) %>%
  mutate(prop = n / sum(n) * 100) %>%
  ungroup()

p9 <- ggplot(df_sexe_niveau,
             aes(x = sexe_label, y = prop, fill = niveau_educ)) +
  geom_col(position = "fill", color = COL_FOND,
           width = 0.5, linewidth = 0.3) +
  geom_text(aes(label = ifelse(prop >= 3, paste0(round(prop, 1), "%"), "")),
            position = position_fill(vjust = 0.5),
            size = 3.2, color = "white", fontface = "bold") +
  scale_fill_manual(values = palette_niveaux, name = "Niveau d'éducation") +
  scale_y_continuous(labels = percent_format()) +
  labs(title    = "Niveau d'éducation par sexe (adultes 18+)",
       subtitle = "Barres 100% empilées – Nigeria GHS-W4 2018",
       x        = NULL,
       y        = "Proportion",
       caption  = "Source : GHS-W4 Nigeria 2018") +
  coord_flip() +
  theme_tp2() +
  theme(legend.position = "bottom",
        legend.key.size = unit(0.45, "cm"))

ggsave("outputs/fig02_barplot100_niveau_sexe.png", p9,
       width = 9, height = 5.5, dpi = 150, bg = COL_FOND)
cat("  fig02 sauvegardé\n")

# table de contingence + Chi-deux + V de Cramer
tab_contingence <- table(df_adultes$sexe_label, df_adultes$niveau_educ)
cat("\nTable de contingence Sexe x Niveau :\n")
print(tab_contingence)

test_chi2 <- chisq.test(tab_contingence)
cat("\nTest du Chi-deux :\n"); print(test_chi2)

n_obs    <- sum(tab_contingence)
min_dim  <- min(nrow(tab_contingence) - 1, ncol(tab_contingence) - 1)
v_cramer <- sqrt(test_chi2$statistic / (n_obs * min_dim))
cat(sprintf("\nV de Cramer : %.4f\n", v_cramer))

saveRDS(list(table = tab_contingence, chi2 = test_chi2, v_cramer = v_cramer),
        "data/processed/resultats_tache9.rds")

# Boxplot niveau_educ ~ groupe_age + Kruskal-Wallis + Dunn
cat("\n Niveau d'éducation par groupe d'âge \n")

df_age_educ <- df_adultes %>%
  filter(!is.na(groupe_age)) %>%
  mutate(niveau_num = as.numeric(niveau_educ))

kw_test  <- kruskal.test(niveau_num ~ groupe_age, data = df_age_educ)
cat("\nKruskal-Wallis :\n"); print(kw_test)

dunn_res <- df_age_educ %>%
  dunn_test(niveau_num ~ groupe_age, p.adjust.method = "bonferroni")
cat("\nPost-hoc Dunn :\n"); print(dunn_res)

# palette ambre → indigo pour les groupes d'âge
pal_age <- c(
  "18-30" = "#C0572B",
  "31-45" = "#C8943A",
  "46-60" = "#5A9EA8",
  "60+"   = "#1E3A6E"
)

p10 <- ggplot(df_age_educ,
              aes(x = groupe_age, y = niveau_num, fill = groupe_age)) +
  geom_boxplot(alpha = 0.78,
               outlier.alpha = 0.12, outlier.size = 0.6,
               width = 0.48, color = "#3A2F25") +
  geom_jitter(alpha = 0.04, width = 0.14, size = 0.45,
              color = "#6B5C4A") +
  scale_y_continuous(breaks = 1:5,
                     labels = levels(df_adultes$niveau_educ)) +
  scale_fill_manual(values = pal_age, guide = "none") +
  annotate("label",
           x = 3.5, y = 5.35,
           label = paste0("Kruskal-Wallis : χ² = ",
                          round(kw_test$statistic, 2), ", p < 0.001"),
           fill = COL_FOND, color = COL_MUTED,
           size = 3.2, fontface = "italic", label.size = 0.2) +
  labs(title    = "Niveau d'éducation selon le groupe d'âge (adultes 18+)",
       subtitle = "Médiane et dispersion par tranche d'âge",
       x        = "Groupe d'âge",
       y        = "Niveau d'éducation",
       caption  = "Source : GHS-W4 Nigeria 2018") +
  theme_tp2() +
  theme(panel.grid.major.x = element_blank())

ggsave("outputs/fig03_boxplot_niveau_age.png", p10,
       width = 9, height = 6, dpi = 150, bg = COL_FOND)
cat("  fig03 sauvegardé\n")

saveRDS(list(kruskal = kw_test, dunn = dunn_res),
        "data/processed/resultats_tache10.rds")

# Scolarisation 6-17 ans par zone
cat("\n Scolarisation 6-17 ans par zone \n")
tab_scol_zone <- table(df_scol$zone_scol, df_scol$scolarise)
colnames(tab_scol_zone) <- c("Non scolarisé", "Scolarisé")
cat("\nTable de contingence Zone x Scolarisation :\n"); print(tab_scol_zone)

test_chi2_scol <- chisq.test(tab_scol_zone)
cat("\nChi-deux scolarisation :\n"); print(test_chi2_scol)

df_taux_scol <- df_scol %>%
  group_by(zone_scol) %>%
  summarise(
    n      = n(),
    n_scol = sum(scolarise, na.rm = TRUE),
    prop   = n_scol / n,
    se     = sqrt(prop * (1 - prop) / n),
    lower  = pmax(0, prop - 1.96 * se),
    upper  = pmin(1, prop + 1.96 * se)
  )
cat("\nTaux de scolarisation avec IC 95% :\n"); print(df_taux_scol)

# couleurs Urbain/Rural
pal_zone <- c("Urbain" = "#C0572B", "Rural" = "#5B7A52")

p11 <- ggplot(df_taux_scol,
              aes(x = zone_scol, y = prop * 100, fill = zone_scol,
                  ymin = lower * 100, ymax = upper * 100)) +
  geom_col(width = 0.48, alpha = 0.88, color = "#3A2F25", linewidth = 0.3) +
  geom_errorbar(width = 0.14, color = "#3A2F25", linewidth = 0.75) +
  geom_text(aes(label = paste0(round(prop * 100, 1), "%")),
            vjust = -2.0, fontface = "bold", size = 4.5,
            color = COL_TEXTE) +
  scale_fill_manual(values = pal_zone, guide = "none") +
  scale_y_continuous(limits = c(0, 110),
                     labels = function(x) paste0(x, "%")) +
  annotate("label",
           x = 1.5, y = 105,
           label = paste0("χ² = ", round(test_chi2_scol$statistic, 1),
                          "  |  p < 0.001"),
           fill = COL_FOND, color = COL_MUTED,
           size = 3.3, fontface = "italic", label.size = 0.2) +
  labs(title    = "Taux de scolarisation des enfants (6-17 ans) par zone",
       subtitle = "IC à 95% — Nigeria GHS-W4 2018",
       x        = NULL,
       y        = "Taux de scolarisation (%)",
       caption  = "Source : GHS-W4 Nigeria 2018") +
  theme_tp2() +
  theme(panel.grid.major.x = element_blank())

ggsave("outputs/fig04_scolarisation_zone.png", p11,
       width = 7, height = 5.5, dpi = 150, bg = COL_FOND)
cat("  fig04 sauvegardé\n")

saveRDS(list(table = tab_scol_zone, chi2 = test_chi2_scol, taux = df_taux_scol),
        "data/processed/resultats_tache11.rds")

# Heatmap État × Niveau d'éducation
cat("\n Heatmap analphabétisme par État \n")

df_heatmap <- df_adultes %>%
  filter(!is.na(state_name)) %>%
  group_by(state_name) %>%
  summarise(
    n_total       = n(),
    n_sans_instru = sum(niveau_educ == "Aucun", na.rm = TRUE),
    part_aucun    = n_sans_instru / n_total * 100
  ) %>%
  ungroup() %>%
  mutate(quintile = ntile(part_aucun, 5))

cat("\nTop 10 États sans instruction :\n")
print(df_heatmap %>% arrange(desc(part_aucun)) %>% head(10))

# heatmap 1 colonne : % sans instruction
p12a <- ggplot(df_heatmap,
               aes(x = 1,
                   y = fct_reorder(state_name, part_aucun),
                   fill = part_aucun)) +
  geom_tile(color = "white", linewidth = 0.35) +
  geom_text(aes(label = paste0(round(part_aucun, 1), "%")),
            size = 2.8, color = "white", fontface = "bold") +
  scale_fill_gradientn(
    colors = c("#EDE6DC", "#C5A882", "#8B6340", "#5C3A1E", "#2D1A08"),
    name   = "% sans instruction",
    labels = function(x) paste0(x, "%")
  ) +
  scale_x_continuous(expand = c(0, 0)) +
  labs(title    = "Part des adultes (18+) sans instruction par État nigérian",
       subtitle = "Du plus faible (bas) au plus élevé (haut) – GHS Wave 4 (2018)",
       x        = NULL,
       y        = NULL,
       caption  = "Source : GHS-W4 Nigeria 2018") +
  theme_tp2(base_size = 10) +
  theme(plot.title   = element_text(face = "bold", size = 12),
        axis.text.x  = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid   = element_blank(),
        legend.position = "right")

ggsave("outputs/fig05_heatmap_analphabetisme_etat.png", p12a,
       width = 8, height = 11.5, dpi = 150, bg = COL_FOND)
cat("  fig05 sauvegardé\n")

# heatmap croisée État × Niveau — % des adultes dans chaque case
df_etat_niveau <- df_adultes %>%
  filter(!is.na(state_name)) %>%
  group_by(state_name, niveau_educ) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(state_name) %>%
  mutate(prop = n / sum(n) * 100) %>%
  ungroup()

p12b <- ggplot(df_etat_niveau,
               aes(x = niveau_educ,
                   y = fct_reorder(state_name,
                                   ifelse(niveau_educ == "Aucun", prop, 0),
                                   .fun = max),
                   fill = prop)) +
  geom_tile(color = "white", linewidth = 0.25) +
  geom_text(aes(label = ifelse(prop >= 5, paste0(round(prop, 0), "%"), "")),
            size = 2.5, color = "white", fontface = "bold") +
  # dégradé ivoire → indigo pour le % de chaque niveau
    scale_fill_gradientn(
      colors = c("#EDF2F4", "#8D99AE", "#2B2D42", "#023047"), 
      name   = "% des adultes",
      labels = function(x) paste0(x, "%"),
      limits = c(0, 100)
  ) +
  labs(title    = "Distribution du niveau d'éducation par État nigérian",
       subtitle = "% des adultes (18+) – GHS Wave 4 (2018)",
       x        = "Niveau d'éducation",
       y        = NULL,
       caption  = "Source : GHS-W4 Nigeria 2018") +
  theme_tp2(base_size = 9) +
  theme(plot.title  = element_text(face = "bold", size = 11),
        axis.text.x = element_text(angle = 30, hjust = 1),
        panel.grid  = element_blank())

ggsave("outputs/fig06_heatmap_etat_niveau.png", p12b,
       width = 10, height = 12.5, dpi = 150, bg = COL_FOND)
cat("  fig06 sauvegardé\n")

saveRDS(df_heatmap, "data/processed/df_heatmap.rds")

cat("\n====== Toutes les analyses terminées ======\n")
for (f in list.files("outputs/", pattern = "*.png"))
  cat(" -", f, "\n")
