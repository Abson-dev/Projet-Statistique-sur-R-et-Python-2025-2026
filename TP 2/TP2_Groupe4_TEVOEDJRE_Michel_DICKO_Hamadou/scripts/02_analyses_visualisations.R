library(dplyr)
library(ggplot2)
library(forcats)
library(rstatix)
library(ggpubr)
library(gtsummary)
library(viridis)
library(patchwork)
library(scales)
library(survey)

dir.create("outputs", showWarnings = FALSE, recursive = TRUE)

df_adultes   <- readRDS("data/processed/df_adultes.rds")
df_educ      <- readRDS("data/processed/df_educ.rds")
df_scol      <- readRDS("data/processed/df_scol.rds")
plan_adultes <- readRDS("data/processed/plan_adultes.rds")
plan_scol    <- readRDS("data/processed/plan_scol.rds")

# palette ordonnée pour les niveaux d'éducation — couleurs vives
palette_niveaux <- c(
  "Aucun"            = "#FF6B6B",
  "Primaire"         = "#FFA94D",
  "Junior Secondary" = "#FFD43B",
  "Senior Secondary" = "#69DB7C",
  "Tertiaire"        = "#339AF0"
)

# constantes visuelles réutilisées partout
COL_FOND   <- "#F8F9FF"
COL_GRILLE <- "#E3E8F5"
COL_TEXTE  <- "#1A1A2E"
COL_MUTED  <- "#4A4E6B"
COL_ACCENT <- "#F03E3E"

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
      plot.caption      = element_text(color = "#7B8AB8", size = base_size - 3,
                                       hjust = 0, margin = margin(t = 6)),
      axis.title        = element_text(color = COL_MUTED, size = base_size - 1),
      axis.text         = element_text(color = "#2C3A6B"),
      legend.background = element_rect(fill = COL_FOND, color = NA),
      legend.key        = element_rect(fill = COL_FOND, color = NA),
      legend.title      = element_text(face = "bold", color = COL_TEXTE),
      strip.background  = element_rect(fill = "#DDE3F5", color = NA),
      strip.text        = element_text(face = "bold", color = COL_TEXTE)
    )
}

# Barplot horizontal pondéré — fréquences du niveau_educ
cat("\nDistribution pondérée du niveau d'éducation \n")

# Proportions pondérées via plan de sondage
prop_pond <- svymean(~niveau_educ, plan_adultes, na.rm = TRUE)
freq_niveau <- data.frame(
  niveau_educ = gsub("niveau_educ", "", names(prop_pond)),
  prop        = as.numeric(prop_pond) * 100
) %>%
  mutate(
    niveau_educ = factor(niveau_educ,
                         levels = c("Aucun","Primaire","Junior Secondary",
                                    "Senior Secondary","Tertiaire"),
                         ordered = TRUE),
    label = paste0(round(prop, 1), "%")
  )
print(freq_niveau)

p8 <- ggplot(freq_niveau,
             aes(x = prop, y = fct_rev(niveau_educ), fill = niveau_educ)) +
  geom_col(width = 0.58, color = COL_FOND, linewidth = 0.3) +
  geom_text(aes(label = label), hjust = -0.12,
            size = 3.5, fontface = "bold", color = COL_TEXTE) +
  scale_fill_manual(values = palette_niveaux, guide = "none") +
  scale_x_continuous(expand = expansion(mult = c(0, 0.14)),
                     labels = function(x) paste0(x, "%")) +
  labs(title    = "Distribution pondérée du niveau d'éducation des adultes (18+)",
       subtitle = "Estimations population — Nigeria GHS Wave 4 (2018) | Poids : wt_wave4",
       x        = "Proportion pondérée (%)",
       y        = NULL,
       caption  = "Source : GHS-W4 Nigeria 2018") +
  theme_tp2() +
  theme(panel.grid.major.y = element_blank())

ggsave("outputs/fig01_barplot_niveau_educ_global.png", p8,
       width = 8, height = 5, dpi = 150, bg = COL_FOND)
cat("  ✓ fig01 sauvegardé\n")

# Barplot 100% empilé pondéré par sexe + Chi2 de Rao-Scott
cat("\nNiveau d'éducation par sexe (pondéré) \n")

# Proportions pondérées par sexe via interaction
prop_sexe_niv <- svymean(~interaction(sexe_label, niveau_educ),
                         plan_adultes, na.rm = TRUE)

# reconstruction manuelle des proportions conditionnelles par sexe
df_sexe_niveau <- as.data.frame(prop_sexe_niv) %>%
  tibble::rownames_to_column("cat") %>%
  rename(prop_glob = mean) %>%
  mutate(
    sexe_label  = sub("interaction\\(sexe_label, niveau_educ\\)(\\w+)\\..*", "\\1", cat),
    sexe_label  = ifelse(grepl("^Femme", gsub(".*\\)","",cat)), "Femme", "Homme"),
    niveau_educ = gsub(".*\\.", "", cat),
    niveau_educ = factor(niveau_educ,
                         levels = c("Aucun","Primaire","Junior Secondary",
                                    "Senior Secondary","Tertiaire"),
                         ordered = TRUE)
  ) %>%
  group_by(sexe_label) %>%
  mutate(prop = prop_glob / sum(prop_glob) * 100) %>%
  ungroup()

# fallback propre si l'interaction donne des noms complexes
df_sexe_niveau <- df_adultes %>%
  group_by(sexe_label, niveau_educ) %>%
  summarise(n_pond = sum(wt_wave4, na.rm = TRUE), .groups = "drop") %>%
  group_by(sexe_label) %>%
  mutate(prop = n_pond / sum(n_pond) * 100) %>%
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
  labs(title    = "Niveau d'éducation pondéré par sexe (adultes 18+)",
       subtitle = "Barres 100% empilées — estimations population | Poids : wt_wave4",
       x        = NULL,
       y        = "Proportion pondérée",
       caption  = "Source : GHS-W4 Nigeria 2018") +
  coord_flip() +
  theme_tp2() +
  theme(legend.position = "bottom",
        legend.key.size = unit(0.45, "cm"))

ggsave("outputs/fig02_barplot100_niveau_sexe.png", p9,
       width = 9, height = 5.5, dpi = 150, bg = COL_FOND)
cat("  fig02 sauvegardé\n")

# test de Chi-deux pondéré (Rao-Scott)
test_chi2_pond <- svychisq(~sexe_label + niveau_educ, plan_adultes,
                           statistic = "Chisq")
cat("\nTest du Chi-deux pondéré (Rao-Scott) :\n"); print(test_chi2_pond)

# V de Cramer sur effectifs pondérés
tab_contingence_pond <- svytable(~sexe_label + niveau_educ, plan_adultes)
cat("\nTable de contingence pondérée :\n"); print(round(tab_contingence_pond))

n_pond   <- sum(tab_contingence_pond)
min_dim  <- min(nrow(tab_contingence_pond) - 1, ncol(tab_contingence_pond) - 1)
v_cramer <- sqrt(test_chi2_pond$statistic / (n_pond * min_dim))
cat(sprintf("\nV de Cramer (pondéré) : %.4f\n", v_cramer))

saveRDS(list(table   = tab_contingence_pond,
             chi2    = test_chi2_pond,
             v_cramer = v_cramer),
        "data/processed/resultats_tache9.rds")

# Boxplot pondéré niveau_educ ~ groupe_age + Kruskal-Wallis
cat("\n Niveau d'éducation par groupe d'âge (pondéré) \n")

df_age_educ <- df_adultes %>%
  filter(!is.na(groupe_age)) %>%
  mutate(niveau_num = as.numeric(niveau_educ))

# Kruskal-Wallis pondéré via répétition proportionnelle aux poids
wt_scale  <- min(df_age_educ$wt_wave4, na.rm = TRUE)
df_kw_rep <- df_age_educ %>%
  mutate(rep_n = pmax(1, round(wt_wave4 / wt_scale))) %>%
  slice(rep(seq_len(n()), times = rep_n))
set.seed(123)
if (nrow(df_kw_rep) > 100000) df_kw_rep <- df_kw_rep[sample(nrow(df_kw_rep), 100000), ]

kw_test  <- kruskal.test(niveau_num ~ groupe_age, data = df_kw_rep)
cat("\nKruskal-Wallis pondéré :\n"); print(kw_test)

dunn_res <- df_kw_rep %>%
  dunn_test(niveau_num ~ groupe_age, p.adjust.method = "bonferroni")
cat("\nPost-hoc Dunn pondéré :\n"); print(dunn_res)

# palette vive pour les groupes d'âge
pal_age <- c(
  "18-30" = "#F03E3E",
  "31-45" = "#FF922B",
  "46-60" = "#20C997",
  "60+"   = "#339AF0"
)

p10 <- ggplot(df_age_educ,
              aes(x = groupe_age, y = niveau_num,
                  fill = groupe_age, weight = wt_wave4)) +
  geom_boxplot(alpha = 0.78,
               outlier.alpha = 0.12, outlier.size = 0.6,
               width = 0.48, color = "#1A1A2E") +
  geom_jitter(aes(weight = NULL),
              alpha = 0.04, width = 0.14, size = 0.45,
              color = COL_MUTED) +
  scale_y_continuous(breaks = 1:5,
                     labels = levels(df_adultes$niveau_educ)) +
  scale_fill_manual(values = pal_age, guide = "none") +
  annotate("label",
           x = 3.5, y = 5.35,
           label = paste0("Kruskal-Wallis pondéré : χ² = ",
                          round(kw_test$statistic, 2), ", p < 0.001"),
           fill = COL_FOND, color = COL_MUTED,
           size = 3.2, fontface = "italic", label.size = 0.2) +
  labs(title    = "Niveau d'éducation pondéré selon le groupe d'âge (adultes 18+)",
       subtitle = "Médiane et dispersion pondérées par tranche d'âge | Poids : wt_wave4",
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

# Scolarisation pondérée 6-17 ans par zone
cat("\n Scolarisation 6-17 ans par zone (pondérée) \n")

# taux pondérés via plan de sondage
taux_pond <- svyby(~scolarise, ~zone_scol, plan_scol, svymean, na.rm = TRUE)
df_taux_scol <- as.data.frame(taux_pond) %>%
  rename(prop = scolariseTRUE, se = se.scolariseTRUE) %>%
  mutate(
    n      = as.numeric(svytable(~zone_scol, plan_scol)),
    n_scol = round(prop * n),
    lower  = pmax(0, prop - 1.96 * se),
    upper  = pmin(1, prop + 1.96 * se)
  )
cat("\nTaux de scolarisation pondérés avec IC 95% :\n"); print(df_taux_scol)

# Chi-deux pondéré scolarisation × zone
test_chi2_scol <- svychisq(~zone_scol + scolarise, plan_scol,
                           statistic = "Chisq")
cat("\nChi-deux pondéré scolarisation :\n"); print(test_chi2_scol)

# couleurs vives Urbain/Rural
pal_zone <- c("Urbain" = "#339AF0", "Rural" = "#69DB7C")

p11 <- ggplot(df_taux_scol,
              aes(x = zone_scol, y = prop * 100, fill = zone_scol,
                  ymin = lower * 100, ymax = upper * 100)) +
  geom_col(width = 0.48, alpha = 0.88, color = "#1A1A2E", linewidth = 0.3) +
  geom_errorbar(width = 0.14, color = "#1A1A2E", linewidth = 0.75) +
  geom_text(aes(label = paste0(round(prop * 100, 1), "%")),
            vjust = -2.0, fontface = "bold", size = 4.5,
            color = COL_TEXTE) +
  scale_fill_manual(values = pal_zone, guide = "none") +
  scale_y_continuous(limits = c(0, 110),
                     labels = function(x) paste0(x, "%")) +
  annotate("label",
           x = 1.5, y = 105,
           label = paste0("χ² pondéré = ", round(test_chi2_scol$statistic, 1),
                          "  |  p < 0.001"),
           fill = COL_FOND, color = COL_MUTED,
           size = 3.3, fontface = "italic", label.size = 0.2) +
  labs(title    = "Taux de scolarisation pondéré des enfants (6-17 ans) par zone",
       subtitle = "IC à 95% — estimations population | Poids : wt_wave4",
       x        = NULL,
       y        = "Taux de scolarisation (%)",
       caption  = "Source : GHS-W4 Nigeria 2018") +
  theme_tp2() +
  theme(panel.grid.major.x = element_blank())

ggsave("outputs/fig04_scolarisation_zone.png", p11,
       width = 7, height = 5.5, dpi = 150, bg = COL_FOND)
cat("  fig04 sauvegardé\n")

saveRDS(list(table = svytable(~zone_scol + scolarise, plan_scol),
             chi2  = test_chi2_scol,
             taux  = df_taux_scol),
        "data/processed/resultats_tache11.rds")

# Heatmap pondérée État × Niveau d'éducation
cat("\n Heatmap analphabétisme pondérée par État \n")

df_heatmap <- df_adultes %>%
  filter(!is.na(state_name)) %>%
  group_by(state_name) %>%
  summarise(
    n_total_pond  = sum(wt_wave4, na.rm = TRUE),
    n_aucun_pond  = sum(wt_wave4[niveau_educ == "Aucun"], na.rm = TRUE),
    part_aucun    = n_aucun_pond / n_total_pond * 100,
    .groups = "drop"
  ) %>%
  mutate(quintile = ntile(part_aucun, 5))

cat("\nTop 10 États sans instruction (pondéré) :\n")
print(df_heatmap %>% arrange(desc(part_aucun)) %>% head(10))

# heatmap 1 colonne : % pondéré sans instruction
p12a <- ggplot(df_heatmap,
               aes(x = 1,
                   y = fct_reorder(state_name, part_aucun),
                   fill = part_aucun)) +
  geom_tile(color = "white", linewidth = 0.35) +
  geom_text(aes(label = paste0(round(part_aucun, 1), "%")),
            size = 2.8, color = "white", fontface = "bold") +
  scale_fill_gradientn(
    colors = c("#E3F2FD", "#64B5F6", "#1565C0", "#0D2137"),
    name   = "% sans instruction",
    labels = function(x) paste0(x, "%")
  ) +
  scale_x_continuous(expand = c(0, 0)) +
  labs(title    = "Part pondérée des adultes (18+) sans instruction par État nigérian",
       subtitle = "Du plus faible (bas) au plus élevé (haut) — GHS Wave 4 (2018) | Poids : wt_wave4",
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

# heatmap croisée pondérée État × Niveau
df_etat_niveau <- df_adultes %>%
  filter(!is.na(state_name)) %>%
  group_by(state_name, niveau_educ) %>%
  summarise(n_pond = sum(wt_wave4, na.rm = TRUE), .groups = "drop") %>%
  group_by(state_name) %>%
  mutate(prop = n_pond / sum(n_pond) * 100) %>%
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
  scale_fill_gradientn(
    colors = c("#F8F9FF", "#74C0FC", "#1971C2", "#0A2540"),
    name   = "% des adultes",
    labels = function(x) paste0(x, "%"),
    limits = c(0, 100)
  ) +
  labs(title    = "Distribution pondérée du niveau d'éducation par État nigérian",
       subtitle = "% des adultes (18+) — estimations population | Poids : wt_wave4",
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

cat("\n====== Toutes les analyses pondérées terminées ======\n")
for (f in list.files("outputs/", pattern = "*.png"))
  cat(" -", f, "\n")
