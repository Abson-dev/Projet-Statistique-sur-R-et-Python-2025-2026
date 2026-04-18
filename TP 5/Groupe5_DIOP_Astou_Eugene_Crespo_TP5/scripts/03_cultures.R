# =============================================================================
# TP5 - Script 03 : Cultures pratiquées et diversification
# Tâche 25 : 15 cultures les plus fréquentes
# Tâche 26 : Indice de diversification culturale
# =============================================================================

message("--- Tâche 25 & 26 : Cultures et diversification ---")

# =============================================================================
# TÂCHE 25 : 15 cultures les plus fréquentes (barplot horizontal)
# =============================================================================

# Fréquence de chaque culture (nombre de ménages ayant cultivé)
freq_cultures <- df_recolte_clean %>%
  group_by(cropcode, nom_culture, type_culture) %>%
  summarise(
    n_menages   = n_distinct(hhid),
    n_parcelles = n(),
    .groups = "drop"
  ) %>%
  arrange(desc(n_menages)) %>%
  slice_head(n = 15) %>%
  mutate(
    nom_culture  = if_else(is.na(nom_culture),
                           paste0("Culture_", cropcode),
                           nom_culture),
    type_culture = if_else(is.na(type_culture), "Autre", type_culture),
    nom_culture  = fct_reorder(nom_culture, n_menages),
    pct_menages  = n_menages / n_distinct(df_recolte_clean$hhid) * 100
  )

# Sauvegarde du tableau
save_table(freq_cultures, "T25_cultures_frequentes")

# Graphique : barplot horizontal des 15 cultures
couleurs_type <- c(
  "Céréale"        = "#2166AC",
  "Légumineuse"    = "#4DAC26",
  "Tubercule"      = "#D01C8B",
  "Culture de rente" = "#F1A340",
  "Fruit"          = "#92C5DE",
  "Légume"         = "#B8E186",
  "Autre"          = "#878787"
)

p25 <- ggplot(freq_cultures,
              aes(x = nom_culture, y = n_menages, fill = type_culture)) +
  geom_col(width = 0.75, color = "white", linewidth = 0.3) +
  geom_text(aes(label = paste0(round(pct_menages, 1), "%")),
            hjust = -0.15, size = 3.2, color = "grey30") +
  scale_fill_manual(values = couleurs_type, name = "Type de culture") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.18))) +
  coord_flip() +
  labs(
    title    = "Les 15 cultures les plus fréquentes au Nigeria (W4, 2018/2019)",
    subtitle = "Nombre de ménages ayant déclaré la récolte de chaque culture",
    x        = NULL,
    y        = "Nombre de ménages",
    caption  = "Source : Nigeria GHS Panel Wave 4 (NBS/World Bank, 2018/2019)"
  ) +
  theme_tp5 +
  theme(legend.position = "right")

save_plot(p25, "T25_cultures_frequentes", width = 12, height = 8)

# =============================================================================
# TÂCHE 26 : Indice de diversification culturale par ménage
# =============================================================================

# Calcul du nombre de cultures distinctes par ménage
diversification <- df_recolte_clean %>%
  group_by(hhid) %>%
  summarise(
    n_cultures = n_distinct(cropcode),
    .groups = "drop"
  ) %>%
  left_join(df_menage, by = "hhid")

# Statistiques descriptives
stats_divers <- diversification %>%
  summarise(
    N       = n(),
    Moyenne = mean(n_cultures, na.rm = TRUE),
    Médiane = median(n_cultures, na.rm = TRUE),
    EcartType = sd(n_cultures, na.rm = TRUE),
    Min     = min(n_cultures, na.rm = TRUE),
    Max     = max(n_cultures, na.rm = TRUE)
  )

print(stats_divers)
save_table(stats_divers, "T26_stats_diversification")

# Test de Wilcoxon rural vs urbain
test_wilcox_divers <- diversification %>%
  filter(!is.na(milieu)) %>%
  wilcox_test(n_cultures ~ milieu) %>%
  add_significance()

print(test_wilcox_divers)
save_table(as.data.frame(test_wilcox_divers), "T26_test_wilcoxon_diversification")

# Statistiques par milieu
stats_milieu_divers <- diversification %>%
  filter(!is.na(milieu)) %>%
  group_by(milieu) %>%
  summarise(
    N       = n(),
    Médiane = median(n_cultures, na.rm = TRUE),
    Moyenne = mean(n_cultures, na.rm = TRUE),
    EcartType = sd(n_cultures, na.rm = TRUE),
    .groups = "drop"
  )

print(stats_milieu_divers)
save_table(stats_milieu_divers, "T26_stats_diversification_milieu")

# Graphique 1 : histogramme de la distribution
p26a <- ggplot(diversification, aes(x = n_cultures)) +
  geom_histogram(binwidth = 1, fill = "#2166AC", color = "white",
                 alpha = 0.85) +
  geom_vline(xintercept = median(diversification$n_cultures, na.rm = TRUE),
             color = "red", linetype = "dashed", linewidth = 1) +
  annotate("text",
           x = median(diversification$n_cultures, na.rm = TRUE) + 0.3,
           y = Inf, vjust = 2, hjust = 0,
           label = paste0("Médiane = ",
                          median(diversification$n_cultures, na.rm = TRUE)),
           color = "red", size = 3.5) +
  scale_x_continuous(breaks = 1:max(diversification$n_cultures, na.rm = TRUE)) +
  labs(
    title    = "Distribution de l'indice de diversification culturale",
    subtitle = "Nombre de cultures différentes par ménage (W4, 2018/2019)",
    x        = "Nombre de cultures distinctes",
    y        = "Nombre de ménages",
    caption  = "Source : Nigeria GHS Panel Wave 4 (NBS/World Bank, 2018/2019)"
  ) +
  theme_tp5

# Graphique 2 : violin plot rural vs urbain
p26b_data <- diversification %>% filter(!is.na(milieu))

# Label p-value pour annotation
pval_label <- paste0("Test de Wilcoxon\np = ",
                     round(test_wilcox_divers$p, 4))

p26b <- ggplot(p26b_data, aes(x = milieu, y = n_cultures, fill = milieu)) +
  geom_violin(alpha = 0.6, trim = FALSE, color = NA) +
  geom_boxplot(width = 0.15, outlier.shape = NA,
               color = "grey30", fill = "white", alpha = 0.8) +
  stat_summary(fun = median, geom = "point",
               shape = 21, size = 3, fill = "white", color = "black") +
  scale_fill_manual(values = c("Rural" = "#4DAC26", "Urbain" = "#2166AC"),
                    guide = "none") +
  annotate("text", x = 1.5, y = max(p26b_data$n_cultures, na.rm = TRUE) * 0.95,
           label = pval_label, size = 3.5, color = "grey30") +
  labs(
    title    = "Diversification culturale selon le milieu de résidence",
    subtitle = "Nombre de cultures distinctes par ménage — Rural vs Urbain",
    x        = NULL,
    y        = "Nombre de cultures distinctes",
    caption  = "Source : Nigeria GHS Panel Wave 4 (NBS/World Bank, 2018/2019)"
  ) +
  theme_tp5

# Combinaison des deux graphiques
p26_combined <- p26a / p26b +
  plot_annotation(
    title = "Tâche 26 : Diversification culturale des ménages nigérians",
    theme = theme(plot.title = element_text(face = "bold", size = 14, hjust = 0.5))
  )

save_plot(p26_combined, "T26_diversification_culturale", width = 12, height = 12)
save_plot(p26a, "T26a_histogramme_diversification", width = 10, height = 6)
save_plot(p26b, "T26b_violin_diversification_milieu", width = 8, height = 6)

message("=== Tâches 25 & 26 terminées ===")
