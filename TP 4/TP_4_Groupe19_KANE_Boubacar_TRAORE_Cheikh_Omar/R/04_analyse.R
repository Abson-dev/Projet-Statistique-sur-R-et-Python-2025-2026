# ==============================================================================
# TP4 — Analyse des parcelles agricoles
# Fichier : 04_analyse.R
# Objectif : Analyses complètes — Questions 19, 20, 21, 23, 24
# ==============================================================================
# Prérequis : avoir exécuté 01_import.R, 02_recodage.R, 03_nettoyage.R
# Les bases propres sont dans data/processed/

# Rechargement si nécessaire
if (!exists("sect11a1_p4")) {
  sect11a1_p4    <- readRDS(file.path(proj_root, "data/processed/sect11a1_clean.rds"))
  sect11b1_p4    <- readRDS(file.path(proj_root, "data/processed/sect11b1_clean.rds"))
  menage_parcelle <- readRDS(file.path(proj_root, "data/processed/menage_parcelle.rds"))
}

fig_dir   <- file.path(proj_root, "output/figures")
table_dir <- file.path(proj_root, "output/tables")

# ============================================================================ #
# QUESTION 19 — Valeurs manquantes, aberrantes et qualité des données
# ============================================================================ #

# Les statistiques de qualité ont été produites dans 03_nettoyage.R.
# On génère ici le tableau de conversion et les statistiques de base.

## Tableau de conversion 
general_conv <- tibble::tibble(
  Unite = c("Plots", "Acres", "Hectares", "Metres carres"),
  Facteur_ha = c(0.0667, 0.4000, 1.0000, 0.0001)
)

table_conv <- flextable(general_conv) |>
  set_header_labels(Unite = "Unité", Facteur_ha = "Facteur (→ ha)") |>
  add_header_row(values = "Facteurs généraux de conversion en hectares",
                 colwidths = 2) |>
  theme_booktabs() |> autofit()
save(table_conv, file = file.path(table_dir, "table_conv.RDATA"))

## Tableau de conversion — facteurs spécifiques par zone
zone_conv <- tibble::tibble(
  Zone    = c("North Central (1)", "North East (2)", "North West (3)",
              "South East (4)",   "South South (5)", "South West (6)"),
  Heaps  = c(0.00012, 0.00016, 0.00011, 0.00019, 0.00021, 0.00012),
  Ridges = c(0.00270, 0.00400, 0.00494, 0.00230, 0.00230, 0.00001),
  Stands = c(0.00006, 0.00016, 0.00004, 0.00004, 0.00013, 0.00041)
)

table_conv_s <- flextable(zone_conv) |>
  add_header_row(
    values = "Facteurs spécifiques de conversion en hectares (par zone géopolitique)",
    colwidths = 4) |>
  theme_booktabs() |> autofit()
save(table_conv_s, file = file.path(table_dir, "table_conv_s.RDATA"))


# ============================================================================ #
# QUESTION 20 — Analyse univariée de la superficie
# ============================================================================ #

# ---------------------------------------------------------------------------- #
# 20a. Statistiques descriptives par décile (parcelle)
# ---------------------------------------------------------------------------- #

stats_parcelle <- sect11a1_p4 |>
  summarise(
    n      = n(),
    mean   = round(weighted.mean(superficie_ha, wt_wave4, na.rm = TRUE), 3),
    sd     = round(sqrt(Hmisc::wtd.var(superficie_ha, wt_wave4)), 3),
    median = round(Hmisc::wtd.quantile(superficie_ha, wt_wave4, probs = 0.50), 3),
    Q1     = round(Hmisc::wtd.quantile(superficie_ha, wt_wave4, probs = 0.25), 3),
    Q3     = round(Hmisc::wtd.quantile(superficie_ha, wt_wave4, probs = 0.75), 3),
    min    = round(min(superficie_ha, na.rm = TRUE), 4),
    max    = round(max(superficie_ha, na.rm = TRUE), 3)
  )

cat("\n=== Q20 — Statistiques superficie par parcelle (ha) ===\n")
print(stats_parcelle)
write.csv(stats_parcelle, file.path(table_dir, "stats_superf_parcelle.csv"),
          row.names = FALSE)

# Déciles pondérés
if (!requireNamespace("Hmisc", quietly = TRUE)) install.packages("Hmisc")
library(Hmisc)

deciles_parcelle <- wtd.quantile(
  sect11a1_p4$superficie_ha,
  weights = sect11a1_p4$wt_wave4,
  probs   = seq(0, 1, 0.1),
  na.rm   = TRUE
)
cat("\nDéciles superficie parcelle (ha) :\n")
print(round(deciles_parcelle, 4))

decile_df <- data.frame(
  Decile    = paste0(seq(0, 100, 10), "%"),
  Superf_ha = round(deciles_parcelle, 4)
)
write.csv(decile_df, file.path(table_dir, "deciles_superf_parcelle.csv"),
          row.names = FALSE)

# ---------------------------------------------------------------------------- #
# 20b. Statistiques descriptives (ménage)
# ---------------------------------------------------------------------------- #

stats_menage <- menage_parcelle |>
  filter(!is.na(wt_wave4)) |>
  summarise(
    n      = n(),
    mean   = round(weighted.mean(superficie_totale_menage, wt_wave4, na.rm = TRUE), 3),
    sd     = round(sqrt(Hmisc::wtd.var(superficie_totale_menage, wt_wave4)), 3),
    median = round(Hmisc::wtd.quantile(superficie_totale_menage, wt_wave4, probs = 0.50), 3),
    Q1     = round(Hmisc::wtd.quantile(superficie_totale_menage, wt_wave4, probs = 0.25), 3),
    Q3     = round(Hmisc::wtd.quantile(superficie_totale_menage, wt_wave4, probs = 0.75), 3),
    min    = round(min(superficie_totale_menage, na.rm = TRUE), 4),
    max    = round(max(superficie_totale_menage, na.rm = TRUE), 3)
  )

cat("\n=== Q20 — Statistiques superficie totale par ménage (ha) ===\n")
print(stats_menage)
write.csv(stats_menage, file.path(table_dir, "stats_superf_menage.csv"),
          row.names = FALSE)

# ---------------------------------------------------------------------------- #
# 20c. Histogramme superficie par ménage (pondéré, echelle log)
# ---------------------------------------------------------------------------- #

hist_superficie_menage <- ggplot(
  sect11a1_p4 |> filter(superficie_totale_menage > 0),
  aes(x = superficie_totale_menage, weight = wt_wave4)
) +
  geom_histogram(fill = "steelblue", color = "black", bins = 50, alpha = 0.7) +
  scale_x_log10(
    breaks = c(0.01, 0.1, 1, 10, 100, 500),
    labels = c("0.01", "0.1", "1", "10", "100", "500")
  ) +
  labs(
    x = "Superficie agricole totale du ménage (ha, échelle log)",
    y = "Nombre de ménages"
  ) +
  theme_minimal() +
  theme(
    panel.grid.minor     = element_blank(),
    panel.grid.major.x   = element_blank(),
    axis.title           = element_text(face = "bold"),
    plot.title           = element_text(hjust = 0.5, face = "italic", size = 12)
  )
print(hist_superficie_menage)
ggsave(file.path(fig_dir, "hist_superficie_menage.png"),
       hist_superficie_menage, width = 6, height = 4, dpi = 300)
save(hist_superficie_menage, file = file.path(fig_dir, "hist_superficie_menage.RDATA"))

# ---------------------------------------------------------------------------- #
# 20d. Histogramme superficie par parcelle (pondéré, echelle log)
# ---------------------------------------------------------------------------- #

hist_superficie_parcelle <- ggplot(
  sect11a1_p4 |> filter(superficie_ha > 0),
  aes(x = superficie_ha, weight = wt_wave4)
) +
  geom_histogram(fill = "skyblue", color = "black", bins = 50, alpha = 0.7) +
  scale_x_log10(
    breaks = c(0.001, 0.01, 0.1, 1, 10, 100),
    labels = c("0.001", "0.01", "0.1", "1", "10", "100")
  ) +
  labs(
    x = "Superficie de la parcelle (ha, échelle log)",
    y = "Nombre de parcelles"
  ) +
  theme_minimal() +
  theme(
    panel.grid.minor   = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.title         = element_text(face = "bold"),
    plot.title         = element_text(hjust = 0.5, face = "italic", size = 12)
  )
print(hist_superficie_parcelle)
ggsave(file.path(fig_dir, "hist_superficie_parcelle.png"),
       hist_superficie_parcelle, width = 6, height = 4, dpi = 300)
save(hist_superficie_parcelle, file = file.path(fig_dir, "hist_superficie_parcelle.RDATA"))

# ---------------------------------------------------------------------------- #
# 20e. Boxplots (parcelle + ménage, côte à côte via patchwork)
# ---------------------------------------------------------------------------- #

p_box_parcelle <- ggplot(
  sect11a1_p4 |> filter(superficie_ha > 0),
  aes(y = superficie_ha, weight = wt_wave4)
) +
  geom_boxplot(fill = "skyblue", outlier.shape = NA) +
  scale_y_log10() +
  labs(title = "Superficie des parcelles", y = "Superficie (ha, log)") +
  theme_minimal() +
  theme(
    axis.text.x        = element_blank(),
    axis.ticks.x       = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor   = element_blank(),
    plot.title         = element_text(face = "italic", size = 11)
  )

p_box_menage <- ggplot(
  sect11a1_p4 |> filter(superficie_totale_menage > 0),
  aes(y = superficie_totale_menage, weight = wt_wave4)
) +
  geom_boxplot(fill = "steelblue", outlier.shape = NA) +
  scale_y_log10() +
  labs(title = "Superficie totale du ménage", y = "Superficie (ha, log)") +
  theme_minimal() +
  theme(
    axis.text.x        = element_blank(),
    axis.ticks.x       = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor   = element_blank(),
    plot.title         = element_text(face = "italic", size = 11)
  )

boxplot_mp <- p_box_parcelle + p_box_menage
print(boxplot_mp)
ggsave(file.path(fig_dir, "boxplot_MP.png"),
       boxplot_mp, width = 7, height = 5, dpi = 300)
save(boxplot_mp, file = file.path(fig_dir, "boxplot_MP.RDATA"))

# ---------------------------------------------------------------------------- #
# 20f. Comparaison superficie déclarée vs GPS (scatter plot + corrélation)
# ---------------------------------------------------------------------------- #

data_comp <- sect11a1_p4 |>
  filter(!is.na(superficie_ha), !is.na(superficie_gps_ha),
         superficie_ha > 0, superficie_gps_ha > 0)

cat("\nParcelles avec les deux mesures :", nrow(data_comp), "\n")

# Corrélation de Spearman
corel_spearman <- cor.test(
  data_comp$superficie_ha,
  data_comp$superficie_gps_ha,
  method = "spearman",
  use    = "complete.obs"
)
cat("\n=== Corrélation de Spearman (déclarée vs GPS) ===\n")
print(corel_spearman)

# Biais de déclaration
pct_sur_declare <- round(
  mean(data_comp$superficie_ha > data_comp$superficie_gps_ha, na.rm = TRUE) * 100, 1
)
cat("% parcelles sur-déclarées :", pct_sur_declare, "%\n")

# Scatter plot
plot_SG <- ggplot(
  data_comp,
  aes(x = superficie_ha, y = superficie_gps_ha, weight = wt_wave4)
) +
  geom_point(alpha = 0.4, color = "steelblue", size = 1.5) +
  geom_abline(intercept = 0, slope = 1,
              color = "red", linetype = "dashed", linewidth = 0.8) +
  scale_x_log10(
    breaks = c(0.001, 0.01, 0.1, 1, 10, 100),
    labels = c("0.001", "0.01", "0.1", "1", "10", "100")
  ) +
  scale_y_log10(
    breaks = c(0.001, 0.01, 0.1, 1, 10, 100),
    labels = c("0.001", "0.01", "0.1", "1", "10", "100")
  ) +
  annotate("text",
           x = Inf, y = -Inf,
           label = paste0("Spearman ρ = ", round(corel_spearman$estimate, 3),
                          "\np < 2.2e-16"),
           hjust = 1.1, vjust = -0.5, size = 3.5) +
  labs(
    x = "Superficie déclarée (ha, échelle log)",
    y = "Superficie GPS (ha, échelle log)"
  ) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.title = element_text(face = "bold")
  )
print(plot_SG)
ggsave(file.path(fig_dir, "plot_SG.png"), plot_SG, width = 6, height = 5, dpi = 300)
save(plot_SG, file = file.path(fig_dir, "plot_SG.RDATA"))

cat("\n✔ Q20 — Graphiques et statistiques sauvegardés.\n")

# ============================================================================ #
# QUESTION 21 — Régime de tenure foncière
# ============================================================================ #

# ---------------------------------------------------------------------------- #
# 21a. Fréquences et proportions de tenure
# ---------------------------------------------------------------------------- #

tenure_freq <- sect11b1_p4 |>
  filter(!is.na(tenure), !is.na(wt_wave4)) |>
  count(tenure) |>
  mutate(
    prop  = n / sum(n) * 100,
    label = paste0(round(prop, 1), "%")
  ) |>
  arrange(desc(prop)) |>
  mutate(tenure = factor(tenure, levels = unique(tenure)))

cat("\n=== Q21 — Régime de tenure ===\n")
print(tenure_freq)
write.csv(tenure_freq, file.path(table_dir, "tenure_freq.csv"), row.names = FALSE)

# ---------------------------------------------------------------------------- #
# 21b. Barplot horizontal — tenure
# ---------------------------------------------------------------------------- #

bar_regime_tenure <- ggplot(tenure_freq, aes(x = prop, y = tenure)) +
  geom_bar(stat = "identity", fill = "steelblue", width = 0.7) +
  geom_text(aes(label = label),
            color = "saddlebrown", hjust = -0.1, size = 3.5) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(
    x = "Proportion (%)",
    y = NULL
  ) +
  theme_minimal() +
  theme(
    panel.grid   = element_blank(),
    axis.ticks.x = element_blank()
  )
print(bar_regime_tenure)
ggsave(file.path(fig_dir, "bar_regime_tenure.png"),
       bar_regime_tenure, width = 7, height = 4, dpi = 300)
save(bar_regime_tenure, file = file.path(fig_dir, "bar_regime_tenure.RDATA"))

# ---------------------------------------------------------------------------- #
# 21c. Tableau croisé tenure × milieu de résidence
# ---------------------------------------------------------------------------- #

tab_tenure_secteur <- sect11b1_p4 |>
  filter(!is.na(tenure), !is.na(secteur)) |>
  count(secteur, tenure) |>
  tidyr::pivot_wider(names_from = secteur, values_from = n, values_fill = 0)

cat("\n=== Tableau croisé tenure × milieu ===\n")
print(tab_tenure_secteur)
write.csv(tab_tenure_secteur, file.path(table_dir, "tenure_par_milieu.csv"),
          row.names = FALSE)

# ---------------------------------------------------------------------------- #
# 21d. Test du Chi-deux tenure × milieu
# ---------------------------------------------------------------------------- #

mat_chi2 <- table(
  sect11b1_p4$secteur[!is.na(sect11b1_p4$secteur) & !is.na(sect11b1_p4$tenure)],
  sect11b1_p4$tenure[ !is.na(sect11b1_p4$secteur) & !is.na(sect11b1_p4$tenure)]
)

K2_test_secteur_tenure <- chisq.test(mat_chi2)
cramer_val <- rstatix::cramer_v(mat_chi2)

cat("\n=== Test Chi-deux tenure × milieu ===\n")
print(K2_test_secteur_tenure)
cat("V de Cramér :", round(cramer_val, 3), "\n")

capture.output(print(K2_test_secteur_tenure),
               file = file.path(table_dir, "chi2_tenure_milieu.txt"))

cat("\n✔ Q21 — Analyses de tenure sauvegardées.\n")

# ============================================================================ #
# QUESTION 23 — Relation superficie totale × nombre de parcelles
# ============================================================================ #

# ---------------------------------------------------------------------------- #
# 23a. Corrélation de Spearman avec IC 95%
# ---------------------------------------------------------------------------- #

corel_spearma_SN <- cor.test(
  menage_parcelle$nombre_parcelle,
  menage_parcelle$superficie_totale_menage,
  method = "spearman"
)

cat("\n=== Q23 — Corrélation Spearman (superficie ~ nb parcelles) ===\n")
print(corel_spearma_SN)
capture.output(print(corel_spearma_SN),
               file = file.path(table_dir, "spearman_superf_nb_parcelles.txt"))

# ---------------------------------------------------------------------------- #
# 23b. Scatter plot + courbe loess
# ---------------------------------------------------------------------------- #

geom_point_SN <- ggplot(
  menage_parcelle |> filter(!is.na(wt_wave4)),
  aes(x = nombre_parcelle, y = superficie_totale_menage, weight = wt_wave4)
) +
  geom_point(alpha = 0.4, color = "steelblue", size = 1.5) +
  geom_smooth(method = "loess", se = TRUE,
              color = "orange", fill = "orange", alpha = 0.2) +
  scale_y_log10(
    breaks = c(0.01, 0.1, 1, 10, 100, 500),
    labels = c("0.01", "0.1", "1", "10", "100", "500")
  ) +
  scale_x_continuous(breaks = 1:max(menage_parcelle$nombre_parcelle, na.rm = TRUE)) +
  annotate("text",
           x = Inf, y = Inf,
           label = paste0("Spearman ρ = ",
                          round(corel_spearma_SN$estimate, 3),
                          "\np < 2.2e-16"),
           hjust = 1.1, vjust = 1.5, size = 3.5) +
  labs(
    x = "Nombre de parcelles du ménage",
    y = "Superficie agricole totale (ha, échelle log)"
  ) +
  theme_minimal() +
  theme(
    panel.grid  = element_blank(),
    axis.title  = element_text(face = "bold"),
    plot.title  = element_text(face = "italic", size = 12)
  )
print(geom_point_SN)
ggsave(file.path(fig_dir, "geom_point_SN.png"),
       geom_point_SN, width = 6, height = 5, dpi = 300)
save(geom_point_SN, file = file.path(fig_dir, "geom_point_SN.RDATA"))

cat("\n✔ Q23 — Scatter plot sauvegardé.\n")

# ============================================================================ #
# QUESTION 24 — Disparités géographiques (heatmap État × superficies)
# ============================================================================ #

# ---------------------------------------------------------------------------- #
# 24a. Plan de sondage et médiane pondérée par État
# ---------------------------------------------------------------------------- #

library(survey)

data_design <- menage_parcelle |>
  filter(!is.na(wt_wave4))

design <- svydesign(ids = ~1, data = data_design, weights = ~wt_wave4)

median_state <- svyby(
  ~superficie_totale_menage,
  ~state,
  design,
  svyquantile,
  quantiles = 0.5,
  ci        = FALSE,
  keep.var  = FALSE
)

median_state <- median_state |>
  mutate(
    vague       = "Vague 4",
    state_label = as_factor(state)
  )

cat("\n=== Q24 — Médiane pondérée par État (Vague 4) ===\n")
print(median_state)
write.csv(median_state, file.path(table_dir, "superf_mediane_par_etat.csv"),
          row.names = FALSE)

# ---------------------------------------------------------------------------- #
# 24b. Heatmap État × superficie médiane
# ---------------------------------------------------------------------------- #

geom_heatmap <- ggplot(
  median_state,
  aes(x = vague, y = state_label, fill = statistic)
) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(statistic, 2)),
            color = "black", size = 2.8) +
  scale_fill_gradient(low = "lightblue", high = "darkblue",
                      name = "Superficie\nmédiane (ha)") +
  labs(
    x = NULL,
    y = "État"
  ) +
  theme_minimal() +
  theme(
    axis.text.x  = element_text(face = "bold"),
    axis.text.y  = element_text(size = 7),
    panel.grid   = element_blank(),
    plot.title   = element_text(face = "italic", size = 12)
  )
print(geom_heatmap)
ggsave(file.path(fig_dir, "heatmap.png"),
       geom_heatmap, width = 5, height = 10, dpi = 300)
save(geom_heatmap, file = file.path(fig_dir, "heatmap.RDATA"))

cat("\n✔ Q24 — Heatmap sauvegardée.\n")

# ============================================================================ #
# TABLEAU DE SYNTHÈSE DES TESTS STATISTIQUES
# ============================================================================ #

tests_tbl <- tibble::tibble(
  Variable = c(
    "Superficie declaree vs GPS",
    "Milieu de residence x regime de tenure",
    "Superficie totale x nombre de parcelles"
  ),
  Test = c(
    "Correlation Spearman",
    "Khi-deux",
    "Correlation Spearman"
  ),
  Resultat = c(
    paste0("rho = ", round(corel_spearman$estimate, 3),
           ", p = ", signif(corel_spearman$p.value, 3)),
    paste0("chi2 = ", round(K2_test_secteur_tenure$statistic, 1),
           ", df = ", K2_test_secteur_tenure$parameter,
           ", p = ", signif(K2_test_secteur_tenure$p.value, 3)),
    paste0("rho = ", round(corel_spearma_SN$estimate, 3),
           ", p = ", signif(corel_spearma_SN$p.value, 3))
  )
)

table_test <- flextable(tests_tbl) |>
  set_header_labels(
    Variable = "Variable analysee",
    Test     = "Test utilise",
    Resultat = "Resultat du test"
  ) |>
  add_header_row(values = "Resume des tests statistiques", colwidths = 3) |>
  theme_booktabs() |>
  autofit()

save(table_test, file = file.path(table_dir, "table_test.RDATA"))
