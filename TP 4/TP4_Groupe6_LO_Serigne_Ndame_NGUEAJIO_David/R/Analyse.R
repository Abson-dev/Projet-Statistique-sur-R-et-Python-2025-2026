# =============================================================================
# FICHIER : Analyse.R
# PROJET  : TP4 - Analyse Foncière - GHS Nigeria Vague 4
# GROUPE  : Groupe 6 — LO Serigne Ndame & KEBJAM Jackson
# DATE    : 2025
# OBJET   : Analyses univariées, bivariées et livraisons graphiques
#           Questions 19 à 24
# =============================================================================
# REPRODUCTIBILITÉ
#   → Ce script doit être exécuté APRÈS Import_Nettoyage.R.
#   → Il doit être lancé depuis la racine du projet (.Rproj).
#   → Il lit les données nettoyées dans data/processed/
#   → Il écrit les figures dans output/figures/ et les tableaux dans output/tables/
#   → Toutes les opérations sont déterministes (pas de set.seed nécessaire).
# =============================================================================

# -----------------------------------------------------------------------------
# 0. PACKAGES
# -----------------------------------------------------------------------------
library(haven)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggrepel)
library(scales)
library(patchwork)
library(rstatix)
library(gtsummary)
library(viridis)
library(forcats)

# Dossier de sortie des figures
dir.create("output/figures", recursive = TRUE, showWarnings = FALSE)
dir.create("output/tables",  recursive = TRUE, showWarnings = FALSE)

# Thème graphique commun (appliqué globalement)
theme_set(
  theme_minimal(base_size = 12) +
    theme(
      plot.title    = element_text(face = "bold", size = 13),
      plot.subtitle = element_text(colour = "grey40"),
      axis.title    = element_text(size = 11),
      legend.position = "bottom"
    )
)

# -----------------------------------------------------------------------------
# 1. CHARGEMENT DES DONNÉES NETTOYÉES
# -----------------------------------------------------------------------------
# Ces fichiers ont été produits par Import_Nettoyage.R

parcelles_full    <- readRDS("data/processed/parcelles_full.rds")
superficie_menage <- readRDS("data/processed/superficie_menage.rds")
tenure_data       <- readRDS("data/processed/tenure.rds")

# Aperçu rapide
cat("Parcelles :", nrow(parcelles_full), "obs.\n")
cat("Ménages   :", nrow(superficie_menage), "obs.\n")

# =============================================================================
# QUESTION 19 — VALEURS MANQUANTES ET ABERRANTES (bilan détaillé)
# =============================================================================

cat("\n===== Q19 : Valeurs manquantes et aberrantes =====\n")

# --- 19.1 Bilan global des NA ---
bilan_na <- parcelles_full %>%
  summarise(
    n_total         = n(),
    na_sup_ha       = sum(is.na(superficie_ha)),
    pct_na_sup      = round(mean(is.na(superficie_ha)) * 100, 1),
    na_sup_gps      = sum(is.na(superficie_gps_ha)),
    pct_na_gps      = round(mean(is.na(superficie_gps_ha)) * 100, 1),
    na_tenure       = sum(is.na(tenure_categorie)),
    pct_na_tenure   = round(mean(is.na(tenure_categorie)) * 100, 1)
  )
cat("\nBilan des NA :\n") ; print(bilan_na)

# --- 19.2 Valeurs aberrantes dans les données brutes ---
cat("\nAberrants détectés avant nettoyage :\n")
parcelles_full %>%
  summarise(
    n_neg_ha  = sum(superficie_ha_brute < 0,   na.rm = TRUE),
    n_ext_ha  = sum(superficie_ha_brute > 500, na.rm = TRUE),
    n_neg_gps = sum(superficie_gps_ha_brute < 0,   na.rm = TRUE),
    n_ext_gps = sum(superficie_gps_ha_brute > 500, na.rm = TRUE)
  ) %>% print()

# --- 19.3 Superficie totale par ménage : résumé ---
cat("\nSuperficie totale ménage (ha) :\n")
print(summary(superficie_menage$superficie_totale_ha))

# =============================================================================
# QUESTION 20 — ANALYSE UNIVARIÉE DE LA SUPERFICIE
# =============================================================================

cat("\n===== Q20 : Analyse univariée superficie =====\n")

# --- 20.1 Statistiques descriptives par décile ---

# Fonction de calcul des déciles + stats classiques
decile_stats <- function(x, label) {
  x <- x[!is.na(x)]
  tibble(
    variable  = label,
    n         = length(x),
    min       = min(x),
    p10       = quantile(x, 0.10),
    p25       = quantile(x, 0.25),
    mediane   = median(x),
    p75       = quantile(x, 0.75),
    p90       = quantile(x, 0.90),
    p95       = quantile(x, 0.95),
    p99       = quantile(x, 0.99),
    max       = max(x),
    moyenne   = mean(x),
    ecart_type = sd(x)
  )
}

stats_parcelle <- decile_stats(parcelles_full$superficie_ha,       "Superficie parcelle (ha)")
stats_menage   <- decile_stats(superficie_menage$superficie_totale_ha, "Superficie ménage (ha)")

stats_table <- bind_rows(stats_parcelle, stats_menage)
cat("\nStatistiques descriptives par décile :\n")
print(stats_table)

# Sauvegarde en CSV
write.csv(stats_table, "output/tables/Q20_stats_deciles.csv", row.names = FALSE)

# --- 20.2 Histogramme en échelle log (superficie par parcelle) ---

p_hist_parcelle <- ggplot(
  parcelles_full %>% filter(!is.na(superficie_ha), superficie_ha > 0),
  aes(x = superficie_ha)
) +
  geom_histogram(bins = 50, fill = "#8B5A2B", colour = "white", alpha = 0.85) +
  scale_x_log10(
    labels = label_number(accuracy = 0.01),
    breaks = c(0.001, 0.01, 0.1, 1, 10, 100, 500)
  ) +
  labs(
    title    = "Distribution de la superficie par parcelle (échelle log)",
    subtitle = "GHS Nigeria — Vague 4 (Planting season)",
    x        = "Superficie (ha) — échelle logarithmique",
    y        = "Nombre de parcelles",
    caption  = "Source : GHS-Panel Nigeria W4 | Groupe 6"
  ) +
  annotation_logticks(sides = "b")

ggsave("output/figures/Q20_hist_superficie_parcelle.png",
       p_hist_parcelle, width = 8, height = 5, dpi = 300)

# --- 20.3 Histogramme en échelle log (superficie totale par ménage) ---

p_hist_menage <- ggplot(
  superficie_menage %>% filter(!is.na(superficie_totale_ha), superficie_totale_ha > 0),
  aes(x = superficie_totale_ha)
) +
  geom_histogram(bins = 50, fill = "#D6604D", colour = "white", alpha = 0.85) +
  scale_x_log10(
    labels = label_number(accuracy = 0.01),
    breaks = c(0.001, 0.01, 0.1, 1, 10, 100, 500)
  ) +
  labs(
    title    = "Distribution de la superficie totale par ménage (échelle log)",
    subtitle = "GHS Nigeria — Vague 4",
    x        = "Superficie totale (ha) — échelle logarithmique",
    y        = "Nombre de ménages",
    caption  = "Source : GHS-Panel Nigeria W4 | Groupe 6"
  ) +
  annotation_logticks(sides = "b")

ggsave("output/figures/Q20_hist_superficie_menage.png",
       p_hist_menage, width = 8, height = 5, dpi = 300)

# --- 20.4 Boxplot (parcelle et ménage côte à côte) ---
# On prépare un tableau long pour ggplot
data_box <- bind_rows(
  parcelles_full    %>% filter(!is.na(superficie_ha)) %>%
    transmute(superficie = superficie_ha,      niveau = "Parcelle"),
  superficie_menage %>% filter(!is.na(superficie_totale_ha)) %>%
    transmute(superficie = superficie_totale_ha, niveau = "Ménage")
)

p_boxplot <- ggplot(data_box %>% filter(superficie > 0),
                    aes(x = niveau, y = superficie, fill = niveau)) +
  geom_boxplot(outlier.shape = 21, outlier.size = 1.5,
               outlier.alpha = 0.4, width = 0.5) +
  scale_y_log10(labels = label_number(accuracy = 0.01)) +
  scale_fill_manual(values = c("Parcelle" = "#2166AC", "Ménage" = "#D6604D")) +
  labs(
    title    = "Boxplot de la superficie — parcelle vs ménage (échelle log)",
    x        = NULL, y = "Superficie (ha) — échelle log",
    fill     = NULL,
    caption  = "Source : GHS-Panel Nigeria W4 | Groupe 6"
  ) +
  theme(legend.position = "none")

ggsave("output/figures/Q20_boxplot_superficie.png",
       p_boxplot, width = 6, height = 5, dpi = 300)

cat("Figure Q20 sauvegardées dans output/figures/\n")

# --- 20.5 Scatter plot : superficie déclarée vs GPS (LIVRABLE PRINCIPAL) ---

# Données : uniquement les parcelles où les deux mesures sont disponibles
data_scatter <- parcelles_full %>%
  filter(!is.na(superficie_ha), !is.na(superficie_gps_ha),
         superficie_ha > 0, superficie_gps_ha > 0)

n_obs <- nrow(data_scatter)
cat("\nNombre de parcelles avec les deux mesures :", n_obs, "\n")

# Calcul de la corrélation de Spearman (robuste aux distributions asymétriques)
spearman_result <- cor.test(
  data_scatter$superficie_ha,
  data_scatter$superficie_gps_ha,
  method = "spearman"
)
rho_val <- round(spearman_result$estimate, 3)
p_val   <- format.pval(spearman_result$p.value, digits = 3, eps = 0.001)
cat(sprintf("\nCorrélation Spearman (déclarée vs GPS) : rho = %.3f  (p = %s)\n",
            rho_val, p_val))

# Scatter + ligne 45° + tendance loess
p_scatter_gps <- ggplot(data_scatter,
                        aes(x = superficie_gps_ha, y = superficie_ha)) +
  # Nuage de points semi-transparent
  geom_point(alpha = 0.3, size = 1.2, colour = "#2166AC") +
  # Ligne d'égaité parfaite (45°)
  geom_abline(slope = 1, intercept = 0,
              colour = "red", linetype = "dashed", linewidth = 0.9,
              show.legend = TRUE) +
  # Courbe de tendance loess locale
  geom_smooth(method = "loess", se = TRUE,
              colour = "#D6604D", fill = "#D6604D", alpha = 0.15, linewidth = 1) +
  scale_x_log10(labels = label_number(accuracy = 0.01)) +
  scale_y_log10(labels = label_number(accuracy = 0.01)) +
  annotation_logticks(sides = "bl") +
  labs(
    title    = "Superficie déclarée vs superficie GPS",
    subtitle = sprintf("Corrélation de Spearman : rho = %.3f | n = %d parcelles",
                       rho_val, n_obs),
    x        = "Superficie GPS (ha) — échelle log",
    y        = "Superficie déclarée (ha) — échelle log",
    caption  = paste("Ligne rouge = égalité parfaite (pente 45°)",
                     "| Courbe orange = tendance LOESS",
                     "| Source : GHS-Panel Nigeria W4")
  )

ggsave("output/figures/Q20_scatter_declare_vs_gps.png",
       p_scatter_gps, width = 8, height = 7, dpi = 300)

cat("Scatter plot déclarée vs GPS sauvegardé.\n")

# =============================================================================
# QUESTION 21 — ANALYSE DU RÉGIME DE TENURE
# =============================================================================

cat("\n===== Q21 : Analyse du régime de tenure =====\n")

# --- 21.1 Fréquences et proportions ---
tenure_freq <- tenure_data %>%
  filter(!is.na(tenure_categorie)) %>%
  count(tenure_categorie, .drop = FALSE) %>%
  mutate(
    proportion = n / sum(n),
    pct        = round(proportion * 100, 1)
  ) %>%
  arrange(desc(n))

cat("\nFréquences du régime de tenure :\n")
print(tenure_freq)

write.csv(tenure_freq, "output/tables/Q21_tenure_frequences.csv", row.names = FALSE)

# --- 21.2 Barplot horizontal ---
p_tenure_bar <- ggplot(tenure_freq,
                       aes(x = pct,
                           y = fct_reorder(tenure_categorie, pct))) +
  geom_col(fill = "#4292C6", width = 0.65) +
  geom_text(aes(label = sprintf("%.1f%%  (n=%d)", pct, n)),
            hjust = -0.1, size = 3.5) +
  scale_x_continuous(limits = c(0, max(tenure_freq$pct) * 1.2),
                     labels = label_percent(scale = 1)) +
  labs(
    title   = "Régime de tenure foncière — fréquences relatives",
    x       = "Proportion (%)",
    y       = NULL,
    caption = "Source : GHS-Panel Nigeria W4 | Groupe 6"
  )

ggsave("output/figures/Q21_barplot_tenure.png",
       p_tenure_bar, width = 8, height = 5, dpi = 300)

# --- 21.3 Test du chi-deux : tenure × milieu (rural/urbain) ---
# H0 : indépendance entre le régime de tenure et la zone (Rural/Urbain)

# Construire la table de contingence
tenure_milieu <- tenure_data %>%
  filter(!is.na(tenure_categorie), !is.na(milieu)) %>%
  select(tenure_categorie, milieu)

tab_contingence <- table(tenure_milieu$tenure_categorie,
                          tenure_milieu$milieu)

cat("\nTable de contingence tenure × milieu :\n")
print(tab_contingence)

# Test du chi-deux (avec correction de Yates automatique si 2×2)
chi2_result <- chisq.test(tab_contingence)
cat("\nTest du chi-deux :\n")
print(chi2_result)

# Résidus standardisés (indiquent les cellules qui contribuent le plus)
cat("\nRésidus standardisés :\n")
print(round(chi2_result$stdres, 2))

# Sauvegarde du résultat
sink("output/tables/Q21_chi2_tenure_milieu.txt")
cat("=== Test chi-deux : Régime de tenure × Milieu (Urbain/Rural) ===\n\n")
print(tab_contingence)
cat("\n")
print(chi2_result)
cat("\nRésidus standardisés :\n")
print(round(chi2_result$stdres, 2))
sink()

cat("Test chi-deux sauvegardé.\n")

# =============================================================================
# QUESTION 23 — SUPERFICIE TOTALE MÉNAGE × NOMBRE DE PARCELLES
# =============================================================================

cat("\n===== Q23 : Superficie ménage vs nombre de parcelles =====\n")

# Données : ménages avec superficie valide
data_q23 <- superficie_menage %>%
  filter(!is.na(superficie_totale_ha), superficie_totale_ha > 0, nb_parcelles > 0)

# --- 23.1 Corrélation de Spearman avec intervalle de confiance (bootstrap) ---
# rstatix::cor_test() fournit rho et p-valeur ; l'IC est calculé par bootstrap
spearman_q23 <- cor.test(
  data_q23$nb_parcelles,
  data_q23$superficie_totale_ha,
  method = "spearman"
)

rho_q23 <- round(spearman_q23$estimate, 3)
p_q23   <- format.pval(spearman_q23$p.value, digits = 3, eps = 0.001)

# IC à 95 % via transformation de Fisher sur le rang (approximation)
n_q23 <- nrow(data_q23)
z_r   <- 0.5 * log((1 + rho_q23) / (1 - rho_q23))      # transformation z de Fisher
se_z  <- 1 / sqrt(n_q23 - 3)
ic_lb <- tanh(z_r - 1.96 * se_z)
ic_ub <- tanh(z_r + 1.96 * se_z)

cat(sprintf(
  "\nCorrélation Spearman (nb_parcelles vs superficie_totale_ha) :\n  rho = %.3f  IC95%% [%.3f, %.3f]  p = %s\n",
  rho_q23, ic_lb, ic_ub, p_q23
))

# --- 23.2 Scatter plot avec courbe LOESS ---
p_sup_parc <- ggplot(data_q23,
                     aes(x = nb_parcelles, y = superficie_totale_ha)) +
  geom_jitter(alpha = 0.3, size = 1.2, colour = "#2166AC",
              width = 0.15, height = 0) +
  geom_smooth(method = "loess", se = TRUE,
              colour = "#D6604D", fill = "#D6604D", alpha = 0.2, linewidth = 1) +
  scale_y_log10(labels = label_number(accuracy = 0.1)) +
  scale_x_continuous(breaks = 1:max(data_q23$nb_parcelles)) +
  labs(
    title    = "Superficie totale du ménage selon le nombre de parcelles",
    subtitle = sprintf("Spearman rho = %.3f  IC95%% [%.3f, %.3f]  |  n = %d ménages",
                       rho_q23, ic_lb, ic_ub, n_q23),
    x        = "Nombre de parcelles",
    y        = "Superficie totale (ha) — échelle log",
    caption  = "Courbe orange = tendance LOESS | Source : GHS-Panel Nigeria W4"
  )

ggsave("output/figures/Q23_scatter_superficie_vs_nparcelles.png",
       p_sup_parc, width = 8, height = 6, dpi = 300)

cat("Scatter Q23 sauvegardé.\n")

# =============================================================================
# QUESTION 24 — HEATMAP SUPERFICIE MÉDIANE : ÉTAT × SECTEUR
# =============================================================================
# NOTE : Comme nous ne disposons que de la Vague 4 (une seule vague),
# la dimension temporelle "vague" n'est pas disponible.
# La heatmap est donc construite sur État × Milieu (Urbain/Rural),
# ce qui permet de capturer les disparités géographiques ET sectorielles.
# C'est l'adaptation la plus pertinente au cadre d'un unique round.

cat("\n===== Q24 : Heatmap superficie médiane État × Milieu =====\n")

# Calcul de la superficie médiane par État × Milieu
heatmap_data <- superficie_menage %>%
  filter(!is.na(superficie_totale_ha), !is.na(state_code)) %>%
  group_by(state_code, milieu) %>%
  summarise(
    mediane_sup = median(superficie_totale_ha, na.rm = TRUE),
    n_menages   = n(),
    .groups = "drop"
  ) %>%
  # Exclure les cellules avec très peu d'observations (< 5) — peu fiables
  filter(n_menages >= 5)

# Étiquettes des états nigérians (36 états + FCT)
# Correspondance partielle selon les codes GHS
state_labels <- c(
  "1"  = "Abia",        "2"  = "Adamawa",    "3"  = "Akwa Ibom",
  "4"  = "Anambra",     "5"  = "Bauchi",     "6"  = "Bayelsa",
  "7"  = "Benue",       "8"  = "Borno",      "9"  = "Cross River",
  "10" = "Delta",       "11" = "Ebonyi",     "12" = "Edo",
  "13" = "Ekiti",       "14" = "Enugu",      "15" = "Gombe",
  "16" = "Imo",         "17" = "Jigawa",     "18" = "Kaduna",
  "19" = "Kano",        "20" = "Katsina",    "21" = "Kebbi",
  "22" = "Kogi",        "23" = "Kwara",      "24" = "Lagos",
  "25" = "Nassarawa",   "26" = "Niger",      "27" = "Ogun",
  "28" = "Ondo",        "29" = "Osun",       "30" = "Oyo",
  "31" = "Plateau",     "32" = "Rivers",     "33" = "Sokoto",
  "34" = "Taraba",      "35" = "Yobe",       "36" = "Zamfara",
  "37" = "FCT Abuja"
)

heatmap_data <- heatmap_data %>%
  mutate(
    state_nom = recode(as.character(state_code), !!!state_labels),
    state_nom = if_else(is.na(state_nom),
                        paste("State", state_code),
                        state_nom)
  )

# Heatmap : État (ordonnée par médiane décroissante) × Milieu
# Couleur = superficie médiane en ha (palette viridis)
p_heatmap <- ggplot(
  heatmap_data,
  aes(
    x    = milieu,
    y    = fct_reorder(state_nom, mediane_sup, .fun = max),
    fill = mediane_sup
  )
) +
  geom_tile(colour = "white", linewidth = 0.5) +
  geom_text(aes(label = sprintf("%.2f\n(n=%d)", mediane_sup, n_menages)),
            size = 2.5, colour = "white") +
  scale_fill_viridis_c(
    option    = "plasma",
    name      = "Superficie\nmédiane (ha)",
    trans     = "log10",
    labels    = label_number(accuracy = 0.01),
    na.value  = "grey80"
  ) +
  labs(
    title    = "Superficie médiane des exploitations par État et par Milieu",
    subtitle = "GHS Nigeria — Vague 4 | Adaptation : État × Milieu (1 seule vague disponible)",
    x        = "Milieu",
    y        = "État",
    caption  = "Cellules exclues si n < 5 ménages | Source : GHS-Panel Nigeria W4 | Groupe 6"
  ) +
  theme(
    axis.text.y = element_text(size = 7.5),
    axis.text.x = element_text(size = 10),
    legend.key.height = unit(1.5, "cm")
  )

ggsave("output/figures/Q24_heatmap_superficie_etat_milieu.png",
       p_heatmap, width = 9, height = 14, dpi = 300)

cat("Heatmap Q24 sauvegardée.\n")

# =============================================================================
# TABLEAU COMPARATIF INTER-VAGUES — ADAPTÉ : COMPARAISON RURAL vs URBAIN
# =============================================================================
# NOTE : En l'absence de plusieurs vagues, on produit un tableau comparatif
# des indicateurs de structure foncière entre milieu Urbain et Rural.
# Ce tableau remplace le "comparatif inter-vagues" demandé.

cat("\n===== Tableau comparatif Rural vs Urbain =====\n")

tab_comparatif <- superficie_menage %>%
  filter(!is.na(superficie_totale_ha)) %>%
  group_by(milieu) %>%
  summarise(
    n_menages    = n(),
    sup_moy      = round(mean(superficie_totale_ha, na.rm = TRUE),   3),
    sup_med      = round(median(superficie_totale_ha, na.rm = TRUE), 3),
    sup_sd       = round(sd(superficie_totale_ha, na.rm = TRUE),     3),
    sup_p10      = round(quantile(superficie_totale_ha, 0.10, na.rm = TRUE), 3),
    sup_p90      = round(quantile(superficie_totale_ha, 0.90, na.rm = TRUE), 3),
    moy_parcelles = round(mean(nb_parcelles, na.rm = TRUE), 2),
    .groups = "drop"
  )

cat("\nTableau comparatif (milieu) :\n")
print(tab_comparatif)

write.csv(tab_comparatif,
          "output/tables/Q24_tableau_comparatif_milieu.csv",
          row.names = FALSE)


# =============================================================================
# RÉCAPITULATIF DES SORTIES
# =============================================================================
cat("\n====================================================\n")
cat("  ANALYSE TERMINÉE — Sorties générées :\n\n")
cat("  FIGURES (output/figures/) :\n")
cat("   - Q20_hist_superficie_parcelle.png\n")
cat("   - Q20_hist_superficie_menage.png\n")
cat("   - Q20_boxplot_superficie.png\n")
cat("   - Q20_scatter_declare_vs_gps.png       [LIVRABLE]\n")
cat("   - Q21_barplot_tenure.png\n")
cat("   - Q23_scatter_superficie_vs_nparcelles.png\n")
cat("   - Q24_heatmap_superficie_etat_milieu.png [LIVRABLE]\n\n")
cat("  TABLEAUX (output/tables/) :\n")
cat("   - Q20_stats_deciles.csv\n")
cat("   - Q21_tenure_frequences.csv\n")
cat("   - Q21_chi2_tenure_milieu.txt\n")
cat("   - Q24_tableau_comparatif_milieu.csv      [LIVRABLE]\n")
cat("====================================================\n")
