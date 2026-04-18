# ================================================================
# PROJET ENSAE ISE 1 — GHS Nigeria Panel (W4)
# SCRIPT   : scripts/02_tache20_analyse_univariee.R
# TÂCHE 20 : Analyse univariée superficie + comparaison déclaré/GPS
# BASE     : data/processed/processed_tp4_w4.rds
#            data/processed/superficie_menage.rds
# SORTIES  : outputs/figures/t20_*.png | outputs/tables/t20_*.csv
# ================================================================

source("scripts/functions.R")

library(dplyr)
library(ggplot2)
library(scales)

donnees           <- readRDS("data/processed/processed_tp4_w4.rds")
superficie_menage <- readRDS("data/processed/superficie_menage.rds")


# ================================================================
# PARTIE A — NIVEAU PARCELLE
# ================================================================

# --- A.1 Statistiques descriptives complètes ---
stats_par <- donnees %>%
  filter(!is.na(superficie_ha)) %>%
  summarise(
    n          = n(),
    moyenne    = mean(superficie_ha),
    mediane    = median(superficie_ha),
    ecart_type = sd(superficie_ha),
    cv_pct     = sd(superficie_ha) / mean(superficie_ha) * 100,
    iqr        = IQR(superficie_ha),
    p10 = quantile(superficie_ha, 0.10),
    p25 = quantile(superficie_ha, 0.25),
    p75 = quantile(superficie_ha, 0.75),
    p90 = quantile(superficie_ha, 0.90),
    min = min(superficie_ha),
    max = max(superficie_ha)
  ) %>%
  mutate(across(where(is.numeric), ~round(., 4)))

cat("=== Stats descriptives — niveau parcelle ===\n")
print(stats_par)
sauvegarder_tableau(stats_par, "t20_stats_parcelle.csv")


# --- A.2 Histogramme log avec médiane ---
med_par <- median(donnees$superficie_ha, na.rm = TRUE)

p_histo <- donnees %>%
  filter(!is.na(superficie_ha), superficie_ha > 0) %>%
  ggplot(aes(x = superficie_ha)) +
  geom_histogram(bins = 60, fill = "#2171B5", color = "white", alpha = 0.85) +
  geom_vline(xintercept = med_par, color = "#D73027",
             linewidth = 0.9, linetype = "dashed") +
  annotate("text",
           x = med_par * 1.35, y = Inf, vjust = 1.8, hjust = 0,
           label = sprintf("Médiane = %.3f ha", med_par),
           color = "#D73027", size = 3.5) +
  scale_x_log10(labels = label_number(accuracy = 0.001)) +
  labs(
    title    = "Distribution de la superficie des parcelles (échelle log)",
    subtitle = sprintf("CV = %.0f%% | P10 = %.3f ha | P90 = %.3f ha",
                       stats_par$cv_pct, stats_par$p10, stats_par$p90),
    x        = "Superficie (ha) — échelle log",
    y        = "Nombre de parcelles",
    caption  = sprintf("GHS Nigeria W4 — n = %d parcelles", stats_par$n)
  ) +
  theme_tp4()

print(p_histo)
sauvegarder_figure("t20_histo_parcelle.png")


# --- A.3 Boxplot niveau parcelle ---
p_box_par <- donnees %>%
  filter(!is.na(superficie_ha), superficie_ha > 0) %>%
  ggplot(aes(y = superficie_ha)) +
  geom_boxplot(fill = "#6BAED6", color = "#08519C",
               outlier.alpha = 0.15, outlier.size = 0.8) +
  scale_y_log10(labels = label_number(accuracy = 0.001)) +
  labs(
    title   = "Boxplot de la superficie des parcelles",
    subtitle = "Axe y en échelle logarithmique",
    y       = "Superficie (ha) — échelle log",
    x       = NULL,
    caption = "GHS Nigeria W4"
  ) +
  theme_tp4() +
  theme(axis.text.x = element_blank())

print(p_box_par)
sauvegarder_figure("t20_boxplot_parcelle.png")


# ================================================================
# PARTIE B — NIVEAU MÉNAGE (brut vs pondéré)
# ================================================================

# --- B.1 Statistiques brutes ---
stats_men_brut <- superficie_menage %>%
  filter(!is.na(superficie_totale)) %>%
  summarise(
    type       = "Non pondéré",
    n          = n(),
    moyenne    = mean(superficie_totale),
    mediane    = median(superficie_totale),
    cv_pct     = sd(superficie_totale) / mean(superficie_totale) * 100,
    p10        = quantile(superficie_totale, 0.10),
    p90        = quantile(superficie_totale, 0.90)
  )

# --- B.2 Statistiques pondérées (weight_wave4) ---
# Moyenne pondérée : Σ(w_i * x_i) / Σ(w_i)
# Médiane pondérée : via la fonction mediane_ponderee() de functions.R
df_pond <- superficie_menage %>%
  filter(!is.na(superficie_totale), !is.na(poids_vague4))

moy_pond <- weighted.mean(df_pond$superficie_totale, df_pond$poids_vague4)
med_pond <- mediane_ponderee(df_pond$superficie_totale, df_pond$poids_vague4)

# Pour P10 et P90 pondérés : on utilise la même logique cumulative
quantile_pondere <- function(x, poids, prob) {
  ok <- !is.na(x) & !is.na(poids) & poids > 0
  x <- x[ok]; poids <- poids[ok]
  ord <- order(x)
  x <- x[ord]; poids <- poids[ord]
  cum <- cumsum(poids) / sum(poids)
  x[which(cum >= prob)[1]]
}

stats_men_pond <- data.frame(
  type     = "Pondéré (weight_wave4)",
  n        = nrow(df_pond),
  moyenne  = moy_pond,
  mediane  = med_pond,
  cv_pct   = sd(df_pond$superficie_totale) / moy_pond * 100,
  p10      = quantile_pondere(df_pond$superficie_totale, df_pond$poids_vague4, 0.10),
  p90      = quantile_pondere(df_pond$superficie_totale, df_pond$poids_vague4, 0.90)
)

tableau_comparatif <- bind_rows(stats_men_brut, stats_men_pond) %>%
  mutate(across(where(is.numeric), ~round(., 3)))

cat("\n=== Comparaison brut vs pondéré — niveau ménage ===\n")
print(tableau_comparatif)
sauvegarder_tableau(tableau_comparatif, "t20_brut_vs_pondere.csv")


# --- B.3 Histogramme pondéré (ménage) ---
# aes(weight = ...) dans ggplot2 pondère chaque barre
# → la figure représente la population nigériane, pas l'échantillon brut
med_men <- median(superficie_menage$superficie_totale, na.rm = TRUE)

p_histo_men <- superficie_menage %>%
  filter(!is.na(superficie_totale), superficie_totale > 0, !is.na(poids_vague4)) %>%
  ggplot(aes(x = superficie_totale, weight = poids_vague4)) +
  geom_histogram(bins = 50, fill = "#238B45", color = "white", alpha = 0.85) +
  geom_vline(xintercept = med_men, color = "#D73027",
             linewidth = 0.9, linetype = "dashed") +
  annotate("text",
           x = med_men * 1.35, y = Inf, vjust = 1.8, hjust = 0,
           label = sprintf("Médiane = %.3f ha", med_men),
           color = "#D73027", size = 3.5) +
  scale_x_log10(labels = label_number(accuracy = 0.01)) +
  labs(
    title    = "Distribution de la superficie totale par ménage (pondérée)",
    subtitle = "Histogramme pondéré — représente la population nigériane",
    x        = "Superficie totale (ha) — échelle log",
    y        = "Effectif pondéré",
    caption  = sprintf("GHS Nigeria W4 — weight_wave4 | n = %d ménages",
                       nrow(superficie_menage))
  ) +
  theme_tp4()

print(p_histo_men)
sauvegarder_figure("t20_histo_menage_pondere.png")


# ================================================================
# PARTIE C — SCATTER DÉCLARÉ vs GPS (comparaison des sources)
# ================================================================

# On compare superficie déclarée (superficie_brute en m², converti)
# et superficie GPS préremplie (gps_prefill_ha)
# Ces deux sources coexistent pour un sous-ensemble de parcelles

# Conversion de la superficie déclarée brute en ha
# HYPOTHÈSE : sa1q11 est en m² pour les GPS (sa1q9==1)
#             et en unité déclarée pour les autres (on garde uniquement GPS ici)
donnees_scatter <- donnees %>%
  filter(
    gps_mesure_recolte == 1,          # GPS mesuré à la récolte
    !is.na(gps_recolte_ha),           # Superficie GPS récolte disponible
    !is.na(gps_prefill_ha)            # Superficie GPS préremplie disponible
  ) %>%
  select(
    superficie_gps_recolte  = gps_recolte_ha,
    superficie_gps_prefill  = gps_prefill_ha
  )

n_scatter <- nrow(donnees_scatter)
cat(sprintf("\nParcelles avec les deux sources GPS : %d\n", n_scatter))

if (n_scatter >= 30) {
  # Corrélation de Spearman avec IC (transformation de Fisher)
  res_sp <- ic_spearman(
    donnees_scatter$superficie_gps_recolte,
    donnees_scatter$superficie_gps_prefill
  )
  cat(sprintf("Rho de Spearman = %.3f [%.3f ; %.3f], p = %.2e\n",
              res_sp$rho, res_sp$ic_inf, res_sp$ic_sup, res_sp$p_val))

  # Part de sur/sous-déclaration
  n_sur  <- sum(donnees_scatter$superficie_gps_recolte >
                  donnees_scatter$superficie_gps_prefill)
  n_sous <- sum(donnees_scatter$superficie_gps_recolte <
                  donnees_scatter$superficie_gps_prefill)
  cat(sprintf("GPS récolte > GPS prérempli (sur-mesure) : %d (%.0f%%)\n",
              n_sur,  n_sur  / n_scatter * 100))
  cat(sprintf("GPS récolte < GPS prérempli (sous-mesure): %d (%.0f%%)\n",
              n_sous, n_sous / n_scatter * 100))

  # Scatter plot
  label_rho <- sprintf("ρ = %.3f\nIC95 [%.3f ; %.3f]",
                        res_sp$rho, res_sp$ic_inf, res_sp$ic_sup)

  p_scatter <- donnees_scatter %>%
    ggplot(aes(x = superficie_gps_prefill, y = superficie_gps_recolte)) +
    geom_point(alpha = 0.35, color = "#2171B5", size = 1.2) +
    geom_abline(intercept = 0, slope = 1,
                color = "#D73027", linewidth = 0.9,
                linetype = "dashed") +           # Ligne 45° = parfaite concordance
    geom_smooth(method = "loess", se = TRUE,
                color = "#238B45", fill = "#A1D99B",
                linewidth = 0.8) +                # Courbe LOESS
    annotate("text",
             x = -Inf, y = Inf, hjust = -0.15, vjust = 1.5,
             label = label_rho, size = 3.5, color = "grey20") +
    scale_x_log10(labels = label_number(accuracy = 0.001)) +
    scale_y_log10(labels = label_number(accuracy = 0.001)) +
    labs(
      title    = "Superficie GPS récolte vs GPS préremplie",
      subtitle = "Ligne rouge = concordance parfaite (45°) | Courbe verte = LOESS",
      x        = "GPS prérempli — vague précédente (ha, log)",
      y        = "GPS récolte W4 (ha, log)",
      caption  = sprintf("n = %d parcelles | ρ de Spearman", n_scatter)
    ) +
    theme_tp4()

  print(p_scatter)
  sauvegarder_figure("t20_scatter_gps_vs_gps.png")

  # Sauvegarder les résultats Spearman
  res_tableau <- data.frame(
    rho    = round(res_sp$rho,    3),
    ic_inf = round(res_sp$ic_inf, 3),
    ic_sup = round(res_sp$ic_sup, 3),
    p_val  = signif(res_sp$p_val, 3),
    n      = res_sp$n
  )
  sauvegarder_tableau(res_tableau, "t20_spearman_gps.csv")

} else {
  cat("Sous-échantillon trop petit pour le scatter plot.\n")
}

cat("\n✔ Tâche 20 terminée.\n")
