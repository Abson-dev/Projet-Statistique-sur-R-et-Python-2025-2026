# =============================================================================
# 03_analyse.R — Analyses des parcelles agricoles
# Projet  : Nigeria GHS Panel (W1–W4, 2010–2018)
# Auteur  : ENSAE ISE 1 | 2025–2026
# Tâches  : 19, 20, 21, 23, 24
# Note    : transformation log(1 + x) pour éviter −∞ sur les valeurs nulles
# Convention : aucun opérateur :: dans le code
# =============================================================================

# ── 0. Packages ───────────────────────────────────────────────────────────────
library(haven)
library(dplyr)
library(tidyr)
library(forcats)
library(ggplot2)
library(ggrepel)
library(scales)
library(patchwork)
library(viridis)
library(rstatix)
library(gtsummary)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)

source("R/fonctions.R")

# ── 1. Chemins & données ──────────────────────────────────────────────────────
processed <- "data/processed"
figure    <- "output/figures"
table     <- "output/tables"
data_path <- "data/raw"

dir.create(figure, showWarnings = FALSE, recursive = TRUE)
dir.create(table,  showWarnings = FALSE, recursive = TRUE)

parcelles_agricoles <- readRDS(file.path(processed, "parcelles_agricoles_w4.rds"))

# Poids parcelle = poids ménage / nombre de parcelles du ménage
parcelles_agricoles <- parcelles_agricoles %>%
  group_by(identifiant_menage) %>%
  mutate(wt_parcelle = poids_menage / n()) %>%
  ungroup()

# Transformation ln(1 + x) — évite −∞ pour x = 0
parcelles_agricoles <- parcelles_agricoles %>%
  mutate(
    log1p_ha      = log1p(superficie_ha_propre),
    log1p_gps_ha  = log1p(superficie_gps_ha)
  )

cat("=============================================================\n")
cat("  03_analyse.R — Nigeria GHS Wave 4\n")
cat(sprintf("  %d parcelles | %d ménages\n",
            nrow(parcelles_agricoles),
            n_distinct(parcelles_agricoles$identifiant_menage)))
cat("=============================================================\n\n")

# =============================================================================
# TÂCHE 19 — VALEURS MANQUANTES & ABERRANTES
# =============================================================================

cat("── TÂCHE 19 ─────────────────────────────────────────────────────\n")

N <- nrow(parcelles_agricoles)

# ── 19.1 Rapport valeurs manquantes ──────────────────────────────────────────
missing_tbl <- tibble(
  variable  = c("superficie_declaree", "superficie_parcelle",
                "superficie_ha", "superficie_ha_propre",
                "superficie_gps_ha", "poids_menage"),
  n_missing = c(
    sum(is.na(parcelles_agricoles$superficie_declaree)),
    sum(is.na(parcelles_agricoles$superficie_parcelle)),
    sum(is.na(parcelles_agricoles$superficie_ha)),
    sum(is.na(parcelles_agricoles$superficie_ha_propre)),
    sum(is.na(parcelles_agricoles$superficie_gps_ha)),
    sum(is.na(parcelles_agricoles$poids_menage))
  )
) %>% mutate(pct = round(n_missing / N * 100, 2))

cat("\n  [VALEURS MANQUANTES]\n")
print(missing_tbl)
save_tab(missing_tbl, "T00_missing_values")

# ── 19.2 Rapport outliers ─────────────────────────────────────────────────────
cat("\n  [OUTLIERS — superficie_ha]\n")

outlier_tbl <- parcelles_agricoles %>%
  filter(!is.na(superficie_ha)) %>%
  mutate(cat = case_when(
    superficie_ha <= 0  ~ "≤ 0 ha",
    superficie_ha > 500 ~ "> 500 ha",
    TRUE                ~ "valide"
  )) %>%
  count(cat) %>%
  mutate(pct = round(n / N * 100, 3))

print(outlier_tbl)
save_tab(outlier_tbl, "T00b_outliers")

# Top outliers
top_out <- parcelles_agricoles %>%
  filter(!is.na(superficie_ha), superficie_ha > 500) %>%
  select(identifiant_menage, identifiant_parcelle,
         superficie_parcelle, unite_superficie, superficie_ha) %>%
  arrange(desc(superficie_ha))

if (nrow(top_out) > 0) {
  cat(sprintf("\n  Cas aberrants (> 500 ha) : %d\n", nrow(top_out)))
  print(head(top_out, 10))
}

# ── 19.3 Superficie totale par ménage ─────────────────────────────────────────
superficie_menage <- parcelles_agricoles %>%
  group_by(identifiant_menage) %>%
  summarise(
    nb_parcelles          = n(),
    nb_parcelles_valides  = sum(!is.na(superficie_ha_propre)),
    superficie_totale_ha  = sum(superficie_ha_propre, na.rm = TRUE),
    poids_menage          = first(poids_menage),
    etat                  = first(etat),
    region                = first(region),
    zone_residence        = first(zone_residence),
    .groups               = "drop"
  ) %>%
  mutate(
    superficie_totale_ha = if_else(nb_parcelles_valides == 0,
                                   NA_real_, superficie_totale_ha),
    log1p_tot_ha         = log1p(superficie_totale_ha)
  )

stats_q19 <- desc_w(superficie_menage$superficie_totale_ha,
                    superficie_menage$poids_menage,
                    "superficie_totale_ménage_ha")
cat("\n  [STATISTIQUES PONDÉRÉES — superficie totale / ménage]\n")
print(stats_q19)
save_tab(stats_q19, "T01_stats_superficie_menage")
save_tab(superficie_menage, "T01b_superficie_menage_detail")

# =============================================================================
# TÂCHE 20 — ANALYSE UNIVARIÉE
# =============================================================================

cat("\n── TÂCHE 20 ─────────────────────────────────────────────────────\n")

parcelles_v <- parcelles_agricoles %>%
  filter(!is.na(superficie_ha_propre), superficie_ha_propre >= 0,
         !is.na(wt_parcelle))

menages_v <- superficie_menage %>%
  filter(!is.na(superficie_totale_ha), superficie_totale_ha >= 0,
         !is.na(poids_menage))

cat(sprintf("  Parcelles valides : %d / %d\n", nrow(parcelles_v), N))
cat(sprintf("  Ménages valides   : %d / %d\n", nrow(menages_v),
            n_distinct(parcelles_agricoles$identifiant_menage)))

# ── 20.1 Statistiques pondérées + déciles ────────────────────────────────────
stats_parc <- desc_w(parcelles_v$superficie_ha_propre,
                     parcelles_v$wt_parcelle, "sup_parcelle_ha")
stats_men  <- desc_w(menages_v$superficie_totale_ha,
                     menages_v$poids_menage, "sup_totale_menage_ha")

stats_all <- bind_rows(stats_parc, stats_men) %>%
  mutate(across(where(is.numeric), ~ round(.x, 4)))

cat("\n  [STATS PONDÉRÉES]\n")
print(stats_all)
save_tab(stats_all, "T02_stats_ponderees")

dec_parc <- decile_w(parcelles_v, "superficie_ha_propre", "wt_parcelle")
dec_men  <- decile_w(menages_v,   "superficie_totale_ha",  "poids_menage")

cat("\n  [DÉCILES PONDÉRÉS — parcelles]\n"); print(dec_parc)
cat("\n  [DÉCILES PONDÉRÉS — ménages]\n");   print(dec_men)
save_tab(dec_parc, "T03a_deciles_parcelles")
save_tab(dec_men,  "T03b_deciles_menages")

# ── 20.2 Histogrammes ln(1 + x) ───────────────────────────────────────────────
# ln(1+x) : valeurs nulles → 0 au lieu de −∞ ; axe retransformé en ha
lbl_inv_log1p <- function(x) {
  vals <- x
  ifelse(vals < 0.01, sprintf("%.3f", vals),
         ifelse(vals < 1, sprintf("%.2f", vals), sprintf("%.1f", vals)))
}

breaks_log1p <- log1p(c(0, 0.01, 0.05, 0.1, 0.5, 1, 2, 5, 10, 50, 100))

p_h1 <- ggplot(parcelles_v,
               aes(x = log1p(superficie_ha_propre), weight = wt_parcelle)) +
  geom_histogram(bins = 50, fill = palette_ensae["bleu"],
                 colour = "white", alpha = 0.90) +
  geom_vline(xintercept = log1p(stats_parc$median),
             colour = palette_ensae["rouge"], linewidth = 0.8, linetype = "dashed") +
  annotate("text",
           x = log1p(stats_parc$median) + 0.05,
           y = Inf, vjust = 1.4, hjust = 0, size = 3,
           label = sprintf("Médiane = %.3f ha", stats_parc$median),
           colour = palette_ensae["rouge"], fontface = "italic") +
  scale_x_continuous(
    name   = "Superficie (ha)  [axe : ln(1 + x)]",
    breaks = breaks_log1p,
    labels = lbl_inv_log1p
  ) +
  scale_y_continuous(labels = label_comma()) +
  labs(title    = "Distribution des superficies de parcelles",
       subtitle = "Transformation ln(1 + x) — valeurs nulles incluses",
       y        = "Fréquence pondérée",
       caption  = "Source : GHS W4 — pondéré par wt_wave4 / nb_parcelles") +
  theme_ensae()
p_h1

p_h2 <- ggplot(menages_v,
               aes(x = log1p(superficie_totale_ha), weight = poids_menage)) +
  geom_histogram(bins = 50, fill = palette_ensae["vert"],
                 colour = "white", alpha = 0.90) +
  geom_vline(xintercept = log1p(stats_men$median),
             colour = palette_ensae["rouge"], linewidth = 0.8, linetype = "dashed") +
  annotate("text",
           x = log1p(stats_men$median) + 0.05,
           y = Inf, vjust = 1.4, hjust = 0, size = 3,
           label = sprintf("Médiane = %.3f ha", stats_men$median),
           colour = palette_ensae["rouge"], fontface = "italic") +
  scale_x_continuous(
    name   = "Superficie totale (ha)  [axe : ln(1 + x)]",
    breaks = breaks_log1p,
    labels = lbl_inv_log1p
  ) +
  scale_y_continuous(labels = label_comma()) +
  labs(title    = "Distribution des superficies totales par ménage",
       subtitle = "Transformation ln(1 + x) — valeurs nulles incluses",
       y        = "Fréquence pondérée",
       caption  = "Source : GHS W4 — pondéré par wt_wave4") +
  theme_ensae()
p_h2
p_hist <- (p_h1 / p_h2) +
  plot_annotation(
    title   = "Superficies agricoles — Nigeria GHS Wave 4",
    caption = "Note : axe x en échelle ln(1 + x) pour inclure les valeurs nulles sans −∞.",
    theme   = theme(plot.title = element_text(face = "bold", size = 14,
                                              colour = "#1D3557"))
  )
p_hist

save_fig(p_hist, "Fig01_histogrammes", h = 11)
cat("  ✔ Fig01_histogrammes.png\n")

# ── 20.3 Boxplots ─────────────────────────────────────────────────────────────
df_box <- bind_rows(
  parcelles_v %>% transmute(type = "Par parcelle",
                            val  = log1p(superficie_ha_propre)),
  menages_v   %>% transmute(type = "Par ménage",
                            val  = log1p(superficie_totale_ha))
) %>% mutate(type = factor(type, levels = c("Par parcelle", "Par ménage")))

# Calcul des stats pondérées pour annotation
stats_box <- bind_rows(
  tibble(type = "Par parcelle",
         med = log1p(stats_parc$median),
         p25 = log1p(stats_parc$p25),
         p75 = log1p(stats_parc$p75)),
  tibble(type = "Par ménage",
         med = log1p(stats_men$median),
         p25 = log1p(stats_men$p25),
         p75 = log1p(stats_men$p75))
) %>% mutate(type = factor(type, levels = c("Par parcelle", "Par ménage")))

couleurs_box <- c("Par parcelle" = palette_ensae["bleu"],
                  "Par ménage"   = palette_ensae["vert"])

p_box <- ggplot(df_box, aes(x = type, y = val, fill = type)) +
  geom_violin(alpha = 0.25, linewidth = 0.3, colour = "grey60") +
  geom_boxplot(width = 0.35, outlier.shape = 16,
               outlier.alpha = 0.2, outlier.size = 0.7,
               notch = TRUE, notchwidth = 0.6) +
  # Médiane pondérée (point rouge)
  geom_point(data = stats_box,
             aes(x = type, y = med),
             colour = palette_ensae["rouge"], size = 3, shape = 18,
             inherit.aes = FALSE) +
  scale_y_continuous(
    name   = "Superficie (ha)  [axe : ln(1 + x)]",
    breaks = breaks_log1p,
    labels = lbl_inv_log1p
  ) +
  scale_fill_manual(values = couleurs_box) +
  labs(
    title    = "Boxplots des superficies agricoles — Nigeria GHS Wave 4",
    subtitle = "Violin + boxplot | Encoche = IC 95% médiane | ◆ rouge = médiane pondérée",
    x        = NULL,
    caption  = "Source : Nigeria GHS Panel W4"
  ) +
  theme_ensae() +
  theme(legend.position = "none")
p_box
save_fig(p_box, "Fig02_boxplots", w = 7, h = 6)
cat("  ✔ Fig02_boxplots.png\n")

# ── 20.4 Scatter : superficie déclarée vs GPS ─────────────────────────────────
gps_df <- parcelles_agricoles %>%
  filter(
    !is.na(superficie_ha_propre), superficie_ha_propre >= 0,
    !is.na(superficie_gps_ha),    superficie_gps_ha    >= 0,
    superficie_gps_ha <= 500
  ) %>%
  mutate(
    x_plot     = log1p(superficie_ha_propre),
    y_plot     = log1p(superficie_gps_ha),
    diff_ha    = superficie_gps_ha - superficie_ha_propre,
    sous_estime = superficie_ha_propre > superficie_gps_ha
  )

cat(sprintf("\n  Paires (déclaré, GPS) valides : %d (%.1f%%)\n",
            nrow(gps_df), 100 * nrow(gps_df) / N))

rho_gps <- spearman_ci(gps_df$superficie_ha_propre, gps_df$superficie_gps_ha)

biais_tbl <- tibble(
  n              = nrow(gps_df),
  rho            = rho_gps$rho,
  ci_lower       = rho_gps$ci_lower,
  ci_upper       = rho_gps$ci_upper,
  p_value        = rho_gps$p_value,
  biais_moyen    = mean(gps_df$diff_ha),
  biais_median   = median(gps_df$diff_ha),
  pct_sous_estime = 100 * mean(gps_df$sous_estime)
)
cat(sprintf("  Spearman (décl./GPS) : ρ = %.4f [%.4f ; %.4f]\n",
            rho_gps$rho, rho_gps$ci_lower, rho_gps$ci_upper))
cat(sprintf("  Biais médian         : %.4f ha | Sous-estimation : %.1f%%\n",
            biais_tbl$biais_median, biais_tbl$pct_sous_estime))
save_tab(biais_tbl, "T04_biais_GPS")

lbl_gps <- sprintf(
  "ρ de Spearman = %.4f\nIC 95 %% : [%.4f ; %.4f]\nBiais médian = %.4f ha\nSous-estimation : %.1f %%",
  rho_gps$rho, rho_gps$ci_lower, rho_gps$ci_upper,
  biais_tbl$biais_median, biais_tbl$pct_sous_estime
)

# Limites des axes (échelle ln(1+x))
xlim_gps <- quantile(gps_df$x_plot, c(0.001, 0.999), na.rm = TRUE)
ylim_gps <- quantile(gps_df$y_plot, c(0.001, 0.999), na.rm = TRUE)

p_gps <- ggplot(gps_df, aes(x = x_plot, y = y_plot)) +
  geom_hex(bins = 55, aes(fill = after_stat(log(count + 1))), alpha = 0.85) +
  scale_fill_viridis(option = "C", name = "log(n + 1)",
                     direction = -1, guide = guide_colorbar(barwidth = 8)) +
  geom_abline(slope = 1, intercept = 0,
              colour = palette_ensae["rouge"],
              linewidth = 1.0, linetype = "dashed") +
  geom_smooth(method = "loess", se = TRUE,
              colour = palette_ensae["bleu"], linewidth = 0.85,
              fill = "#AED6F1", alpha = 0.4) +
  annotate("label",
           x     = xlim_gps[1] + 0.1 * diff(xlim_gps),
           y     = ylim_gps[2] - 0.04 * diff(ylim_gps),
           label = lbl_gps,
           hjust = 0, vjust = 1, size = 3.0,
           label.size = 0.3, fill = "white",
           colour = "#1D3557", fontface = "italic") +
  scale_x_continuous(
    name   = "Superficie déclarée (ha)  [ln(1 + x)]",
    limits = xlim_gps,
    breaks = breaks_log1p[breaks_log1p >= xlim_gps[1] &
                            breaks_log1p <= xlim_gps[2]],
    labels = lbl_inv_log1p
  ) +
  scale_y_continuous(
    name   = "Superficie GPS (ha)  [ln(1 + x)]",
    limits = ylim_gps,
    breaks = breaks_log1p[breaks_log1p >= ylim_gps[1] &
                            breaks_log1p <= ylim_gps[2]],
    labels = lbl_inv_log1p
  ) +
  labs(
    title    = "Superficie déclarée vs. superficie mesurée au GPS",
    subtitle = "Ligne rouge (--) = biais nul (45°) | Courbe bleue = LOESS | Hexagones = densité",
    caption  = paste0("Source : Nigeria GHS Panel W4.\n",
                      "Axe ln(1 + x) pour inclure les valeurs nulles sans −∞.")
  ) +
  theme_ensae()
p_gps
save_fig(p_gps, "Fig03_scatter_GPS", w = 9, h = 7)
cat("  ✔ Fig03_scatter_GPS.png\n")

# =============================================================================
# TÂCHE 21 — MODE D'ACQUISITION (TENURE)
# =============================================================================

cat("\n── TÂCHE 21 ─────────────────────────────────────────────────────\n")

tenure_df <- parcelles_agricoles %>%
  filter(!is.na(mode_acquisition), !is.na(wt_parcelle))

tenure_freq <- tenure_df %>%
  group_by(mode_acquisition) %>%
  summarise(n_brut = n(),
            n_eff  = sum(wt_parcelle, na.rm = TRUE), .groups = "drop") %>%
  mutate(
    pct_brut = round(n_brut / sum(n_brut) * 100, 2),
    pct_pond = round(n_eff  / sum(n_eff)  * 100, 2),
    pct_cum  = cumsum(pct_pond)
  ) %>%
  arrange(desc(pct_pond))

cat("\n  [FRÉQUENCES PONDÉRÉES — MODE D'ACQUISITION]\n")
print(tenure_freq)
save_tab(tenure_freq, "T05_tenure_frequences")

# ── Barplot horizontal ────────────────────────────────────────────────────────
couleurs_tenure <- viridis(nrow(tenure_freq), option = "D", direction = -1)

p_tenure <- ggplot(
  tenure_freq,
  aes(x = pct_pond,
      y = fct_reorder(as.character(mode_acquisition), pct_pond),
      fill = fct_reorder(as.character(mode_acquisition), pct_pond))
) +
  geom_col(show.legend = FALSE, alpha = 0.92, width = 0.72) +
  geom_text(
    aes(label = sprintf("%.1f %%   (n = %s)",
                        pct_pond,
                        format(round(n_eff), big.mark = "\u202f"))),
    hjust  = -0.05, size = 3.2, colour = "grey20", fontface = "plain"
  ) +
  scale_fill_viridis_d(option = "D", direction = -1) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.22)),
                     labels = label_percent(scale = 1)) +
  labs(
    title    = "Mode d'acquisition des parcelles agricoles",
    subtitle = "Nigeria GHS Wave 4 — proportions pondérées (wt_wave4 / nb_parcelles)",
    x        = "Part pondérée (%)",
    y        = NULL,
    caption  = "Source : Nigeria GHS Panel — sect11b1_plantingw4 (s11b1q4)"
  ) +
  theme_ensae() +
  theme(panel.grid.major.y = element_blank())
p_tenure
save_fig(p_tenure, "Fig04_tenure_barplot", w = 9, h = 5)
cat("  ✔ Fig04_tenure_barplot.png\n")

# ── Chi-deux tenure × zone rurale / urbaine ───────────────────────────────────
chi2_res  <- chi2_w(tenure_df, "mode_acquisition", "zone_residence", "wt_parcelle")
cr        <- chi2_res$cramer_v
force_lbl <- case_when(cr < 0.10 ~ "négligeable",
                       cr < 0.30 ~ "faible",
                       cr < 0.50 ~ "modérée",
                       TRUE      ~ "forte")

cat(sprintf(
  "\n  χ²(df=%d) = %.2f  p = %.4f  V de Cramér = %.4f (%s)\n  ⚠ Approx. (effectifs pondérés arrondis). Méthode exacte = Rao-Scott.\n",
  chi2_res$test$parameter, chi2_res$test$statistic,
  chi2_res$test$p.value, cr, force_lbl
))

save_tab(
  tibble(X2 = as.numeric(chi2_res$test$statistic),
         df = chi2_res$test$parameter,
         p_value = chi2_res$test$p.value,
         cramer_v = cr, force = force_lbl,
         note = "effectifs_ponderés_arrondis"),
  "T06_chi2_tenure_secteur"
)

# Graphique contingence pondérée 
cting <- as.data.frame(as.table(chi2_res$table)) %>%
  rename(Tenure = Var1, Secteur = Var2, n_eff = Freq)

p_cting <- ggplot(
  cting,
  aes(x = Secteur,
      y = fct_reorder(Tenure, n_eff, sum),
      fill = n_eff)
) +
  geom_tile(colour = "white", linewidth = 0.6) +
  geom_text(aes(label = format(round(n_eff), big.mark = "\u202f")),
            colour = "white", size = 3.2, fontface = "bold") +
  scale_fill_gradient(low = "#AED6F1", high = "#1D3557",
                      name = "Effectif\npondéré",
                      labels = label_comma()) +
  labs(
    title    = "Contingence pondérée : Mode d'acquisition × Secteur",
    subtitle = sprintf(
      "χ²(%d) = %.2f, p = %.4f, V de Cramér = %.3f (%s)",
      chi2_res$test$parameter, chi2_res$test$statistic,
      chi2_res$test$p.value, cr, force_lbl),
    caption  = "Source : Nigeria GHS Panel W4\n⚠ Approximation : effectifs pondérés arrondis",
    x        = NULL, y = NULL
  ) +
  theme_ensae() +
  theme(panel.grid = element_blank())
p_cting
save_fig(p_cting, "Fig05_contingence_tenure_secteur", w = 8, h = 5)
cat("  ✔ Fig05_contingence_tenure_secteur.png\n")

# =============================================================================
# TÂCHE 23 — SUPERFICIE TOTALE vs. NOMBRE DE PARCELLES
# =============================================================================

cat("\n── TÂCHE 23 ─────────────────────────────────────────────────────\n")

np_df <- superficie_menage %>%
  filter(!is.na(superficie_totale_ha), superficie_totale_ha >= 0,
         !is.na(poids_menage), nb_parcelles <= 25)

rho_np <- spearman_ci(np_df$nb_parcelles, np_df$superficie_totale_ha)

cat(sprintf("  Spearman (nb_parcelles ~ sup_totale) : ρ = %.4f [%.4f ; %.4f]  p < 0.001\n",
            rho_np$rho, rho_np$ci_lower, rho_np$ci_upper))
save_tab(rho_np, "T07_spearman_nbparcelles")

lbl_np <- sprintf(
  "ρ = %.4f\nIC 95 %% : [%.4f ; %.4f]\np < 0.001",
  rho_np$rho, rho_np$ci_lower, rho_np$ci_upper
)

p_np <- ggplot(np_df,
               aes(x = nb_parcelles, y = log1p(superficie_totale_ha))) +
  geom_jitter(aes(size = poids_menage, colour = region),
              alpha = 0.30, width = 0.18, height = 0) +
  geom_smooth(aes(weight = poids_menage),
              method = "loess", se = TRUE,
              colour = palette_ensae["rouge"],
              fill   = "#F4A261",
              linewidth = 1.1, alpha = 0.30) +
  scale_colour_viridis_d(option = "D", name = "Région",
                         guide = guide_legend(override.aes = list(size = 3,
                                                                  alpha = 0.8))) +
  scale_size_continuous(name = "Poids sondage",
                        range = c(0.4, 3.5),
                        guide = guide_legend(override.aes = list(alpha = 0.6))) +
  scale_y_continuous(
    name   = "Superficie totale (ha)  [ln(1 + x)]",
    breaks = breaks_log1p,
    labels = lbl_inv_log1p
  ) +
  scale_x_continuous(breaks = seq(1, 25, 2)) +
  annotate("label",
           x = 15.5, y = 0.08,
           label = lbl_np,
           hjust = 0, size = 3.0,
           label.size = 0.3, fill = "white",
           colour = "#1D3557", fontface = "italic") +
  labs(
    title    = "Superficie totale du ménage selon le nombre de parcelles",
    subtitle = "LOESS pondéré (wt_wave4) | Points colorés par région géographique",
    x        = "Nombre de parcelles par ménage",
    caption  = "Source : Nigeria GHS Panel W4 — ménages avec ≤ 25 parcelles."
  ) +
  theme_ensae()
p_np
save_fig(p_np, "Fig06_scatter_nbparcelles", w = 10, h = 6)
cat("  ✔ Fig06_scatter_nbparcelles.png\n")

# =============================================================================
# TÂCHE 24 — CARTE DU NIGERIA (SUPERFICIES PAR ÉTAT)
# =============================================================================

cat("\n── TÂCHE 24 — CARTE NIGERIA ─────────────────────────────────────────\n")



# 1. Récupérer la carte du Nigeria
cat("  Chargement de la carte du Nigeria...\n")
nigeria_map <- ne_states(country = "nigeria", returnclass = "sf")

# 2. Préparer les données de superficie pour la vague 4 uniquement
# Utiliser votre base parcelles_agricoles existante
superficie_etats <- parcelles_agricoles %>%
  filter(!is.na(superficie_ha_propre), !is.na(etat)) %>%
  group_by(etat) %>%
  summarise(
    superficie_mediane_ha = median(superficie_ha_propre, na.rm = TRUE),
    superficie_moyenne_ha = mean(superficie_ha_propre, na.rm = TRUE),
    n_parcelles = n(),
    .groups = "drop"
  ) %>%
  filter(n_parcelles >= 5)  # Garder seulement les états avec au moins 5 parcelles

# 3. Joindre les données à la carte
# Normaliser les noms des états (les cartes peuvent utiliser minuscules/majuscules)
nigeria_map <- nigeria_map %>%
  mutate(name = as.character(name)) %>%
  left_join(
    superficie_etats %>% 
      mutate(etat = as.character(etat)),
    by = c("name" = "etat")
  )

# 4. Créer la carte avec étiquettes et légende
p_carte <- ggplot(nigeria_map) +
  # Polygones des états
  geom_sf(aes(fill = superficie_mediane_ha),
          color = "white", size = 0.3, alpha = 0.9) +
  # Étiquettes des états (centroïdes)
  geom_sf_text(aes(label = name),
               size = 2.5, color = "grey20", fontface = "bold",
               check_overlap = TRUE) +
  # Échelle de couleurs
  scale_fill_viridis_c(
    option = "C",
    name = "Superficie médiane\n(hectares)",
    direction = -1,
    trans = "log1p",  # Échelle log pour mieux visualiser
    labels = label_number(accuracy = 0.01),
    na.value = "grey85"
  ) +
  # Titres et légendes
  labs(
    title = "Superficie médiane des exploitations agricoles par État",
    subtitle = "Nigeria - GHS Panel Wave 4 (2018) | Échelle logarithmique ln(1+x)",
    caption = "Source : Nigeria GHS Panel Wave 4\nSeuls les États avec ≥ 5 parcelles sont représentés",
    fill = "Superficie\nmédiane (ha)"
  ) +
  # Thème personnalisé
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14, colour = "#1D3557",
                              hjust = 0, margin = margin(b = 4)),
    plot.subtitle = element_text(size = 10, colour = "grey40", hjust = 0,
                                 margin = margin(b = 8)),
    plot.caption = element_text(size = 8, colour = "grey55", hjust = 1,
                                margin = margin(t = 6)),
    legend.position = "right",
    legend.title = element_text(face = "bold", size = 9),
    legend.text = element_text(size = 8),
    panel.grid = element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank()
  )
p_carte
# Sauvegarder la carte
save_fig(p_carte, "Fig07_carte_nigeria_superficies", w = 12, h = 10)
cat("  ✔ Fig07_carte_nigeria_superficies.png\n")

# 5. Version alternative avec classes discrètes (pour meilleure lisibilité)
p_carte_discrete <- ggplot(nigeria_map) +
  geom_sf(aes(fill = cut(superficie_mediane_ha, 
                         breaks = c(0, 0.1, 0.25, 0.5, 1, 2, 5, Inf),
                         labels = c("<0,1 ha", "0,1-0,25 ha", "0,25-0,5 ha", 
                                    "0,5-1 ha", "1-2 ha", "2-5 ha", ">5 ha"))),
          color = "white", size = 0.3, alpha = 0.9) +
  geom_sf_text(aes(label = name),
               size = 2.5, color = "grey20", fontface = "bold",
               check_overlap = TRUE) +
  scale_fill_brewer(
    palette = "YlOrRd",
    name = "Superficie médiane",
    na.value = "grey85",
    direction = 1
  ) +
  labs(
    title = "Superficie médiane des exploitations agricoles par État",
    subtitle = "Nigeria - GHS Panel Wave 4 (2018) | Classes discrètes",
    caption = "Source : Nigeria GHS Panel Wave 4"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14, colour = "#1D3557", hjust = 0),
    plot.subtitle = element_text(size = 10, colour = "grey40", hjust = 0),
    legend.position = "right",
    panel.grid = element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank()
  )
p_carte_discrete
save_fig(p_carte_discrete, "Fig07b_carte_nigeria_classes", w = 12, h = 10)
cat("  ✔ Fig07b_carte_nigeria_classes.png\n")

# 6. Tableau récapitulatif des superficies par État
cat("\n  Tableau récapitulatif des superficies par État (Vague 4) :\n")
print(superficie_etats %>% arrange(desc(superficie_mediane_ha)))

save_tab(superficie_etats, "T08_superficies_par_etat_w4")

# 7. Statistiques récapitulatives
cat("\n  Statistiques nationales (Vague 4) :\n")
cat(sprintf("  - Médiane nationale : %.3f ha\n", 
            median(parcelles_agricoles$superficie_ha_propre, na.rm = TRUE)))
cat(sprintf("  - Moyenne nationale : %.3f ha\n", 
            mean(parcelles_agricoles$superficie_ha_propre, na.rm = TRUE)))
cat(sprintf("  - Nombre d'états avec données : %d\n", nrow(superficie_etats)))
cat(sprintf("  - Taux de données GPS : %.1f%%\n",
            mean(!is.na(parcelles_agricoles$superficie_gps_ha)) * 100))

