# ================================================================
# PROJET ENSAE ISE 1 — GHS Nigeria Panel (W4)
# SCRIPT   : scripts/04_tache23_superficie_nparcelles.R
# TÂCHE 23 : Relation superficie totale × nombre de parcelles
# BASE     : data/processed/superficie_menage.rds
# SORTIES  : outputs/figures/t23_*.png | outputs/tables/t23_*.csv
# ================================================================

source("scripts/functions.R")

library(dplyr)
library(ggplot2)
library(scales)

superficie_menage <- readRDS("data/processed/superficie_menage.rds")


# ================================================================
# ÉTAPE 1 — PRÉPARATION
# On travaille au niveau ménage avec :
#   - superficie_totale (ha)
#   - nb_parcelles (nombre de parcelles du ménage)
#   - milieu_label
#   - poids_vague4
# ================================================================

df <- superficie_menage %>%
  filter(
    !is.na(superficie_totale),
    !is.na(nb_parcelles),
    superficie_totale > 0,
    nb_parcelles >= 1
  )

cat(sprintf("Ménages disponibles pour l'analyse : %d\n", nrow(df)))
cat(sprintf("Nombre de parcelles : min=%d | max=%d | médiane=%.0f\n",
            min(df$nb_parcelles),
            max(df$nb_parcelles),
            median(df$nb_parcelles)))


# ================================================================
# ÉTAPE 2 — CORRÉLATION DE SPEARMAN GLOBALE AVEC IC
#
# Pourquoi Spearman et non Pearson ?
#   - superficie_totale est très asymétrique (log-normale)
#   - nb_parcelles est discret et borné
#   - Spearman utilise les RANGS, ce qui est robuste aux extrêmes
#
# IC via transformation de Fisher :
#   z = atanh(rho) est approximativement normal, N(0, 1/(n-3))
#   → IC sur z → retransformer en rho via tanh
# ================================================================

res_sp_global <- ic_spearman(df$superficie_totale, df$nb_parcelles)

cat(sprintf("\n=== Spearman global ===\n"))
cat(sprintf("ρ = %.3f | IC95 [%.3f ; %.3f] | p = %.2e | n = %d\n",
            res_sp_global$rho,
            res_sp_global$ic_inf,
            res_sp_global$ic_sup,
            res_sp_global$p_val,
            res_sp_global$n))

# Spearman par milieu
res_sp_milieu <- df %>%
  filter(!is.na(milieu_label)) %>%
  group_by(milieu_label) %>%
  summarise(
    rho    = ic_spearman(superficie_totale, nb_parcelles)$rho,
    ic_inf = ic_spearman(superficie_totale, nb_parcelles)$ic_inf,
    ic_sup = ic_spearman(superficie_totale, nb_parcelles)$ic_sup,
    p_val  = ic_spearman(superficie_totale, nb_parcelles)$p_val,
    n      = n(),
    .groups = "drop"
  ) %>%
  mutate(across(c(rho, ic_inf, ic_sup), ~round(., 3)))

cat("\n=== Spearman par milieu ===\n")
print(res_sp_milieu)

sauvegarder_tableau(res_sp_milieu, "t23_spearman_par_milieu.csv")


# ================================================================
# ÉTAPE 3 — TABLEAU : superficie médiane par nombre de parcelles
# ================================================================

tableau_par_nparcelles <- df %>%
  group_by(nb_parcelles) %>%
  summarise(
    n             = n(),
    mediane_ha    = round(median(superficie_totale), 3),
    moyenne_ha    = round(mean(superficie_totale),   3),
    p25_ha        = round(quantile(superficie_totale, 0.25), 3),
    p75_ha        = round(quantile(superficie_totale, 0.75), 3),
    .groups       = "drop"
  ) %>%
  filter(n >= 5)   # Ignorer les groupes avec trop peu d'observations

cat("\n=== Superficie médiane par nombre de parcelles ===\n")
print(tableau_par_nparcelles)
sauvegarder_tableau(tableau_par_nparcelles, "t23_superficie_par_nparcelles.csv")


# ================================================================
# ÉTAPE 4 — SCATTER PLOT : superficie totale × nb parcelles
# Avec jitter, loess et annotation du rho
# ================================================================

label_rho <- sprintf("ρ = %.3f\nIC95 [%.3f ; %.3f]\np = %.2e",
                     res_sp_global$rho,
                     res_sp_global$ic_inf,
                     res_sp_global$ic_sup,
                     res_sp_global$p_val)

p_scatter <- df %>%
  ggplot(aes(x = nb_parcelles, y = superficie_totale, color = milieu_label)) +
  # Jitter horizontal : évite la superposition sur les valeurs entières
  geom_jitter(width = 0.25, alpha = 0.25, size = 0.9) +
  # Courbe LOESS globale (toutes modalités confondues)
  geom_smooth(aes(group = 1),
              method = "loess", se = TRUE,
              color = "#D73027", fill = "#FDBB84",
              linewidth = 0.9) +
  annotate("text",
           x = max(df$nb_parcelles) * 0.75,
           y = max(df$superficie_totale, na.rm = TRUE) * 0.85,
           label = label_rho,
           size = 3.5, hjust = 0, color = "grey20") +
  scale_y_log10(labels = label_number(accuracy = 0.01)) +
  scale_x_continuous(breaks = 1:max(df$nb_parcelles)) +
  scale_color_manual(
    values = c("Rural" = "#1D6996", "Urbain" = "#E17C05"),
    name   = "Milieu",
    na.value = "grey60"
  ) +
  labs(
    title    = "Superficie totale du ménage selon le nombre de parcelles",
    subtitle = "Axe y en log | Courbe rouge = LOESS global",
    x        = "Nombre de parcelles",
    y        = "Superficie totale (ha) — échelle log",
    caption  = sprintf("GHS Nigeria W4 — n = %d ménages", nrow(df))
  ) +
  theme_tp4()

print(p_scatter)
sauvegarder_figure("t23_scatter_superficie_nparcelles.png")


# ================================================================
# ÉTAPE 5 — SCATTER PAR MILIEU (facettes)
# Permet de comparer la relation rural vs urbain
# ================================================================

p_facettes <- df %>%
  filter(!is.na(milieu_label)) %>%
  ggplot(aes(x = nb_parcelles, y = superficie_totale)) +
  geom_jitter(width = 0.25, alpha = 0.25, size = 0.8, color = "#2171B5") +
  geom_smooth(method = "loess", se = TRUE,
              color = "#D73027", fill = "#FDBB84",
              linewidth = 0.9) +
  scale_y_log10(labels = label_number(accuracy = 0.01)) +
  scale_x_continuous(breaks = 1:max(df$nb_parcelles)) +
  facet_wrap(~milieu_label, ncol = 2, scales = "free_y") +
  # Annoter le rho par milieu sur chaque panneau
  geom_text(
    data = res_sp_milieu,
    aes(label = sprintf("ρ = %.3f", rho)),
    x = Inf, y = -Inf,
    hjust = 1.1, vjust = -0.5,
    inherit.aes = FALSE,
    size = 3.5, color = "grey25"
  ) +
  labs(
    title    = "Superficie totale × nombre de parcelles, par milieu",
    subtitle = "Courbe LOESS avec intervalle de confiance à 95%",
    x        = "Nombre de parcelles",
    y        = "Superficie totale (ha) — échelle log",
    caption  = "GHS Nigeria W4"
  ) +
  theme_tp4()

print(p_facettes)
sauvegarder_figure("t23_scatter_facettes_milieu.png", largeur = 12)

cat("\n✔ Tâche 23 terminée.\n")
