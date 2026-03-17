# =============================================================================
# Analyse 3 – Script 06 : Dépenses de santé – rural vs urbain
# Tâche 18 : Comparer les dépenses médianes entre zones rurales et urbaines :
#            test de Wilcoxon-Mann-Whitney, violin plot + boxplot superposé.
#
# Méthode :
#   – Test de Wilcoxon bilatéral (hypothèse nulle : médianes égales)
#   – Taille d'effet r = |Z| / sqrt(n) (corrélation de rang de Rosenthal)
#   – Violin plot sur échelle log pour rendre la forme visible
#     (sans log, les outliers écrasent les violons à zéro)
#
# Auteurs  : Groupe 7 – Herman YAMAHA | Bourama DIALLO
# Données  : Nigeria GHS Panel – Wave 4 (2018), Post-Harvest
# =============================================================================

library(haven)
library(dplyr)
library(ggplot2)
library(scales)
library(rstatix)
library(patchwork)
library(gtsummary)

# --------------------------------------------------------------------------
# 1. CHARGEMENT
# --------------------------------------------------------------------------

df_health <- readRDS("data/df_health_base.rds")
cons      <- read_dta("data/raw/totcons_final.dta")

# --------------------------------------------------------------------------
# 2. CONSTRUCTION DES DÉPENSES ET DES QUINTILES
# --------------------------------------------------------------------------

cons_clean <- cons %>%
  select(hhid, totcons_adj) %>%
  mutate(
    quintile = ntile(totcons_adj, 5),
    quintile_label = factor(
      quintile, levels = 1:5,
      labels = c("Q1 (plus pauvres)", "Q2", "Q3", "Q4", "Q5 (plus riches)")
    )
  )

# Agrégation des trois composantes de dépense
df_depenses <- df_health %>%
  mutate(
    dep_consultation = if_else(is.na(s4aq9),  0, as.numeric(s4aq9)),
    dep_medicaments  = if_else(is.na(s4aq14), 0, as.numeric(s4aq14)),
    dep_hopital      = if_else(is.na(s4aq17), 0, as.numeric(s4aq17)),
    dep_totale       = dep_consultation + dep_medicaments + dep_hopital
  ) %>%
  filter(dep_totale > 0, !is.na(milieu_label)) %>%
  left_join(cons_clean, by = "hhid")

cat("Individus avec dépenses > 0 :", nrow(df_depenses), "\n")

# --------------------------------------------------------------------------
# 3. STATISTIQUES DESCRIPTIVES PAR MILIEU
# --------------------------------------------------------------------------

stats_milieu <- df_depenses %>%
  group_by(Milieu = milieu_label) %>%
  summarise(
    N       = n(),
    Médiane = median(dep_totale),
    Moyenne = round(mean(dep_totale)),
    Q25     = quantile(dep_totale, 0.25),
    Q75     = quantile(dep_totale, 0.75),
    .groups = "drop"
  )

cat("\n=== Statistiques des dépenses par milieu (Naira) ===\n")
print(stats_milieu)

# --------------------------------------------------------------------------
# 4. TEST DE WILCOXON-MANN-WHITNEY
# --------------------------------------------------------------------------
# Test non paramétrique approprié car la distribution des dépenses est
# très asymétrique (non normale). On compare les rangs, pas les moyennes.

wilcox_res <- wilcox.test(
  dep_totale ~ milieu_label,
  data     = df_depenses,
  exact    = FALSE,
  conf.int = TRUE
)

# Taille d'effet r de Rosenthal : r = |Z| / sqrt(n_total)
# Z est obtenu via la p-value du test bilatéral (approximation normale)
n_tot <- nrow(df_depenses)
z_val <- qnorm(wilcox_res$p.value / 2)
r_eff <- abs(z_val) / sqrt(n_tot)

cat("\n=== Test de Wilcoxon-Mann-Whitney ===\n")
cat("W        =", wilcox_res$statistic, "\n")
cat("p-value  =", format.pval(wilcox_res$p.value, digits = 4), "\n")
cat("r (rang) =", round(r_eff, 4),
    "→",
    ifelse(r_eff < 0.10, "effet négligeable",
    ifelse(r_eff < 0.30, "petit effet",
    ifelse(r_eff < 0.50, "effet moyen", "grand effet"))), "\n")

# Sauvegarde des résultats
write.csv(
  data.frame(
    Test       = "Wilcoxon-Mann-Whitney",
    W          = wilcox_res$statistic,
    p_value    = wilcox_res$p.value,
    r_effet    = r_eff,
    med_urbain = stats_milieu$Médiane[stats_milieu$Milieu == "Urbain"],
    med_rural  = stats_milieu$Médiane[stats_milieu$Milieu == "Rural"]
  ),
  "outputs/tables/06_wilcoxon_rural_urbain.csv",
  row.names = FALSE
)

# --------------------------------------------------------------------------
# 5. VIOLIN PLOT + BOXPLOT SUPERPOSÉ : RURAL VS URBAIN
# --------------------------------------------------------------------------
# CORRECTION MAJEURE :
# – Utilisation de scale_y_log10() : les dépenses s'étalent sur plusieurs
#   ordres de grandeur. Sans log, les violons sont écrasés en bas et
#   invisibles. Avec log, la forme de la distribution est parfaitement
#   visible sur toute sa gamme.
# – L'annotation Wilcoxon est placée en haut de la zone de tracé avec
#   annotate("label") qui ajoute un fond blanc pour la lisibilité.

p_label <- ifelse(
  wilcox_res$p.value < 0.001,
  "p < 0,001",
  paste0("p = ", round(wilcox_res$p.value, 3))
)

p_violin <- ggplot(
  df_depenses,
  aes(x = milieu_label, y = dep_totale, fill = milieu_label)
) +
  # Violin : montre la densité de la distribution
  geom_violin(trim = TRUE, alpha = 0.55, color = NA) +
  # Boxplot superposé : montre médiane, Q1-Q3, et whiskers
  geom_boxplot(
    width         = 0.12,
    fill          = "white",
    color         = "grey30",
    outlier.shape = NA,    # outliers masqués (trop nombreux, bruitent)
    linewidth     = 0.7
  ) +
  # Point rouge = médiane
  stat_summary(
    fun   = median,
    geom  = "point",
    shape = 18,
    size  = 4,
    color = "#E63946"
  ) +
  # CORRECTION : annotation Wilcoxon avec fond blanc (lisible sur tout fond)
  annotate(
    "label",
    x          = 1.5,
    y          = Inf,
    label      = paste0("Test de Wilcoxon\n",
                        p_label,
                        " | r = ", round(r_eff, 3)),
    vjust      = 1.2,
    size       = 3.8,
    fill       = "white",
    label.size = 0.3,
    fontface   = "italic",
    color      = "grey20"
  ) +
  # Échelle log : indispensable pour rendre les violons lisibles
  scale_y_log10(
    labels = label_comma(big.mark = " "),
    breaks = c(10, 100, 500, 1000, 5000, 10000, 50000)
  ) +
  scale_fill_manual(
    values = c("Urbain" = "#3A86FF", "Rural" = "#FFBE0B"),
    name   = "Milieu"
  ) +
  labs(
    title    = "Distribution des dépenses de santé : rural vs urbain",
    subtitle = "Échelle logarithmique — Wave 4 (2018) | losange rouge = médiane",
    x        = NULL,
    y        = "Dépense totale de santé (Naira, échelle log)",
    caption  = "Source : Nigeria GHS Panel W4"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title      = element_text(face = "bold", size = 14),
    plot.subtitle   = element_text(color = "grey40"),
    legend.position = "none",
    axis.text.x     = element_text(size = 13, face = "bold")
  )

ggsave("outputs/figures/06a_violin_rural_urbain.png", p_violin,
       width = 8, height = 6, dpi = 300)
cat("Graphique sauvegardé : outputs/figures/06a_violin_rural_urbain.png\n")

# --------------------------------------------------------------------------
# 6. VIOLIN PLOT PAR QUINTILE DE CONSOMMATION
# --------------------------------------------------------------------------

df_plot_q <- df_depenses %>% filter(!is.na(quintile_label))

p_violin_quint <- ggplot(
  df_plot_q,
  aes(x = quintile_label, y = dep_totale, fill = quintile_label)
) +
  geom_violin(trim = TRUE, alpha = 0.6, color = NA) +
  geom_boxplot(
    width         = 0.1,
    fill          = "white",
    color         = "grey30",
    outlier.shape = NA,
    linewidth     = 0.6
  ) +
  stat_summary(
    fun   = median,
    geom  = "point",
    shape = 18,
    size  = 3.5,
    color = "#E63946"
  ) +
  scale_y_log10(
    labels = label_comma(big.mark = " "),
    breaks = c(10, 100, 500, 1000, 5000, 10000, 50000)
  ) +
  scale_fill_brewer(palette = "RdYlBu", direction = -1) +
  labs(
    title    = "Dépenses de santé par quintile de consommation",
    subtitle = "Échelle logarithmique — Wave 4 (2018) | losange rouge = médiane",
    x        = "Quintile de consommation",
    y        = "Dépense totale de santé (Naira, échelle log)",
    caption  = "Source : Nigeria GHS Panel W4"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title      = element_text(face = "bold", size = 14),
    plot.subtitle   = element_text(color = "grey40"),
    legend.position = "none"
  )

ggsave("outputs/figures/06b_violin_quintiles.png", p_violin_quint,
       width = 10, height = 6, dpi = 300)
cat("Graphique sauvegardé : outputs/figures/06b_violin_quintiles.png\n")

# --------------------------------------------------------------------------
# 7. FIGURE COMBINÉE (patchwork)
# --------------------------------------------------------------------------

p_combined <- p_violin / p_violin_quint +
  plot_annotation(
    title    = "Dépenses de santé — disparités par milieu et niveau de richesse",
    subtitle = "Nigeria GHS Panel — Wave 4, Post-Harvest 2018",
    theme    = theme(
      plot.title    = element_text(face = "bold", size = 15),
      plot.subtitle = element_text(color = "grey40", size = 12)
    )
  )

ggsave("outputs/figures/06c_violin_combined.png", p_combined,
       width = 10, height = 12, dpi = 300)
cat("Graphique sauvegardé : outputs/figures/06c_violin_combined.png\n")

# --------------------------------------------------------------------------
# 8. TABLEAU RÉCAPITULATIF gtsummary
# --------------------------------------------------------------------------

tbl_milieu <- df_depenses %>%
  select(milieu_label, dep_totale, dep_consultation,
         dep_medicaments, dep_hopital) %>%
  tbl_summary(
    by        = milieu_label,
    label     = list(
      dep_totale       ~ "Dépense totale (Naira)",
      dep_consultation ~ "  Consultation",
      dep_medicaments  ~ "  Médicaments",
      dep_hopital      ~ "  Hospitalisation"
    ),
    statistic = list(
      all_continuous() ~ "{median} [{p25} – {p75}]"
    ),
    digits    = list(all_continuous() ~ 0)
  ) %>%
  add_p(test = list(all_continuous() ~ "wilcox.test")) %>%
  modify_caption("**Dépenses de santé médianes par milieu de résidence**")

print(tbl_milieu)

cat("\n=== Script 06 terminé avec succès ===\n")
