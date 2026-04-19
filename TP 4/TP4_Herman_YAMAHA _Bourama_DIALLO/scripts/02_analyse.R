# =============================================================================
# Script 02 : Exploration statistique et visualisations (avec pondérations)
# TP4 — Analyse des parcelles agricoles
# Nigeria GHS-Panel Wave 4 (2018/19)
#
# Ce script produit :
#   fig01 — Distribution de la superficie par parcelle
#   fig02 — Distribution du régime de tenure
#   fig03 — Heatmap État x wave4
#   fig04 — Superficie totale et nombre de parcelles agricoles
#
# NOTE MÉTHODOLOGIQUE :
#   Toutes les statistiques descriptives sont calculées avec les pondérations
#   transversales wt_wave4 afin que les estimations soient représentatives
#   de la population nigériane (et non seulement de l'échantillon).
#
# Auteurs  : Herman YAMAHA | Bourama DIALLO
# =============================================================================


# =============================================================================
# Installation automatique des packages manquants
# =============================================================================
packages_cran <- c(
  "haven", "tidyverse", "naniar", "here",
  "survey", "scales", "wCorr", "dplyr"
)

packages_github <- list(
  "Hmisc" = "harrelfe/Hmisc"
)

for (pkg in packages_cran) {
  if (!require(pkg, character.only = TRUE)) {
    message(paste("Installation CRAN:", pkg))
    install.packages(pkg, dependencies = TRUE)
    library(pkg, character.only = TRUE)
  }
}

if (!require("remotes", character.only = TRUE)) {
  install.packages("remotes")
  library(remotes)
}

for (pkg in names(packages_github)) {
  if (!require(pkg, character.only = TRUE)) {
    message(paste("Installation GitHub:", pkg))
    remotes::install_github(packages_github[[pkg]])
    library(pkg, character.only = TRUE)
  }
}

# --------------------------------------------------------------------------
# Création des dossiers outputs si nécessaire
# --------------------------------------------------------------------------
dir.create(here("outputs", "figures"), showWarnings = FALSE, recursive = TRUE)
dir.create(here("outputs", "tables"),  showWarnings = FALSE, recursive = TRUE)

# --------------------------------------------------------------------------
# Vérification : si les objets .rds n'existent pas encore, exécuter le script 01
# --------------------------------------------------------------------------
if (!file.exists(here("data", "processed", "df_poids.rds"))) {
  cat("Objets intermédiaires absents — exécution de 01_nettoyage.R...\n")
  source(here("scripts", "01_nettoyage.R"))
}

# --------------------------------------------------------------------------
# Chargement des objets préparés par le script 01
# --------------------------------------------------------------------------
df_poids      <- readRDS(here("data", "processed", "df_poids.rds"))
base_parcelle <- readRDS(here("data", "processed", "base_parcelle.rds"))

# =============================================================================
#  Analyse univariée de la superficie par parcelle ET par ménage
# =============================================================================

# -----------------------------------------------------------------------------
# DONNÉES DE BASE
# -----------------------------------------------------------------------------

# Filtrer parcelles valides
bp_clean <- base_parcelle %>%
  filter(!is.na(superficie_ha), superficie_ha > 0)

cat("Parcelles avec superficie valide :", nrow(bp_clean), "\n")

# Agrégation par ménage (superficie totale = somme des parcelles)
menage_clean <- bp_clean %>%
  group_by(hhid) %>%
  summarise(
    superficie_totale_ha = sum(superficie_ha, na.rm = TRUE),
    n_parcelles          = n(),
    wt_wave4             = first(wt_wave4),
    .groups = "drop"
  ) %>%
  filter(!is.na(wt_wave4), superficie_totale_ha > 0)

cat("Ménages avec superficie valide :", nrow(menage_clean), "\n")

# =============================================================================
# PARTIE 1 — Histogrammes (échelle log) par parcelle ET par ménage
# =============================================================================

breaks_ha <- c(0, 0.01, 0.05, 0.1, 0.25, 0.5, 1, 2, 5, 10, 50, Inf)
labels_ha <- c("0–0.01", "0.01–0.05", "0.05–0.1", "0.1–0.25",
               "0.25–0.5", "0.5–1", "1–2", "2–5", "5–10", "10–50", ">50")

# -- Parcelle --
prop_parcelle <- bp_clean %>%
  mutate(classe = cut(superficie_ha, breaks = breaks_ha,
                      labels = labels_ha,
                      include.lowest = TRUE, right = FALSE)) %>%
  group_by(classe) %>%
  summarise(poids = sum(wt_wave4, na.rm = TRUE)) %>%
  mutate(proportion = poids / sum(poids))

hist_parcelle <- ggplot(prop_parcelle, aes(x = classe, y = proportion)) +
  geom_col(fill = "#2E86AB", color = "black", alpha = 0.85) +
  geom_text(aes(label = percent(proportion, accuracy = 0.1)),
            vjust = -0.5, size = 3.5) +
  scale_y_continuous(labels = percent_format(accuracy = 1),
                     breaks = seq(0, 0.40, by = 0.02),
                     limits = c(0, 0.42)) +
  labs(title    = "Distribution de la superficie par parcelle (échelle log)",
       subtitle = "Wave 4 — Nigeria GHS Panel | Pondéré par wt_wave4",
       x = "Classe de superficie (ha)", y = "Proportion pondérée") +
  theme_minimal(base_size = 13) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.major.x = element_blank())

ggsave(here("outputs", "figures", "hist_superficie_parcelle.png"),
       plot = hist_parcelle, width = 10, height = 7, dpi = 300)

# -- Ménage --
prop_menage <- menage_clean %>%
  mutate(classe = cut(superficie_totale_ha, breaks = breaks_ha,
                      labels = labels_ha,
                      include.lowest = TRUE, right = FALSE)) %>%
  group_by(classe) %>%
  summarise(poids = sum(wt_wave4, na.rm = TRUE)) %>%
  mutate(proportion = poids / sum(poids))

hist_menage <- ggplot(prop_menage, aes(x = classe, y = proportion)) +
  geom_col(fill = "#E07B39", color = "black", alpha = 0.85) +
  geom_text(aes(label = percent(proportion, accuracy = 0.1)),
            vjust = -0.5, size = 3.5) +
  scale_y_continuous(labels = percent_format(accuracy = 1),
                     breaks = seq(0, 0.40, by = 0.02),
                     limits = c(0, 0.42)) +
  labs(title    = "Distribution de la superficie totale par ménage (échelle log)",
       subtitle = "Wave 4 — Nigeria GHS Panel | Pondéré par wt_wave4",
       x = "Classe de superficie (ha)", y = "Proportion pondérée") +
  theme_minimal(base_size = 13) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.major.x = element_blank())

ggsave(here("outputs", "figures", "hist_superficie_menage.png"),
       plot = hist_menage, width = 10, height = 7, dpi = 300)

# =============================================================================
# PARTIE 2 — Statistiques descriptives par décile (parcelle ET ménage)
# =============================================================================

# -- Parcelle --
deciles_breaks_parcelle <- wtd.quantile(
  bp_clean$superficie_ha,
  weights = bp_clean$wt_wave4,
  probs   = seq(0, 1, by = 0.1)
)

bp_clean <- bp_clean %>%
  mutate(decile = cut(superficie_ha,
                      breaks         = deciles_breaks_parcelle,
                      labels         = paste0("D", 1:10),
                      include.lowest = TRUE))

stats_decile_parcelle <- bp_clean %>%
  group_by(decile) %>%
  summarise(
    n_parcelles  = n(),
    moy_ponderee = round(weighted.mean(superficie_ha, wt_wave4, na.rm = TRUE), 3),
    mediane_pond = round(wtd.quantile(superficie_ha, weights = wt_wave4, probs = 0.5), 3),
    min          = round(min(superficie_ha, na.rm = TRUE), 3),
    max          = round(max(superficie_ha, na.rm = TRUE), 3),
    .groups = "drop"
  )

cat("\n--- Statistiques par décile (PARCELLE) ---\n")
print(stats_decile_parcelle, n = 10)

# -- Ménage --
deciles_breaks_menage <- wtd.quantile(
  menage_clean$superficie_totale_ha,
  weights = menage_clean$wt_wave4,
  probs   = seq(0, 1, by = 0.1)
)

menage_clean <- menage_clean %>%
  mutate(decile = cut(superficie_totale_ha,
                      breaks         = deciles_breaks_menage,
                      labels         = paste0("D", 1:10),
                      include.lowest = TRUE))

stats_decile_menage <- menage_clean %>%
  group_by(decile) %>%
  summarise(
    n_menages    = n(),
    moy_ponderee = round(weighted.mean(superficie_totale_ha, wt_wave4, na.rm = TRUE), 3),
    mediane_pond = round(wtd.quantile(superficie_totale_ha, weights = wt_wave4, probs = 0.5), 3),
    min          = round(min(superficie_totale_ha, na.rm = TRUE), 3),
    max          = round(max(superficie_totale_ha, na.rm = TRUE), 3),
    .groups = "drop"
  )

cat("\n--- Statistiques par décile (MÉNAGE) ---\n")
print(stats_decile_menage, n = 10)

# =============================================================================
# PARTIE 3 — Boxplots pondérés (parcelle ET ménage) — graphique combiné
# =============================================================================

# Fonction pour calculer les quantiles pondérés pour le boxplot
boxplot_stats_pond <- function(x, w, label) {
  q <- wtd.quantile(x, weights = w, probs = c(0.25, 0.5, 0.75))
  iqr <- q[3] - q[1]
  data.frame(
    groupe = label,
    ymin   = max(min(x), q[1] - 1.5 * iqr),
    lower  = q[1],
    middle = q[2],
    upper  = q[3],
    ymax   = min(max(x), q[3] + 1.5 * iqr)
  )
}

# Calcul des stats pour chaque groupe
bp_stats  <- boxplot_stats_pond(bp_clean$superficie_ha,
                                bp_clean$wt_wave4,
                                "Parcelle")

men_stats <- boxplot_stats_pond(menage_clean$superficie_totale_ha,
                                menage_clean$wt_wave4,
                                "Ménage")

# Combinaison des deux
combined_stats <- bind_rows(bp_stats, men_stats)

# Graphique combiné
box_combined <- ggplot(combined_stats, aes(x = groupe)) +
  geom_boxplot(
    aes(ymin = ymin, lower = lower, middle = middle,
        upper = upper, ymax = ymax, fill = groupe),
    stat = "identity", alpha = 0.7, width = 0.4
  ) +
  scale_fill_manual(
    values = c("Parcelle" = "#2E86AB", "Ménage" = "#E07B39"),
    guide  = "none"
  ) + 
  scale_y_continuous(
    breaks = seq(0, max(combined_stats$ymax), by = 0.25) 
    ) +
  labs(
    title    = "Boxplots pondérés de la superficie — Parcelle vs Ménage",
    subtitle = "Quantiles pondérés par wt_wave4 — Wave 4 Nigeria GHS",
    x        = "",
    y        = "Superficie (ha)"
  ) +
  theme_minimal(base_size = 13)

ggsave(here("outputs", "figures", "boxplot_superficie_combined.png"),
       plot = box_combined, width = 8, height = 7, dpi = 300)

# =============================================================================
# PARTIE 4 — Scatter déclaré vs GPS + ligne 45° + Spearman pondéré
# =============================================================================

bp_gps <- base_parcelle %>%
  filter(!is.na(superficie_ha), !is.na(s11aq4c),
         superficie_ha > 0, s11aq4c > 0,
         !is.na(wt_wave4)) %>%
  mutate(superficie_gps_ha = s11aq4c / 10000)  # Conversion m² → ha

cat("\nObservations avec GPS non manquant :", nrow(bp_gps), "\n")
cat("Valeurs manquantes s11aq4c :", sum(is.na(base_parcelle$s11aq4c)), "\n")

# Corrélation de Spearman pondérée
rho_spearman <- weightedCorr(
  bp_gps$superficie_ha,
  bp_gps$superficie_gps_ha,
  method  = "Spearman",
  weights = bp_gps$wt_wave4
)
cat("Corrélation de Spearman pondérée :", round(rho_spearman, 3), "\n")

scatter_gps <- ggplot(bp_gps, aes(x = superficie_ha, y = superficie_gps_ha)) +
  geom_point(aes(size = wt_wave4), alpha = 0.3, color = "#2E86AB") +
  geom_abline(slope = 1, intercept = 0,
              color = "red", linewidth = 1, linetype = "dashed") +
  scale_x_log10() +
  scale_y_log10() +
  scale_size_continuous(guide = "none") +
  labs(
    title    = "Superficie déclarée vs Superficie GPS",
    subtitle = paste0("Wave 4 | Spearman pondéré ρ = ", round(rho_spearman, 3),
                      " | N = ", nrow(bp_gps), " parcelles"),
    x        = "Superficie déclarée (ha) — échelle log",
    y        = "Superficie GPS (ha) — échelle log",
    caption  = paste0("Note : ", sum(is.na(base_parcelle$s11aq4c)),
                      " valeurs manquantes exclues pour s11aq4c")
  ) +
  theme_minimal(base_size = 13)

ggsave(here("outputs", "figures", "scatter_declare_vs_gps.png"),
       plot = scatter_gps, width = 8, height = 7, dpi = 300)





# =============================================================================
# PARTIE 5 — Régime de tenure foncière
# =============================================================================

# Nettoyage : garder uniquement les parcelles avec tenure, secteur et poids valides
bp_tenure <- base_parcelle %>%
  filter(!is.na(s11b1q4), !is.na(sector_11b1), !is.na(wt_wave4))

# Recoder les labels de tenure
tenure_labels <- c(
  "1" = "Outright purchase",
  "2" = "Rented (cash or in-kind)",
  "3" = "Used free of charge",
  "4" = "Distributed by community",
  "5" = "Family inheritance",
  "6" = "Sharecropped in",
  "7" = "Temporary land exchange"
)

bp_tenure <- bp_tenure %>%
  mutate(tenure_label = factor(s11b1q4, levels = 1:7, labels = tenure_labels))

# Définir le plan d'enquête pour la tenure
design_tenure <- svydesign(
  ids     = ~ea_11a1,
  strata  = ~zone_11a1,
  weights = ~wt_wave4,
  data = bp_tenure,
  nest = TRUE
  
  )

# Fréquences pondérées par régime de tenure
freq_tenure <- svytable(~tenure_label, design_tenure)
prop_tenure <- prop.table(freq_tenure)

print(freq_tenure)
print(round(prop_tenure, 3))

# Barplot pondéré
barplot_tenure <- bp_tenure %>%
  group_by(tenure_label) %>%
  summarise(freq = sum(wt_wave4)) %>%
  mutate(prop = freq / sum(freq)) %>%
  ggplot(aes(x = tenure_label, y = prop, fill = tenure_label)) +
  geom_col() +
  geom_text(aes(label = scales::percent(prop, accuracy = 0.1)),
            vjust = -0.5, size = 4) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    title = "Distribution pondérée du régime de tenure",
    x     = "Régime de tenure",
    y     = "Proportion pondérée"
  ) +
  theme_minimal(base_size = 10) +
  theme(
    legend.title = element_blank(),
    axis.text.x  = element_text(angle = 45, hjust = 1)
  )

ggsave(
  here("outputs", "figures", "Distribution_du_regime_de_tenure.png"),
  plot = barplot_tenure, width = 10, height = 7, dpi = 300
)

# Test du chi-deux pondéré (tenure × secteur)
chi_test <- svychisq(~tenure_label + sector_11b1, design_tenure)
print(chi_test)


# =============================================================================
# PARTIE 6 — Corrélation superficie totale × nombre de parcelles
# =============================================================================

menage_data <- base_parcelle %>%
  group_by(hhid) %>%
  summarise(
    superficie_totale_ha = sum(superficie_ha, na.rm = TRUE),
    n_parcelles          = n(),
    wt_wave4             = first(wt_wave4),
    .groups = "drop"
  ) %>%
  filter(!is.na(wt_wave4), superficie_totale_ha > 0, n_parcelles > 0)

cat("Nombre de ménages retenus :", nrow(menage_data), "\n")

# Corrélation de Spearman pondérée
rho_spearman_q23 <- weightedCorr(
  menage_data$superficie_totale_ha,
  menage_data$n_parcelles,
  method  = "Spearman",
  weights = menage_data$wt_wave4
)
cat("Corrélation de Spearman pondérée :", round(rho_spearman_q23, 3), "\n")

# Scatter plot
scatter_plot <- ggplot(menage_data,
                       aes(x = n_parcelles, y = superficie_totale_ha,
                           weight = wt_wave4)) +
  geom_point(alpha = 0.4, color = "#2E86AB", size = 1.5) +
  geom_smooth(method = "loess", se = TRUE, color = "red", linewidth = 1) +
  scale_y_log10() +
  labs(
    title    = "Superficie totale vs nombre de parcelles",
    subtitle = paste0("Wave 4 | Spearman pondéré ρ = ", round(rho_spearman_q23, 3),
                      " | N = ", nrow(menage_data), " ménages"),
    x = "Nombre de parcelles par ménage",
    y = "Superficie totale (ha, échelle log)"
  ) +
  theme_minimal(base_size = 13)

ggsave(
  here("outputs", "figures", "superficie_totale_vs_nombre_de_parcelles.png"),
  plot = scatter_plot, width = 10, height = 7, dpi = 300
)


# =============================================================================
# PARTIE 7 — Heatmap superficie médiane par État
# =============================================================================
# Agrégation par ménage avec variables géographiques
superficie_menage <- base_parcelle %>%
  mutate(
    state_name = as_factor(state_11a1) %>%
      as.character() %>%
      sub("^[0-9]+\\.\\s*", "", .)  # supprime le préfixe "1. ", "37. " etc.
  ) %>%
  group_by(hhid, state_name, ea_11a1, zone_11a1) %>%
  summarise(
    superficie_totale_ha = sum(superficie_ha, na.rm = TRUE),
    n_parcelles          = n(),
    wt_wave4             = first(wt_wave4),
    .groups = "drop"
  ) %>%
  filter(!is.na(wt_wave4), superficie_totale_ha > 0)

# Plan de sondage multi-degrés
design_menage <- svydesign(
  ids     = ~ea_11a1,
  strata  = ~zone_11a1,
  weights = ~wt_wave4,
  data    = superficie_menage,
  nest    = TRUE
)

# Médiane pondérée par État
median_state <- svyby(
  ~superficie_totale_ha,
  ~state_name,              # ← state_name à la place de state_11a1
  design_menage,
  svyquantile,
  quantiles = 0.5,
  ci        = TRUE,
  keep.var  = TRUE
)

# Préparation des données
median_state_df <- median_state %>%
  as_tibble() %>%
  rename(
    median_ha = superficie_totale_ha,
    se_median = se.superficie_totale_ha
  ) %>%
  mutate(
    wave       = "Wave 4",
    state_name = reorder(state_name, median_ha)  # ← state_name
  )

# Heatmap
heatmap <- ggplot(median_state_df,
                  aes(x = wave, y = state_name, fill = median_ha)) +  # ← state_name
  geom_tile(color = "white") +
  scale_fill_gradient(
    low  = "#d0e8f5",
    high = "#08306b",
    name = "Médiane (ha)"
  ) +
  geom_text(
    aes(label = round(median_ha, 2)),
    color = "white", size = 3, fontface = "bold"
  ) +
  labs(
    title    = "Superficie médiane pondérée par État (Wave 4)",
    subtitle = "États classés du plus grand au plus petit",
    x = "Vague", y = "État", fill = "Médiane (ha)"
  ) +
  theme_minimal(base_size = 13) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(
  here("outputs", "figures", "heatmap_etats.png"),
  plot = heatmap, width = 10, height = 10, dpi = 300
)

# =============================================================================
# Sauvegarde des bases intermédiaires
# =============================================================================
saveRDS(bp_clean,         here("data", "processed", "bp_clean.rds"))
saveRDS(base_parcelle,        here("data", "processed", "base_parcelle.rds"))
saveRDS(bp_tenure,        here("data", "processed", "bp_tenure.rds"))
saveRDS(superficie_menage, here("data", "processed", "superficie_menage.rds"))