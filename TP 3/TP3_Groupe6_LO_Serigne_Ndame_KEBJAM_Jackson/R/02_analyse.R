# =============================================================================
# 03_analyse.R — Analyses statistiques et visualisations
# TP3 : Accès aux services de santé et chocs sanitaires
# GHS Panel Nigeria | Wave 4 | ENSAE ISE 1 — 2025-2026
# Tâches couvertes : 13 → 18
# NB : zone  = 6 zones géopolitiques nigérianes
#      zone1 = regroupement Nord / Sud
# =============================================================================

library(dplyr)
library(ggplot2)
library(scales)
library(patchwork)
library(gtsummary)
library(rstatix)
library(naniar)
library(writexl)

source("R/fonctions.R")

# --- Chargement des données nettoyées ----------------------------------------
data_morbid   <- readRDS("data/processed/data_morbid.rds")
data_depenses <- readRDS("data/processed/data_depenses.rds")
data_recours  <- readRDS("data/processed/data_recours.rds")
sect3a_clean  <- readRDS("data/processed/sect3a_clean.rds")
sect3b_clean  <- readRDS("data/processed/sect3b_clean.rds")

# Palette couleurs zones géopolitiques
palette_zone <- c(
  "Centre-Nord" = "#a6cee3",
  "Nord-Est"    = "#1f78b4",
  "Nord-Ouest"  = "#08519c",
  "Sud-Est"     = "#f4a582",
  "Sud-Sud"     = "#d6604d",
  "Sud-Ouest"   = "#b2182b"
)
palette_zone1 <- c("Nord" = "#1f78b4", "Sud" = "#d6604d")

# =============================================================================
# TÂCHE 13 — Taux de morbidité par sexe et groupe d'âge
# =============================================================================

cat("=== TÂCHE 13 — Taux de morbidité ===\n")
cat("Taux global :",
    round(mean(data_morbid$malade, na.rm = TRUE) * 100, 2), "%\n")

# IC 95% par sexe
ic_sexe <- calc_ic_proportion(
  data        = data_morbid |> filter(!is.na(sexe)),
  group_var   = "sexe",
  success_var = "malade"
)
print(ic_sexe)

p_morbid_sexe <- ggplot(ic_sexe, aes(x = sexe, y = taux, fill = sexe)) +
  geom_col(alpha = 0.85, width = 0.5) +
  geom_errorbar(aes(ymin = ic_lo, ymax = ic_hi), width = 0.2, linewidth = 0.8) +
  geom_text(aes(label = paste0(round(taux, 1), "%")),
            vjust = -0.8, size = 4.5, fontface = "bold") +
  scale_fill_manual(values = c("Homme" = "#2166ac", "Femme" = "#d6604d")) +
  labs(title = "Taux de morbidité par sexe (W4, 2018)",
       x = NULL, y = "% ayant déclaré une maladie/blessure") +
  theme_minimal(base_size = 13) +
  theme(legend.position = "none")

# IC 95% par groupe d'âge
ic_age <- calc_ic_proportion(
  data        = data_morbid |> filter(!is.na(groupe_age)),
  group_var   = "groupe_age",
  success_var = "malade"
)
print(ic_age)

p_morbid_age <- ggplot(ic_age, aes(x = groupe_age, y = taux, fill = groupe_age)) +
  geom_col(alpha = 0.85) +
  geom_errorbar(aes(ymin = ic_lo, ymax = ic_hi), width = 0.3, linewidth = 0.8) +
  geom_text(aes(label = paste0(round(taux, 1), "%")),
            vjust = -0.8, size = 4) +
  scale_fill_brewer(palette = "RdYlBu", direction = -1) +
  labs(title = "Taux de morbidité par groupe d'âge (W4, 2018)",
       x = "Groupe d'âge", y = "% malades/blessés") +
  theme_minimal(base_size = 13) +
  theme(legend.position = "none")

# IC 95% par zone géopolitique (zone)
ic_zone <- calc_ic_proportion(
  data        = data_morbid |> filter(!is.na(zone)),
  group_var   = "zone",
  success_var = "malade"
)
print(ic_zone)

p_morbid_zone <- ggplot(ic_zone,
                        aes(x = reorder(zone, taux), y = taux, fill = zone)) +
  geom_col(alpha = 0.85) +
  geom_errorbar(aes(ymin = ic_lo, ymax = ic_hi), width = 0.3, linewidth = 0.8) +
  geom_text(aes(label = paste0(round(taux, 1), "%")),
            hjust = -0.2, size = 3.8) +
  coord_flip() +
  scale_fill_manual(values = palette_zone) +
  labs(title = "Taux de morbidité par zone géopolitique (W4)",
       x = NULL, y = "% malades/blessés") +
  theme_minimal(base_size = 13) +
  theme(legend.position = "none")

p13 <- (p_morbid_sexe | p_morbid_age) / p_morbid_zone
print(p13)
sauvegarder_figure(p13, "fig13_morbidite_sexe_age_zone", width = 13, height = 10)

# =============================================================================
# TÂCHE 14 — Distribution des types de maladies (Top 10)
# =============================================================================

cat("\n=== TÂCHE 14 — Types de maladies ===\n")

top10_maladies <- sect3a_clean |>
  filter(malade == 1, !is.na(type_maladie)) |>
  count(type_maladie, sort = TRUE) |>
  slice_head(n = 10) |>
  mutate(
    type_maladie = as.character(type_maladie),
    categorie = case_when(
      grepl("malaria|fever|typhoid|cholera|diarrhea|tuberculosis|HIV|infection",
            type_maladie, ignore.case = TRUE) ~ "Infectieuse",
      grepl("accident|injury|fracture|wound|burn|trauma",
            type_maladie, ignore.case = TRUE) ~ "Traumatique",
      grepl("diabetes|hypertension|asthma|cancer|heart|chronic",
            type_maladie, ignore.case = TRUE) ~ "Chronique",
      TRUE ~ "Autre"
    ) |> factor(levels = c("Infectieuse", "Traumatique", "Chronique", "Autre")),
    proportion = n / sum(n) * 100
  )

print(top10_maladies)

p14 <- ggplot(top10_maladies,
              aes(x = reorder(type_maladie, n), y = n, fill = categorie)) +
  geom_col(alpha = 0.85) +
  geom_text(aes(label = paste0(round(proportion, 1), "%")),
            hjust = -0.15, size = 3.8) +
  coord_flip() +
  scale_fill_manual(
    values = c("Infectieuse" = "#d73027", "Traumatique" = "#fc8d59",
               "Chronique"   = "#4575b4", "Autre"       = "#91bfdb")
  ) +
  labs(title    = "Top 10 des affections déclarées — W4 (2018)",
       subtitle = "Parmi les individus ayant déclaré une maladie/blessure",
       x = NULL, y = "Nombre de cas", fill = "Catégorie") +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom")

print(p14)
sauvegarder_figure(p14, "fig14_types_maladies", width = 11, height = 7)

# =============================================================================
# TÂCHE 15 — Recours aux soins par type de prestataire
# =============================================================================

cat("\n=== TÂCHE 15 — Recours aux soins ===\n")

freq_prestataire <- sect3b_clean |>
  count(prestataire, sort = TRUE) |>
  mutate(proportion = n / sum(n) * 100)

print(freq_prestataire)
write_xlsx(freq_prestataire, "output/tables/tab15_prestataires.xlsx")

p15 <- ggplot(freq_prestataire,
              aes(x = reorder(prestataire, proportion), y = proportion,
                  fill = prestataire)) +
  geom_col(alpha = 0.85, show.legend = FALSE) +
  geom_text(aes(label = paste0(round(proportion, 1), "%")),
            hjust = -0.15, size = 4) +
  coord_flip() +
  scale_fill_brewer(palette = "Set2") +
  labs(title = "Recours aux soins par type de prestataire (W4, 2018)",
       x = NULL, y = "Proportion (%)") +
  theme_minimal(base_size = 13)

print(p15)
sauvegarder_figure(p15, "fig15_prestataires", width = 10, height = 6)

# =============================================================================
# TÂCHE 16 — Distribution des dépenses de santé
# =============================================================================

cat("\n=== TÂCHE 16 — Dépenses de santé ===\n")

depenses_pos <- data_depenses |>
  filter(!is.na(depense) & depense > 0 & is.finite(depense))

# Statistiques descriptives et déciles
stats_dep  <- stats_descriptives(depenses_pos$depense, "Dépenses santé (Naira)")
deciles_dep <- stats_par_decile(depenses_pos$depense)
print(stats_dep)
print(deciles_dep)
write_xlsx(stats_dep,   "output/tables/tab16_stats_depenses.xlsx")
write_xlsx(deciles_dep, "output/tables/tab16_deciles_depenses.xlsx")

# Histogramme échelle log
p16a <- ggplot(depenses_pos, aes(x = depense)) +
  geom_histogram(bins = 50, fill = "#4dac26", color = "white", alpha = 0.8) +
  scale_x_log10(labels = comma) +
  labs(title    = "Distribution des dépenses de santé — échelle log (W4)",
       subtitle = "Individus avec dépenses > 0",
       x = "Dépenses (Naira, log)", y = "Effectif") +
  theme_minimal(base_size = 13)

# Boxplot par type de prestataire
p16b <- ggplot(depenses_pos |> filter(!is.na(prestataire)),
               aes(x = reorder(prestataire, depense, FUN = median),
                   y = depense, fill = prestataire)) +
  geom_boxplot(alpha = 0.7, outlier.colour = "red",
               outlier.size = 1, outlier.alpha = 0.5) +
  scale_y_log10(labels = comma) +
  coord_flip() +
  scale_fill_brewer(palette = "Set2") +
  labs(title = "Dépenses de santé par prestataire (W4)",
       x = NULL, y = "Dépenses (log, Naira)") +
  theme_minimal(base_size = 13) +
  theme(legend.position = "none")

# Valeurs aberrantes IQR × 3
Q1_d       <- quantile(depenses_pos$depense, 0.25, na.rm = TRUE)
Q3_d       <- quantile(depenses_pos$depense, 0.75, na.rm = TRUE)
IQR_d      <- Q3_d - Q1_d
n_outliers <- sum(depenses_pos$depense > Q3_d + 3 * IQR_d, na.rm = TRUE)
cat("Valeurs aberrantes (IQR×3) :", n_outliers,
    "| Seuil :", round(Q3_d + 3 * IQR_d, 0), "Naira\n")

p16 <- p16a | p16b
print(p16)
sauvegarder_figure(p16, "fig16_depenses_sante", width = 14, height = 6)

# =============================================================================
# TÂCHE 17 — Recours aux soins × quintile de consommation
# =============================================================================

cat("\n=== TÂCHE 17 — Recours aux soins × quintile ===\n")

data_t17 <- data_recours |>
  filter(!is.na(quintile) & !is.na(consulte)) |>
  mutate(
    statut   = factor(consulte, levels = c(0, 1),
                      labels = c("Non consulté", "Consulté")),
    quintile = factor(quintile)
  )

tab_contingence <- table(data_t17$quintile, data_t17$statut)
cat("\nTableau de contingence recours × quintile :\n")
print(tab_contingence)
write_xlsx(as.data.frame(tab_contingence), "output/tables/tab17_contingence.xlsx")

# Chi-deux ou Fisher
min_cell <- min(tab_contingence)
if (min_cell < 5) {
  test_res <- fisher.test(tab_contingence, simulate.p.value = TRUE, B = 10000)
  cat("Test exact de Fisher | p-value =", test_res$p.value, "\n")
  label_test <- paste0("Fisher | p = ",
                       format(test_res$p.value, digits = 3))
} else {
  test_res   <- chisq.test(tab_contingence)
  cat("Chi-deux =", round(test_res$statistic, 3),
      "| df =", test_res$parameter,
      "| p-value =", test_res$p.value, "\n")
  v_cramer(test_res,
           n_total    = sum(tab_contingence),
           n_lignes   = nrow(tab_contingence),
           n_colonnes = ncol(tab_contingence))
  label_test <- paste0("Chi-deux | p = ",
                       format(test_res$p.value, digits = 3))
}

data_prop_q <- data_t17 |>
  group_by(quintile, statut) |>
  summarise(n = n(), .groups = "drop") |>
  group_by(quintile) |>
  mutate(proportion = n / sum(n) * 100)

p17 <- ggplot(data_prop_q, aes(x = quintile, y = proportion, fill = statut)) +
  geom_col(position = "fill", alpha = 0.85) +
  geom_text(aes(label = paste0(round(proportion, 1), "%")),
            position = position_fill(vjust = 0.5),
            size = 4, color = "white", fontface = "bold") +
  scale_y_continuous(labels = percent) +
  scale_fill_manual(
    values = c("Non consulté" = "#d73027", "Consulté" = "#1a9850")
  ) +
  labs(title    = "Recours aux soins selon le quintile de consommation (W4)",
       subtitle = label_test,
       x    = "Quintile de consommation (Q1 = plus pauvres)",
       y    = "Proportion",
       fill = "Statut") +
  theme_minimal(base_size = 13)

print(p17)
sauvegarder_figure(p17, "fig17_recours_quintile", width = 10, height = 6)

# =============================================================================
# TÂCHE 18 — Dépenses médianes : Nord vs Sud (zone1)
# =============================================================================

cat("\n=== TÂCHE 18 — Dépenses santé Nord vs Sud ===\n")

data_t18 <- depenses_pos |>
  filter(!is.na(zone1) & is.finite(depense))

# Statistiques par zone1
stats_zone1 <- data_t18 |>
  group_by(zone1) |>
  summarise(
    n       = n(),
    mediane = median(depense, na.rm = TRUE),
    moyenne = round(mean(depense, na.rm = TRUE), 0),
    Q1      = quantile(depense, 0.25, na.rm = TRUE),
    Q3      = quantile(depense, 0.75, na.rm = TRUE)
  )
print(stats_zone1)
write_xlsx(stats_zone1, "output/tables/tab18_depenses_zone1.xlsx")

# Test de Wilcoxon Nord vs Sud
wilcox_zone1 <- wilcox.test(depense ~ zone1, data = data_t18, exact = FALSE)
cat("Wilcoxon W =", wilcox_zone1$statistic,
    "| p-value =", wilcox_zone1$p.value, "\n")

n_nord <- sum(data_t18$zone1 == "Nord", na.rm = TRUE)
n_sud  <- sum(data_t18$zone1 == "Sud",  na.rm = TRUE)
effet_wilcoxon_r(wilcox_zone1, n_nord, n_sud)

# Violin plot + boxplot superposé (Nord vs Sud)
p18a <- ggplot(data_t18, aes(x = zone1, y = depense, fill = zone1)) +
  geom_violin(alpha = 0.55, trim = TRUE, color = NA) +
  geom_boxplot(width = 0.12, fill = "white",
               outlier.colour = "red", outlier.size = 0.8,
               outlier.alpha  = 0.5) +
  scale_y_log10(labels = comma) +
  scale_fill_manual(values = palette_zone1) +
  labs(
    title    = "Dépenses de santé : Nord vs Sud (W4, 2018)",
    subtitle = paste0("Test de Wilcoxon | p = ",
                      format(wilcox_zone1$p.value, digits = 3)),
    x = NULL, y = "Dépenses (Naira, log)", fill = "Zone"
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "none")

# Violin plot par zone géopolitique (6 zones)
p18b <- ggplot(depenses_pos |> filter(!is.na(zone1)),
               aes(x = zone1, y = depense, fill = zone1)) +
  geom_violin(alpha = 0.55, trim = TRUE, color = NA) +
  geom_boxplot(width = 0.1, fill = "white",
               outlier.colour = "red", outlier.size = 0.6,
               outlier.alpha  = 0.4) +
  scale_y_log10(labels = comma) +
  scale_fill_manual(values = palette_zone) +
  labs(
    title = "Dépenses de santé par zone géopolitique (W4, 2018)",
    x = NULL, y = "Dépenses (Naira, log)"
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position  = "none",
        axis.text.x      = element_text(angle = 30, hjust = 1))

p18 <- p18a | p18b
print(p18)
sauvegarder_figure(p18, "fig18_depenses_zone", width = 14, height = 7)

# =============================================================================
# TABLEAU RÉCAPITULATIF gtsummary stratifié par zone1 (Nord/Sud)
# =============================================================================

data_tab <- data_morbid |>
  left_join(
    data_depenses |>
      select(hhid, depense, prestataire, quintile) |>
      distinct(hhid, .keep_all = TRUE),
    by = "hhid"
  ) |>
  filter(!is.na(zone1)) |>
  select(malade, sexe, groupe_age, depense, zone1)

tableau_sante <- data_tab |>
  mutate(malade = factor(malade, levels = c(0, 1), labels = c("Non", "Oui"))) |>
  tbl_summary(
    by        = zone1,
    label     = list(
      malade     ~ "Maladie/blessure déclarée",
      sexe       ~ "Sexe",
      groupe_age ~ "Groupe d'âge",
      depense    ~ "Dépenses de santé (Naira)"
    ),
    statistic = list(
      all_continuous()  ~ "{median} [{p25}, {p75}]",
      all_categorical() ~ "{n} ({p}%)"
    ),
    missing = "ifany"
  ) |>
  add_p()       |>
  add_overall() |>
  bold_labels() |>
  modify_caption("**Tableau TP3 — Indicateurs de santé Nord vs Sud (W4)**")

print(tableau_sante)
sauvegarder_tableau(tableau_sante, "tableau_sante_zone1")

# =============================================================================
# FIGURE DE SYNTHÈSE
# =============================================================================

p_synthese <- (p_morbid_sexe | p_morbid_age) /
  p14 /
  (p15 | p18a) /
  p17 +
  plot_annotation(
    title    = "TP3 — Accès aux soins et dépenses de santé (GHS Nigeria W4, 2018)",
    subtitle = "Sources : sect3a/3b_harvestw4.dta + totcons_final.dta | LSMS-ISA Banque Mondiale",
    theme    = theme(plot.title = element_text(face = "bold", size = 14))
  )

sauvegarder_figure(p_synthese, "fig00_synthese_tp3", width = 16, height = 22)

