# =============================================================================
# Analyses statistiques et visualisations
# TP1 : Profil démographique des ménages nigérians
# GHS Panel Nigeria | Wave 4 | ENSAE ISE 1 — 2025-2026
# =============================================================================

library(dplyr)
library(ggplot2)
library(apyramid)
library(naniar)
library(gtsummary)
library(rstatix)
library(patchwork)
library(writexl)
library(moments)

source("R/fonctions.R")

# --- Chargement des données nettoyées ----------------------------------------
data_tp1      <- readRDS("data/processed/data_tp1_clean.rds")
taille_menage <- readRDS("data/processed/taille_menage.rds")

# =============================================================================
# TÂCHE 1 — Exploration et valeurs manquantes
# =============================================================================

cat("=== Structure du fichier ===\n")
str(data_tp1)
glimpse(data_tp1)
statistic <- data_tp1 |>
  select(age, groupe_age, taille_menage, ) 
summary(statistic)

# Visualisation des valeurs manquantes
vis_miss(data_tp1, warn_large_data = FALSE)
# Les valeurs manquantes dans la suite seras ignorer

# Vérification doublons hhid + indiv_id
doublons <- data_tp1 |>
  group_by(hhid, indiv) |>
  filter(n() > 1)
cat("Nombre de doublons :", nrow(doublons), "\n")

# =============================================================================
# TÂCHE 2 — Analyse univariée de l'âge
# =============================================================================

# Statistiques descriptives
stats_age <- stats_descriptives(data_tp1$age, "Âge")
print(stats_age)
# Exporter sous Excel
write_xlsx(stats_age, "output/tables/stats_age.xlsx")

# Histogramme (binwidth = 5)
p_hist <- ggplot(data_tp1[!is.na(data_tp1$age), ], aes(x = age)) +
  geom_histogram(binwidth = 5, fill = "#2166ac", color = "white", alpha = 0.85) +
  labs(
    title    = "Distribution de l'âge des membres des ménages (W4, 2018)",
    subtitle = "GHS Panel Nigeria — Post-Harvest Wave 4",
    x        = "Âge (années)",
    y        = "Effectif"
  ) +
  theme_minimal(base_size = 13)
p_hist

# Boîte à moustaches
p_box <- ggplot(data_tp1[!is.na(data_tp1$age), ], aes(y = age)) +
  geom_boxplot(fill = "#4dac26", alpha = 0.7,
               outlier.colour = "red", outlier.size = 1.5) +
  labs(title = "Boxplot de l'âge (W4)", y = "Âge (années)") +
  theme_minimal(base_size = 13)
p_box
sauvegarder_figure(p_hist | p_box, "fig1_age_distribution", width = 12, height = 5)

# Test de normalité Shapiro-Wilk (échantillon aléatoire si n > 5000)
set.seed(42)
n_dispo     <- sum(!is.na(data_tp1$age))
age_sample  <- sample(na.omit(data_tp1$age), min(5000, n_dispo))
shapiro_res <- shapiro.test(age_sample)

cat("\n--- Test de Shapiro-Wilk ---\n")
cat("W =", round(shapiro_res$statistic, 4), "| p-value =", shapiro_res$p.value, "\n")
cat("→", ifelse(shapiro_res$p.value < 0.05,
                "Distribution NON normale (p < 0.05)",
                "Distribution normale (p ≥ 0.05)"), "\n")

# =============================================================================
# TÂCHE 3 — Pyramide des âges par sexe (W4)
# =============================================================================

data_pyramide <- data_tp1 |>
  filter(!is.na(sexe), !is.na(groupe_age))

p_pyramide <- age_pyramid(
  data             = data_pyramide,
  age_group        = "groupe_age",
  split_by         = "sexe",
  #show.axis.titles = TRUE
) +
  labs(
    title    = "Pyramide des âges — GHS Nigeria Wave 4 (2018)",
    subtitle = "Groupes quinquennaux | Post-Harvest",
    x        = "Effectif",
    y        = "Groupe d'âge",
    fill     = "Sexe"
  ) +
  theme_minimal(base_size = 13)

print(p_pyramide)
sauvegarder_figure(p_pyramide, "fig2_pyramide_ages", width = 9, height = 11)

# =============================================================================
# TÂCHE 4 — Lien de parenté : fréquences + IC à 95%
# =============================================================================

freq_lien <- data_tp1 |>
  count(lien_parente) |>
  filter(!is.na(lien_parente)) |>
  mutate(proportion = n / sum(n) * 100) |>
  arrange(desc(n))

freq_lien

# IC à 95% via binom.test pour chaque modalité
total_n   <- sum(freq_lien$n)
freq_lien <- freq_lien |>
  rowwise() |>
  mutate(
    ic_lo = binom.test(n, total_n)$conf.int[1] * 100,
    ic_hi = binom.test(n, total_n)$conf.int[2] * 100
  ) |>
  ungroup()

print(freq_lien)
# Exporter sous Excel
write_xlsx(freq_lien, "output/tables/lien_parente_ic95.xlsx")

# Barplot horizontal ordonné
p_lien <- ggplot(freq_lien, aes(x = reorder(lien_parente, proportion), y = proportion)) +
  geom_col(fill = "#d7191c", alpha = 0.8) +
  geom_errorbar(aes(ymin = ic_lo, ymax = ic_hi), width = 0.3, color = "black") +
  geom_text(aes(label = paste0(round(proportion, 1), "%")),
            hjust = -0.2, size = 4) +
  coord_flip() +
  labs(
    title = "Lien de parenté avec le chef de ménage — IC 95% (W4)",
    x     = NULL,
    y     = "Proportion (%)"
  ) +
  theme_minimal(base_size = 13)

print(p_lien)
sauvegarder_figure(p_lien, "fig3_lien_parente", width = 9, height = 5)

# =============================================================================
# TÂCHE 5 — Taille des ménages : nord et sud + test Wilcoxon (ici  zones n'est pas rurales ou urbaines)
# =============================================================================

taille_zone <- taille_menage |>
  left_join(data_tp1 |> distinct(hhid, zone), by = "hhid") |>
  filter(!is.na(zone))

taille_zone1 <- taille_menage |>
  left_join(data_tp1 |> distinct(hhid, zone1), by = "hhid") |>
  filter(!is.na(zone1))

# Boxplot groupé
p_taille <- ggplot(taille_zone, aes(x = zone, y = taille_menage, fill = zone)) +
  geom_boxplot(alpha = 0.75,
               outlier.colour = "red", outlier.size = 1.5) +
  scale_fill_manual(values = c("Centre-Nord" = "yellow", "Nord-Est" = "blue", "Nord-Ouest" = "#762a83", "Sud-Est"= "green", "Sud-Sud" = "#102a03", "Sud-Ouest" = "#1b7837")) +
  labs(
    title = "Taille des ménages selon la zone de résidence (W4)",
    x     = NULL,
    y     = "Nombre de membres"
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "none")

p_taille1 <- ggplot(taille_zone1, aes(x = zone1, y = taille_menage, fill = zone1)) +
  geom_boxplot(alpha = 0.75,
               outlier.colour = "red", outlier.size = 1.5) +
  scale_fill_manual(values = c("Nord" = "blue", "Sud"= "green")) +
  labs(
    title = "Taille des ménages selon la zone de résidence (W4)",
    x     = NULL,
    y     = "Nombre de membres"
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "none")

print(p_taille)
sauvegarder_figure(p_taille, "fig4_taille_zone", width = 7, height = 6)
print(p_taille1)
sauvegarder_figure(p_taille1, "fig4_taille_zone1", width = 7, height = 6)

# Test de Wilcoxon-Mann-Whitney
# H0 :les distributions des deux groupes sont identiques
wilcox_res <- wilcox.test(taille_menage ~ zone1, data = taille_zone1, exact = FALSE)
cat("\n--- Test de Wilcoxon (taille ménage par zone) ---\n")
cat("W =", wilcox_res$statistic, "| p-value =", wilcox_res$p.value, "\n")

# Taille d'effet r de rang
n_Nord  <- sum(taille_zone1$zone1 == "Nord",  na.rm = TRUE)
n_Sud <- sum(taille_zone1$zone1 == "Sud", na.rm = TRUE)

effet_wilcoxon_r(wilcox_res, n_Nord, n_Sud)

# =============================================================================
# TÂCHE 6 — Tableau gtsummary stratifié par zone
# =============================================================================

tableau_demo <- data_tp1 |>
  select(age, sexe, taille_menage, zone) |>
  filter(!is.na(zone)) |>
  tbl_summary(
    by        = zone,
    label     = list(
      age           ~ "Âge (années)",
      sexe          ~ "Sexe",
      taille_menage ~ "Taille du ménage"
    ),
    statistic = list(
      all_continuous()  ~ "{mean} ({sd}) | médiane : {median} [{p25}, {p75}]",
      all_categorical() ~ "{n} ({p}%)"
    ),
    missing   = "ifany" # Afficher les valleurs manquantes
  ) |>
  add_p()        |> # Ajouter les p-values
  add_overall()  |> # Ajouter la colonne overall
  bold_labels()  |> #Met les noms des variables en gras pour plus de lisibilité
  modify_caption("**Tableau 1 — Statistiques démographiques par zone (W4)**")

print(tableau_demo)
sauvegarder_tableau(tableau_demo, "tableau1_demo_zone")

# =============================================================================
# FIGURE DE SYNTHÈSE (patchwork)
# =============================================================================

p_synthese <- (p_hist | p_box) /
  (p_lien | p_taille) /
  p_pyramide +
  plot_annotation(
    title    = "TP1 — Profil démographique des ménages nigérians (GHS W4, 2018)",
    subtitle = "Sources : sect1_harvestw4.dta + secta_harvestw4.dta | LSMS-ISA Banque Mondiale",
    theme    = theme(plot.title = element_text(face = "bold", size = 15))
  )

sauvegarder_figure(p_synthese, "fig0_synthese_tp1", width = 16, height = 18)

