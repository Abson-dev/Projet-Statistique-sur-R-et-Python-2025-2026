# ================================================================
# PROJET ENSAE ISE 1 — GHS Nigeria Panel (W4)
# SCRIPT   : scripts/01_tache19_superficie.R
# TÂCHE 19 : Construction et description de la variable superficie
# BASE     : data/processed/processed_tp4_w4.rds
# SORTIES  : outputs/figures/t19_*.png | outputs/tables/t19_*.csv
# ================================================================

source("scripts/functions.R")

library(dplyr)
library(ggplot2)
library(scales)

donnees <- readRDS("data/processed/processed_tp4_w4.rds")


# ================================================================
# ÉTAPE 1 — VÉRIFICATION DE L'UNITÉ DE MESURE GPS
# Avant toute analyse, on valide que les superficies sont bien
# en m² dans les bases sources (hypothèse de travail).
# Un plot médian de 2 000-3 000 m² (~0.2-0.3 ha) est cohérent
# avec la petite agriculture familiale nigériane.
# ================================================================

cat("=== Vérification de l'unité ===\n")
cat("Distribution de superficie_gps_prefill (brute, en m²) :\n")
summary(donnees$superficie_gps_prefill)

cat("\nDistribution de superficie_ha (après conversion / 10 000) :\n")
summary(donnees$superficie_ha)

# Un hectare = 10 000 m². Si la médiane est ~0.25 ha,
# la valeur brute devrait être ~2 500 m². C'est le test de cohérence.
mediane_brute   <- median(donnees$superficie_gps_prefill, na.rm = TRUE)
mediane_ha      <- median(donnees$superficie_ha, na.rm = TRUE)
cat(sprintf("\nMédiane brute : %.0f  |  Médiane en ha : %.4f\n",
            mediane_brute, mediane_ha))
cat(sprintf("Ratio : %.0f (doit être proche de 10 000)\n",
            mediane_brute / mediane_ha))


# ================================================================
# ÉTAPE 2 — TRAITEMENT DES VALEURS ABERRANTES
# On identifie les parcelles > P99.5 et < P0.5 comme extrêmes.
# On les conserve (pas de suppression) mais on les signale.
# Le winsorizing n'est pas appliqué car on travaille en log.
# ================================================================

# Seuils d'aberrance (niveau parcelle)
p005 <- quantile(donnees$superficie_ha, 0.005, na.rm = TRUE)
p995 <- quantile(donnees$superficie_ha, 0.995, na.rm = TRUE)

n_inf <- sum(donnees$superficie_ha < p005, na.rm = TRUE)
n_sup <- sum(donnees$superficie_ha > p995, na.rm = TRUE)

cat(sprintf("\n--- Valeurs aberrantes ---\n"))
cat(sprintf("Seuil bas  P0.5  : %.4f ha → %d parcelles\n", p005, n_inf))
cat(sprintf("Seuil haut P99.5 : %.4f ha → %d parcelles\n", p995, n_sup))

# Ajout d'un indicateur (utile pour les graphiques)
donnees <- donnees %>%
  mutate(aberrant = superficie_ha < p005 | superficie_ha > p995)


# ================================================================
# ÉTAPE 3 — STATISTIQUES DESCRIPTIVES PAR PARCELLE
# ================================================================

stats_parcelle <- donnees %>%
  filter(!is.na(superficie_ha)) %>%
  summarise(
    n        = n(),
    moyenne  = mean(superficie_ha),
    mediane  = median(superficie_ha),
    ecart_type = sd(superficie_ha),
    cv       = sd(superficie_ha) / mean(superficie_ha) * 100,
    min      = min(superficie_ha),
    p10      = quantile(superficie_ha, 0.10),
    p25      = quantile(superficie_ha, 0.25),
    p75      = quantile(superficie_ha, 0.75),
    p90      = quantile(superficie_ha, 0.90),
    max      = max(superficie_ha)
  ) %>%
  mutate(across(where(is.numeric), ~round(., 4)))

cat("\n=== Stats descriptives — niveau parcelle ===\n")
print(stats_parcelle)

sauvegarder_tableau(stats_parcelle, "t19_stats_parcelle.csv")


# ================================================================
# ÉTAPE 4 — RÉPARTITION DES SOURCES DE SUPERFICIE
# ================================================================

repartition_sources <- donnees %>%
  count(source_superficie) %>%
  mutate(pct = round(n / sum(n) * 100, 1)) %>%
  arrange(desc(n))

cat("\n=== Répartition des sources ===\n")
print(repartition_sources)

sauvegarder_tableau(repartition_sources, "t19_sources_superficie.csv")


# ================================================================
# ÉTAPE 5 — AGRÉGATION AU NIVEAU MÉNAGE
# On additionne les superficies de toutes les parcelles d'un ménage.
# Un ménage est retenu s'il a au moins une parcelle avec superficie.
# ================================================================

superficie_menage <- donnees %>%
  group_by(identifiant_menage) %>%
  summarise(
    nb_parcelles       = n(),
    nb_parcelles_val   = sum(!is.na(superficie_ha)),
    superficie_totale  = sum(superficie_ha, na.rm = TRUE),
    poids_vague4       = first(poids_vague4),
    milieu_label       = first(milieu_label),
    nom_etat           = first(nom_etat),
    .groups = "drop"
  ) %>%
  # On ne conserve que les ménages avec au moins une superficie valide
  filter(nb_parcelles_val > 0) %>%
  mutate(superficie_totale = ifelse(superficie_totale == 0,
                                    NA_real_,
                                    superficie_totale))

cat(sprintf("\nMénages avec au moins une superficie : %d\n", nrow(superficie_menage)))

# Sauvegarder pour les scripts suivants
saveRDS(superficie_menage, "data/processed/superficie_menage.rds")


# ================================================================
# ÉTAPE 6 — STATISTIQUES PAR DÉCILE (niveau ménage)
# ================================================================

superficie_menage <- superficie_menage %>%
  mutate(decile = ntile(superficie_totale, 10))

tableau_deciles <- superficie_menage %>%
  group_by(decile) %>%
  summarise(
    n         = n(),
    min_ha    = round(min(superficie_totale,    na.rm = TRUE), 3),
    max_ha    = round(max(superficie_totale,    na.rm = TRUE), 3),
    mediane   = round(median(superficie_totale, na.rm = TRUE), 3),
    moyenne   = round(mean(superficie_totale,   na.rm = TRUE), 3),
    .groups   = "drop"
  )

cat("\n=== Superficie totale par décile (ménage) ===\n")
print(tableau_deciles)

sauvegarder_tableau(tableau_deciles, "t19_deciles_menage.csv")


# ================================================================
# ÉTAPE 7 — FIGURES
# ================================================================

# --- 7.1 Histogramme (niveau parcelle, échelle log) ---
mediane_par <- median(donnees$superficie_ha, na.rm = TRUE)

p1 <- donnees %>%
  filter(!is.na(superficie_ha), superficie_ha > 0) %>%
  ggplot(aes(x = superficie_ha)) +
  geom_histogram(bins = 60, fill = "#2171B5", color = "white", alpha = 0.85) +
  geom_vline(xintercept = mediane_par, color = "#D73027",
             linewidth = 0.9, linetype = "dashed") +
  annotate("text",
           x = mediane_par * 1.3,
           y = Inf, vjust = 2,
           label = sprintf("Médiane\n%.3f ha", mediane_par),
           color = "#D73027", size = 3.5, hjust = 0) +
  scale_x_log10(labels = label_number(accuracy = 0.01)) +
  labs(
    title    = "Distribution de la superficie des parcelles (échelle log)",
    subtitle = "GHS Nigeria W4 — toutes parcelles avec superficie GPS",
    x        = "Superficie (ha) — échelle logarithmique",
    y        = "Nombre de parcelles",
    caption  = sprintf("n = %d parcelles | source_superficie = GPS",
                       sum(!is.na(donnees$superficie_ha)))
  ) +
  theme_tp4()

print(p1)
sauvegarder_figure("t19_histo_parcelle_log.png")


# --- 7.2 Barplot des sources ---
p2 <- repartition_sources %>%
  filter(source_superficie != "aucune") %>%
  mutate(source_superficie = reorder(source_superficie, pct)) %>%
  ggplot(aes(x = source_superficie, y = pct, fill = source_superficie)) +
  geom_col(width = 0.6, show.legend = FALSE) +
  geom_text(aes(label = paste0(pct, "%")),
            hjust = -0.15, size = 3.8) +
  coord_flip() +
  scale_y_continuous(limits = c(0, 100),
                     labels = function(x) paste0(x, "%")) +
  scale_fill_manual(values = c(
    "gps_recolte"  = "#2171B5",
    "gps_semis"    = "#6BAED6",
    "gps_prefill"  = "#BDD7E7"
  )) +
  labs(
    title    = "Source de la superficie utilisée par parcelle",
    subtitle = "Priorité : GPS récolte > GPS semis > GPS prérempli",
    x        = NULL, y = "Part des parcelles (%)",
    caption  = "GHS Nigeria W4"
  ) +
  theme_tp4()

print(p2)
sauvegarder_figure("t19_sources_superficie.png")


# --- 7.3 Histogramme superficie totale ménage (échelle log) ---
mediane_men <- median(superficie_menage$superficie_totale, na.rm = TRUE)

p3 <- superficie_menage %>%
  filter(!is.na(superficie_totale), superficie_totale > 0) %>%
  ggplot(aes(x = superficie_totale)) +
  geom_histogram(bins = 50, fill = "#238B45", color = "white", alpha = 0.85) +
  geom_vline(xintercept = mediane_men, color = "#D73027",
             linewidth = 0.9, linetype = "dashed") +
  annotate("text",
           x = mediane_men * 1.3,
           y = Inf, vjust = 2,
           label = sprintf("Médiane\n%.3f ha", mediane_men),
           color = "#D73027", size = 3.5, hjust = 0) +
  scale_x_log10(labels = label_number(accuracy = 0.01)) +
  labs(
    title    = "Distribution de la superficie totale par ménage (échelle log)",
    subtitle = "GHS Nigeria W4 — ménages avec au moins une parcelle mesurée",
    x        = "Superficie totale (ha) — échelle logarithmique",
    y        = "Nombre de ménages",
    caption  = sprintf("n = %d ménages", nrow(superficie_menage))
  ) +
  theme_tp4()

print(p3)
sauvegarder_figure("t19_histo_menage_log.png")

cat("\n✔ Tâche 19 terminée.\n")
