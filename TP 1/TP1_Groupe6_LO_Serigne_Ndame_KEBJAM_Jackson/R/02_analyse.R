# =============================================================================
# Analyses statistiques et visualisations
# TP1 : Profil démographique des ménages nigérians
# GHS Panel Nigeria | Wave 4 | ENSAE ISE 1 — 2025-2026
# =============================================================================

# =============================================================================
# TÂCHE 1 — Exploration et valeurs manquantes
# =============================================================================

# Visualisation des valeurs manquantes
vis_miss(data_tp1, warn_large_data = FALSE)

# Vérification doublons hhid + indiv_id
doublons <- data_tp1 |>
  group_by(hhid, indiv) |>
  filter(n() > 1)
cat("Nombre de doublons :", nrow(doublons), "\n")

# =============================================================================
# TÂCHE 2 — Analyse univariée de l'âge (avec pondération)
# =============================================================================

# Statistiques descriptives pondérées
stats_age <- stats_descriptives(data_tp1$age, data_tp1$poids_individu, "Âge")
knitr::kable(stats_age, 
             caption = "Tableau 2 — Statistiques descriptives de l'âge des membres (pondéré)",
             digits = 2)

# Histogramme pondéré (binwidth = 5)
p_hist <- ggplot(data_tp1[!is.na(data_tp1$age), ], 
                 aes(x = age, weight = poids_individu)) +
  geom_histogram(binwidth = 5, fill = "#2166ac", color = "white", alpha = 0.85) +
  labs(
    title = "Distribution de l'âge des membres des ménages (W4, 2018)",
    x = "Âge (années)", y = "Effectif pondéré"
  ) +
  theme_minimal(base_size = 13)
p_hist

# Boîte à moustaches pondérée
p_box <- ggplot(data_tp1[!is.na(data_tp1$age), ], 
                aes(y = age, weight = poids_individu)) +
  geom_boxplot(fill = "#4dac26", alpha = 0.7,
               outlier.colour = "red", outlier.size = 1.5) +
  labs(title = "Boxplot de l'âge (W4)", y = "Âge (années)") +
  theme_minimal(base_size = 13)
p_box

# Test de normalité Shapiro-Wilk (échantillon aléatoire)
set.seed(42)
n_dispo <- sum(!is.na(data_tp1$age))
age_sample <- sample(na.omit(data_tp1$age), min(5000, n_dispo))
shapiro_res <- shapiro.test(age_sample)

cat("\n--- Test de Shapiro-Wilk ---\n")
cat("W =", round(shapiro_res$statistic, 4), 
    "| p-value =", format(shapiro_res$p.value, scientific = TRUE, digits = 3), "\n")

# =============================================================================
# TÂCHE 3 — Pyramide des âges par sexe (W4)
# =============================================================================

data_pyramide <- data_tp1 |>
  filter(!is.na(sexe), !is.na(groupe_age))

age_pyramid(
  data = data_pyramide,
  age_group = "groupe_age",
  split_by = "sexe"
) +
  labs(
    title = "Pyramide des âges — GHS Nigeria Wave 4 (2018)",
    x = "Effectif", y = "Groupe d'âge", fill = "Sexe"
  ) +
  theme_minimal(base_size = 13)

# =============================================================================
# TÂCHE 4 — Lien de parenté : fréquences pondérées + IC à 95%
# =============================================================================

# Calcul des proportions pondérées
freq_lien <- data_tp1 |>
  filter(!is.na(lien_parente)) |>
  group_by(lien_parente) |>
  summarise(
    effectif_pondere = sum(poids_individu, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(
    proportion = effectif_pondere / sum(effectif_pondere) * 100,
    se = sqrt(proportion/100 * (1 - proportion/100) / sum(effectif_pondere)),
    ic_lo = (proportion - 1.96 * se * 100) |> pmax(0),
    ic_hi = (proportion + 1.96 * se * 100) |> pmin(100)
  ) |>
  arrange(desc(proportion))

# Tableau
knitr::kable(
  freq_lien |>
    mutate(
      effectif_pondere = round(effectif_pondere, 0),
      proportion = round(proportion, 1),
      ic_lo = round(ic_lo, 1),
      ic_hi = round(ic_hi, 1)
    ) |>
    select(lien_parente, effectif_pondere, proportion, ic_lo, ic_hi),
  col.names = c("Lien de parenté", "Effectif pondéré", "Proportion (%)", 
                "IC 95% inf.", "IC 95% sup."),
  caption = "Tableau 3 — Répartition des membres par lien de parenté (pondéré)"
)

# Barplot horizontal
ggplot(freq_lien, aes(x = reorder(lien_parente, proportion), y = proportion)) +
  geom_col(fill = "#d7191c", alpha = 0.8) +
  geom_errorbar(aes(ymin = ic_lo, ymax = ic_hi), width = 0.2, color = "black") +
  geom_text(aes(label = paste0(round(proportion, 1), "%")),
            hjust = -0.2, size = 4) +
  coord_flip() +
  labs(
    title = "Lien de parenté avec le chef de ménage — IC 95% (pondéré)",
    x = NULL, y = "Proportion (%)"
  ) +
  theme_minimal(base_size = 13)

# =============================================================================
# TÂCHE 5 — Taille des ménages : Nord/Sud + test Wilcoxon
# =============================================================================

# Création des jeux de données
taille_zone <- taille_menage |>
  left_join(data_tp1 |> distinct(hhid, zone, poids_menage), by = "hhid") |>
  filter(!is.na(zone))

taille_zone1 <- taille_menage |>
  left_join(data_tp1 |> distinct(hhid, zone1, poids_menage), by = "hhid") |>
  filter(!is.na(zone1))

# Boxplot par zone géopolitique (6 zones)
ggplot(taille_zone, aes(x = zone, y = taille_menage, fill = zone)) +
  geom_boxplot(alpha = 0.75, outlier.colour = "red", outlier.size = 1.5) +
  scale_fill_manual(values = c("Centre-Nord" = "#a6cee3", "Nord-Est" = "#1f78b4",
                               "Nord-Ouest" = "#08519c", "Sud-Est" = "#f4a582",
                               "Sud-Sud" = "#d6604d", "Sud-Ouest" = "#b2182b")) +
  labs(title = "Taille des ménages par zone géopolitique (W4)",
       x = NULL, y = "Nombre de membres") +
  theme_minimal(base_size = 13) +
  theme(legend.position = "none")

# Boxplot par zone regroupée (Rural/Urban)
ggplot(taille_zone1, aes(x = zone1, y = taille_menage, fill = zone1)) +
  geom_boxplot(alpha = 0.75, outlier.colour = "red", outlier.size = 1.5) +
  scale_fill_manual(values = c("Rural" = "#1b7837", "Urban" = "#762a83")) +
  labs(title = "Taille des ménages par zone (Rural/Urban)",
       x = NULL, y = "Nombre de membres") +
  theme_minimal(base_size = 13) +
  theme(legend.position = "none")

# Test de Wilcoxon-Mann-Whitney
wilcox_res <- wilcox.test(taille_menage ~ zone1, data = taille_zone1, exact = FALSE)

n_rural <- sum(taille_zone1$zone1 == "Rural", na.rm = TRUE)
n_urban <- sum(taille_zone1$zone1 == "Urban", na.rm = TRUE)

# Calcul correct de la taille d'effet r (toujours positif)
Z <- qnorm(wilcox_res$p.value / 2)
r_effet <- abs(Z) / sqrt(n_rural + n_urban)

cat("\n--- Test de Wilcoxon (taille ménage par zone) ---\n")
cat("W =", wilcox_res$statistic, "| p-value =", format(wilcox_res$p.value, scientific = TRUE, digits = 3), "\n")
cat("Taille d'effet r =", round(r_effet, 3), "→", 
    ifelse(r_effet < 0.1, "négligeable",
           ifelse(r_effet < 0.3, "petit",
                  ifelse(r_effet < 0.5, "moyen", "grand"))), "\n")

# =============================================================================
# TÂCHE 6 — Tableau gtsummary stratifié par zone (pondéré)
# =============================================================================

# Préparation des données
data_tableau <- data_tp1 |>
  mutate(
    zone = zone1,
    sexe = factor(sexe, levels = c("Homme", "Femme")),
    taille_menage = as.numeric(taille_menage)
  ) |>
  filter(
    !is.na(zone), !is.na(age), !is.na(sexe),
    !is.na(taille_menage), !is.na(poids_individu)
  )

# Création du design d'enquête
design_individus <- svydesign(
  ids = ~hhid,
  weights = ~poids_individu,
  data = data_tableau,
  nest = TRUE
)

# Tableau pondéré
tbl_svysummary(
  design_individus,
  by = zone,
  include = c(age, sexe, taille_menage),
  label = list(
    age ~ "Âge (années)",
    sexe ~ "Sexe",
    taille_menage ~ "Taille du ménage"
  ),
  statistic = list(
    all_continuous() ~ "{mean} ({sd}) | médiane : {median} [{p25}, {p75}]",
    all_categorical() ~ "{n} ({p}%)"
  ),
  missing = "ifany"
) |>
  add_p() |>
  add_overall() |>
  bold_labels() |>
  modify_caption("**Tableau 1 — Statistiques démographiques par zone (pondéré)**")