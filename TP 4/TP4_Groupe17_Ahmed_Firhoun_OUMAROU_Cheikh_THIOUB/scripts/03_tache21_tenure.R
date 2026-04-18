# ================================================================
# PROJET ENSAE ISE 1 — GHS Nigeria Panel (W4)
# SCRIPT   : scripts/03_tache21_tenure.R
# TÂCHE 21 : Régime de tenure — fréquences, barplot, chi-deux rural/urbain
# BASE     : data/processed/processed_tp4_w4.rds
# SORTIES  : outputs/figures/t21_*.png | outputs/tables/t21_*.csv
# ================================================================

source("scripts/functions.R")

library(dplyr)
library(ggplot2)

donnees <- readRDS("data/processed/processed_tp4_w4.rds")


# ================================================================
# ÉTAPE 1 — RECODAGE DU RÉGIME DE TENURE
# Variable source : acquisition_mode (s11b1q4 dans sect11b1)
# 7 codes → 6 catégories analytiques
# ================================================================

donnees <- donnees %>%
  mutate(
    # Regroupement : codes 4 (Don communautaire) et 7 (Échange temporaire)
    # sont regroupés dans "Autre" pour simplifier le graphique
    tenure_6 = dplyr::case_when(
      acquisition_mode == 1 ~ "Propriété pleine",
      acquisition_mode == 2 ~ "Location",
      acquisition_mode == 3 ~ "Prêt",
      acquisition_mode == 5 ~ "Héritage",
      acquisition_mode == 6 ~ "Métayage",
      acquisition_mode %in% c(4, 7) ~ "Autre",
      TRUE ~ NA_character_
    ),
    # Facteur ordonné par fréquence (pour le barplot trié)
    tenure_6 = factor(tenure_6,
                      levels = c("Héritage", "Propriété pleine", "Location",
                                 "Prêt", "Autre", "Métayage"))
  )

# Vérification
cat("Répartition des modalités de tenure_6 :\n")
table(donnees$tenure_6, useNA = "ifany")


# ================================================================
# ÉTAPE 2 — TABLEAU DES FRÉQUENCES (brut et pondéré)
# ================================================================

# Brut
freq_brut <- donnees %>%
  filter(!is.na(tenure_6)) %>%
  count(tenure_6, name = "n_brut") %>%
  mutate(pct_brut = round(n_brut / sum(n_brut) * 100, 1))

# Pondéré : on somme les poids par modalité
freq_pond <- donnees %>%
  filter(!is.na(tenure_6), !is.na(poids_vague4)) %>%
  group_by(tenure_6) %>%
  summarise(somme_poids = sum(poids_vague4), .groups = "drop") %>%
  mutate(pct_pond = round(somme_poids / sum(somme_poids) * 100, 1))

tableau_freq <- left_join(freq_brut, freq_pond, by = "tenure_6") %>%
  select(tenure_6, n_brut, pct_brut, pct_pond)

cat("\n=== Fréquences et proportions ===\n")
print(tableau_freq)
sauvegarder_tableau(tableau_freq, "t21_frequences_tenure.csv")


# ================================================================
# ÉTAPE 3 — BARPLOT HORIZONTAL TRIÉ
# ================================================================

# Palette de 6 couleurs distinctes
palette_tenure <- c(
  "Héritage"         = "#1D6996",
  "Propriété pleine" = "#0F8554",
  "Location"         = "#E17C05",
  "Prêt"             = "#CC503E",
  "Autre"            = "#94346E",
  "Métayage"         = "#6F4070"
)

p_barplot <- tableau_freq %>%
  mutate(tenure_6 = reorder(tenure_6, pct_brut)) %>%
  ggplot(aes(x = tenure_6, y = pct_brut, fill = tenure_6)) +
  geom_col(width = 0.65, show.legend = FALSE) +
  geom_text(aes(label = paste0(pct_brut, "%")),
            hjust = -0.15, size = 3.8, color = "grey20") +
  coord_flip() +
  scale_y_continuous(limits = c(0, 75),
                     labels = function(x) paste0(x, "%")) +
  scale_fill_manual(values = palette_tenure) +
  labs(
    title    = "Régime de tenure des parcelles agricoles",
    subtitle = "GHS Nigeria W4 — proportions brutes",
    x        = NULL,
    y        = "Part des parcelles (%)",
    caption  = sprintf("n = %d parcelles | Variable : acquisition_mode (s11b1q4)",
                       sum(!is.na(donnees$tenure_6)))
  ) +
  theme_tp4()

print(p_barplot)
sauvegarder_figure("t21_barplot_tenure.png")


# ================================================================
# ÉTAPE 4 — TEST D'INDÉPENDANCE CHI-DEUX : tenure × milieu
#
# H0 : le régime de tenure est indépendant du milieu (rural/urbain)
# H1 : il existe une association significative
#
# Conditions de validité du chi-deux :
#   - Tableau de contingence avec effectifs observés
#   - Tous les effectifs THÉORIQUES ≥ 5
# Si certains effectifs théoriques < 5, le test est invalide
# et il faudrait regrouper des modalités.
# ================================================================

# Table de contingence
df_chi2 <- donnees %>%
  filter(!is.na(tenure_6), !is.na(milieu_label))

tableau_contingence <- table(df_chi2$tenure_6, df_chi2$milieu_label)

cat("\n=== Table de contingence tenure × milieu ===\n")
print(tableau_contingence)

# Effectifs théoriques (calculés avant le test)
# Théorique(i,j) = total_ligne(i) * total_colonne(j) / total_général
eff_theoriques <- chisq.test(tableau_contingence)$expected

cat("\n=== Effectifs théoriques (validité du chi-deux) ===\n")
print(round(eff_theoriques, 1))
cat(sprintf("Minimum : %.1f (doit être ≥ 5)\n", min(eff_theoriques)))

# Test chi-deux
test_chi2 <- chisq.test(tableau_contingence)
cat("\n=== Résultat du test chi-deux ===\n")
print(test_chi2)

# Résidus standardisés : (observé - théorique) / sqrt(théorique)
# Un |résidu| > 2 indique une cellule qui contribue significativement
# à l'association. C'est le diagnostic le plus informatif du chi-deux.
cat("\n=== Résidus standardisés ===\n")
cat("(|résidu| > 2 = contribution significative)\n")
print(round(test_chi2$stdres, 2))

# Sauvegarder les résultats
res_chi2 <- data.frame(
  statistique = round(test_chi2$statistic, 2),
  ddl         = test_chi2$parameter,
  p_valeur    = signif(test_chi2$p.value, 3),
  eff_th_min  = round(min(eff_theoriques), 2),
  validite    = ifelse(min(eff_theoriques) >= 5, "Oui", "Non")
)
sauvegarder_tableau(res_chi2, "t21_chi2_tenure_milieu.csv")

# Résidus en tableau
res_stdres <- as.data.frame(round(test_chi2$stdres, 2))
names(res_stdres) <- c("tenure", "milieu", "residu_standardise")
sauvegarder_tableau(res_stdres, "t21_residus_standardises.csv")


# ================================================================
# ÉTAPE 5 — GRAPHIQUE BARRES GROUPÉES PAR MILIEU
# ================================================================

# Calcul des proportions par milieu (% dans chaque milieu)
df_plot_milieu <- df_chi2 %>%
  count(milieu_label, tenure_6) %>%
  group_by(milieu_label) %>%
  mutate(pct = round(n / sum(n) * 100, 1)) %>%
  ungroup()

label_chi2 <- sprintf(
  "χ² = %.1f | ddl = %d | p < 2.2e-16",
  test_chi2$statistic, test_chi2$parameter
)

p_milieu <- df_plot_milieu %>%
  ggplot(aes(x = tenure_6, y = pct, fill = milieu_label)) +
  geom_col(position = "dodge", width = 0.7, alpha = 0.9) +
  geom_text(aes(label = paste0(pct, "%")),
            position = position_dodge(width = 0.7),
            vjust = -0.4, size = 3, color = "grey20") +
  scale_fill_manual(
    values = c("Rural" = "#1D6996", "Urbain" = "#E17C05"),
    name   = "Milieu"
  ) +
  scale_y_continuous(limits = c(0, 80),
                     labels = function(x) paste0(x, "%")) +
  labs(
    title    = "Régime de tenure selon le milieu de résidence",
    subtitle = "Proportions calculées à l'intérieur de chaque milieu",
    x        = "Régime de tenure",
    y        = "Part des parcelles dans le milieu (%)",
    caption  = label_chi2
  ) +
  theme_tp4() +
  theme(axis.text.x = element_text(angle = 20, hjust = 1))

print(p_milieu)
sauvegarder_figure("t21_tenure_par_milieu.png")

cat("\n✔ Tâche 21 terminée.\n")
