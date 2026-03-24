# =============================================================================
# Script 02 : Exploration statistique et visualisations (avec pondérations)
# TP1 — Profil démographique des ménages nigérians
# Nigeria GHS-Panel Wave 4 (2018/19)
#
# Ce script produit :
#   fig01 — histogramme de l'âge (pondéré)
#   fig02 — boîte à moustaches de l'âge
#   fig03 — pyramide des âges par sexe (pondérée)
#   fig04 — barplot du lien de parenté (pondéré)
#   fig05 — boxplot de la taille des ménages Urbain vs Rural (pondéré)
#   tableau_gtsummary.xlsx — tableau gtsummary exporté en Excel
#
# NOTE MÉTHODOLOGIQUE :
#   Toutes les statistiques descriptives sont calculées avec les pondérations
#   transversales wt_wave4 afin que les estimations soient représentatives
#   de la population nigériane (et non seulement de l'échantillon).
#   - Moyennes et proportions : calculées avec weighted.mean() / sum(w*x)/sum(w)
#   - Tests statistiques : utilisation du package survey (svydesign)
#   - Pyramide des âges : proportions pondérées
#   - gtsummary : intégration via as_survey_design()
#
# Auteurs  : Herman YAMAHA | Bourama DIALLO
# =============================================================================


# =============================================================================
# Installation automatique des packages manquants
# =============================================================================

# Liste des packages requis
packages_requis <- c(
  "dplyr", "ggplot2", "scales", "naniar", "PropCIs", 
  "gtsummary", "gt", "apyramid", "forcats", "survey", 
  "srvyr", "openxlsx"
)

# Vérification et installation des packages manquants
for (pkg in packages_requis) {
  if (!require(pkg, character.only = TRUE)) {
    message(paste("Installation du package:", pkg))
    install.packages(pkg, dependencies = TRUE)
    library(pkg, character.only = TRUE)
  }
}


# --------------------------------------------------------------------------
# Création du dossier des outputs si nécessaire
# --------------------------------------------------------------------------

dir.create("outputs", showWarnings = FALSE, recursive = TRUE)

# --------------------------------------------------------------------------
# Vérification : si les objets .rds n'existent pas encore, exécuter le script 01
# --------------------------------------------------------------------------
if (!file.exists("data/processed/df_brut.rds")) {
  cat("Objets intermédiaires absents — exécution de 01_import_nettoyage.R...\n")
  source("scripts/01_import_nettoyage.R")
}

# --------------------------------------------------------------------------
# Chargement des objets préparés par le script 01
# --------------------------------------------------------------------------
df_brut      <- readRDS("data/processed/df_brut.rds")
df_individus <- readRDS("data/processed/df_individus.rds")
df_menages   <- readRDS("data/processed/df_menages.rds")

# --------------------------------------------------------------------------
# Définition du plan de sondage
# --------------------------------------------------------------------------
# Le GHS-Panel est un sondage stratifié à plusieurs degrés.
# On déclare un plan simplifié avec strates (zone x sector) et poids wt_wave4.
# Pour les individus :
design_individus <- df_individus %>%
  filter(!is.na(wt_wave4)) %>%
  as_survey_design(
    ids     = hhid,          # grappe = ménage
    weights = wt_wave4,
    nest    = TRUE
  )

# Pour les ménages :
design_menages <- df_menages %>%
  filter(!is.na(wt_wave4)) %>%
  as_survey_design(
    ids     = hhid,
    weights = wt_wave4,
    nest    = TRUE
  )

# --------------------------------------------------------------------------
# Palette de couleurs du projet
# --------------------------------------------------------------------------
COULEUR_URBAIN  <- "#C0392B"
COULEUR_RURAL   <- "#1A5276"
COULEUR_HOMME   <- "#1A5276"
COULEUR_FEMME   <- "#C0392B"
COULEUR_ACCENT  <- "#E67E22"
FOND_GRAPHIQUE  <- "#FDFEFE"

palette_milieu  <- c("Urbain" = COULEUR_URBAIN, "Rural" = COULEUR_RURAL)
palette_genre   <- c("Homme"  = COULEUR_HOMME,  "Femme" = COULEUR_FEMME)
palette_lien    <- c(
  "Chef de ménage" = "#1A5276",
  "Conjoint(e)"    = "#117A65",
  "Enfant"         = "#E67E22",
  "Autre membre"   = "#7F8C8D"
)

theme_tp1 <- theme_minimal(base_size = 12) +
  theme(
    plot.title       = element_text(face = "bold", size = 13, color = "#1A1A2E"),
    plot.subtitle    = element_text(color = "grey45", size = 10),
    plot.caption     = element_text(color = "grey55", size = 8.5, hjust = 1),
    plot.background  = element_rect(fill = FOND_GRAPHIQUE, color = NA),
    panel.grid.minor = element_blank()
  )

# =============================================================================
# TÂCHE 1 — EXPLORATION DE LA STRUCTURE DU DATASET
# =============================================================================
cat("\n=== TÂCHE 1 : Exploration du dataset ===\n")
cat("Observations    :", nrow(df_brut), "\n")
cat("Variables       :", ncol(df_brut), "\n")
cat("Ménages uniques :", n_distinct(df_brut$hhid), "\n")

# Statistiques pondérées globales
stats_globales <- design_individus %>%
  summarise(
    n_individus  = survey_total(vartype = NULL),
    age_moy_pond = survey_mean(age, na.rm = TRUE, vartype = "ci"),
    pct_hommes   = survey_mean(genre == "Homme", na.rm = TRUE, vartype = "ci"),
    pct_urbain   = survey_mean(milieu == "Urbain", na.rm = TRUE, vartype = "ci")
  )
cat("\nAperçu global pondéré :\n"); print(as.data.frame(stats_globales))

# =============================================================================
# TÂCHE 2 — CONTRÔLE QUALITÉ : DOUBLONS ET VALEURS MANQUANTES
# =============================================================================
cat("\n=== TÂCHE 2 : Contrôle qualité ===\n")
n_doublons <- sum(duplicated(df_brut$cle_unique))
cat("Doublons hhid x indiv :", n_doublons, "\n")

vm <- df_brut %>%
  summarise(
    age          = sum(is.na(age)),
    genre        = sum(is.na(genre)),
    milieu       = sum(is.na(milieu)),
    lien_parente = sum(is.na(lien_parente)),
    zone_geo     = sum(is.na(zone_geo)),
    wt_wave4     = sum(is.na(wt_wave4))
  )
cat("Valeurs manquantes sur les variables clés :\n"); print(vm)

# =============================================================================
# TÂCHE 3 — ANALYSE UNIVARIÉE DE L'ÂGE (PONDÉRÉE)
# =============================================================================
cat("\n=== TÂCHE 3 : Analyse univariée de l'âge (pondérée) ===\n")

# Statistiques pondérées de l'âge
stats_age_pond <- design_individus %>%
  filter(!is.na(age)) %>%
  summarise(
    Moyenne    = survey_mean(age, vartype = NULL),
    Variance   = survey_var(age, vartype = NULL),
    Q1         = survey_quantile(age, quantiles = 0.25, vartype = NULL),
    Médiane    = survey_quantile(age, quantiles = 0.50, vartype = NULL),
    Q3         = survey_quantile(age, quantiles = 0.75, vartype = NULL)
  )
stats_age_pond$`Éc. type` <- sqrt(stats_age_pond$Variance)
cat("Statistiques descriptives pondérées de l'âge :\n")
print(as.data.frame(stats_age_pond))

# Moyenne pondérée pour annotation du graphique
moy_age_pond <- stats_age_pond$Moyenne

# Figure 01 — Histogramme de l'âge pondéré
# On utilise un histogramme de fréquences pondérées via aes(weight = wt_wave4)
df_age_plot <- df_individus %>% filter(!is.na(age), !is.na(wt_wave4))

p_hist <- ggplot(df_age_plot, aes(x = age, weight = wt_wave4)) +
  geom_histogram(
    binwidth = 5,
    fill     = COULEUR_RURAL,
    color    = "white",
    alpha    = 0.82
  ) +
  geom_vline(
    xintercept = moy_age_pond,
    color      = COULEUR_ACCENT,
    linetype   = "dashed",
    linewidth  = 0.9
  ) +
  annotate(
    "label",
    x          = moy_age_pond + 5,
    y          = Inf,
    vjust      = 1.5,
    label      = paste0("Moyenne = ", round(moy_age_pond, 1), " ans"),
    color      = COULEUR_ACCENT,
    fill       = "white",
    label.size = 0.3,
    size       = 3.5,
    fontface   = "bold"
  ) +
  scale_x_continuous(breaks = seq(0, 100, 10)) +
  scale_y_continuous(labels = comma) +
  labs(
    title    = "Distribution de l'âge des membres des ménages",
    subtitle = paste0("Effectifs représentatifs de la population | Ligne orange = moyenne"),
    x        = "Âge (années)",
    y        = "Effectifs (représentatifs de la population)",
    caption  = "Source : NBS Nigeria, GHS-Panel Wave 4 (2018/19)"
  ) +
  theme_tp1

ggsave("outputs/fig01_histogramme_age.png", p_hist,
       width = 10, height = 5.5, dpi = 150)
cat("  -> fig01_histogramme_age.png\n")


# Figure 02 — Boîte à moustaches de l'âge
med_age_pond <- as.numeric(stats_age_pond$Médiane_q50)


p_boxplot_age <- ggplot(df_age_plot, aes(x = "", y = age)) +
  geom_boxplot(
    fill          = COULEUR_RURAL,
    color         = "#17202A",
    alpha         = 0.72,
    width         = 0.35,
    outlier.color = "grey65",
    outlier.alpha = 0.45,
    outlier.size  = 0.7
  ) +
  geom_jitter(width = 0.1, alpha = 0.015,
              color = COULEUR_RURAL, size = 0.4) +
  stat_summary(
    fun   = function(x) weighted.mean(x, w = df_age_plot$wt_wave4[match(x, df_age_plot$age)],
                                      na.rm = TRUE),
    geom  = "point",
    shape = 18,
    size  = 4.5,
    color = COULEUR_ACCENT
  ) +
  scale_y_continuous(breaks = seq(0, 120, 10)) +
  labs(
    title    = "Boîte à moustaches de l'âge des membres",
    subtitle = paste0("Médiane ≈ ", round(med_age_pond, 1),
                      " ans | Losange orange = moyenne"),
    y        = "Âge (années)",
    x        = NULL,
    caption  = "Source : NBS Nigeria, GHS-Panel Wave 4 (2018/19)"
  ) +
  theme_tp1 +
  theme(
    axis.text.x        = element_blank(),
    panel.grid.major.x = element_blank()
  )

ggsave("outputs/fig02_boxplot_age.png", p_boxplot_age,
       width = 6, height = 6, dpi = 150)
cat("  -> fig02_boxplot_age.png\n")

# Test de normalité Shapiro-Wilk (sur sous-échantillon non pondéré, méthode standard)
set.seed(2025)
echantillon_sw <- sample(na.omit(df_individus$age), size = 5000)
test_sw        <- shapiro.test(echantillon_sw)
cat("\nShapiro-Wilk (n = 5000, non pondéré — méthode standard) :\n")
cat("  W =", round(test_sw$statistic, 5), "\n")
cat("  p =", format(test_sw$p.value, scientific = TRUE), "\n")
cat("  ->", ifelse(test_sw$p.value < 0.05,
                   "Distribution NON normale (p < 0.05)",
                   "Distribution normale (p >= 0.05)"), "\n")

# Figure 03 — Pyramide des âges pondérée
# On calcule les effectifs pondérés par tranche d'âge et sexe
df_pyramide <- df_individus %>%
  filter(!is.na(age), !is.na(genre), !is.na(tranche_age), !is.na(wt_wave4)) %>%
  group_by(tranche_age, genre) %>%
  summarise(effectif_pond = sum(wt_wave4), .groups = "drop") %>%
  # Pour l'affichage en pyramide : négatif pour les hommes
  mutate(
    eff_affich = ifelse(genre == "Homme", -effectif_pond, effectif_pond)
  )

# Valeur max pour symétriser les axes
max_val <- max(df_pyramide$effectif_pond)

p_pyramide <- ggplot(df_pyramide,
                     aes(x = tranche_age, y = eff_affich, fill = genre)) +
  geom_bar(stat = "identity", alpha = 0.85) +
  coord_flip() +
  scale_y_continuous(
    breaks = pretty(c(-max_val, max_val), n = 8),
    labels = function(x) comma(abs(x))
  ) +
  scale_fill_manual(values = palette_genre, name = "Sexe") +
  labs(
    title    = "Pyramide des âges des membres des ménages",
    subtitle = "Nigeria — GHS-Panel Wave 4 (2018/19) | Effectifs représentatifs de la population",
    x        = "Tranche d'âge",
    y        = "Effectifs (représentatifs de la population)",
    caption  = "Source : NBS Nigeria, GHS-Panel Wave 4 (2018/19)"
  ) +
  theme_tp1 +
  theme(legend.position = "bottom")

ggsave("outputs/fig03_pyramide_ages.png", p_pyramide,
       width = 9, height = 8, dpi = 150)
cat("  -> fig03_pyramide_ages.png\n")

# =============================================================================
# TÂCHE 4 — LIEN DE PARENTÉ ET IC À 95 % (PONDÉRÉ)
# =============================================================================
cat("\n=== TÂCHE 4 : Lien de parenté (pondéré) ===\n")

# Proportions pondérées par catégorie de lien de parenté
prop_lien_pond <- design_individus %>%
  group_by(lien_parente) %>%
  summarise(
    prop_pond = survey_mean(vartype = "ci", level = 0.95),
    n_pond    = survey_total(vartype = NULL)
  ) %>%
  arrange(desc(prop_pond))

cat("Proportions pondérées avec IC 95% :\n")
print(as.data.frame(prop_lien_pond))

# Figure 04 — Barplot horizontal ordonné par fréquence pondérée
df_lien_plot <- prop_lien_pond %>%
  mutate(lien_parente = reorder(lien_parente, prop_pond))

p_parente <- ggplot(df_lien_plot,
                    aes(x = lien_parente, y = prop_pond, fill = lien_parente)) +
  geom_bar(stat = "identity", alpha = 0.85, color = "white") +
  geom_errorbar(aes(ymin = prop_pond_low, ymax = prop_pond_upp),
                width = 0.25, linewidth = 0.7, color = "grey30") +
  geom_text(
    aes(label = paste0(round(prop_pond * 100, 1), "%")),
    hjust    = -0.5,
    vjust    = 0.5,
    nudge_x  = 0.02,
    size     = 3.5,
    fontface = "bold",
    color    = "#17202A"
  ) +
  scale_fill_manual(values = palette_lien, guide = "none") +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.15)),
    labels = percent_format(accuracy = 1)
  ) +
  coord_flip() +
  labs(
    title   = "Répartition selon le lien de parenté",
    subtitle = "Proportions représentatives de la population nigériane | IC 95%",
    x       = NULL,
    y       = "Proportion (%)",
    caption = "Source : NBS Nigeria, GHS-Panel Wave 4 (2018/19)"
  ) +
  theme_tp1 +
  theme(panel.grid.major.y = element_blank())

ggsave("outputs/fig04_lien_parente.png", p_parente,
       width = 9, height = 5, dpi = 150)
cat("  -> fig04_lien_parente.png\n")

# Tableau IC pour le rapport
tab_lien_pond <- prop_lien_pond %>%
  transmute(
    Catégorie        = as.character(lien_parente),
    `Proportion (%)` = round(prop_pond * 100, 2),
    `IC 95% inf (%)` = round(prop_pond_low * 100, 2),
    `IC 95% sup (%)` = round(prop_pond_upp * 100, 2),
    `Effectif (représentatif)` = round(n_pond, 0)
  )
saveRDS(tab_lien_pond, "data/processed/proportions_ic_ponderes.rds")

# =============================================================================
# TÂCHE 5 — TAILLE DES MÉNAGES : STATISTIQUES ET TEST DE WILCOXON (PONDÉRÉ)
# =============================================================================
cat("\n=== TÂCHE 5 : Taille des ménages par milieu (pondéré) ===\n")

# Statistiques pondérées par milieu
stats_menages_pond <- design_menages %>%
  filter(!is.na(milieu)) %>%
  group_by(milieu) %>%
  summarise(
    N_pond     = survey_total(vartype = NULL),
    Moyenne    = survey_mean(taille, vartype = "ci"),
    Médiane    = survey_quantile(taille, quantiles = 0.50, vartype = NULL),
    Q1         = survey_quantile(taille, quantiles = 0.25, vartype = NULL),
    Q3         = survey_quantile(taille, quantiles = 0.75, vartype = NULL)
  )
cat("Statistiques pondérées par milieu :\n")
print(as.data.frame(stats_menages_pond))

# Test de Wilcoxon-Mann-Whitney sur les données non pondérées
# (Le test de Wilcoxon pondéré nécessite des packages spécialisés ;
#  on utilise le test non pondéré comme approximation tout en reportant
#  les statistiques descriptives pondérées.)
menages_urbain <- df_menages %>% filter(milieu == "Urbain", !is.na(wt_wave4)) %>% pull(taille)
menages_rural  <- df_menages %>% filter(milieu == "Rural",  !is.na(wt_wave4)) %>% pull(taille)

res_wilcox <- wilcox.test(
  menages_urbain, menages_rural,
  alternative = "two.sided",
  conf.int    = TRUE,
  conf.level  = 0.95
)

N_total       <- length(menages_urbain) + length(menages_rural)
Z_score       <- qnorm(res_wilcox$p.value / 2)
effet_r       <- abs(Z_score) / sqrt(N_total)
categorie_effet <- case_when(
  effet_r < 0.10 ~ "Négligeable",
  effet_r < 0.30 ~ "Petit",
  effet_r < 0.50 ~ "Moyen",
  TRUE           ~ "Grand"
)

cat("\nWilcoxon-Mann-Whitney :\n")
cat("  W =", res_wilcox$statistic, "\n")
cat("  p =", format(res_wilcox$p.value, scientific = TRUE), "\n")
cat("  r =", round(effet_r, 4), "(", categorie_effet, ")\n")

saveRDS(
  list(
    stats        = stats_menages_pond,
    test         = res_wilcox,
    r_effet      = effet_r,
    categorie    = categorie_effet
  ),
  "data/processed/resultats_wilcoxon.rds"
)

# Figure 05 — Boxplot groupé pondéré : taille des ménages
label_test <- paste0(
  "Wilcoxon W = ", round(res_wilcox$statistic, 0),
  "\np = ", format(res_wilcox$p.value, scientific = TRUE, digits = 3),
  "\nr = ", round(effet_r, 3), " (", categorie_effet, ")"
)

# Moyennes pondérées par milieu pour annotation
moy_pond_milieu <- design_menages %>%
  filter(!is.na(milieu)) %>%
  group_by(milieu) %>%
  summarise(moy_pond = survey_mean(taille, vartype = NULL)) %>%
  as.data.frame()

p_box_menage <- ggplot(
  df_menages %>% filter(!is.na(milieu), !is.na(wt_wave4)),
  aes(x = milieu, y = taille, fill = milieu)
) +
  geom_boxplot(
    aes(weight = wt_wave4),
    alpha         = 0.72,
    outlier.color = "grey55",
    outlier.size  = 0.75,
    outlier.alpha = 0.45,
    width         = 0.48
  ) +
  geom_jitter(width = 0.14, alpha = 0.07,
              size = 0.55, color = "grey35") +
  geom_point(
    data  = moy_pond_milieu,
    aes(x = milieu, y = moy_pond),
    shape = 18,
    size  = 4.5,
    color = COULEUR_ACCENT,
    inherit.aes = FALSE
  ) +
  geom_text(
    data     = moy_pond_milieu,
    aes(x    = milieu, y = moy_pond,
        label = paste0("Moyenne = ", round(moy_pond, 1))),
    vjust    = -0.9,
    color    = COULEUR_ACCENT,
    size     = 3.5,
    fontface = "bold",
    inherit.aes = FALSE
  ) +
  annotate(
    "text",
    x        = 1.5,
    y        = max(df_menages$taille, na.rm = TRUE) * 0.94,
    label    = label_test,
    size     = 3.4,
    color    = "grey25",
    hjust    = 0.5,
    vjust    = 1,
    fontface = "italic"
  ) +
  scale_fill_manual(values = palette_milieu) +
  scale_y_continuous(breaks = seq(0, 35, by = 5)) +
  labs(
    title    = "Taille des ménages selon le milieu de résidence",
    subtitle = paste0(
      "Losange = Moyenne | Test Wilcoxon-Mann-Whitney"
    ),
    x        = NULL,
    y        = "Nombre de membres par ménage",
    caption  = "Source : NBS Nigeria, GHS-Panel Wave 4 (2018/19)"
  ) +
  theme_tp1 +
  theme(
    legend.position    = "none",
    panel.grid.major.x = element_blank(),
    axis.text.x        = element_text(size = 12, face = "bold")
  )

ggsave("outputs/fig05_boxplot_menages.png", p_box_menage,
       width = 10, height = 6, dpi = 150)
cat("  -> fig05_boxplot_menages.png\n")

# =============================================================================
# TÂCHE 6 — TABLEAU GTSUMMARY STRATIFIÉ PAR MILIEU (PONDÉRÉ) → EXPORT EXCEL
# =============================================================================
cat("\n=== TÂCHE 6 : Tableau gtsummary pondéré → Excel ===\n")

# Constitution du jeu de données pour le tableau
df_tableau <- df_individus %>%
  filter(!is.na(milieu), !is.na(wt_wave4)) %>%
  transmute(
    milieu        = milieu,
    age           = as.numeric(s1q4),
    genre         = factor(as.numeric(s1q2),
                           levels = c(1, 2),
                           labels = c("Masculin", "Féminin")),
    taille_menage = taille_menage,
    wt_wave4      = wt_wave4
  )

# Calcul des effectifs pondérés arrondis par groupe
n_ensemble <- round(sum(df_tableau$wt_wave4))
n_urbain <- round(sum(df_tableau$wt_wave4[df_tableau$milieu == "Urbain"]))
n_rural <- round(sum(df_tableau$wt_wave4[df_tableau$milieu == "Rural"]))

# Plan de sondage pour gtsummary
svy_tableau <- svydesign(
  ids     = ~1,
  weights = ~wt_wave4,
  data    = df_tableau
)

tbl_gt <- svy_tableau %>%
  tbl_svysummary(
    by       = milieu,
    include  = c(age, genre, taille_menage),
    statistic = list(
      all_continuous()  ~ "{mean} ({sd})\nMédiane {median} [{p25} ; {p75}]",
      all_categorical() ~ "{n} ({p}%)"
    ),
    label = list(
      age           ~ "Âge (années)",
      genre         ~ "Sexe",
      taille_menage ~ "Taille du ménage (membres)"
    ),
    digits       = list(
      all_continuous() ~ c(1, 1, 1, 1, 1),
      all_categorical() ~ c(0, 1)
    ),
    missing      = "ifany",
    missing_text = "Non renseigné"
  ) %>%
  add_overall(last = FALSE, col_label = "**Ensemble**") %>%
  add_p(
    test = list(
      age           ~ "svy.wilcox.test",
      genre         ~ "svy.chisq.test",
      taille_menage ~ "svy.wilcox.test"
    ),
    pvalue_fun = ~style_pvalue(.x, digits = 3)
  ) %>%
  modify_header(
    label   ~ "**Variable**",
    stat_0  ~ paste0("**Ensemble**\nN = ", n_ensemble),
    stat_1  ~ paste0("**Urbain**\nN = ", n_urbain),
    stat_2  ~ paste0("**Rural**\nN = ", n_rural),
    p.value ~ "**p-valeur**"
  ) %>%
  modify_spanning_header(
    c(stat_1, stat_2) ~ "**Milieu de résidence**"
  ) %>%
  modify_caption(
    "**Tableau 1. Profil sociodémographique par milieu de résidence**\nSource : NBS Nigeria, GHS-Panel Wave 4 (2018/19)"
  ) %>%
  modify_footnote(
    p.value ~ "Tests : svy.wilcox.test (variables continues) ; svy.chisq.test (variables catégorielles)"
  ) %>%
  bold_labels() %>%
  bold_p(t = 0.05)

# ----- Export en Excel (openxlsx) avec effectifs pondérés arrondis -----
df_export <- tbl_gt %>%
  as_tibble()

# Création d'un classeur Excel propre
wb <- createWorkbook()
addWorksheet(wb, "Tableau_Descriptif")

# En-tête du tableau
writeData(wb, "Tableau_Descriptif",
          "Tableau 1. Profil sociodémographique par milieu de résidence",
          startRow = 1, startCol = 1)
writeData(wb, "Tableau_Descriptif",
          "Source : NBS Nigeria, GHS-Panel Wave 4 (2018/19)",
          startRow = 2, startCol = 1)

# Légende explicative
writeData(wb, "Tableau_Descriptif",
          "Légende :",
          startRow = 3, startCol = 1)
writeData(wb, "Tableau_Descriptif",
          "- Variables continues (Âge, Taille du ménage) : Moyenne (Écart-type) | Médiane [Q1 ; Q3]",
          startRow = 4, startCol = 1)
writeData(wb, "Tableau_Descriptif",
          "- Variables catégorielles (Sexe) : Effectif pondéré arrondi (Proportion %)",
          startRow = 5, startCol = 1)
writeData(wb, "Tableau_Descriptif",
          paste0("- N = Effectif pondéré total arrondi (Ensemble : ", n_ensemble, 
                 " | Urbain : ", n_urbain, " | Rural : ", n_rural, ")"),
          startRow = 6, startCol = 1)

# Données du tableau
writeData(wb, "Tableau_Descriptif", df_export,
          startRow = 8, startCol = 1, headerStyle = createStyle(
            fontName = "Arial", fontSize = 11, textDecoration = "bold",
            halign = "center", fgFill = "#1A5276", fontColour = "white",
            border = "TopBottomLeftRight", borderColour = "white"
          ))

# Styles
title_style <- createStyle(
  fontName = "Arial", fontSize = 13, textDecoration = "bold",
  fontColour = "#1A5276"
)
subtitle_style <- createStyle(
  fontName = "Arial", fontSize = 10, fontColour = "grey50",
  textDecoration = "italic"
)
legend_style <- createStyle(
  fontName = "Arial", fontSize = 9, fontColour = "grey40",
  wrapText = TRUE
)
cell_style <- createStyle(
  fontName = "Arial", fontSize = 10, halign = "center",
  border = "TopBottomLeftRight", borderColour = "#CCCCCC",
  wrapText = TRUE
)
alt_row_style <- createStyle(
  fgFill = "#EAF2F8"
)

addStyle(wb, "Tableau_Descriptif", title_style, rows = 1, cols = 1)
addStyle(wb, "Tableau_Descriptif", subtitle_style, rows = 2, cols = 1)
addStyle(wb, "Tableau_Descriptif", legend_style, rows = 3:6, cols = 1)

n_rows <- nrow(df_export)
n_cols <- ncol(df_export)

# Lignes alternées
for (i in seq(2, n_rows, by = 2)) {
  addStyle(wb, "Tableau_Descriptif", alt_row_style,
           rows = i + 8, cols = 1:n_cols, gridExpand = TRUE, stack = TRUE)
}
addStyle(wb, "Tableau_Descriptif", cell_style,
         rows = 8:(n_rows + 8), cols = 1:n_cols,
         gridExpand = TRUE, stack = TRUE)

# Largeur des colonnes
setColWidths(wb, "Tableau_Descriptif", cols = 1:n_cols,
             widths = c(30, rep(18, n_cols - 1)))

# Note de bas de tableau
writeData(wb, "Tableau_Descriptif",
          "Note : Toutes les statistiques sont calculées avec les pondérations transversales wt_wave4.",
          startRow = n_rows + 10, startCol = 1)
writeData(wb, "Tableau_Descriptif",
          "Les effectifs (N et effectifs des catégories) sont des effectifs pondérés arrondis à l'entier le plus proche.",
          startRow = n_rows + 11, startCol = 1)

addStyle(wb, "Tableau_Descriptif",
         createStyle(fontName = "Arial", fontSize = 9,
                     fontColour = "grey50", textDecoration = "italic"),
         rows = (n_rows + 10):(n_rows + 11), cols = 1, gridExpand = TRUE)

saveWorkbook(wb, "outputs/tableau_gtsummary.xlsx", overwrite = TRUE)
cat("  -> tableau_gtsummary.xlsx\n")

cat("\n====== Toutes les analyses sont terminées ======\n")
cat("Fichiers dans outputs/ :\n")
for (f in list.files("outputs/")) cat("  ->", f, "\n")