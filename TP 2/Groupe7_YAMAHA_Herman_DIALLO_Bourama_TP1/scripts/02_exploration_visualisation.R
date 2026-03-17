# =============================================================================
# Script 02 : Exploration statistique et visualisations
# TP1 — Profil démographique des ménages nigérians
# Nigeria GHS-Panel Wave 4 (2018/19)
#
# Ce script produit :
#   fig01 — histogramme de l'âge
#   fig02 — boîte à moustaches de l'âge
#   fig03 — pyramide des âges par sexe
#   fig04 — barplot du lien de parenté
#   fig04b — pointrange des IC 95% du lien de parenté
#   fig05 — boxplot de la taille des ménages Urbain vs Rural
#   tableau — tableau gtsummary exporté en HTML
#
# Auteurs  : Herman YAMAHA | Bourama DIALLO
# =============================================================================

library(dplyr)
library(ggplot2)
library(scales)
library(naniar)
library(PropCIs)
library(gtsummary)
library(gt)
library(apyramid)
library(forcats)

dir.create("outputs", showWarnings = FALSE, recursive = TRUE)

# --------------------------------------------------------------------------
# Chargement des objets préparés par le script 01
# --------------------------------------------------------------------------
df_brut      <- readRDS("data/processed/df_brut.rds")
df_individus <- readRDS("data/processed/df_individus.rds")
df_menages   <- readRDS("data/processed/df_menages.rds")

# --------------------------------------------------------------------------
# Palette de couleurs du projet
# Différente de tout autre groupe : teintes corail / ardoise / vert sauge
# --------------------------------------------------------------------------
COULEUR_URBAIN  <- "#C0392B"   # rouge brique — milieu urbain
COULEUR_RURAL   <- "#1A5276"   # bleu marine   — milieu rural
COULEUR_HOMME   <- "#1A5276"   # bleu marine   — hommes
COULEUR_FEMME   <- "#C0392B"   # rouge brique  — femmes
COULEUR_ACCENT  <- "#E67E22"   # orange        — annotations
FOND_GRAPHIQUE  <- "#FDFEFE"   # blanc cassé   — fond des figures

palette_milieu  <- c("Urbain" = COULEUR_URBAIN, "Rural" = COULEUR_RURAL)
palette_genre   <- c("Homme"  = COULEUR_HOMME,  "Femme" = COULEUR_FEMME)
palette_lien    <- c(
  "Chef de ménage" = "#1A5276",
  "Conjoint(e)"    = "#117A65",
  "Enfant"         = "#E67E22",
  "Autre membre"   = "#7F8C8D"
)

# Thème commun à tous les graphiques du projet
theme_tp1 <- theme_minimal(base_size = 12) +
  theme(
    plot.title       = element_text(face = "bold", size = 13,
                                    color = "#1A1A2E"),
    plot.subtitle    = element_text(color = "grey45", size = 10),
    plot.caption     = element_text(color = "grey55", size = 8.5,
                                    hjust = 1),
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

stats_globales <- df_brut %>%
  summarise(
    n_individus  = n(),
    n_menages    = n_distinct(hhid),
    age_moyen    = round(mean(age, na.rm = TRUE), 1),
    age_median   = median(age, na.rm = TRUE),
    pct_hommes   = round(mean(genre == "Homme", na.rm = TRUE) * 100, 1),
    pct_urbain   = round(mean(milieu == "Urbain", na.rm = TRUE) * 100, 1)
  )
cat("\nAperçu global :\n"); print(stats_globales)

# =============================================================================
# TÂCHE 2 — CONTRÔLE QUALITÉ : DOUBLONS ET VALEURS MANQUANTES
# =============================================================================
cat("\n=== TÂCHE 2 : Contrôle qualité ===\n")

# Doublons
n_doublons <- sum(duplicated(df_brut$cle_unique))
cat("Doublons hhid x indiv :", n_doublons, "\n")

# Valeurs manquantes sur les variables clés
vm <- df_brut %>%
  summarise(
    age          = sum(is.na(age)),
    genre        = sum(is.na(genre)),
    milieu       = sum(is.na(milieu)),
    lien_parente = sum(is.na(lien_parente)),
    zone_geo     = sum(is.na(zone_geo))
  )
cat("Valeurs manquantes sur les variables clés :\n"); print(vm)

# =============================================================================
# TÂCHE 3 — ANALYSE UNIVARIÉE DE L'ÂGE
# =============================================================================
cat("\n=== TÂCHE 3 : Analyse univariée de l'âge ===\n")

stats_age <- df_brut %>%
  filter(!is.na(age)) %>%
  summarise(
    Moyenne     = round(mean(age), 2),
    Médiane     = median(age),
    `Éc. type`  = round(sd(age), 2),
    Min         = min(age),
    Q1          = quantile(age, .25),
    Q3          = quantile(age, .75),
    Max         = max(age),
    IQR         = IQR(age)
  )
cat("Statistiques descriptives de l'âge :\n"); print(stats_age)

# Figure 01 — Histogramme de l'âge
# La moyenne est représentée par une ligne verticale en orange
moy_age <- mean(df_brut$age, na.rm = TRUE)

p_hist <- ggplot(df_brut %>% filter(!is.na(age)), aes(x = age)) +
  geom_histogram(
    binwidth = 5,
    fill     = COULEUR_RURAL,
    color    = "white",
    alpha    = 0.82
  ) +
  geom_vline(
    xintercept = moy_age,
    color      = COULEUR_ACCENT,
    linetype   = "dashed",
    linewidth  = 0.9
  ) +
  annotate(
    "label",
    x          = moy_age + 5,
    y          = Inf,
    vjust      = 1.5,
    label      = paste0("Moy. = ", round(moy_age, 1), " ans"),
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
    subtitle = paste0("N = ", comma(sum(!is.na(df_brut$age))),
                      " individus | Ligne orange = moyenne"),
    x        = "Âge (années)",
    y        = "Effectifs",
    caption  = "Source : NBS Nigeria, GHS-Panel Wave 4 (2018/19)"
  ) +
  theme_tp1

ggsave("outputs/fig01_histogramme_age.png", p_hist,
       width = 10, height = 5.5, dpi = 150)
cat("  -> fig01_histogramme_age.png\n")

# Figure 02 — Boîte à moustaches de l'âge
p_boxplot_age <- ggplot(df_brut %>% filter(!is.na(age)),
                        aes(x = "", y = age)) +
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
    fun   = mean,
    geom  = "point",
    shape = 18,
    size  = 4.5,
    color = COULEUR_ACCENT
  ) +
  scale_y_continuous(breaks = seq(0, 120, 10)) +
  labs(
    title    = "Boîte à moustaches de l'âge des membres",
    subtitle = paste0("Médiane = ", median(df_brut$age, na.rm = TRUE),
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

# Test de normalité Shapiro-Wilk
# La taille maximale pour ce test est 5000 ; on tire un sous-échantillon aléatoire
set.seed(2025)
echantillon_sw <- sample(na.omit(df_brut$age), size = 5000)
test_sw        <- shapiro.test(echantillon_sw)
cat("\nShapiro-Wilk (n = 5000) :\n")
cat("  W =", round(test_sw$statistic, 5), "\n")
cat("  p =", format(test_sw$p.value, scientific = TRUE), "\n")
cat("  ->", ifelse(test_sw$p.value < 0.05,
                   "Distribution NON normale (p < 0.05)",
                   "Distribution normale (p >= 0.05)"), "\n")

# Figure 03 — Pyramide des âges
p_pyramide <- df_brut %>%
  filter(!is.na(age), !is.na(genre), !is.na(tranche_age)) %>%
  age_pyramid(
    age_group  = "tranche_age",
    split_by   = "genre",
    proportion = FALSE
  ) +
  scale_fill_manual(values = palette_genre, name = "Sexe") +
  labs(
    title    = "Pyramide des âges des membres des ménages",
    subtitle = "Nigeria — GHS-Panel Wave 4 (2018/19)",
    x        = "Effectifs",
    y        = "Tranche d'âge",
    caption  = "Source : NBS Nigeria, GHS-Panel Wave 4 (2018/19)"
  ) +
  theme_tp1 +
  theme(legend.position = "bottom")

ggsave("outputs/fig03_pyramide_ages.png", p_pyramide,
       width = 9, height = 8, dpi = 150)
cat("  -> fig03_pyramide_ages.png\n")

# =============================================================================
# TÂCHE 4 — LIEN DE PARENTÉ ET IC À 95 %
# =============================================================================
cat("\n=== TÂCHE 4 : Lien de parenté ===\n")

# Figure 04 — Barplot horizontal ordonné par fréquence
p_parente <- ggplot(
  df_brut,
  aes(
    x    = fct_rev(fct_infreq(lien_parente)),
    fill = lien_parente
  )
) +
  geom_bar(alpha = 0.85, color = "white") +
  geom_text(
    stat     = "count",
    aes(label = comma(after_stat(count))),
    hjust    = -0.12,
    size     = 3.5,
    fontface = "bold",
    color    = "#17202A"
  ) +
  scale_fill_manual(values = palette_lien, guide = "none") +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.16)),
    labels = comma
  ) +
  coord_flip() +
  labs(
    title   = "Répartition des membres selon le lien de parenté",
    x       = NULL,
    y       = "Effectifs",
    caption = "Source : NBS Nigeria, GHS-Panel Wave 4 (2018/19)"
  ) +
  theme_tp1 +
  theme(panel.grid.major.y = element_blank())

ggsave("outputs/fig04_lien_parente.png", p_parente,
       width = 9, height = 5, dpi = 150)
cat("  -> fig04_lien_parente.png\n")

# Calcul des proportions et IC 95 % exacts (méthode Clopper-Pearson)
# Cette méthode est préférée à l'approximation normale pour les proportions
# potentiellement proches de 0 ou 1
tab_lien <- table(df_brut$lien_parente)
ic_liste <- lapply(tab_lien, function(x) {
  exactci(x, n = sum(tab_lien), conf.level = 0.95)
})

df_ic <- data.frame(
  Catégorie         = names(tab_lien),
  Effectif          = as.integer(tab_lien),
  `Proportion (%)` = round(as.integer(tab_lien) / sum(tab_lien) * 100, 2),
  `IC inf. (%)` = round(sapply(ic_liste, \(x) x$conf.int[1]) * 100, 2),
  `IC sup. (%)` = round(sapply(ic_liste, \(x) x$conf.int[2]) * 100, 2),
  check.names = FALSE
)
cat("Proportions avec IC 95 % :\n"); print(df_ic)
saveRDS(df_ic, "data/processed/proportions_ic.rds")

# =============================================================================
# TÂCHE 5 — TAILLE DES MÉNAGES : STATISTIQUES ET TEST DE WILCOXON
# =============================================================================
cat("\n=== TÂCHE 5 : Taille des ménages par milieu ===\n")

stats_menages <- df_menages %>%
  group_by(milieu) %>%
  summarise(
    N          = n(),
    Moyenne    = round(mean(taille), 2),
    Médiane    = median(taille),
    Q1         = quantile(taille, 0.25),
    Q3         = quantile(taille, 0.75),
    `Éc. type` = round(sd(taille), 2),
    Min        = min(taille),
    Max        = max(taille),
    .groups    = "drop"
  )
cat("Statistiques par milieu :\n"); print(stats_menages)

# Test de Wilcoxon-Mann-Whitney bilatéral
# Hypothèse nulle : les distributions de taille sont identiques en Urbain et Rural
# Hypothèse alternative (bilatérale) : elles diffèrent
menages_urbain <- df_menages %>% filter(milieu == "Urbain") %>% pull(taille)
menages_rural  <- df_menages %>% filter(milieu == "Rural")  %>% pull(taille)

res_wilcox <- wilcox.test(
  menages_urbain, menages_rural,
  alternative = "two.sided",
  conf.int    = TRUE,
  conf.level  = 0.95
)

# Taille d'effet r de Rosenthal : r = |Z| / sqrt(n)
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
cat("  IC 95 % : [",
    round(res_wilcox$conf.int[1], 3), ";",
    round(res_wilcox$conf.int[2], 3), "]\n")

# Sauvegarde des résultats pour le rapport
saveRDS(
  list(
    stats        = stats_menages,
    test         = res_wilcox,
    r_effet      = effet_r,
    categorie    = categorie_effet
  ),
  "data/processed/resultats_wilcoxon.rds"
)

# Figure 05 — Boxplot groupé : taille des ménages Urbain vs Rural
label_test <- paste0(
  "Wilcoxon W = ", round(res_wilcox$statistic, 0),
  "\np = ", format(res_wilcox$p.value, scientific = TRUE, digits = 3),
  "\nr = ", round(effet_r, 3), " (", categorie_effet, ")"
)

df_moyennes <- df_menages %>%
  group_by(milieu) %>%
  summarise(moy = mean(taille), .groups = "drop")

p_box_menage <- ggplot(df_menages,
                       aes(x = milieu, y = taille, fill = milieu)) +
  geom_boxplot(
    alpha         = 0.72,
    outlier.color = "grey55",
    outlier.size  = 0.75,
    outlier.alpha = 0.45,
    width         = 0.48
  ) +
  geom_jitter(width = 0.14, alpha = 0.07,
              size = 0.55, color = "grey35") +
  geom_point(
    data  = df_moyennes,
    aes(x = milieu, y = moy),
    shape = 18,
    size  = 4.5,
    color = COULEUR_ACCENT
  ) +
  geom_text(
    data     = df_moyennes,
    aes(x    = milieu, y = moy,
        label = paste0("Moy. = ", round(moy, 1))),
    vjust    = -0.9,
    color    = COULEUR_ACCENT,
    size     = 3.5,
    fontface = "bold"
  ) +
  annotate(
    "text",
    x        = 1.5,
    y        = max(df_menages$taille) * 0.94,
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
      "n = ", comma(nrow(df_menages)),
      " ménages | Losange = Moyenne | Test Wilcoxon-Mann-Whitney"
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
# TÂCHE 6 — TABLEAU GTSUMMARY STRATIFIÉ PAR MILIEU
# =============================================================================
cat("\n=== TÂCHE 6 : Tableau gtsummary ===\n")

# Constitution du jeu de données pour le tableau
effectifs_hhid <- df_brut %>%
  group_by(hhid) %>%
  summarise(taille_menage = n(), .groups = "drop")

df_tableau <- df_individus %>%
  transmute(
    milieu        = milieu,
    age           = as.numeric(s1q4),
    genre         = factor(as.numeric(s1q2),
                           levels = c(1, 2),
                           labels = c("Masculin", "Féminin")),
    taille_menage = taille_menage
  ) %>%
  filter(!is.na(milieu))

tbl_gt <- df_tableau %>%
  tbl_summary(
    by       = milieu,
    include  = c(age, genre, taille_menage),
    statistic = list(
      all_continuous()  ~ "{mean} ({sd})\nMéd. {median} [{p25} ; {p75}]",
      all_categorical() ~ "{n} ({p}%)"
    ),
    label = list(
      age           ~ "Âge (années)",
      genre         ~ "Sexe",
      taille_menage ~ "Taille du ménage (membres)"
    ),
    digits       = list(all_continuous() ~ 1,
                        all_categorical() ~ c(0, 1)),
    missing      = "ifany",
    missing_text = "Non renseigné"
  ) %>%
  add_overall(last = FALSE, col_label = "**Ensemble**") %>%
  add_p(
    test = list(
      age           ~ "wilcox.test",
      genre         ~ "chisq.test",
      taille_menage ~ "wilcox.test"
    ),
    pvalue_fun = ~style_pvalue(.x, digits = 3)
  ) %>%
  add_n() %>%
  modify_header(
    label   ~ "**Variable**",
    stat_0  ~ "**Ensemble**\nN = {N}",
    stat_1  ~ "**Urbain**\nN = {n}",
    stat_2  ~ "**Rural**\nN = {n}",
    p.value ~ "**p-valeur**"
  ) %>%
  modify_spanning_header(
    c(stat_1, stat_2) ~ "**Milieu de résidence**"
  ) %>%
  modify_caption(
    "**Tableau 1. Profil sociodémographique par milieu de résidence**\nSource : NBS Nigeria, GHS-Panel Wave 4 (2018/19)"
  ) %>%
  modify_footnote(
    p.value ~ "Test Wilcoxon-Mann-Whitney (variables continues) ; Chi-deux (variables catégorielles)"
  ) %>%
  bold_labels() %>%
  bold_p(t = 0.05)

# Export HTML
tbl_gt %>%
  as_gt() %>%
  gt::gtsave("outputs/tableau_gtsummary.html")
cat("  -> tableau_gtsummary.html\n")

cat("\n====== Toutes les analyses sont terminées ======\n")
cat("Fichiers dans outputs/ :\n")
for (f in list.files("outputs/")) cat("  ->", f, "\n")
