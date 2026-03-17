# =============================================================================
# Analyse 3 – Script 04 : Distribution des dépenses de santé
# Tâche 16 : Histogramme avec échelle log, statistiques par décile,
#            boxplot par type de prestataire, identification des outliers.
#
# Variables clés :
#   s4aq9    = montant payé pour la première consultation (Naira)
#   s4aq14   = montant payé pour médicaments sans ordonnance (Naira)
#   s4aq17   = montant payé pour hospitalisation (Naira)
#   s4aq6a   = type de praticien consulté (pour le boxplot)
#
# Méthode pour les outliers : règle de Tukey étendue (IQR × 3)
#   Seuil = Q3 + 3 × (Q3 - Q1)
#
# Auteurs  : Groupe 7 – Herman YAMAHA | Bourama DIALLO
# Données  : Nigeria GHS Panel – Wave 4 (2018), Post-Harvest
# =============================================================================

library(haven)
library(dplyr)
library(ggplot2)
library(scales)
library(patchwork)

# --------------------------------------------------------------------------
# 1. CHARGEMENT
# --------------------------------------------------------------------------

df_health <- readRDS("data/df_health_base.rds")

# --------------------------------------------------------------------------
# 2. CONSTRUCTION DE LA DÉPENSE TOTALE DE SANTÉ
# --------------------------------------------------------------------------
# La dépense totale agrège trois composantes :
#   – Consultation (s4aq9)
#   – Médicaments sans ordonnance (s4aq14)
#   – Frais d'hospitalisation (s4aq17)
# Les valeurs manquantes (NA) sont remplacées par 0 : si la variable est
# manquante, l'individu n'a pas engagé ce type de dépense.

df_depenses <- df_health %>%
  mutate(
    dep_consultation = if_else(is.na(s4aq9),  0, as.numeric(s4aq9)),
    dep_medicaments  = if_else(is.na(s4aq14), 0, as.numeric(s4aq14)),
    dep_hopital      = if_else(is.na(s4aq17), 0, as.numeric(s4aq17)),
    dep_totale       = dep_consultation + dep_medicaments + dep_hopital
  ) %>%
  filter(dep_totale > 0)   # conserver uniquement ceux ayant dépensé

cat("Individus avec dépenses > 0 :", nrow(df_depenses), "\n")

# --------------------------------------------------------------------------
# 3. STATISTIQUES DESCRIPTIVES PAR DÉCILE
# --------------------------------------------------------------------------
# Découper la distribution en 10 groupes d'effectifs égaux (ntile)
# et calculer les indicateurs de position pour chaque décile.

df_depenses <- df_depenses %>%
  mutate(decile = ntile(dep_totale, 10))

stats_decile <- df_depenses %>%
  group_by(decile) %>%
  summarise(
    n       = n(),
    min     = min(dep_totale),
    Q25     = quantile(dep_totale, 0.25),
    mediane = median(dep_totale),
    moyenne = round(mean(dep_totale)),
    Q75     = quantile(dep_totale, 0.75),
    max     = max(dep_totale),
    .groups = "drop"
  )

cat("\n=== Statistiques des dépenses par décile (Naira) ===\n")
print(stats_decile, n = 10)

write.csv(stats_decile, "outputs/tables/04_depenses_decile.csv",
          row.names = FALSE)

# --------------------------------------------------------------------------
# 4. IDENTIFICATION DES VALEURS ABERRANTES (règle de Tukey étendue)
# --------------------------------------------------------------------------
# La règle standard utilise IQR × 1.5 ; on adopte ici IQR × 3 pour
# n'identifier que les outliers extrêmes (dépenses très anormales).

q1  <- quantile(df_depenses$dep_totale, 0.25)
q3  <- quantile(df_depenses$dep_totale, 0.75)
iqr <- q3 - q1
seuil_haut <- q3 + 3 * iqr

outliers <- df_depenses %>% filter(dep_totale > seuil_haut)

cat("\n=== Valeurs aberrantes (> Q3 + 3×IQR) ===\n")
cat("Seuil   :", format(round(seuil_haut), big.mark = " "), "Naira\n")
cat("Outliers:", nrow(outliers), "individus\n")
cat("Dépense max :", format(max(df_depenses$dep_totale), big.mark = " "),
    "Naira\n")

# --------------------------------------------------------------------------
# 5. HISTOGRAMME AVEC ÉCHELLE LOGARITHMIQUE
# --------------------------------------------------------------------------
# L'échelle log est indispensable ici : les dépenses s'étalent de
# quelques dizaines à plusieurs millions de Naira (distribution très
# asymétrique à droite). Sur cette échelle, la distribution devient
# approximativement symétrique et lisible.
#
# CORRECTION :
# – L'annotation de la médiane est placée dans le coin supérieur GAUCHE
#   (x = quantile 10 %, y = 95 % du max) pour éviter tout chevauchement
#   avec les barres de l'histogramme.

med_val <- median(df_depenses$dep_totale)
y_max   <- layer_scales(
              ggplot(df_depenses, aes(x = dep_totale)) +
              geom_histogram(bins = 40) +
              scale_x_log10()
            )$y$get_limits()[2]

# Position de l'annotation : à gauche de la médiane, en haut du graphique
x_annot <- quantile(df_depenses$dep_totale, 0.02)   # extrême gauche
y_annot_frac <- 0.90                                 # 90 % de la hauteur

p_histo <- ggplot(df_depenses, aes(x = dep_totale)) +
  geom_histogram(
    bins  = 40,
    fill  = "#3A86FF",
    color = "white",
    alpha = 0.85
  ) +
  # Ligne verticale sur la médiane
  geom_vline(
    xintercept = med_val,
    linetype   = "dashed",
    color      = "#E63946",
    linewidth  = 1
  ) +
  scale_x_log10(labels = label_comma(big.mark = " ")) +
  scale_y_continuous(labels = label_comma()) +
  # CORRECTION : annotation en haut à gauche, bien dégagée des barres
  annotate(
    "label",                             # "label" ajoute un fond blanc
    x          = x_annot,
    y          = Inf,
    label      = paste0("Médiane : ",
                        format(round(med_val), big.mark = " ")),
    hjust      = 0,
    vjust      = 1.2,
    color      = "#E63946",
    size       = 4,
    fill       = "white",
    label.size = 0.3,
    fontface   = "bold"
  ) +
  labs(
    title    = "Distribution des dépenses de santé (échelle logarithmique)",
    subtitle = "Individus ayant engagé des dépenses — Wave 4 (2018)",
    x        = "Dépense totale de santé (Naira, échelle log)",
    y        = "Nombre d'individus",
    caption  = "Source : Nigeria GHS Panel W4 | Ligne rouge pointillée = médiane"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title    = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(color = "grey40")
  )

ggsave("outputs/figures/04a_depenses_histogramme.png", p_histo,
       width = 10, height = 5.5, dpi = 300)
cat("Graphique sauvegardé : outputs/figures/04a_depenses_histogramme.png\n")

# --------------------------------------------------------------------------
# 6. BOXPLOT DES DÉPENSES PAR TYPE DE PRESTATAIRE
# --------------------------------------------------------------------------
# Les valeurs extrêmes (> seuil_haut) sont exclues pour que la forme
# des boîtes soit lisible. L'échelle log est maintenue.

groupe_praticien <- c(
  "0"  = "Aucun recours",
  "1"  = "Tradipraticien",
  "2"  = "Hôpital / Clinique",
  "3"  = "Hôpital / Clinique",
  "4"  = "Hôpital / Clinique",
  "5"  = "Hôpital / Clinique",
  "6"  = "Hôpital / Clinique",
  "7"  = "Pharmacie",
  "8"  = "Pharmacie",
  "9"  = "Tradipraticien",
  "10" = "Tradipraticien",
  "11" = "Pharmacie",
  "13" = "Autre",
  "14" = "Agent de santé comm.",
  "15" = "Agent de santé comm."
)

df_box <- df_depenses %>%
  mutate(
    code_prat = as.character(
      if_else(is.na(s4aq6a) | s4aq6a == 0, 0, as.integer(s4aq6a))
    ),
    praticien_groupe = groupe_praticien[code_prat]
  ) %>%
  filter(
    !is.na(praticien_groupe),
    praticien_groupe != "Aucun recours",   # pas de dépense associée
    dep_totale <= seuil_haut               # outliers extrêmes exclus
  )

p_boxplot <- ggplot(
  df_box,
  aes(
    x    = reorder(praticien_groupe, dep_totale, FUN = median),
    y    = dep_totale,
    fill = praticien_groupe
  )
) +
  geom_boxplot(
    outlier.shape = 21,
    outlier.alpha = 0.3,
    outlier.size  = 1.2,
    show.legend   = FALSE,
    width         = 0.6
  ) +
  # Médiane annotée sous forme de point rouge
  stat_summary(
    fun   = median,
    geom  = "point",
    shape = 18,
    size  = 3,
    color = "#E63946"
  ) +
  scale_y_log10(labels = label_comma(big.mark = " ")) +
  scale_fill_brewer(palette = "Set2") +
  coord_flip() +
  labs(
    title    = "Dépenses de santé par type de prestataire",
    subtitle = "Échelle log — outliers extrêmes exclus (> Q3 + 3×IQR) | losange rouge = médiane",
    x        = NULL,
    y        = "Dépense totale de santé (Naira, échelle log)",
    caption  = "Source : Nigeria GHS Panel W4"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title         = element_text(face = "bold", size = 14),
    plot.subtitle      = element_text(color = "grey40", size = 10),
    panel.grid.major.y = element_blank()
  )

ggsave("outputs/figures/04b_depenses_boxplot_prestataire.png", p_boxplot,
       width = 10, height = 5.5, dpi = 300)
cat("Graphique sauvegardé : outputs/figures/04b_depenses_boxplot_prestataire.png\n")

# Sauvegarde du seuil pour usage dans script 06
saveRDS(
  list(seuil_haut = seuil_haut, groupe_praticien = groupe_praticien),
  "data/params_depenses.rds"
)

cat("\n=== Script 04 terminé avec succès ===\n")
