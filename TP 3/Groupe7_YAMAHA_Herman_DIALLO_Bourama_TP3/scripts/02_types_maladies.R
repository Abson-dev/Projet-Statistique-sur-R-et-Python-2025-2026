# =============================================================================
# Analyse 3 - Script 02 : Types de maladies déclarées (s4aq3b_1)
# Tâche 14 : Diagramme en barres horizontales des 10 affections les plus
#            fréquentes, coloré par catégorie (infectieuse, traumatique,
#            chronique)
# Auteurs   : Groupe 7 - Herman YAMAHA | Bourama DIALLO
# Données   : Nigeria GHS Panel - Wave 4 (2018), Post-Harvest
# =============================================================================

library(haven)
library(dplyr)
library(ggplot2)
library(forcats)
library(scales)

# --- 1. Chargement -----------------------------------------------------------

df_health <- readRDS("data/df_health_base.rds")

# --- 2. Dictionnaire des maladies (codes LABC wave 4) -------------------------

maladie_labels <- c(
  "1"  = "Paludisme",
  "2"  = "Tuberculose",
  "3"  = "Fièvre jaune",
  "4"  = "Typhoïde",
  "5"  = "Choléra",
  "6"  = "Diarrhée",
  "7"  = "Méningite",
  "8"  = "Varicelle",
  "9"  = "Pneumonie",
  "10" = "Rhume commun",
  "11" = "Blessure/Traumatisme",
  "12" = "Autre",
  "13" = "Hypertension",
  "14" = "Grippe",
  "15" = "Rhinite/Catarrhe",
  "16" = "Toux",
  "17" = "Céphalée",
  "18" = "Diabète",
  "19" = "Ver de Guinée",
  "20" = "Dysenterie",
  "21" = "Infection cutanée",
  "22" = "Rougeole",
  "23" = "Infection urinaire",
  "24" = "Douleurs articulaires",
  "25" = "Fièvre non spécifiée",
  "26" = "Trouble digestif",
  "27" = "Faiblesse/Fatigue"
)

# Catégorisation par type
categorie_maladie <- c(
  "1"  = "Infectieuse",
  "2"  = "Infectieuse",
  "3"  = "Infectieuse",
  "4"  = "Infectieuse",
  "5"  = "Infectieuse",
  "6"  = "Infectieuse",
  "7"  = "Infectieuse",
  "8"  = "Infectieuse",
  "9"  = "Infectieuse",
  "10" = "Infectieuse",
  "11" = "Traumatique",
  "12" = "Autre",
  "13" = "Chronique",
  "14" = "Infectieuse",
  "15" = "Infectieuse",
  "16" = "Infectieuse",
  "17" = "Autre",
  "18" = "Chronique",
  "19" = "Infectieuse",
  "20" = "Infectieuse",
  "21" = "Infectieuse",
  "22" = "Infectieuse",
  "23" = "Infectieuse",
  "24" = "Chronique",
  "25" = "Infectieuse",
  "26" = "Autre",
  "27" = "Autre"
)

# --- 3. Préparation des données ----------------------------------------------

df_maladies <- df_health %>%
  filter(!is.na(s4aq3b_1), s4aq3 == 1) %>%
  mutate(
    code_str    = as.character(as.integer(s4aq3b_1)),
    maladie     = maladie_labels[code_str],
    categorie   = categorie_maladie[code_str]
  ) %>%
  filter(!is.na(maladie))

# Top 10 maladies
top10 <- df_maladies %>%
  count(maladie, categorie, sort = TRUE) %>%
  mutate(pct = n / sum(n) * 100) %>%
  slice_head(n = 10) %>%
  mutate(maladie = fct_reorder(maladie, n))

cat("=== Top 10 maladies déclarées ===\n")
print(top10)

# Sauvegarde tableau
write.csv(top10, "outputs/tables/02_top10_maladies.csv",
          row.names = FALSE)

# --- 4. Graphique barres horizontales ----------------------------------------

couleurs_cat <- c(
  "Infectieuse"  = "#3A86FF",
  "Chronique"    = "#FF6B6B",
  "Traumatique"  = "#FFBE0B",
  "Autre"        = "#8338EC"
)

p_maladies <- ggplot(top10,
                     aes(x = maladie, y = n,
                         fill = categorie)) +
  geom_col(width = 0.7) +
  geom_text(aes(label = paste0(round(pct, 1), "%")),
            hjust = -0.15, size = 3.8, fontface = "bold") +
  coord_flip() +
  scale_fill_manual(values = couleurs_cat, name = "Catégorie") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.18))) +
  labs(
    title    = "Dix affections les plus fréquemment déclarées",
    subtitle = "Individus ayant déclaré une maladie ou blessure — Wave 4 (2018)",
    x        = NULL,
    y        = "Nombre d'individus",
    caption  = "Source : Nigeria GHS Panel W4, sect4a_harvestw4"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title       = element_text(face = "bold", size = 14),
    plot.subtitle    = element_text(color = "grey40", size = 11),
    legend.position  = "top",
    panel.grid.major.y = element_blank(),
    panel.grid.minor   = element_blank()
  )

ggsave("outputs/figures/02_types_maladies.png", p_maladies,
       width = 10, height = 6, dpi = 300)
cat("Figure sauvegardée : outputs/figures/02_types_maladies.png\n")

# --- 5. Répartition par catégorie --------------------------------------------

df_cat <- df_maladies %>%
  count(categorie, sort = TRUE) %>%
  mutate(pct = n / sum(n) * 100)

cat("\n=== Répartition par catégorie ===\n")
print(df_cat)

cat("\n=== Script 02 terminé ===\n")
