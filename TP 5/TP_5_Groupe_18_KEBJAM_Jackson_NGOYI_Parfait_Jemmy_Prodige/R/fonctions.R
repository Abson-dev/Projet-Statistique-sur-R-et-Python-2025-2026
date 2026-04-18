# ============================================================================
# FONCTIONS POUR LES ANALYSES AGRICOLES
# ============================================================================

# Chargement des bibliothèques nécessaires
library(dplyr)
library(ggplot2)
library(scales)
library(stringr)
library(forcats)
library(tidyr)

# Palette de couleurs pour les types de culture
palette_type <- c(
  "céréale"    = "#2E86AB",
  "tubercule"     = "#A23B72",
  "légumineuse"    = "#F18F01",
  "culture de rente" = "#3B4F2B",
  "autres"     = "#888888"
)

# Palette pour les zones
palette_zone <- c("Urbain" = "#2E86AB", "Rural" = "#A23B72")

# Mapping des codes état vers noms
state_labels <- c(
  "1" = "Abia", "2" = "Adamawa", "3" = "Akwa Ibom", "4" = "Anambra",
  "5" = "Bauchi", "6" = "Bayelsa", "7" = "Benue", "8" = "Borno",
  "9" = "Cross River", "10" = "Delta", "11" = "Ebonyi", "12" = "Edo",
  "13" = "Ekiti", "14" = "Enugu", "15" = "Gombe", "16" = "Imo",
  "17" = "Jigawa", "18" = "Kaduna", "19" = "Kano", "20" = "Katsina",
  "21" = "Kebbi", "22" = "Kogi", "23" = "Kwara", "24" = "Lagos",
  "25" = "Nasarawa", "26" = "Niger", "27" = "Ogun", "28" = "Ondo",
  "29" = "Osun", "30" = "Oyo", "31" = "Plateau", "32" = "Rivers",
  "33" = "Sokoto", "34" = "Taraba", "35" = "Yobe", "36" = "Zamfara", "37" = "FCT"
)

#' Nettoyer les labels des cultures
clean_crop_label <- function(crop_name) {
  str_trim(str_remove(crop_name, "^[0-9]+\\.\\s*")) %>% str_to_title()
}

#' Calculer l'intervalle de confiance de Wilson
wilson_ci <- function(n, total, conf_level = 0.95) {
  z <- qnorm((1 + conf_level) / 2)
  p <- n / total
  denom <- 1 + z^2 / total
  centre <- (p + z^2 / (2 * total)) / denom
  erreur <- z * sqrt(p * (1 - p) / total + z^2 / (4 * total^2)) / denom
  data.frame(ic_lo = centre - erreur, ic_hi = centre + erreur)
}

#' Créer un boxplot pondéré par État
plot_yield_by_state <- function(data, crop_name, color_fill, title) {
  data %>%
    filter(culture == crop_name) %>%
    mutate(state_name = fct_reorder(state_name, rendement_kg_ha, median, .na_rm = TRUE)) %>%
    ggplot(aes(x = state_name, y = rendement_kg_ha, weight = wt_wave4)) +
    geom_boxplot(fill = color_fill, outlier.size = 0.4, alpha = 0.85, linewidth = 0.4) +
    coord_flip() +
    scale_y_continuous(labels = comma_format()) +
    labs(title = title, x = NULL, y = "Rendement (kg/ha)",
         caption = "Source : Nigeria LSMS-ISA, 2018-2019 vague 4") +
    theme_minimal(base_size = 11) +
    theme(axis.text.y = element_text(size = 8), panel.grid.major.y = element_blank())
}

#' Calculer les statistiques pondérées par groupe
weighted_stats <- function(data, group_var, value_var, weight_var) {
  data %>%
    group_by({{ group_var }}) %>%
    summarise(
      n = n(),
      n_pondere = sum({{ weight_var }}, na.rm = TRUE),
      mediane = median({{ value_var }}, na.rm = TRUE),
      moyenne_ponderee = weighted.mean({{ value_var }}, {{ weight_var }}, na.rm = TRUE),
      q25 = quantile({{ value_var }}, 0.25, na.rm = TRUE),
      q75 = quantile({{ value_var }}, 0.75, na.rm = TRUE),
      .groups = "drop"
    )
}