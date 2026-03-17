# =============================================================================
# PYRAMIDE DES ÂGES PAR SEXE - sect1_harvestw4
# Variable âge : s1q4a | Variable sexe : s1q2 (1=Male, 2=Female)
# =============================================================================

library(haven)
library(dplyr)
library(ggplot2)
library(scales)


# -----------------------------------------------------------------------------
# 1. CHARGEMENT ET PRÉPARATION
# -----------------------------------------------------------------------------
sect1 <- read_dta("DATA/NGA_2018_GHSP-W4_v03_M_Stata12/sect1_harvestw4.dta")

# Préparer les données
pyramide <- sect1 %>%
  select(age = s1q4, sexe = s1q2) %>%
  filter(!is.na(age), !is.na(sexe)) %>%
  mutate(
    # Convertir le sexe en label lisible
    sexe = case_when(
      as.numeric(sexe) == 1 ~ "Homme",
      as.numeric(sexe) == 2 ~ "Femme",
      TRUE ~ NA_character_
    ),
    # Créer les groupes d'âge de 5 ans
    groupe_age = cut(
      age,
      breaks = seq(0, max(age, na.rm = TRUE) + 5, by = 5),
      right  = FALSE,
      labels = paste0(
        seq(0, max(age, na.rm = TRUE), by = 5), "-",
        seq(4, max(age, na.rm = TRUE) + 4, by = 5)
      )
    )
  ) %>%
  filter(!is.na(groupe_age), !is.na(sexe))

# Calculer les effectifs par groupe d'âge et sexe
data_pyramide <- pyramide %>%
  group_by(groupe_age, sexe) %>%
  summarise(effectif = n(), .groups = "drop") %>%
  mutate(
    # Les hommes à gauche (valeurs négatives), femmes à droite
    effectif_plot = ifelse(sexe == "Homme", -effectif, effectif)
  )

# Valeur max pour les limites de l'axe X
max_eff <- max(data_pyramide$effectif)


# -----------------------------------------------------------------------------
# 2. PYRAMIDE DES ÂGES AVEC ggplot2
# -----------------------------------------------------------------------------
ggplot(data_pyramide,
       aes(x = groupe_age,
           y = effectif_plot,
           fill = sexe)) +

  geom_bar(stat = "identity", width = 0.85, alpha = 0.9) +

  # Axe Y symétrique avec labels positifs des deux côtés
  scale_y_continuous(
    limits = c(-max_eff * 1.05, max_eff * 1.05),
    breaks = pretty(c(-max_eff, max_eff), n = 8),
    labels = function(x) scales::comma(abs(x))
  ) +

  # Ligne centrale
  geom_hline(yintercept = 0, color = "black", linewidth = 0.4) +

  # Couleurs
  scale_fill_manual(values = c("Homme" = "#2C7BB6", "Femme" = "#D7191C")) +

  # Retourner les axes pour avoir les groupes d'âge en vertical
  coord_flip() +

  # Annotations : étiquettes Hommes / Femmes
  annotate("text", x = nlevels(data_pyramide$groupe_age) + 0.8,
           y = -max_eff * 0.5,
           label = "◄ Hommes", color = "#2C7BB6",
           fontface = "bold", size = 4) +
  annotate("text", x = nlevels(data_pyramide$groupe_age) + 0.8,
           y = max_eff * 0.5,
           label = "Femmes ►", color = "#D7191C",
           fontface = "bold", size = 4) +

  labs(
    title    = "Pyramide des âges — GHSP Nigeria Wave 4 (2018/19)",
    subtitle = paste0("n = ", scales::comma(nrow(pyramide)),
                      " individus | Groupes de 5 ans"),
    x        = "Groupe d'âge (années)",
    y        = "Effectif",
    fill     = "Sexe",
    caption  = "Source : NBS Nigeria, GHSP-Panel Wave 4 (2018/19)"
  ) +

  theme_minimal(base_size = 12) +
  theme(
    plot.title      = element_text(face = "bold", size = 14),
    plot.subtitle   = element_text(color = "grey40", size = 11),
    plot.caption    = element_text(color = "grey50", size = 9),
    legend.position = "none",   # Les annotations remplacent la légende
    panel.grid.minor = element_blank(),
    axis.text.y     = element_text(size = 8)
  )
ggsave("output/pyramide_age.png", width = 14, height = 5, dpi = 300)
