# =============================================================================
# LIVRABLE 1 — PYRAMIDE DES ÂGES ANNOTÉE
# GHSP Nigeria — Wave 4 (2018/19) | sect1_harvestw4
# Variables : âge (s1q4), sexe (s1q2)
# =============================================================================


library(haven)
library(dplyr)
library(ggplot2)
library(scales)


# -----------------------------------------------------------------------------
# 1. CHARGEMENT ET PRÉPARATION
# -----------------------------------------------------------------------------
sect1 <- read_dta("DATA/NGA_2018_GHSP-W4_v03_M_Stata12/sect1_harvestw4.dta")

pyramide <- sect1 %>%
  select(age = s1q4, sexe = s1q2) %>%
  filter(!is.na(age), !is.na(sexe)) %>%
  mutate(
    sexe = case_when(
      as.numeric(sexe) == 1 ~ "Homme",
      as.numeric(sexe) == 2 ~ "Femme",
      TRUE                  ~ NA_character_
    ),
    groupe_age = cut(
      age,
      breaks = seq(0, 100, by = 5),
      right  = FALSE,
      labels = paste0(seq(0, 95, by = 5), "-", seq(4, 99, by = 5))
    )
  ) %>%
  filter(!is.na(groupe_age), !is.na(sexe))

# Effectifs par groupe d'âge et sexe
data_pyramide <- pyramide %>%
  group_by(groupe_age, sexe) %>%
  summarise(effectif = n(), .groups = "drop") %>%
  mutate(effectif_plot = ifelse(sexe == "Homme", -effectif, effectif))

max_eff <- max(data_pyramide$effectif)

# Statistiques pour annotations
n_total  <- nrow(pyramide)
n_hommes <- sum(pyramide$sexe == "Homme")
n_femmes <- sum(pyramide$sexe == "Femme")
age_med  <- median(pyramide$age, na.rm = TRUE)
pct_0_14 <- round(mean(pyramide$age < 15) * 100, 1)
pct_15_64 <- round(mean(pyramide$age >= 15 & pyramide$age < 65) * 100, 1)
pct_65p  <- round(mean(pyramide$age >= 65) * 100, 1)


# -----------------------------------------------------------------------------
# 2. GRAPHIQUE
# -----------------------------------------------------------------------------
p_pyramide <- ggplot(data_pyramide,
       aes(x = groupe_age, y = effectif_plot, fill = sexe)) +

  # Barres
  geom_bar(stat = "identity", width = 0.88, alpha = 0.9) +

  # Axe Y symétrique avec labels positifs
  scale_y_continuous(
    limits = c(-max_eff * 1.1, max_eff * 1.1),
    breaks = pretty(c(-max_eff, max_eff), n = 8),
    labels = function(x) scales::comma(abs(x))
  ) +

  # Ligne centrale
  geom_hline(yintercept = 0, color = "black", linewidth = 0.5) +

  # Couleurs
  scale_fill_manual(values = c("Homme" = "#2C7BB6", "Femme" = "#D7191C")) +

  coord_flip() +

  # ── Annotations principales ───────────────────────────────────────────────

  # Étiquettes côtés
  annotate("text",
           x = nlevels(data_pyramide$groupe_age) + 1.2,
           y = -max_eff * 0.55,
           label = paste0("◄ Hommes\nn = ", scales::comma(n_hommes)),
           color = "#2C7BB6", fontface = "bold", size = 3.8, hjust = 0.5) +

  annotate("text",
           x = nlevels(data_pyramide$groupe_age) + 1.2,
           y = max_eff * 0.55,
           label = paste0("Femmes ►\nn = ", scales::comma(n_femmes)),
           color = "#D7191C", fontface = "bold", size = 3.8, hjust = 0.5) +

  # Ligne médiane d'âge
  geom_hline(yintercept = 0, color = "black", linewidth = 0.4) +

  # Encadré : structure par grands groupes d'âge
  annotate("rect",
           xmin = 1, xmax = 6.5,
           ymin = max_eff * 0.55, ymax = max_eff * 1.08,
           fill = "white", color = "grey70", alpha = 0.9) +

  annotate("text",
           x = 4.5, y = max_eff * 0.82,
           label = paste0(
             "Structure par âge\n",
             "0-14 ans  : ", pct_0_14,  "%\n",
             "15-64 ans : ", pct_15_64, "%\n",
             "65+ ans   : ", pct_65p,   "%\n",
             "Âge méd.  : ", age_med,   " ans"
           ),
           size = 3.2, color = "grey20", hjust = 0.5, fontface = "italic") +

  # Annotation vague
  annotate("text",
           x = 1, y = -max_eff * 0.98,
           label = "Wave 4 — Post-Harvest\nJanvier/Février 2019",
           size = 3, color = "grey40", hjust = 0, fontface = "italic") +

  labs(
    title    = "Pyramide des âges — GHSP Nigeria Wave 4 (2018/19)",
    subtitle = paste0("n = ", scales::comma(n_total),
                      " individus | Groupes quinquennaux | Post-Harvest Visit"),
    x        = "Groupe d'âge (années)",
    y        = "Effectif",
    caption  = "Source : National Bureau of Statistics Nigeria, GHSP-Panel Wave 4 (2018/19)"
  ) +

  theme_minimal(base_size = 12) +
  theme(
    plot.title         = element_text(face = "bold", size = 14),
    plot.subtitle      = element_text(color = "grey40", size = 10),
    plot.caption       = element_text(color = "grey50", size = 8),
    legend.position    = "none",
    panel.grid.minor   = element_blank(),
    panel.grid.major.x = element_line(color = "grey90"),
    axis.text.y        = element_text(size = 7.5)
  )

p_pyramide


# -----------------------------------------------------------------------------
# 3. EXPORT
# -----------------------------------------------------------------------------
ggsave(
  filename = "output/analyse_agel1.png",
  plot     = p_pyramide,
  width    = 10,
  height   = 8,
  dpi      = 300
)


