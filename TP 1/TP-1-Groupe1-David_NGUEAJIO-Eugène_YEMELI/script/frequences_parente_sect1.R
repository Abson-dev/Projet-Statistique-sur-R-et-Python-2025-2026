# =============================================================================
# FRÉQUENCES DU LIEN DE PARENTÉ - sect1_harvestw4
# Variable : s1q3
# Diagramme en barres horizontales ordonné + IC à 95%
# =============================================================================

# -----------------------------------------------------------------------------
# 0. PACKAGES
# -----------------------------------------------------------------------------
# install.packages(c("haven", "dplyr", "ggplot2", "broom"))

library(haven)
library(dplyr)
library(ggplot2)


# -----------------------------------------------------------------------------
# 1. CHARGEMENT ET RECODAGE
# -----------------------------------------------------------------------------
sect1 <- read_dta("DATA/NGA_2018_GHSP-W4_v03_M_Stata12/sect1_harvestw4.dta")

parenté <- sect1 %>%
  filter(!is.na(s1q3)) %>%
  mutate(
    lien = case_when(
      as.numeric(s1q3) == 1              ~ "Chef de ménage",
      as.numeric(s1q3) == 2              ~ "Conjoint(e)",
      as.numeric(s1q3) %in% c(3, 4, 5)  ~ "Enfant",
      TRUE                               ~ "Autre"
    )
  )

N_total <- nrow(parenté)
cat("N total (hors NA) :", N_total, "\n")


# -----------------------------------------------------------------------------
# 2. CALCUL DES PROPORTIONS ET IC À 95% AVEC binom.test()
# -----------------------------------------------------------------------------
freq <- parenté %>%
  count(lien) %>%
  arrange(desc(n))

# Appliquer binom.test() sur chaque modalité
resultats <- freq %>%
  rowwise() %>%
  mutate(
    test      = list(binom.test(n, N_total, conf.level = 0.95)),
    proportion = test$estimate,
    ic_bas     = test$conf.int[1],
    ic_haut    = test$conf.int[2]
  ) %>%
  ungroup() %>%
  select(lien, n, proportion, ic_bas, ic_haut) %>%
  # Ordonner par fréquence croissante pour le coord_flip
  mutate(lien = factor(lien, levels = lien[order(proportion)]))

# Affichage console
cat("\n==============================================\n")
cat("   PROPORTIONS ET IC 95% - Lien de parenté\n")
cat("==============================================\n")
resultats %>%
  mutate(
    `%`      = paste0(round(proportion * 100, 1), "%"),
    `IC 95%` = paste0("[", round(ic_bas * 100, 1), "% ; ",
                       round(ic_haut * 100, 1), "%]")
  ) %>%
  select(Modalité = lien, Effectif = n, `%`, `IC 95%`) %>%
  print()


# -----------------------------------------------------------------------------
# 3. DIAGRAMME EN BARRES HORIZONTALES
# -----------------------------------------------------------------------------
ggplot(resultats,
       aes(x = lien, y = proportion)) +

  # Barres
  geom_bar(stat = "identity",
           fill = "#2C7BB6",
           alpha = 0.85,
           width = 0.6) +

  # Intervalles de confiance
  geom_errorbar(aes(ymin = ic_bas, ymax = ic_haut),
                width  = 0.2,
                color  = "#D7191C",
                linewidth = 0.8) +

  # Étiquettes : % + effectif
  geom_text(aes(label = paste0(round(proportion * 100, 1), "%\n(n=",
                                scales::comma(n), ")")),
            hjust  = -0.15,
            size   = 3.5,
            color  = "grey20") +

  # Axe Y en pourcentage
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1),
    limits = c(0, max(resultats$ic_haut) * 1.25)
  ) +

  # Barres horizontales
  coord_flip() +

  labs(
    title    = "Lien de parenté avec le chef de ménage",
    subtitle = paste0("n = ", scales::comma(N_total),
                      " individus | Barres d'erreur = IC à 95%"),
    x        = NULL,
    y        = "Proportion (%)",
    caption  = "Source : NBS Nigeria, GHSP-Panel Wave 4 (2018/19)"
  ) +

  theme_minimal(base_size = 12) +
  theme(
    plot.title      = element_text(face = "bold", size = 14),
    plot.subtitle   = element_text(color = "grey40", size = 11),
    plot.caption    = element_text(color = "grey50", size = 9),
    panel.grid.major.y = element_blank(),
    panel.grid.minor   = element_blank(),
    axis.text.y     = element_text(size = 11, face = "bold")
  )
ggsave("output/frequence_lien.png", width = 14, height = 5, dpi = 300)

sect1 %>% count(sector)
# Taille ménage = nombre d'individus par hhid
sect1 %>% count(hhid) %>% summarise(min=min(n), max=max(n), mean=mean(n))
