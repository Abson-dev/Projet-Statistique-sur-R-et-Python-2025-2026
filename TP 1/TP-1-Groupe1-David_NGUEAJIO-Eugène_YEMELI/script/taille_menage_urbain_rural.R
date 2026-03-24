# =============================================================================
# COMPARAISON TAILLE DES MÉNAGES - URBAIN vs RURAL
# sect1_harvestw4
# 1. Calcul de la taille des ménages
# 2. Boxplot groupé
# 3. Test de Wilcoxon-Mann-Whitney
# 4. Taille d'effet : r de rang (r = Z / sqrt(N))
# =============================================================================


library(haven)
library(dplyr)
library(ggplot2)
library(scales)


# -----------------------------------------------------------------------------
# 1. CHARGEMENT ET CALCUL DE LA TAILLE DES MÉNAGES
# -----------------------------------------------------------------------------
sect1 <- read_dta("DATA/NGA_2018_GHSP-W4_v03_M_Stata12/sect1_harvestw4.dta")

# Taille du ménage = nombre de membres par hhid
# On récupère aussi le secteur (urbain/rural) depuis sect1
# Un ménage a un seul secteur → on prend la première valeur non-NA
taille_menage <- sect1 %>%
  group_by(hhid) %>%
  summarise(
    taille  = n(),
    secteur = first(as.numeric(sector))
  ) %>%
  ungroup() %>%
  filter(!is.na(secteur)) %>%
  mutate(
    secteur_label = case_when(
      secteur == 1 ~ "Urbain",
      secteur == 2 ~ "Rural"
    )
  )

cat("Nombre de ménages :", nrow(taille_menage), "\n")
cat("Urbain :", sum(taille_menage$secteur_label == "Urbain"), "ménages\n")
cat("Rural  :", sum(taille_menage$secteur_label == "Rural"),  "ménages\n")


# -----------------------------------------------------------------------------
# 2. STATISTIQUES DESCRIPTIVES PAR GROUPE
# -----------------------------------------------------------------------------
stats_groupe <- taille_menage %>%
  group_by(secteur_label) %>%
  summarise(
    N        = n(),
    Moyenne  = round(mean(taille), 2),
    Mediane  = median(taille),
    Q1       = quantile(taille, 0.25),
    Q3       = quantile(taille, 0.75),
    Ecart_type = round(sd(taille), 2),
    Min      = min(taille),
    Max      = max(taille)
  )

cat("\n==============================================\n")
cat("   STATISTIQUES PAR SECTEUR\n")
cat("==============================================\n")
print(stats_groupe)


# -----------------------------------------------------------------------------
# 3. TEST DE WILCOXON-MANN-WHITNEY
# -----------------------------------------------------------------------------
urbain <- taille_menage %>% filter(secteur_label == "Urbain") %>% pull(taille)
rural  <- taille_menage %>% filter(secteur_label == "Rural")  %>% pull(taille)

wilcox_test <- wilcox.test(urbain, rural,
                            alternative = "two.sided",
                            conf.int    = TRUE,
                            conf.level  = 0.95)

cat("\n==============================================\n")
cat("   TEST DE WILCOXON-MANN-WHITNEY\n")
cat("==============================================\n")
cat("Statistique W       :", wilcox_test$statistic, "\n")
cat("p-value             :", format(wilcox_test$p.value, scientific = TRUE), "\n")
cat("Différence de loc.  :", round(wilcox_test$estimate, 3), "\n")
cat("IC 95%              : [",
    round(wilcox_test$conf.int[1], 3), ";",
    round(wilcox_test$conf.int[2], 3), "]\n")

if (wilcox_test$p.value < 0.05) {
  cat("Conclusion : Ifférence significative (p < 0.05)\n")
} else {
  cat("Conclusion : Différence non significative (p ≥ 0.05)\n")
}


# -----------------------------------------------------------------------------
# 4. TAILLE D'EFFET : r DE RANG
#    Formule : r = Z / sqrt(N)
#    Interprétation : |r| < 0.1 = négligeable
#                     |r| < 0.3 = petit
#                     |r| < 0.5 = moyen
#                     |r| >= 0.5 = grand  (Cohen, 1992)
# -----------------------------------------------------------------------------

# Calculer Z à partir de la p-value bilatérale
N_total <- length(urbain) + length(rural)
Z       <- qnorm(wilcox_test$p.value / 2)   # Z négatif car p-value / 2
r_rang  <- abs(Z) / sqrt(N_total)

cat("\n==============================================\n")
cat("   TAILLE D'EFFET - r de rang\n")
cat("==============================================\n")
cat("N total :", N_total, "\n")
cat("Z       :", round(Z, 4), "\n")
cat("r       :", round(r_rang, 4), "\n")

interpretation <- case_when(
  r_rang < 0.1 ~ "Négligeable",
  r_rang < 0.3 ~ "Petit",
  r_rang < 0.5 ~ "Moyen",
  TRUE         ~ "Grand"
)
cat("Interprétation :", interpretation,
    "(seuils de Cohen : 0.1 / 0.3 / 0.5)\n")


# -----------------------------------------------------------------------------
# 5. BOXPLOT GROUPÉ
# -----------------------------------------------------------------------------

# Préparer les annotations pour le graphique
label_wilcox <- paste0(
  "Wilcoxon W = ", round(wilcox_test$statistic, 0),
  "\np = ", format(wilcox_test$p.value, scientific = TRUE, digits = 3),
  "\nr = ", round(r_rang, 3), " (", interpretation, ")"
)

# Moyennes par groupe (pour le point rouge)
moyennes <- taille_menage %>%
  group_by(secteur_label) %>%
  summarise(moyenne = mean(taille))

ggplot(taille_menage,
       aes(x    = secteur_label,
           y    = taille,
           fill = secteur_label)) +

  # Boxplot
  geom_boxplot(alpha        = 0.75,
               outlier.color = "grey50",
               outlier.size  = 0.8,
               outlier.alpha = 0.5,
               width         = 0.5) +

  # Points individuels (jitter) pour voir la distribution
  geom_jitter(width = 0.15, alpha = 0.08, size = 0.6, color = "grey30") +

  # Point de la moyenne
  geom_point(data  = moyennes,
             aes(x = secteur_label, y = moyenne),
             shape = 18, size = 4, color = "#D7191C") +

  # Étiquette de la moyenne
  geom_text(data  = moyennes,
            aes(x = secteur_label,
                y = moyenne,
                label = paste0("Moy. = ", round(moyenne, 1))),
            vjust = -0.8, color = "#D7191C", size = 3.5, fontface = "bold") +

  # Annotation test statistique
  annotate("text",
           x     = 1.5,
           y     = max(taille_menage$taille) * 0.95,
           label = label_wilcox,
           size  = 3.5,
           color = "grey20",
           hjust = 0.5,
           vjust = 1,
           fontface = "italic") +

  # Couleurs
  scale_fill_manual(values = c("Urbain" = "#2C7BB6", "Rural" = "#D7191C")) +

  scale_y_continuous(breaks = seq(0, 35, by = 5)) +

  labs(
    title    = "Taille des ménages selon le secteur (Urbain vs Rural)",
    subtitle = paste0("n = ", scales::comma(nrow(taille_menage)),
                      " ménages | ◆ = Moyenne | Test de Wilcoxon-Mann-Whitney"),
    x        = NULL,
    y        = "Taille du ménage (nombre de membres)",
    caption  = "Source : NBS Nigeria, GHSP-Panel Wave 4 (2018/19)"
  ) +

  theme_minimal(base_size = 12) +
  theme(
    plot.title      = element_text(face = "bold", size = 14),
    plot.subtitle   = element_text(color = "grey40", size = 11),
    plot.caption    = element_text(color = "grey50", size = 9),
    legend.position = "none",
    panel.grid.minor      = element_blank(),
    panel.grid.major.x    = element_blank(),
    axis.text.x     = element_text(size = 12, face = "bold")
  )
ggsave("output/taille_menage.png", width = 14, height = 5, dpi = 300)