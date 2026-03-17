# =============================================================================
# ANALYSE UNIVARIÉE DE L'ÂGE - sect1_harvestw4
# Variable : s1q4a (âge en années complètes)
# 1. Histogramme (binwidth = 5)
# 2. Boîte à moustaches
# 3. Statistiques descriptives
# 4. Test de normalité Shapiro-Wilk
# =============================================================================

# -----------------------------------------------------------------------------
library(haven)
library(dplyr)
library(ggplot2)
library(moments)    # pour skewness()
library(patchwork)  # pour combiner les graphiques


# -----------------------------------------------------------------------------
# 1. CHARGEMENT ET PRÉPARATION
# -----------------------------------------------------------------------------
sect1 <- read_dta("DATA/NGA_2018_GHSP-W4_v03_M_Stata12/sect1_harvestw4.dta")

# Extraire l'âge et supprimer les valeurs manquantes
age <- sect1 %>%
  select(hhid, indiv, age = s1q4) %>%
  filter(!is.na(age))

cat("Nombre d'observations (âge non manquant) :", nrow(age), "\n")
cat("Valeurs manquantes sur s1q4 :",
    sum(is.na(sect1$s1q4)), "/", nrow(sect1), "\n")


# =============================================================================
# PARTIE 1 : STATISTIQUES DESCRIPTIVES
# =============================================================================
x <- age$age

moyenne    <- mean(x)
mediane    <- median(x)
q1         <- quantile(x, 0.25)
q3         <- quantile(x, 0.75)
iqr        <- IQR(x)
ecart_type <- sd(x)
cv         <- (ecart_type / abs(moyenne)) * 100
asym       <- skewness(x)   # > 0 : asymétrie à droite | < 0 : à gauche

cat("\n============================================\n")
cat("   STATISTIQUES DESCRIPTIVES - Âge (s1q4a)\n")
cat("============================================\n")
cat("Minimum       :", min(x), "\n")
cat("Q1            :", q1, "\n")
cat("Médiane       :", mediane, "\n")
cat("Moyenne       :", round(moyenne, 2), "\n")
cat("Q3            :", q3, "\n")
cat("Maximum       :", max(x), "\n")
cat("Écart-type    :", round(ecart_type, 2), "\n")
cat("CV (%)        :", round(cv, 2), "\n")
cat("Asymétrie     :", round(asym, 4),
    ifelse(asym > 0, "→ queue à droite",
    ifelse(asym < 0, "→ queue à gauche", "→ symétrique")), "\n")


# =============================================================================
# PARTIE 2 : TEST DE NORMALITÉ SHAPIRO-WILK
# =============================================================================
# Note : Shapiro-Wilk est limité à 5000 observations.
# Si n > 5000, on tire un échantillon aléatoire de 5000.

cat("\n============================================\n")
cat("   TEST DE NORMALITÉ - Shapiro-Wilk\n")
cat("============================================\n")

if (length(x) > 5000) {
  set.seed(123)
  x_sample <- sample(x, 5000)
  cat("  n >5000 : test appliqué sur un échantillon de 5000 observations\n")
  shapiro_test <- shapiro.test(x_sample)
} else {
  shapiro_test <- shapiro.test(x)
}

cat("Statistique W :", round(shapiro_test$statistic, 4), "\n")
cat("p-value       :", format(shapiro_test$p.value, scientific = TRUE), "\n")

if (shapiro_test$p.value < 0.05) {
  cat("Conclusion    : Rejet de H0 (p < 0.05)",
      "→ La distribution n'est PAS normale\n")
} else {
  cat("Conclusion    : on-rejet de H0 (p ≥ 0.05)",
      "→ Distribution compatible avec la normalité\n")
}


# =============================================================================
# PARTIE 3 : VISUALISATIONS
# =============================================================================

# Couleurs
col_bleu  <- "#2C7BB6"
col_rouge <- "#D7191C"

# -----------------------------------------------------------------------------
# 3.1 Histogramme (binwidth = 5)
# -----------------------------------------------------------------------------
p_hist <- ggplot(age, aes(x = age)) +
  geom_histogram(binwidth = 5,
                 fill = col_bleu,
                 color = "white",
                 alpha = 0.85) +
  geom_vline(xintercept = moyenne,
             color = col_rouge, linetype = "dashed", linewidth = 0.8) +
  geom_vline(xintercept = mediane,
             color = "darkgreen", linetype = "dotted", linewidth = 0.8) +
  annotate("text", x = moyenne + 1.5, y = Inf,
           label = paste0("Moy. = ", round(moyenne, 1)),
           color = col_rouge, vjust = 2, hjust = 0, size = 3.5) +
  annotate("text", x = mediane - 1.5, y = Inf,
           label = paste0("Méd. = ", round(mediane, 1)),
           color = "darkgreen", vjust = 3.5, hjust = 1, size = 3.5) +
  labs(
    title    = "Distribution de l'âge des membres du ménage",
    subtitle = paste0("n = ", nrow(age),
                      " | Binwidth = 5 ans | CV = ", round(cv, 1), "%"),
    x        = "Âge (années)",
    y        = "Effectif"
  ) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold"))


# -----------------------------------------------------------------------------
# 3.2 Boîte à moustaches
# -----------------------------------------------------------------------------
p_box <- ggplot(age, aes(x = 1, y = age)) +
  geom_boxplot(fill = col_bleu,
               color = "navy",
               alpha = 0.7,
               outlier.color = col_rouge,
               outlier.size  = 1.5) +
  stat_summary(fun = mean,
               geom = "point",
               shape = 18,
               size = 4,
               color = col_rouge) +
  annotate("text", x = 1.45, y = moyenne,
           label = paste0("Moy. = ", round(moyenne, 1)),
           color = col_rouge, size = 3.5, hjust = 1) +
  labs(
    title    = "Boîte à moustaches - Âge",
    subtitle = paste0("Q1 = ", q1, " | Méd. = ", mediane,
                      " | Q3 = ", q3,
                      " | IQR = ", iqr),
    y        = "Âge (années)",
    x        = ""
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title   = element_text(face = "bold"),
    axis.text.x  = element_blank(),
    axis.ticks.x = element_blank()
  )


# -----------------------------------------------------------------------------
# 3.3 QQ-plot (pour visualiser la normalité)
# -----------------------------------------------------------------------------
p_qq <- ggplot(age, aes(sample = age)) +
  stat_qq(color = col_bleu, alpha = 0.4, size = 0.8) +
  stat_qq_line(color = col_rouge, linewidth = 0.8) +
  labs(
    title    = "QQ-plot - Âge vs distribution normale",
    subtitle = paste0("W = ", round(shapiro_test$statistic, 4),
                      " | p = ", format(shapiro_test$p.value, scientific = TRUE,
                                        digits = 3)),
    x        = "Quantiles théoriques",
    y        = "Quantiles observés"
  ) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold"))


# -----------------------------------------------------------------------------
# 3.4 Assemblage des 3 graphiques avec patchwork
# -----------------------------------------------------------------------------
(p_hist | p_box | p_qq) +
  plot_annotation(
    title   = "Analyse univariée de l'âge — sect1_harvestw4 (GHSP Wave 4)",
    caption = "Source : NBS Nigeria, GHSP-Panel Wave 4 (2018/19)",
    theme   = theme(plot.title = element_text(face = "bold", size = 14))
  )

ggsave("output/analyse_age.png", width = 14, height = 5, dpi = 300)

sect1 %>% select(s1q3) %>% count(s1q3) %>% print(n = Inf)