# =============================================================================
# Analyse 3 – Script 01 : Taux de morbidité par sexe et groupe d'âge
# Tâche 13 : Calculer le taux de morbidité (proportion d'individus ayant
#            déclaré une maladie/blessure dans les 4 semaines précédant
#            l'enquête) et le décrire par sexe et groupe d'âge avec
#            barplots + intervalles de confiance à 95 %.
#
# Variables clés :
#   s4aq3    = maladie ou blessure dans les 4 dernières semaines (1=oui, 2=non)
#   s1q2     = sexe de l'individu (1=homme, 2=femme)
#   s1q4     = âge en années révolues
#   sector   = milieu de résidence (1=urbain, 2=rural)
#
# Auteurs  : Groupe 7 – Herman YAMAHA | Bourama DIALLO
# Données  : Nigeria GHS Panel – Wave 4 (2018), Post-Harvest
# =============================================================================

library(haven)     # lecture des fichiers .dta Stata
library(dplyr)     # manipulation des données (filter, mutate, group_by...)
library(ggplot2)   # visualisation
library(naniar)    # diagnostic des valeurs manquantes
library(scales)    # formatage des axes (percent_format)

# --------------------------------------------------------------------------
# 1. CHARGEMENT DES FICHIERS DE DONNÉES
# --------------------------------------------------------------------------
# sect4a_harvestw4 : section santé individuelle (Wave 4)
# sect1_harvestw4  : section démographie – sexe, âge, milieu

sect4a <- read_dta("data/raw/sect4a_harvestw4.dta")
sect1  <- read_dta("data/raw/sect1_harvestw4.dta")

cat("Dimensions sect4a :", nrow(sect4a), "obs ×", ncol(sect4a), "vars\n")
cat("Dimensions sect1  :", nrow(sect1),  "obs ×", ncol(sect1),  "vars\n")

# --------------------------------------------------------------------------
# 2. PRÉPARATION DES DONNÉES DÉMOGRAPHIQUES (sect1)
# --------------------------------------------------------------------------
# On sélectionne uniquement les variables utiles et on crée :
#   sexe_label   : facteur lisible (Homme / Femme)
#   milieu_label : facteur lisible (Urbain / Rural)
#   groupe_age   : variable catégorielle par tranches de 10 ans

sect1_clean <- sect1 %>%
  select(hhid, indiv, s1q2, s1q4, sector) %>%
  rename(sexe = s1q2, age = s1q4, milieu = sector) %>%
  mutate(
    sexe_label   = factor(sexe,   levels = c(1, 2),
                          labels = c("Homme", "Femme")),
    milieu_label = factor(milieu, levels = c(1, 2),
                          labels = c("Urbain", "Rural")),
    # Tranches d'âge cohérentes avec la littérature sur la santé
    groupe_age   = cut(
      age,
      breaks         = c(0, 14, 24, 34, 44, 54, 64, Inf),
      labels         = c("0-14", "15-24", "25-34",
                         "35-44", "45-54", "55-64", "65+"),
      right          = TRUE,
      include.lowest = TRUE
    )
  )

# --------------------------------------------------------------------------
# 3. PRÉPARATION DE LA VARIABLE DE MORBIDITÉ (sect4a)
# --------------------------------------------------------------------------
# s4aq3 = 1 signifie que l'individu a souffert d'une maladie/blessure
# On crée une variable binaire 0/1 pour les calculs de taux

sect4a_clean <- sect4a %>%
  select(hhid, indiv, s4aq3, s4aq1, s4aq3b_1, s4aq6a,
         s4aq9, s4aq14, s4aq17) %>%
  mutate(
    # 1 = malade/blessé, 0 = pas de maladie déclarée
    malade   = if_else(s4aq3 == 1, 1L, 0L, missing = NA_integer_),
    consulte = if_else(s4aq1 == 1, 1L, 0L, missing = NA_integer_)
  )

# --------------------------------------------------------------------------
# 4. FUSION DES DEUX FICHIERS
# --------------------------------------------------------------------------
# Jointure par hhid (ménage) + indiv (identifiant individuel)
# On conserve tous les individus de sect4a (left_join)

df_health <- sect4a_clean %>%
  left_join(sect1_clean, by = c("hhid", "indiv"))

cat("\nNombre total d'individus dans le jeu fusionné :", nrow(df_health), "\n")
cat("Dont avec morbidité renseignée :",
    sum(!is.na(df_health$malade)), "\n")
cat("Dont déclarant une maladie/blessure :",
    sum(df_health$malade == 1, na.rm = TRUE), "\n")

# Sauvegarde du jeu de base pour les scripts suivants
saveRDS(df_health, "data/df_health_base.rds")

# --------------------------------------------------------------------------
# 5. TAUX DE MORBIDITÉ GLOBAL
# --------------------------------------------------------------------------
# IC à 95 % calculé par la formule de Wilson approximée :
#   IC = p ± 1.96 * sqrt(p*(1-p)/n)

taux_global <- df_health %>%
  filter(!is.na(malade)) %>%
  summarise(
    N         = n(),
    n_malades = sum(malade),
    taux      = mean(malade),
    ic_low    = taux - 1.96 * sqrt(taux * (1 - taux) / N),
    ic_high   = taux + 1.96 * sqrt(taux * (1 - taux) / N)
  )

cat("\n=== Taux de morbidité global ===\n")
print(taux_global)

# --------------------------------------------------------------------------
# 6. TAUX DE MORBIDITÉ PAR SEXE – avec IC à 95 %
# --------------------------------------------------------------------------

taux_sexe <- df_health %>%
  filter(!is.na(malade), !is.na(sexe_label)) %>%
  group_by(sexe_label) %>%
  summarise(
    n         = n(),
    n_malades = sum(malade, na.rm = TRUE),
    taux      = mean(malade, na.rm = TRUE),
    ic_low    = taux - 1.96 * sqrt(taux * (1 - taux) / n),
    ic_high   = taux + 1.96 * sqrt(taux * (1 - taux) / n),
    .groups   = "drop"
  )

cat("\n=== Taux de morbidité par sexe ===\n")
print(taux_sexe)

# CORRECTION GRAPHIQUE :
# – Les étiquettes (%) sont positionnées à ic_high + marge pour être
#   STRICTEMENT au-dessus des barres d'erreur, sans aucune superposition
# – La limite y est calculée dynamiquement : max(ic_high) + 4 points de %

marge_label <- 0.012   # espace entre borne haute IC et étiquette
ylim_sexe   <- max(taux_sexe$ic_high) + 0.04

p_sexe <- ggplot(taux_sexe,
                 aes(x = sexe_label, y = taux, fill = sexe_label)) +
  geom_col(width = 0.5, show.legend = FALSE) +
  geom_errorbar(
    aes(ymin = ic_low, ymax = ic_high),
    width     = 0.12,
    linewidth = 0.8,
    color     = "grey25"
  ) +
  # Étiquette placée AU-DESSUS de la borne haute de l'IC (ic_high + marge)
  geom_text(
    aes(y     = ic_high + marge_label,
        label = paste0(round(taux * 100, 1), "%")),
    vjust    = 0,
    size     = 5,
    fontface = "bold",
    color    = "grey20"
  ) +
  scale_y_continuous(
    labels = percent_format(accuracy = 1),
    limits = c(0, ylim_sexe),
    expand = c(0, 0)
  ) +
  scale_fill_manual(
    values = c("Homme" = "#3A86FF", "Femme" = "#FF6B6B")
  ) +
  labs(
    title    = "Taux de morbidité par sexe",
    subtitle = "Part des individus ayant déclaré une maladie ou blessure\ndans les 4 semaines précédant l'enquête — Wave 4 (2018)",
    x        = NULL,
    y        = "Taux de morbidité",
    caption  = "Source : Nigeria GHS Panel W4 | Barres d'erreur : IC à 95 %"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title         = element_text(face = "bold", size = 14),
    plot.subtitle      = element_text(color = "grey40", size = 11),
    panel.grid.major.x = element_blank(),
    axis.text.x        = element_text(size = 12, face = "bold")
  )

ggsave("outputs/figures/01a_morbidite_sexe.png", p_sexe,
       width = 7, height = 5.5, dpi = 300)
cat("Graphique sauvegardé : output/figures/01a_morbidite_sexe.png\n")

# --------------------------------------------------------------------------
# 7. TAUX DE MORBIDITÉ PAR GROUPE D'ÂGE – avec IC à 95 %
# --------------------------------------------------------------------------

taux_age <- df_health %>%
  filter(!is.na(malade), !is.na(groupe_age)) %>%
  group_by(groupe_age) %>%
  summarise(
    n         = n(),
    n_malades = sum(malade, na.rm = TRUE),
    taux      = mean(malade, na.rm = TRUE),
    ic_low    = pmax(0, taux - 1.96 * sqrt(taux * (1 - taux) / n)),
    ic_high   = taux + 1.96 * sqrt(taux * (1 - taux) / n),
    .groups   = "drop"
  )

cat("\n=== Taux de morbidité par groupe d'âge ===\n")
print(taux_age)

# CORRECTION : limite y dynamique → plus aucune barre tronquée (ex : 65+)
ylim_age <- max(taux_age$ic_high, na.rm = TRUE) + 0.05

p_age <- ggplot(taux_age,
                aes(x = groupe_age, y = taux, fill = groupe_age)) +
  geom_col(width = 0.65, show.legend = FALSE) +
  geom_errorbar(
    aes(ymin = ic_low, ymax = ic_high),
    width     = 0.2,
    linewidth = 0.7,
    color     = "grey25"
  ) +
  geom_text(
    aes(y     = ic_high + marge_label,
        label = paste0(round(taux * 100, 1), "%")),
    vjust    = 0,
    size     = 4,
    fontface = "bold",
    color    = "grey20"
  ) +
  scale_y_continuous(
    labels = percent_format(accuracy = 1),
    limits = c(0, ylim_age),
    expand = c(0, 0)
  ) +
  scale_fill_brewer(palette = "Blues", direction = 1) +
  labs(
    title    = "Taux de morbidité par groupe d'âge",
    subtitle = "Nigeria GHS Panel — Wave 4, Post-Harvest 2018",
    x        = "Groupe d'âge",
    y        = "Taux de morbidité",
    caption  = "Source : Nigeria GHS Panel W4 | Barres d'erreur : IC à 95 %"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title         = element_text(face = "bold", size = 14),
    plot.subtitle      = element_text(color = "grey40"),
    panel.grid.major.x = element_blank()
  )

ggsave("outputs/figures/01b_morbidite_age.png", p_age,
       width = 9, height = 5.5, dpi = 300)
cat("Graphique sauvegardé : outputs/figures/01b_morbidite_age.png\n")

# --------------------------------------------------------------------------
# 8. TAUX DE MORBIDITÉ PAR SEXE × GROUPE D'ÂGE (graphique croisé)
# --------------------------------------------------------------------------

taux_sexe_age <- df_health %>%
  filter(!is.na(malade), !is.na(sexe_label), !is.na(groupe_age)) %>%
  group_by(groupe_age, sexe_label) %>%
  summarise(
    n         = n(),
    n_malades = sum(malade, na.rm = TRUE),
    taux      = mean(malade, na.rm = TRUE),
    ic_low    = pmax(0, taux - 1.96 * sqrt(taux * (1 - taux) / n)),
    ic_high   = taux + 1.96 * sqrt(taux * (1 - taux) / n),
    .groups   = "drop"
  )

# CORRECTION :
# – Le width du dodge est identique pour geom_col, geom_errorbar et geom_text
#   → les éléments sont parfaitement alignés verticalement
# – Limite y dynamique pour ne tronquer aucun groupe

dodge_w <- 0.7
ylim_sa <- max(taux_sexe_age$ic_high, na.rm = TRUE) + 0.05

p_sexe_age <- ggplot(
  taux_sexe_age,
  aes(x = groupe_age, y = taux, fill = sexe_label)
) +
  geom_col(
    position = position_dodge(width = dodge_w),
    width    = 0.65
  ) +
  geom_errorbar(
    aes(ymin = ic_low, ymax = ic_high),
    position  = position_dodge(width = dodge_w),
    width     = 0.18,
    linewidth = 0.65,
    color     = "grey25"
  ) +
  geom_text(
    aes(y     = ic_high + marge_label,
        label = paste0(round(taux * 100, 1), "%")),
    position = position_dodge(width = dodge_w),
    vjust    = 0,
    size     = 2.8,
    fontface = "bold",
    color    = "grey20"
  ) +
  scale_y_continuous(
    labels = percent_format(accuracy = 1),
    limits = c(0, ylim_sa),
    expand = c(0, 0)
  ) +
  scale_fill_manual(
    values = c("Homme" = "#3A86FF", "Femme" = "#FF6B6B"),
    name   = "Sexe"
  ) +
  labs(
    title    = "Taux de morbidité par groupe d'âge et par sexe",
    subtitle = "Nigeria GHS Panel — Wave 4, Post-Harvest 2018",
    x        = "Groupe d'âge",
    y        = "Taux de morbidité",
    caption  = "Source : Nigeria GHS Panel W4 | Barres d'erreur : IC à 95 %"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title         = element_text(face = "bold", size = 14),
    plot.subtitle      = element_text(color = "grey40"),
    legend.position    = "top",
    panel.grid.major.x = element_blank()
  )

ggsave("outputs/figures/01c_morbidite_sexe_age.png", p_sexe_age,
       width = 11, height = 5.5, dpi = 300)
cat("Graphique sauvegardé : outputs/figures/01c_morbidite_sexe_age.png\n")

cat("\n=== Script 01 terminé avec succès ===\n")
