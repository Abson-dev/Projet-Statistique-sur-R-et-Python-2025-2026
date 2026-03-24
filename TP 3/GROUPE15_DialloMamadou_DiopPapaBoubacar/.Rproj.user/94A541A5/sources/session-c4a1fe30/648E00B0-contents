# Nettoyage de l'environnement
rm(list=ls())
# importation des packages
{
  library(haven)
  library(dplyr)
  library(ggplot2)
  library(naniar)
  library(rstatix)
  library(scales)
  library(gtsummary)
  library(rstatix)
  library(patchwork)
}
# 13. Importation des fichiers
# Chargement de sect1_harvestw4 avec haven
data <- read_stata("data/raw/sect4a_harvestw4.dta")

# Calcul du taux de morbidité
data %>% 
  summarise(taux_de_morbidite = mean(s4aq3==1, na.rm=TRUE))

data_to_join <- read_stata("data/raw/sect1_harvestw4.dta")
data_to_join <- data_to_join %>% select(hhid, indiv, s1q2, s1q4)
# Jointure avec sect1_harvest
data <- inner_join(data, data_to_join, by=c("hhid","indiv"))

data_morb <- data %>%
  mutate(
    malade = ifelse(s4aq3 == 1, 1, 0),
    sexe = as_factor(s1q2),
    age = s1q4,
    groupe_age = cut(
      age,
      breaks = c(0, 5, 15, 25, 45, 65, Inf),
      right = FALSE,
      labels = c("0-4", "5-14", "15-24", "25-44", "45-64", "65+")
    )
  ) %>%
  filter(!is.na(malade), !is.na(sexe), !is.na(groupe_age))


# Taux de morbidité par sexe avec IC à 95%
tab_sexe <- data_morb %>%
  group_by(sexe) %>%
  summarise(
    n = n(),
    cas = sum(malade, na.rm = TRUE),
    proportion = cas / n,
    IC_low = prop.test(cas, n)$conf.int[1],
    IC_up = prop.test(cas, n)$conf.int[2],
    .groups = "drop"
  )

tab_sexe

# Barplot par sexe
ggplot(tab_sexe, aes(x = sexe, y = proportion)) +
  geom_col(fill = "steelblue") +
  geom_errorbar(aes(ymin = IC_low, ymax = IC_up), width = 0.2) +
  scale_y_continuous(labels = function(x) paste0(round(100 * x, 1), "%")) +
  labs(
    x = "Sexe",
    y = "Taux de morbidité",
    title = "Taux de morbidité par sexe"
  ) +
  theme_minimal()

# Taux de morbidité par groupe d'âge
tab_age <- data_morb %>%
  group_by(groupe_age) %>%
  summarise(
    n = n(),
    cas = sum(malade, na.rm = TRUE),
    proportion = cas / n,
    IC_low = prop.test(cas, n)$conf.int[1],
    IC_up = prop.test(cas, n)$conf.int[2],
    .groups = "drop"
  )

tab_age

# Barplot par groupe d'âge
ggplot(tab_age, aes(x = groupe_age, y = proportion)) +
  geom_col(fill = "darkgreen") +
  geom_errorbar(aes(ymin = IC_low, ymax = IC_up), width = 0.2) +
  scale_y_continuous(labels = function(x) paste0(round(100 * x, 1), "%")) +
  labs(
    x = "Groupe d'âge",
    y = "Taux de morbidité",
    title = "Taux de morbidité par groupe d'âge"
  ) +
  theme_minimal()


# 14.Analyse de la distribution des types de maladies déclarées

data_maladie <- data %>%
  mutate(type_maladie = as_factor(s4aq3b_1)) %>%
  filter(!is.na(type_maladie))

top_maladies <- data_maladie %>%
  count(type_maladie, sort = TRUE) %>%
  slice_head(n = 10)


p_maladies <- ggplot(top_maladies,
       aes(x = n, y = reorder(type_maladie, n))) +
  geom_col(fill = "steelblue") +
  labs(
    x = "Nombre de cas",
    y = "Type de maladie",
    title = "Top 10 des maladies déclarées"
  ) +
  theme_minimal()

ggsave("output/figures/barplot_maladies.png", p_maladies, width = 12, height = 8, dpi = 300)


# 15. Recours aux soins (variables introuvables)

# 16. Analyse de la distribution des dépenses de santé (variables introuvables)
ggplot(data, aes(x = s4aq9)) +
  geom_histogram(binwidth = 5000, fill = "darkgreen", color = "black") +
  scale_y_log10() +  # Échelle logarithmique pour les effectifs
  labs(
    title = "Distribution de l'âge (échelle log sur les effectifs)",
    x = "Dépenses de santé",
    y = "Effectif (échelle log)"
  ) +
  theme_minimal()

# Statistiques par décile pour l'âge
data %>%
  filter(!is.na(s4aq9)) %>%
  mutate(
    decile_age = ntile(s4aq9, 10)  # Crée 10 groupes (déciles)
  ) %>%
  group_by(decile_age) %>%
  summarise(
    n = n(),
    age_min = min(s4aq9),
    age_max = max(s4aq9),
    age_mean = mean(s4aq9),
    age_median = median(s4aq9),
    age_sd = sd(s4aq9),
    age_q1 = quantile(s4aq9, 0.25),
    age_q3 = quantile(s4aq9, 0.75)
  ) %>%
  mutate(
    decile_age = paste0("D", decile_age, " (", age_min, "-", age_max, ")")
  ) %>%
  select(decile_age, n, age_mean, age_median, age_sd, age_q1, age_q3) %>%
  gt::gt() %>%  # Pour un beau tableau (optionnel)
  gt::fmt_number(columns = c(age_mean, age_median, age_sd, age_q1, age_q3), decimals = 1)



# 17. Tests d'indépendance

data_cons <- read_stata("data/raw/totcons_final.dta")

# Construction des quintiles
data_cons <- data_cons %>%
  mutate(quintile = ntile(totcons_adj_norm, 5))
table(data_cons$quintile)
# Fusion avec la base data
data <- data %>%
  left_join(data_cons %>% select(hhid, quintile), by = "hhid")

# Calcul du recours au soin par quintile
tab_recours <- data %>%
  group_by(quintile) %>%
  summarise(
    n = n(),
    recours = mean(s4aq1 == 1, na.rm = TRUE)
  )

tab_recours

# Croisement du quintile et de recours aux soins

tab <- table(data$quintile, data$s4aq1)
tab

library(knitr)

tab_df <- as.data.frame(tab)

kable(tab_df, format = "html") %>%
  cat(file = "output/tables/tableau.html")

# Tous les effectifs sont supérieurs à 5 donc on peut faire un test de Khi-deux
chisq.test(tab)
cramer_v(tab)

# Barplot empilé
ggplot(data, aes(x = quintile, fill = as_factor(s4aq1))) +
  geom_bar(position = "fill") +
  labs(x = "Quintile", y = "Proportion", fill = "Recours aux soins")


# 18. Comparaison des dépenses de santé entre zones (variables introuvables)