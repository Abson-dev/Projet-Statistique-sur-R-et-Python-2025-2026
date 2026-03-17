# ============================================
# ANALYSES - TP3 SANTE
# ============================================
source('R/02_nettoyage.R')
library(tidyr)


# ============================================
# Q13 - TAUX PAR SEXE
# ============================================
taux_sexe <- sect4a_merge %>%
  filter(!is.na(sexe)) %>%
  group_by(sexe) %>%
  summarise(
    n         = n(),
    n_malades = sum(malade, na.rm = TRUE),
    taux      = mean(malade, na.rm = TRUE) * 100,
    ic_bas    = taux - 1.96 * sqrt(taux * (100 - taux) / n),
    ic_haut   = taux + 1.96 * sqrt(taux * (100 - taux) / n)
  )
print(taux_sexe)

# ============================================
# Q13 - TAUX PAR GROUPE D'ÂGE
# ============================================
taux_age <- sect4a_merge %>%
  filter(!is.na(groupe_age)) %>%
  group_by(groupe_age) %>%
  summarise(
    n         = n(),
    n_malades = sum(malade, na.rm = TRUE),
    taux      = mean(malade, na.rm = TRUE) * 100,
    ic_bas    = taux - 1.96 * sqrt(taux * (100 - taux) / n),
    ic_haut   = taux + 1.96 * sqrt(taux * (100 - taux) / n)
  )
print(taux_age)

# ============================================
# Q13 - BARPLOTS AVEC IC 95%
# ============================================
p_sexe <- ggplot(taux_sexe, aes(x = sexe, y = taux, fill = sexe)) +
  geom_bar(stat = "identity", width = 0.5) +
  geom_errorbar(aes(ymin = ic_bas, ymax = ic_haut),
                width = 0.2, color = "black") +
  labs(
    title    = "Taux de morbidité par sexe",
    subtitle = "Avec IC à 95%",
    x        = "Sexe",
    y        = "Taux de morbidité (%)",
    fill     = "Sexe"
  ) +
  theme_minimal()

p_age <- ggplot(taux_age, aes(x = groupe_age, y = taux, fill = groupe_age)) +
  geom_bar(stat = "identity", width = 0.6) +
  geom_errorbar(aes(ymin = ic_bas, ymax = ic_haut),
                width = 0.2, color = "black") +
  labs(
    title    = "Taux de morbidité par groupe d'âge",
    subtitle = "Avec IC à 95%",
    x        = "Groupe d'âge",
    y        = "Taux de morbidité (%)",
    fill     = "Groupe d'âge"
  ) +
  theme_minimal()

# Combiner et afficher
p_sexe + p_age

# Sauvegarder
ggsave("output/figures/q13_morbidite.png",
       plot  = p_sexe + p_age,
       width = 12, height = 6, dpi = 300)

cat("✅ Question 13 terminée !\n")


# ============================================
# Q14 - TYPES DE MALADIES
# ============================================

# Fréquence des types de maladies
maladies <- sect4a_merge %>%
  filter(!is.na(s4aq3b_1)) %>%
  mutate(
    type_maladie = as_factor(s4aq3b_1)
  ) %>%
  count(type_maladie, sort = TRUE) %>%
  top_n(10, n) %>%
  mutate(
    categorie = case_when(
      type_maladie %in% c("1. MALARIA", "2. TB", "3. YELLOW FEVER",
                          "4. TYPHOID", "5. CHOLERA", "6. DIARRHEA",
                          "7. MENINGITIS", "8. CHICKEN POX",
                          "9. PNEUMONIA", "23. HEPATITIS B") ~ "Infectieuse",
      type_maladie %in% c("11. INJURY") ~ "Traumatique",
      type_maladie %in% c("13. HYPERTENSION", "18. DIABETES",
                          "24. ULCER/STOMACH PAIN") ~ "Chronique",
      TRUE ~ "Autre"
    )
  )

print(maladies)

# Barplot horizontal
p_maladies <- ggplot(maladies,
                     aes(x = reorder(type_maladie, n),
                         y = n,
                         fill = categorie)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(
    title = "10 affections les plus fréquentes",
    x     = "Type de maladie",
    y     = "Fréquence",
    fill  = "Catégorie"
  ) +
  theme_minimal()

p_maladies

ggsave("output/figures/q14_maladies.png",
       plot  = p_maladies,
       width = 12, height = 6, dpi = 300)

cat("✅ Question 14 terminée !\n")

# ============================================
# Q15 - RECOURS AUX SOINS
# ============================================
prestataires <- sect4a_merge %>%
  filter(!is.na(s4aq6a)) %>%
  mutate(
    prestataire = as_factor(s4aq6a)
  ) %>%
  count(prestataire, sort = TRUE) %>%
  mutate(
    pct = n / sum(n) * 100
  )

print(prestataires)

# Barplot ordonné
p_prestataires <- ggplot(prestataires,
                         aes(x = reorder(prestataire, n),
                             y = n,
                             fill = prestataire)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(
    title = "Recours aux soins selon le type de prestataire",
    x     = "Type de prestataire",
    y     = "Fréquence",
    fill  = "Prestataire"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

p_prestataires

ggsave("output/figures/q15_prestataires.png",
       plot  = p_prestataires,
       width = 12, height = 6, dpi = 300)

cat("✅ Question 15 terminée !\n")

# Charger totcons
totcons <- read_dta('data/raw/totcons_final.dta') %>%
  mutate(
    depense_sante = health31 + health32,
    quintile = ntile(totcons_adj, 5),
    zone = factor(sector, levels = c(1, 2),
                  labels = c('Urbain', 'Rural'))
  )

# Q16 - Histogramme
p1 <- ggplot(totcons %>% filter(depense_sante > 0),
             aes(x = depense_sante)) +
  geom_histogram(fill = 'steelblue', color = 'white', bins = 50) +
  scale_x_log10(labels = scales::comma) +
  labs(title = 'Distribution des depenses de sante',
       x = 'Depenses de sante (Naira, echelle log)',
       y = 'Frequence') +
  theme_minimal()

# Q17 - Test Chi-deux
totcons <- totcons %>%
  mutate(
    recours = factor(ifelse(depense_sante > 0, 1, 0),
                     levels = c(0, 1),
                     labels = c('Non consulte', 'Consulte'))
  )
tableau <- table(totcons$recours, totcons$quintile)
chi2 <- chisq.test(tableau)
cramer <- rstatix::cramer_v(tableau)

# Q17 - Barplot
totcons_pct <- totcons %>%
  group_by(quintile, recours) %>%
  summarise(n = n(), .groups = 'drop') %>%
  group_by(quintile) %>%
  mutate(pct = n / sum(n) * 100)
p3 <- ggplot(totcons_pct,
             aes(x = factor(quintile), y = pct, fill = recours)) +
  geom_bar(stat = 'identity', position = 'fill') +
  scale_y_continuous(labels = scales::percent) +
  labs(title = 'Recours aux soins par quintile',
       x = 'Quintile', y = 'Proportion', fill = 'Recours') +
  theme_minimal()

# Q18 - Wilcoxon Rural/Urbain
wilcox_test <- wilcox.test(depense_sante ~ zone, data = totcons)
p4 <- ggplot(totcons %>% filter(depense_sante > 0),
             aes(x = zone, y = depense_sante, fill = zone)) +
  geom_violin(alpha = 0.5) +
  geom_boxplot(width = 0.2) +
  scale_y_log10(labels = scales::comma) +
  labs(title = 'Depenses de sante Rural vs Urbain',
       x = 'Zone', y = 'Depenses (log)') +
  theme_minimal()

# Afficher resultats
print(chi2)
cat('V de Cramer:', cramer, '
')
print(wilcox_test)
p1
p3
p4

# Sauvegarder
ggsave('output/figures/q16_depenses.png', p1, width=10, height=6, dpi=300)
ggsave('output/figures/q17_quintile.png', p3, width=10, height=6, dpi=300)
ggsave('output/figures/q18_rural_urbain.png', p4, width=10, height=6, dpi=300)
cat('Analyses terminees !')
