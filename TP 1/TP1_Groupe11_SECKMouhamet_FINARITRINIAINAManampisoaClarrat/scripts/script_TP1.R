# ==============================================================================
# TP1 : Profil démographique des ménages (Vague 4)
# ==============================================================================


# ==============================================================================
# ÉTAPE 0 : INITIALISATION – LIBRAIRIES ET DOSSIERS
# ==============================================================================


# 0.1. Chargement des librairies avec pacman

if (!require("pacman")) install.packages("pacman")
library(pacman)
p_load(haven, dplyr, ggplot2, naniar, tidyr, scales, rstatix, moments, coin, gtsummary, gt)


# 0.2. Création des dossiers de sortie

#dir.create("outputs/tables", recursive = TRUE, showWarnings = FALSE)
#dir.create("outputs/graphs", recursive = TRUE, showWarnings = FALSE)


# ==============================================================================
# ÉTAPE 1 : CHARGEMENT ET EXPLORATION DES DONNÉES (VAGUE 4)
# ==============================================================================


# 1.1. Chargement des fichiers

sect1_w4 <- read_dta("data/raw/sect1_harvestw4.dta")
secta_w4 <- read_dta("data/raw/secta_harvestw4.dta")


# 1.2. Examen de la structure

cat("\n--- Structure de sect1_w4 ---\n")
str(sect1_w4, max.level = 1)
glimpse(sect1_w4)

cat("\n--- Structure de secta_w4 ---\n")
str(secta_w4, max.level = 1)
glimpse(secta_w4)


# 1.3. Vérification des doublons (hhid, indiv) dans sect1_w4

doublons_s1 <- sect1_w4 %>%
  group_by(hhid, indiv) %>%
  filter(n() > 1) %>%
  nrow()
cat("\nDoublons dans sect1_w4 :", doublons_s1, "\n")


# 1.4. Analyse des valeurs manquantes

# --- Variables clés pour l'analyse ---
vars_cles <- c("s1q2", "s1q4", "s1q3", "sector")

# Graphique gg_miss_var
p_miss_s1 <- sect1_w4 %>%
  select(any_of(vars_cles)) %>%
  gg_miss_var(show_pct = TRUE) +
  labs(
    title = "Valeurs manquantes - Sect1 (démographie)",
    x = "Variable",
    y = "% de NA"
  ) +
  theme_minimal()
print(p_miss_s1)
ggsave("outputs/graphs/missing_sect1_tp1.png", p_miss_s1, width = 8, height = 5, dpi = 300)

# Graphique vis_miss pour les patterns
p_vis_s1 <- sect1_w4 %>%
  select(any_of(vars_cles)) %>%
  vis_miss(warn_large_data = FALSE) +
  labs(title = "Patterns de valeurs manquantes - Sect1 (démographie)")
print(p_vis_s1)
ggsave("outputs/graphs/vis_miss_sect1_tp1.png", p_vis_s1, width = 8, height = 5, dpi = 300)

# Tableau récapitulatif des NA (toutes variables)
missing_s1 <- sect1_w4 %>%
  summarise(across(everything(), ~ sum(is.na(.)))) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "nb_na") %>%
  mutate(pct_na = round(nb_na / nrow(sect1_w4) * 100, 2)) %>%
  arrange(desc(nb_na))
write.csv(missing_s1, "outputs/tables/missing_summary_sect1_tp1.csv", row.names = FALSE)

# --- Secta_w4 : aperçu des NA sur variables géographiques ---
vars_secta <- c("sector", "zone", "state", "lga")
p_miss_secta <- secta_w4 %>%
  select(any_of(vars_secta)) %>%
  gg_miss_var(show_pct = TRUE) +
  labs(
    title = "Valeurs manquantes - Secta (couverture)",
    x = "Variable",
    y = "% de NA"
  ) +
  theme_minimal()
print(p_miss_secta)
ggsave("outputs/graphs/missing_secta_tp1.png", p_miss_secta, width = 8, height = 5, dpi = 300)

missing_secta <- secta_w4 %>%
  summarise(across(everything(), ~ sum(is.na(.)))) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "nb_na") %>%
  mutate(pct_na = round(nb_na / nrow(secta_w4) * 100, 2)) %>%
  arrange(desc(nb_na))
write.csv(missing_secta, "outputs/tables/missing_summary_secta_tp1.csv", row.names = FALSE)

cat("Graphiques et tableaux des valeurs manquantes sauvegardés.\n")


# 1.5. Identification des variables clés

cat("\nVariables clés identifiées :\n")
cat("- s1q2 : sexe (1 = MALE, 2 = FEMALE)\n")
cat("- s1q4 : âge (en années)\n")
cat("- s1q3 : lien de parenté (voir labels pour regroupement ultérieur)\n")
cat("- sector : milieu (1 = Urban, 2 = Rural) – présent dans sect1_w4 et secta_w4\n")


# 1.6. Cohérence des identifiants entre sect1 et secta

communs <- intersect(unique(sect1_w4$hhid), unique(secta_w4$hhid))
cat("\nNombre de ménages communs entre sect1 et secta :", length(communs), "\n")




# ==============================================================================
# ÉTAPE 2 : ANALYSE UNIVARIÉE DE L'ÂGE
# ==============================================================================


# 2.1. Filtrage : individus avec âge non manquant

age_data <- sect1_w4 %>%
  filter(!is.na(s1q4)) %>%
  select(s1q4)


# 2.2. Statistiques descriptives

stats_age <- age_data %>%
  summarise(
    moyenne    = mean(s1q4, na.rm = TRUE),
    mediane    = median(s1q4, na.rm = TRUE),
    Q1         = quantile(s1q4, 0.25, na.rm = TRUE),
    Q3         = quantile(s1q4, 0.75, na.rm = TRUE),
    min        = min(s1q4, na.rm = TRUE),
    max        = max(s1q4, na.rm = TRUE),
    ecart_type = sd(s1q4, na.rm = TRUE),
    cv         = ecart_type / moyenne,
    skewness   = moments::skewness(s1q4, na.rm = TRUE)
  )

write.csv(stats_age, "outputs/tables/stats_age_univariees.csv", row.names = FALSE)


# 2.3. Test de normalité de Shapiro-Wilk (échantillon ≤ 5000)

set.seed(123)
age_sample <- sample(age_data$s1q4, size = min(5000, nrow(age_data)))
shapiro_test <- shapiro.test(age_sample)
capture.output(print(shapiro_test), file = "outputs/tables/shapiro_age.txt")


# 2.4. Graphique : Histogramme (binwidth = 5)

p_hist <- ggplot(age_data, aes(x = s1q4)) +
  geom_histogram(binwidth = 5, fill = "steelblue", color = "white", boundary = 0) +
  labs(
    title = "Distribution de l'âge des membres des ménages",
    x = "Âge (années)",
    y = "Effectif"
  ) +
  theme_minimal()

print(p_hist)
ggsave("outputs/graphs/histogramme_age.png", p_hist, width = 8, height = 5, dpi = 300)


# 2.5. Graphique : Boxplot

p_box <- ggplot(age_data, aes(y = s1q4)) +
  geom_boxplot(fill = "lightblue") +
  labs(
    title = "Boîte à moustaches de l'âge",
    y = "Âge (années)"
  ) +
  theme_minimal()

print(p_box)
ggsave("outputs/graphs/boxplot_age.png", p_box, width = 5, height = 5, dpi = 300)


# 2.6. Affichage console pour validation

cat("\n--- Statistiques descriptives de l'âge ---\n")
print(stats_age)
cat("\n--- Test de Shapiro-Wilk (échantillon de 5000) ---\n")
print(shapiro_test)




# ==============================================================================
# ÉTAPE 3 : PYRAMIDE DES ÂGES PAR SEXE
# ==============================================================================


# 3.1. Préparation des données : âge et sexe non manquants

pyramid_data <- sect1_w4 %>%
  filter(!is.na(s1q4), !is.na(s1q2)) %>%
  mutate(
    age_group = cut(
      s1q4,
      breaks = seq(0, 130, by = 5),
      right = FALSE,
      include.lowest = TRUE,
      labels = paste0(seq(0, 125, by = 5), "-", seq(4, 129, by = 5))
    ),
    sexe = ifelse(s1q2 == 1, "Homme", "Femme")
  )


# 3.2. Comptage des effectifs par groupe d'âge et sexe

pyramid_counts <- pyramid_data %>%
  group_by(age_group, sexe) %>%
  summarise(n = n(), .groups = "drop") %>%
  mutate(n_plot = ifelse(sexe == "Homme", -n, n))

write.csv(pyramid_counts %>% select(-n_plot), 
          "outputs/tables/effectifs_pyramide_age.csv", row.names = FALSE)


# 3.3. Graphique : Pyramide des âges

p_pyramid <- ggplot(pyramid_counts, aes(x = age_group, y = n_plot, fill = sexe)) +
  geom_col(position = "identity", width = 0.8) +
  coord_flip() +
  scale_y_continuous(
    labels = abs,
    breaks = seq(-3000, 3000, by = 1000),
    limits = c(
      -max(abs(pyramid_counts$n_plot)) * 1.1,
      max(abs(pyramid_counts$n_plot)) * 1.1
    )
  ) +
  labs(
    title = "Pyramide des âges de la population enquêtée (vague 4)",
    subtitle = "Effectifs par groupe d'âge quinquennal et par sexe",
    x = "Groupe d'âge",
    y = "Effectif",
    fill = "Sexe"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_fill_manual(values = c("Homme" = "steelblue", "Femme" = "firebrick"))

print(p_pyramid)
ggsave("outputs/graphs/pyramide_ages.png", p_pyramid, width = 8, height = 6, dpi = 300)


# 3.4. Affichage console : résumé des effectifs

cat("\n--- Effectifs par groupe d'âge et sexe ---\n")
print(pyramid_counts %>% select(-n_plot) %>% arrange(age_group, sexe))




# ==============================================================================
# ÉTAPE 4 : ANALYSE DU LIEN DE PARENTÉ
# ==============================================================================


# 4.1. Création de la variable de relation simplifiée
# Codes : 1 = HEAD, 2 = SPOUSE, 3 = OWN CHILD

relation_data <- sect1_w4 %>%
  filter(!is.na(s1q3)) %>%
  mutate(
    relation = case_when(
      s1q3 == 1 ~ "Chef",
      s1q3 == 2 ~ "Conjoint",
      s1q3 == 3 ~ "Enfant",
      TRUE      ~ "Autre"
    )
  )


# 4.2. Calcul des effectifs et proportions avec IC à 95%

relation_summary <- relation_data %>%
  group_by(relation) %>%
  summarise(n = n(), .groups = "drop") %>%
  mutate(
    prop      = n / sum(n),
    ic_low    = mapply(function(x, tot) binom.test(x, tot)$conf.int[1], n, sum(n)),
    ic_high   = mapply(function(x, tot) binom.test(x, tot)$conf.int[2], n, sum(n)),
    prop_label = paste0(round(prop * 100, 1), "%")
  ) %>%
  arrange(desc(n))

write.csv(relation_summary, "outputs/tables/lien_parente_frequences.csv", row.names = FALSE)


# 4.3. Graphique : Barplot horizontal avec intervalles de confiance

p_relation <- ggplot(relation_summary, aes(x = reorder(relation, n), y = n, fill = relation)) +
  geom_col() +
  geom_errorbar(aes(ymin = ic_low * sum(n), ymax = ic_high * sum(n)), width = 0.2) +
  geom_text(aes(label = paste0(n, " (", prop_label, ")")), hjust = -0.1, size = 3) +
  coord_flip() +
  labs(
    title = "Répartition des liens de parenté dans les ménages",
    x = NULL,
    y = "Effectif"
  ) +
  theme_minimal() +
  theme(legend.position = "none") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15)))

print(p_relation)
ggsave("outputs/graphs/lien_parente_barplot.png", p_relation, width = 7, height = 5, dpi = 300)


# 4.4. Affichage console

cat("\n--- Fréquences des liens de parenté ---\n")
print(relation_summary)




# ==============================================================================
# ÉTAPE 5 : TAILLE DES MÉNAGES ET COMPARAISON RURAL/URBAIN
# ==============================================================================


# 5.1. Calcul de la taille de chaque ménage

taille_menage <- sect1_w4 %>%
  group_by(hhid) %>%
  summarise(taille = n(), .groups = "drop")


# 5.2. Récupération du secteur (rural/urbain) par ménage

secteur <- sect1_w4 %>%
  group_by(hhid) %>%
  summarise(sector = first(sector), .groups = "drop")


# 5.3. Jointure et filtrage

menage_taille <- taille_menage %>%
  left_join(secteur, by = "hhid") %>%
  filter(!is.na(sector))


# 5.4. Statistiques descriptives par zone

stats_taille <- menage_taille %>%
  group_by(zone = ifelse(sector == 1, "Urbain", "Rural")) %>%
  summarise(
    n_menages  = n(),
    moyenne    = mean(taille),
    mediane    = median(taille),
    ecart_type = sd(taille),
    Q1         = quantile(taille, 0.25),
    Q3         = quantile(taille, 0.75),
    min        = min(taille),
    max        = max(taille)
  )
print(stats_taille)
write.csv(stats_taille, "outputs/tables/stats_taille_menage_par_zone.csv", row.names = FALSE)


# 5.5. Test de Wilcoxon-Mann-Whitney et taille d'effet

wilcox_test <- wilcox.test(taille ~ sector, data = menage_taille)
print(wilcox_test)

effet <- menage_taille %>% wilcox_effsize(taille ~ sector)
print(effet)

capture.output(print(wilcox_test), file = "outputs/tables/wilcoxon_taille_menage.txt")
write.csv(effet, "outputs/tables/effet_taille_menage.csv", row.names = FALSE)


# 5.6. Graphique : Boxplot groupé

p_box <- ggplot(menage_taille, aes(x = factor(sector, labels = c("Urbain", "Rural")), 
                                   y = taille, fill = factor(sector))) +
  geom_boxplot() +
  labs(
    title = "Taille des ménages selon le milieu de résidence",
    x = NULL,
    y = "Nombre de personnes"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

print(p_box)
ggsave("outputs/graphs/boxplot_taille_menage_par_zone.png", p_box, width = 6, height = 5, dpi = 300)


# 5.7. Graphique optionnel : Violin plot

p_violin <- ggplot(menage_taille, aes(x = factor(sector, labels = c("Urbain", "Rural")), 
                                      y = taille, fill = factor(sector))) +
  geom_violin(trim = FALSE) +
  geom_boxplot(width = 0.1, fill = "white") +
  labs(
    title = "Distribution de la taille des ménages par milieu",
    x = NULL,
    y = "Nombre de personnes"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

print(p_violin)
ggsave("outputs/graphs/violin_taille_menage_par_zone.png", p_violin, width = 6, height = 5, dpi = 300)




# ==============================================================================
# ÉTAPE 6 : TABLEAU RÉCAPITULATIF STRATIFIÉ PAR ZONE (RURAL/URBAIN)
# ==============================================================================


# 6.1. Préparation de la base individuelle (âge, sexe, zone)

individus <- sect1_w4 %>%
  filter(!is.na(s1q2), !is.na(s1q4)) %>%
  mutate(
    sexe = factor(s1q2, levels = c(1, 2), labels = c("Homme", "Femme")),
    zone = factor(sector, levels = c(1, 2), labels = c("Urbain", "Rural"))
  ) %>%
  select(hhid, zone, sexe, age = s1q4)


# 6.2. Ajout de la taille du ménage

menage_taille <- menage_taille %>%
  select(hhid, taille_menage = taille)


# 6.3. Jointure finale

base_finale <- individus %>%
  left_join(menage_taille, by = "hhid")


# 6.4. Création du tableau avec gtsummary

tableau <- base_finale %>%
  select(zone, sexe, age, taille_menage) %>%
  tbl_summary(
    by = zone,
    statistic = list(
      age           ~ "{mean} ({sd})",
      taille_menage ~ "{median} ({p25}, {p75})",
      sexe          ~ "{n} ({p}%)"
    ),
    digits = list(age = 1, taille_menage = 0, sexe = 0),
    label = list(
      age           = "Âge (années)",
      sexe          = "Sexe",
      taille_menage = "Taille du ménage"
    )
  ) %>%
  add_p(
    test = list(
      age           ~ "wilcox.test",
      taille_menage ~ "wilcox.test",
      sexe          ~ "chisq.test"
    )
  ) %>%
  add_overall() %>%
  modify_header(label = "**Caractéristique**") %>%
  modify_spanning_header(c("stat_1", "stat_2") ~ "**Zone de résidence**") %>%
  modify_caption("**Tableau 1 : Caractéristiques démographiques des ménages selon la zone**")

# Affichage
tableau


# 6.5. Export du tableau (HTML et TXT)

tableau %>%
  as_gt() %>%
  gt::gtsave(filename = "outputs/tables/tableau_recap_demographie.html")

tableau %>%
  as_kable() %>%
  writeLines("outputs/tables/tableau_recap_demographie.txt")