# ==============================================================================
# TP3 : Accès aux services de santé et chocs sanitaires des ménages (Vague 4)
# ==============================================================================


# ==============================================================================
# ÉTAPE 0 : INITIALISATION — LIBRAIRIES ET DOSSIERS
# ==============================================================================

if (!require("pacman")) install.packages("pacman")
library(pacman)
p_load(haven, dplyr, ggplot2, naniar, tidyr, scales, rstatix,
       survey, Hmisc)

#dir.create("outputs/tables", recursive = TRUE, showWarnings = FALSE)
#dir.create("outputs/graphs", recursive = TRUE, showWarnings = FALSE)


# ==============================================================================
# ÉTAPE 1 : CHARGEMENT ET EXPLORATION DES DONNÉES
# ==============================================================================

# 1.1. Chargement des fichiers

sect4a_w4 <- read_dta("data/raw/sect4a_harvestw4.dta")
sect1_w4  <- read_dta("data/raw/sect1_harvestw4.dta")
secta_w4  <- read_dta("data/raw/secta_harvestw4.dta")
totcons   <- read_dta("data/raw/totcons_final.dta")


# 1.2. Examen de la structure

cat("\n Structure de sect4a_w4 \n")

str(sect4a_w4, max.level = 1)
glimpse(sect4a_w4)

cat("\n Structure de sect1_w4 \n")

str(sect1_w4, max.level = 1)
glimpse(sect1_w4)

cat("\n Structure de secta_w4 \n")

str(secta_w4, max.level = 1)
glimpse(secta_w4)

cat("\n--- Structure de totcons ---\n")
str(totcons, max.level = 1)
glimpse(totcons)


# 1.3. Vérification des doublons

doublons_s4a <- sect4a_w4 %>%
  group_by(hhid, indiv) %>%
  filter(n() > 1) %>%
  nrow()
cat("\nDoublons dans sect4a_w4 :", doublons_s4a, "\n")

doublons_s1 <- sect1_w4 %>%
  group_by(hhid, indiv) %>%
  filter(n() > 1) %>%
  nrow()
cat("Doublons dans sect1_w4 :", doublons_s1, "\n")


# 1.4. Analyse des valeurs manquantes (variables clés)

# sect4a_w4 (santé)

vars_s4a <- c("s4aq3", "s4aq6a", "s4aq9", "s4aq14", "s4aq17", "s4aq1")
p_miss_s4a <- sect4a_w4 %>%
  select(any_of(vars_s4a)) %>%
  gg_miss_var(show_pct = TRUE) +
  labs(title = "Valeurs manquantes - Sect4a (santé)", x = "Variable", y = "% de NA") +
  theme_minimal()

print(p_miss_s4a)
ggsave("outputs/graphs/missing_sect4a.png", p_miss_s4a, width = 8, height = 5, dpi = 300)

# sect1_w4 (démographie)

vars_s1   <- c("s1q2", "s1q4", "sector")
data_miss <- sect1_w4 %>% select(any_of(vars_s1))
na_total  <- sum(is.na(data_miss))

if (na_total > 0) {
  p_miss_s1 <- data_miss %>%
    gg_miss_var(show_pct = TRUE) +
    labs(title = "Valeurs manquantes - Sect1 (démographie)",
         x = "Variable", y = "% de NA") +
    theme_minimal()
  print(p_miss_s1)
  ggsave("outputs/graphs/missing_sect1.png", p_miss_s1,
         width = 8, height = 5, dpi = 300)
} else {
  cat("Aucune valeur manquante sur les variables clés de sect1_w4.\n")
}

# Tableaux récapitulatifs des NA

missing_s4a <- sect4a_w4 %>%
  summarise(across(everything(), ~ sum(is.na(.)))) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "nb_na") %>%
  mutate(pct_na = round(nb_na / nrow(sect4a_w4) * 100, 2)) %>%
  arrange(desc(nb_na))
write.csv(missing_s4a, "outputs/tables/missing_summary_sect4a.csv", row.names = FALSE)

missing_s1 <- sect1_w4 %>%
  summarise(across(everything(), ~ sum(is.na(.)))) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "nb_na") %>%
  mutate(pct_na = round(nb_na / nrow(sect1_w4) * 100, 2)) %>%
  arrange(desc(nb_na))
write.csv(missing_s1, "outputs/tables/missing_summary_sect1.csv", row.names = FALSE)

cat("Graphiques et tableaux des valeurs manquantes sauvegardés.\n")


# 1.5. Cohérence des identifiants

communs_s4a_s1    <- intersect(unique(sect4a_w4$hhid), unique(sect1_w4$hhid))
communs_s4a_secta <- intersect(unique(sect4a_w4$hhid), unique(secta_w4$hhid))
communs_s4a_cons  <- intersect(unique(sect4a_w4$hhid), unique(totcons$hhid))
cat("\nMénages communs entre sect4a et sect1  :", length(communs_s4a_s1), "\n")
cat("Ménages communs entre sect4a et secta  :", length(communs_s4a_secta), "\n")
cat("Ménages communs entre sect4a et totcons:", length(communs_s4a_cons), "\n")


# 1.6. Variables clés

cat("\nVariables clés :\n")
cat("- s4aq3   : maladie/blessure (1 = Oui, 2 = Non)\n")
cat("- s4aq3b_1: type de maladie/blessure\n")
cat("- s4aq6a  : type de prestataire consulté\n")
cat("- s4aq9   : coût de la consultation\n")
cat("- s4aq14  : dépenses médicaments OTC\n")
cat("- s4aq17  : dépenses hospitalisation\n")
cat("- s1q2    : sexe (1 = Homme, 2 = Femme)\n")
cat("- s1q4    : âge (années)\n")
cat("- sector  : milieu (1 = Urbain, 2 = Rural)\n")
cat("- totcons_adj : consommation ajustée (spatiale + temporelle) -> quintiles\n")
cat("- wt_wave4 : poids coupe transversale (secta_w4)\n")


# ==============================================================================
# ÉTAPE 2 : CONSTRUCTION DE LA BASE ANALYTIQUE
# ==============================================================================

# 2.1. Jointure santé + démographie + poids
# Left join : sect4a_w4 est la table principale
# wt_wave4 propagé depuis secta_w4 (niveau ménage) via hhid
# NA sur wt_wave4 : mécanisme MCAR -> exclus des analyses pondérées

health_demo <- sect4a_w4 %>%
  left_join(sect1_w4 %>% select(hhid, indiv,
                                s1q2,
                                s1q4,
                                secteur = sector),  # renommé pour éviter le conflit
            by = c("hhid", "indiv")) %>%
  left_join(secta_w4 %>% select(hhid, wt_wave4), by = "hhid")

cat("Taux de jointure :", nrow(health_demo) / nrow(sect4a_w4), "\n")

# Vérification des NA sur wt_wave4
na_poids <- sum(is.na(health_demo$wt_wave4))
cat("NA sur wt_wave4 :", na_poids,
    "(", round(na_poids / nrow(health_demo) * 100, 2), "%)\n")


# 2.2. Variable morbidité (1 = maladie ou blessure dans les 4 dernières semaines)

health_demo <- health_demo %>%
  mutate(
    morbid = case_when(
      s4aq3 == 1 ~ 1,
      s4aq3 == 2 ~ 0,
      TRUE       ~ NA_real_
    ),
    milieu = ifelse(secteur == 1, "Urbain", "Rural"),   # secteur au lieu de sector
    sexe   = factor(s1q2, levels = c(1, 2), labels = c("Homme", "Femme")),
    age_group = case_when(
      s1q4 <= 14 ~ "0-14 ans",
      s1q4 <= 30 ~ "15-30 ans",
      s1q4 <= 45 ~ "31-45 ans",
      s1q4 <= 60 ~ "46-60 ans",
      s1q4 > 60  ~ "61 ans et plus",
      TRUE       ~ NA_character_
    )
  )

# 2.3. Quintiles de consommation (totcons_adj : ajusté spatialement et temporellement)
# totcons_adj est la variable recommandée pour les analyses de bien-être inter-zones
# car elle corrige les différences de prix entre milieux urbain et rural

quintiles <- totcons %>%
  mutate(quintile = ntile(totcons_adj, 5)) %>%
  select(hhid, quintile, totcons_adj)

health_demo <- health_demo %>%
  left_join(quintiles, by = "hhid")

cat("NA sur quintile :", sum(is.na(health_demo$quintile)),
    "(", round(mean(is.na(health_demo$quintile)) * 100, 2), "%)\n")


# 2.4. Variable dépense totale de santé
# NA structurels sur les composantes : individu non malade -> 0 logique

health_demo <- health_demo %>%
  mutate(
    dep_consult      = ifelse(is.na(s4aq9),  0, s4aq9),
    dep_medoc_otc    = ifelse(is.na(s4aq14), 0, s4aq14),
    dep_hosp         = ifelse(is.na(s4aq17), 0, s4aq17),
    dep_sante_totale = dep_consult + dep_medoc_otc + dep_hosp
  )

saveRDS(health_demo, "data/processed/health_demo.rds")


# ==============================================================================
# ÉTAPE 3 : TAUX DE MORBIDITÉ GLOBAL ET PAR SOUS-GROUPES
# ==============================================================================

# 3.1. Taux global (estimé par svymean)
# svydesign créé une seule fois, réutilisé dans toute l'étape 3

design_morb <- svydesign(
  ids     = ~hhid,
  weights = ~wt_wave4,
  data    = health_demo %>% filter(!is.na(morbid), !is.na(wt_wave4))
)

taux_global <- svymean(~morbid, design_morb, na.rm = TRUE)
ic_global   <- confint(taux_global)

global_taux <- data.frame(
  indicateur = "Taux de morbidité global",
  effectif   = nrow(health_demo %>% filter(!is.na(morbid), !is.na(wt_wave4))),
  proportion = coef(taux_global)["morbid"],
  ic_inf     = ic_global["morbid", 1],
  ic_sup     = ic_global["morbid", 2]
)

print(global_taux)
write.csv(global_taux, "outputs/tables/taux_morbidite_global.csv", row.names = FALSE)


# 3.2. Taux de morbidité par sexe (svyby)

design_sexe <- svydesign(
  ids     = ~hhid,
  weights = ~wt_wave4,
  data    = health_demo %>% filter(!is.na(morbid), !is.na(s1q2), !is.na(wt_wave4))
)

taux_sexe_pond <- svyby(~morbid, ~sexe, design_sexe, svymean,
                        na.rm = TRUE, vartype = "ci")

taux_sexe <- taux_sexe_pond %>%
  rename(prop = morbid, ic_low = ci_l, ic_high = ci_u) %>%
  mutate(prop_label = paste0(round(prop * 100, 1), "%"))

print(taux_sexe)

write.csv(taux_sexe, "outputs/tables/taux_morbidite_par_sexe.csv", row.names = FALSE)


# 3.3. Taux de morbidité par groupe d'âge (svyby)

design_age <- svydesign(
  ids     = ~hhid,
  weights = ~wt_wave4,
  data    = health_demo %>%
    filter(!is.na(morbid), !is.na(age_group), !is.na(wt_wave4))
)

taux_age_pond <- svyby(~morbid, ~age_group, design_age, svymean,
                       na.rm = TRUE, vartype = "ci")

taux_age <- taux_age_pond %>%
  rename(prop = morbid, ic_low = ci_l, ic_high = ci_u) %>%
  mutate(prop_label = paste0(round(prop * 100, 1), "%"))

print(taux_age)

write.csv(taux_age, "outputs/tables/taux_morbidite_par_age.csv", row.names = FALSE)


# 3.4. Graphique : Barplot taux de morbidité par sexe

p_sexe <- ggplot(taux_sexe, aes(x = sexe, y = prop)) +
  geom_col(fill = "steelblue", width = 0.5) +
  geom_text(aes(label = prop_label), vjust = -0.5, size = 3) +
  scale_y_continuous(labels = scales::percent, limits = c(0, 0.11)) +
  labs(x = NULL, y = "Taux de morbidité") +
  theme_minimal()

print(p_sexe)

ggsave("outputs/graphs/taux_morbidite_par_sexe.png", p_sexe, width = 5, height = 3.5, dpi = 300)



# 3.5. Graphique : Barplot taux de morbidité par groupe d'âge

# Ordonner les groupes d'âge
taux_age$age_group <- factor(taux_age$age_group,
  levels = c("0-14 ans", "15-30 ans", "31-45 ans", "46-60 ans", "61 ans et plus"))

p_age <- ggplot(taux_age, aes(x = age_group, y = prop)) +
  geom_col(fill = "steelblue", width = 0.5) +
  geom_text(aes(label = prop_label), vjust = -0.5, size = 3) +
  scale_y_continuous(labels = scales::percent, limits = c(0, 0.23)) +
  labs(x = "Groupe d'âge", y = "Taux de morbidité") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(p_age)

ggsave("outputs/graphs/taux_morbidite_par_age.png", p_age, width = 6, height = 4, dpi = 300)



# ==============================================================================
# ÉTAPE 4 : TYPES DE MALADIES/BLESSURES DÉCLARÉES (10 PLUS FRÉQUENTES)
# ==============================================================================

# 4.1. Filtrage des individus ayant eu un problème de santé

health_cases <- health_demo %>%
  filter(morbid == 1, !is.na(s4aq3b_1))


# 4.2. Labels et catégories des types de maladies

health_cases <- health_cases %>%
  mutate(
    type_lib = case_when(
      s4aq3b_1 == 1  ~ "Malaria",
      s4aq3b_1 == 2  ~ "TB",
      s4aq3b_1 == 3  ~ "Yellow Fever",
      s4aq3b_1 == 4  ~ "Typhoid",
      s4aq3b_1 == 5  ~ "Cholera",
      s4aq3b_1 == 6  ~ "Diarrhea",
      s4aq3b_1 == 7  ~ "Meningitis",
      s4aq3b_1 == 8  ~ "Chicken Pox",
      s4aq3b_1 == 9  ~ "Pneumonia",
      s4aq3b_1 == 10 ~ "Common Cold",
      s4aq3b_1 == 11 ~ "Injury",
      s4aq3b_1 == 12 ~ "Other",
      s4aq3b_1 == 13 ~ "Hypertension",
      s4aq3b_1 == 14 ~ "Flu",
      s4aq3b_1 == 15 ~ "Catarrh",
      s4aq3b_1 == 16 ~ "Cough",
      s4aq3b_1 == 17 ~ "Headache",
      s4aq3b_1 == 18 ~ "Diabetes",
      s4aq3b_1 == 19 ~ "Guinea Worm",
      s4aq3b_1 == 20 ~ "Dysentery",
      s4aq3b_1 == 21 ~ "Scabies",
      s4aq3b_1 == 22 ~ "Ringworm",
      s4aq3b_1 == 23 ~ "Hepatitis B",
      s4aq3b_1 == 24 ~ "Ulcer / Stomach Pain",
      s4aq3b_1 == 25 ~ "Eye Problem",
      s4aq3b_1 == 26 ~ "Tooth Problem",
      s4aq3b_1 == 27 ~ "Body Pains",
      TRUE           ~ paste0("Code ", s4aq3b_1)
    ),
    categorie = case_when(
      s4aq3b_1 %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9, 14, 15, 19, 20, 21, 22, 23) ~ "Infectious",
      s4aq3b_1 %in% c(13, 18)                                                  ~ "Chronic",
      s4aq3b_1 %in% c(11)                                                      ~ "Traumatic",
      s4aq3b_1 %in% c(10, 16, 17, 24, 25, 26, 27)                             ~ "Symptomatic",
      TRUE                                                                      ~ "Other"
    )
  )

# 4.3. Top 10 des maladies les plus fréquentes

top10_maladies <- health_cases %>%
  count(type_lib, categorie, name = "effectif") %>%
  mutate(prop = effectif / sum(effectif),
         prop_label = scales::percent(prop, accuracy = 0.1)) %>%
  arrange(desc(effectif)) %>%
  slice_head(n = 10)

print(top10_maladies)

write.csv(top10_maladies, "outputs/tables/types_maladies_top10.csv", row.names = FALSE)


# 4.4. Graphique : Barplot horizontal coloré par catégorie

p_maladies <- ggplot(top10_maladies,
                     aes(x = reorder(type_lib, effectif),
                         y = effectif,
                         fill = categorie)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = paste0(effectif, " (", prop_label, ")")),
            hjust = -0.1, size = 3) +
  coord_flip() +
  scale_fill_manual(
    values = c(
      "Infectious"  = "#F28E2B",
      "Chronic"     = "#E15759",
      "Traumatic"   = "#76B7B2",
      "Symptomatic" = "#4E79A7",
      "Other"       = "#2D6A2D"
    )
  ) +
  labs(x = NULL, y = "Effectif", fill = "Category") +
  theme_minimal() +
  scale_y_continuous(expand = expansion(mult = c(0, 0.20)))

print(p_maladies)

ggsave("outputs/graphs/types_maladies_top10.png", p_maladies, width = 8, height = 5, dpi = 300)



# ==============================================================================
# ÉTAPE 5 : RECOURS AUX SOINS — TYPE DE PRESTATAIRE
# ==============================================================================

# 5.1. Restriction aux individus malades (morbid = 1)

malades <- health_demo %>% filter(morbid == 1)


# 5.2. Catégorisation du type de prestataire

malades <- malades %>%
  mutate(
    prestataire = case_when(
      s4aq6a == 0                   ~ "No one",
      s4aq6a == 1                   ~ "Traditional Healer",
      s4aq6a %in% c(2, 3, 4, 5, 6) ~ "Medical Personnel (Doctor/Nurse/Midwife)",
      s4aq6a %in% c(7, 8, 11)      ~ "Pharmacy / Medicine Vendor",
      s4aq6a == 9                   ~ "TBA",
      s4aq6a == 10                  ~ "Spiritualist",
      s4aq6a %in% c(14, 15)        ~ "Community Health Worker (CHEW/JCHEW)",
      s4aq6a == 13                  ~ "Other",
      is.na(s4aq6a)                 ~ "Not reported",
      TRUE                          ~ "Other"
    )
  )

# 5.3. Tableau des effectifs et proportions

prestataire_table <- malades %>%
  filter(!is.na(prestataire), prestataire != "Non renseigné") %>%
  count(prestataire, name = "effectif") %>%
  mutate(
    prop       = effectif / sum(effectif),
    prop_label = scales::percent(prop, accuracy = 0.1)
  ) %>%
  arrange(desc(effectif))

print(prestataire_table)

write.csv(prestataire_table, "outputs/tables/recours_soins_prestataire.csv", row.names = FALSE)


# 5.4. Graphique : Barplot horizontal ordonné

p_prest <- ggplot(prestataire_table,
                  aes(x = reorder(prestataire, effectif), y = effectif)) +
  geom_col(fill = "steelblue", width = 0.5) +
  geom_text(aes(label = paste0(effectif, " (", prop_label, ")")),
            hjust = -0.1, size = 3) +
  coord_flip() +
  labs(x = NULL, y = "Nombre d'individus") +
  theme_minimal() +
  scale_y_continuous(expand = expansion(mult = c(0, 0.20)))

print(p_prest)

ggsave("outputs/graphs/recours_soins_prestataire.png", p_prest, width = 7, height = 4, dpi = 300)



# ==============================================================================
# ÉTAPE 6 : ANALYSE DES DÉPENSES DE SANTÉ
# ==============================================================================

# 6.1. Statistiques descriptives globales (individus malades)

stats_globales <- malades %>%
  summarise(
    mean   = mean(dep_sante_totale, na.rm = TRUE),
    sd     = sd(dep_sante_totale, na.rm = TRUE),
    median = median(dep_sante_totale, na.rm = TRUE),
    Q1     = quantile(dep_sante_totale, 0.25, na.rm = TRUE),
    Q3     = quantile(dep_sante_totale, 0.75, na.rm = TRUE),
    min    = min(dep_sante_totale, na.rm = TRUE),
    max    = max(dep_sante_totale, na.rm = TRUE)
  )

deciles <- quantile(malades$dep_sante_totale, probs = seq(0, 1, 0.1), na.rm = TRUE)

print(stats_globales)
print(deciles)
write.csv(stats_globales, "outputs/tables/stats_depenses_globales.csv",
          row.names = FALSE)
write.csv(data.frame(decile = names(deciles), valeur = deciles),
          "outputs/tables/deciles_depenses_sante.csv", row.names = FALSE)


# 6.2. Graphique : Histogramme (échelle log)

p_hist <- ggplot(malades %>% filter(dep_sante_totale > 0),
                 aes(x = dep_sante_totale)) +
  geom_histogram(bins = 40, fill = "steelblue", color = "white") +
  scale_x_log10(labels = scales::comma) +
  labs(
    x = "Dépense totale de santé (Naira, échelle log)",
    y = "Effectif"
  ) +
  theme_minimal()

print(p_hist)

ggsave("outputs/graphs/hist_depenses_sante_log.png", p_hist,
       width = 6, height = 4, dpi = 300)



# 6.3. Graphique : Boxplot des dépenses par prestataire

p_boxplot_prest <- ggplot(
  malades %>% filter(!is.na(prestataire),
                     prestataire != "Non renseigné",
                     dep_sante_totale > 0),
  aes(x = reorder(prestataire, dep_sante_totale,
                  FUN = median, na.rm = TRUE),
      y = dep_sante_totale)) +
  geom_boxplot(fill = "steelblue", width = 0.5, outlier.shape = NA) +
  scale_y_log10(labels = scales::comma) +
  coord_flip() +
  labs(x = NULL, y = "Dépense totale (Naira, échelle log)") +
  theme_minimal()

print(p_boxplot_prest)

ggsave("outputs/graphs/boxplot_depenses_par_prestataire.png", p_boxplot_prest,
       width = 7, height = 5, dpi = 300)



# 6.4. Identification des valeurs aberrantes (règle IQR)

Q1_dep  <- quantile(malades$dep_sante_totale, 0.25, na.rm = TRUE)
Q3_dep  <- quantile(malades$dep_sante_totale, 0.75, na.rm = TRUE)
IQR_dep <- Q3_dep - Q1_dep

outliers_count <- sum(malades$dep_sante_totale > (Q3_dep + 3 * IQR_dep),
                      na.rm = TRUE)
cat("\nNombre de valeurs aberrantes extrêmes (> Q3 + 3*IQR) :", outliers_count, "\n")
cat("Seuil :", Q3_dep + 3 * IQR_dep, "Naira\n")


# ==============================================================================
# ÉTAPE 7 : RECOURS AUX SOINS ET QUINTILES DE CONSOMMATION
# ==============================================================================

# 7.1. Variable binaire "a consulté un prestataire"

malades <- malades %>%
  mutate(
    a_consulte = case_when(
      !is.na(s4aq6a) & s4aq6a != 0 ~ 1,   # A consulté (pas "Aucun")
      s4aq6a == 0                   ~ 0,   # Aucun recours
      is.na(s4aq6a)                 ~ 0,   # Non renseigné -> pas de recours
      TRUE                          ~ NA_real_
    )
  )


# 7.2. Tableau de contingence quintile x recours (effectifs bruts)

tab_cons <- malades %>%
  filter(!is.na(quintile), !is.na(a_consulte)) %>%
  mutate(
    quintile_lib   = paste0("Q", quintile),
    a_consulte_lib = ifelse(a_consulte == 1, "Oui", "Non")
  ) %>%
  count(quintile_lib, a_consulte_lib) %>%
  pivot_wider(names_from = a_consulte_lib, values_from = n, values_fill = 0)

print(tab_cons)

write.csv(tab_cons, "outputs/tables/recours_par_quintile.csv", row.names = FALSE)


# 7.3. Test du Chi-deux et V de Cramér

tab_mat <- malades %>%
  filter(!is.na(quintile), !is.na(a_consulte)) %>%
  {table(.$quintile, .$a_consulte)}

chi_test <- chisq.test(tab_mat)
print(chi_test)
capture.output(print(chi_test),
               file = "outputs/tables/chi2_recours_quintile.txt")

# Fisher si effectifs < 5 (vérification)

min_eff <- min(chi_test$expected)
cat("\nEffectif théorique minimal :", round(min_eff, 1), "\n")
if (min_eff < 5) {
  fisher_test <- fisher.test(tab_mat, simulate.p.value = TRUE)
  print(fisher_test)
  cat("Test exact de Fisher appliqué (effectifs < 5)\n")
} else {
  cat("Chi-deux approprié (tous les effectifs théoriques >= 5)\n")
}

cramer <- cramer_v(tab_mat)
cat("V de Cramér :", round(cramer, 3), "\n")
write(paste("V de Cramér :", round(cramer, 3)),
      file = "outputs/tables/cramer_recours_quintile.txt")


# 7.4. Graphique : Barplot 100% empilé quintile x recours

df_quintile <- malades %>%
  filter(!is.na(quintile), !is.na(a_consulte)) %>%
  mutate(
    quintile_lib   = factor(paste0("Q", quintile), levels = paste0("Q", 1:5)),
    a_consulte_lib = ifelse(a_consulte == 1, "Oui", "Non")
  ) %>%
  count(quintile_lib, a_consulte_lib) %>%
  group_by(quintile_lib) %>%
  mutate(prop = n / sum(n))

p_cons <- ggplot(df_quintile,
                 aes(x = quintile_lib, y = prop, fill = a_consulte_lib)) +
  geom_col(position = "fill", width = 0.5) +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(
    values = c("Oui" = "steelblue", "Non" = "#76B7B2")
  ) +
  labs(
    x    = "Quintile de consommation (Q1 = plus pauvre, Q5 = plus riche)",
    y    = "Proportion",
    fill = "A consulté ?"
  ) +
  theme_minimal()

print(p_cons)

ggsave("outputs/graphs/recours_soins_par_quintile.png", p_cons,
       width = 7, height = 4, dpi = 300)



# ==============================================================================
# ÉTAPE 8 : COMPARAISON RURAL/URBAIN DES DÉPENSES DE SANTÉ
# ==============================================================================

# 8.1. Statistiques descriptives par milieu

stats_rururb <- malades %>%
  filter(!is.na(milieu)) %>%
  group_by(milieu) %>%
  summarise(
    n      = n(),
    mean   = mean(dep_sante_totale, na.rm = TRUE),
    median = median(dep_sante_totale, na.rm = TRUE),
    sd     = sd(dep_sante_totale, na.rm = TRUE),
    Q1     = quantile(dep_sante_totale, 0.25, na.rm = TRUE),
    Q3     = quantile(dep_sante_totale, 0.75, na.rm = TRUE),
    min    = min(dep_sante_totale, na.rm = TRUE),
    max    = max(dep_sante_totale, na.rm = TRUE)
  )
print(stats_rururb)
write.csv(stats_rururb, "outputs/tables/stats_depenses_rural_urbain.csv",
          row.names = FALSE)


# 8.2. Test de Wilcoxon (données brutes — pas de version pondérée standard)

wilcox_test <- wilcox.test(dep_sante_totale ~ milieu,
                            data = malades %>% filter(!is.na(milieu)))
print(wilcox_test)
capture.output(print(wilcox_test),
               file = "outputs/tables/wilcoxon_depenses_rural_urbain.txt")


# 8.3. Graphique : Violin plot + boxplot superposé (échelle log)

p_violin <- ggplot(
  malades %>% filter(!is.na(milieu), dep_sante_totale > 0),
  aes(x = milieu, y = dep_sante_totale, fill = milieu)
) +
  geom_violin(trim = FALSE, alpha = 0.6) +
  geom_boxplot(width = 0.1, fill = "white", outlier.shape = NA) +
  scale_y_log10(labels = scales::comma) +
  scale_fill_manual(values = c("Urbain" = "steelblue", "Rural" = "darkgreen")) +
  labs(
    x    = NULL,
    y    = "Dépense totale de santé (Naira, échelle log)"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

print(p_violin)

ggsave("outputs/graphs/violin_depenses_rural_urbain.png", p_violin,
       width = 5, height = 4, dpi = 300)

