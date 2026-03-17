# ==============================================================================
# TP3 : Accès aux services de santé et chocs sanitaires des ménages (en utilisant la vague 2)
# ==============================================================================



# 0. CHARGEMENT DES LIBRAIRIES

if (!require("pacman")) install.packages("pacman") # Installer et charger pacman si nécessaire
library(pacman)

p_load(haven, dplyr, ggplot2, naniar, tidyr, scales, rstatix) # Utiliser p_load pour charger les librairies (les installe si absentes)


# 1.1. CRÉATION DES DOSSIERS DE SORTIE
#dir.create("outputs/tables", recursive = TRUE, showWarnings = FALSE)
#dir.create("outputs/graphs", recursive = TRUE, showWarnings = FALSE)


# 1.2. CHARGEMENT DES FICHIERS
sect4a_w2 <- read_dta("data/raw/sect4a_harvestw2.dta")
sect1_w2  <- read_dta("data/raw/sect1_harvestw2.dta")
cons_w2   <- read_dta("data/raw/cons_agg_wave2_visit2.dta")

# 1.3. EXAMEN DE LA STRUCTURE
cat("\n--- Structure de sect4a_w2 ---\n")
str(sect4a_w2, max.level = 1)
glimpse(sect4a_w2)

cat("\n--- Structure de sect1_w2 ---\n")
str(sect1_w2, max.level = 1)
glimpse(sect1_w2)

cat("\n--- Structure de cons_w2 ---\n")
str(cons_w2, max.level = 1)
glimpse(cons_w2)

# 1.4. VÉRIFICATION DES DOUBLONS (HHID, INDIV)
# Note : cons_w2 est au niveau ménage, pas d'indiv
doublons_s4a <- sect4a_w2 %>%
  group_by(hhid, indiv) %>%
  filter(n() > 1) %>%
  nrow()
cat("\nDoublons dans sect4a_w2 :", doublons_s4a, "\n")


doublons_s1 <- sect1_w2 %>%
  group_by(hhid, indiv) %>%
  filter(n() > 1) %>%
  nrow()
cat("Doublons dans sect1_w2 :", doublons_s1, "\n")


# 1.4. VISUALISATION DES VALEURS MANQUANTES (VARIABLES CLÉS)

# La visualisation avec vis_miss n'est pas trés claire du au nombre élevés de variables 


# Sect4a_w2 (santé) 

vars_s4a <- c("s4aq3", "s4aq6a", "s4aq9", "s4aq14", "s4aq17", "s4aq19", "s4aq1")
p_miss_s4a <- sect4a_w2 %>%
  select(any_of(vars_s4a)) %>%
  gg_miss_var(show_pct = TRUE) +
  labs(
    title = "Valeurs manquantes - Sect4a (santé)",
    x = "Variable",
    y = "% de NA"
  ) +
  theme_minimal()
print(p_miss_s4a)
ggsave("outputs/graphs/missing_sect4a.png", p_miss_s4a, width = 8, height = 5, dpi = 300)

# Sect1_w2 (démographie)

vars_s1 <- c("s1q2", "s1q4", "sector")
p_miss_s1 <- sect1_w2 %>%
  select(any_of(vars_s1)) %>%
  gg_miss_var(show_pct = TRUE) +
  labs(
    title = "Valeurs manquantes - Sect1 (démographie)",
    x = "Variable",
    y = "% de NA"
  ) +
  theme_minimal()
print(p_miss_s1)
ggsave("outputs/graphs/missing_sect1.png", p_miss_s1, width = 8, height = 5, dpi = 300)

# Cons_w2 (consommation)

vars_cons <- c("totcons", "rururb", "hhsize")
p_miss_cons <- cons_w2 %>%
  select(any_of(vars_cons)) %>%
  gg_miss_var(show_pct = TRUE) +
  labs(
    title = "Valeurs manquantes - Consommation",
    x = "Variable",
    y = "% de NA"
  ) +
  theme_minimal()
print(p_miss_cons)
ggsave("outputs/graphs/missing_cons.png", p_miss_cons, width = 8, height = 5, dpi = 300)


# 1.5. Visualisation des valeurs manquantes avec vis_miss (variables clés)

# Pour sect4a_w2
p_vis_s4a <- sect4a_w2 %>%
  select(any_of(vars_s4a)) %>%
  vis_miss(warn_large_data = FALSE) +
  labs(title = "Patterns de valeurs manquantes - Sect4a (santé)")
print(p_vis_s4a)
ggsave("outputs/graphs/vis_miss_sect4a.png", p_vis_s4a, width = 10, height = 6, dpi = 300)

# Pour sect1_w2
p_vis_s1 <- sect1_w2 %>%
  select(any_of(vars_s1)) %>%
  vis_miss(warn_large_data = FALSE) +
  labs(title = "Patterns de valeurs manquantes - Sect1 (démographie)")
print(p_vis_s1)
ggsave("outputs/graphs/vis_miss_sect1.png", p_vis_s1, width = 8, height = 5, dpi = 300)

# Pour cons_w2
p_vis_cons <- cons_w2 %>%
  select(any_of(vars_cons)) %>%
  vis_miss(warn_large_data = FALSE) +
  labs(title = "Patterns de valeurs manquantes - Consommation")
print(p_vis_cons)
ggsave("outputs/graphs/vis_miss_cons.png", p_vis_cons, width = 8, height = 5, dpi = 300)


# 1.6. TABLEAUX RÉCAPITULATIFS DES NA (TOUTES VARIABLES)

# Sect4a_w2

missing_s4a <- sect4a_w2 %>%
  summarise(across(everything(), ~ sum(is.na(.)))) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "nb_na") %>%
  mutate(pct_na = round(nb_na / nrow(sect4a_w2) * 100, 2)) %>%
  arrange(desc(nb_na))
write.csv(missing_s4a, "outputs/tables/missing_summary_sect4a.csv", row.names = FALSE)

# Sect1_w2 

missing_s1 <- sect1_w2 %>%
  summarise(across(everything(), ~ sum(is.na(.)))) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "nb_na") %>%
  mutate(pct_na = round(nb_na / nrow(sect1_w2) * 100, 2)) %>%
  arrange(desc(nb_na))
write.csv(missing_s1, "outputs/tables/missing_summary_sect1.csv", row.names = FALSE)

# Cons_w2

missing_cons <- cons_w2 %>%
  summarise(across(everything(), ~ sum(is.na(.)))) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "nb_na") %>%
  mutate(pct_na = round(nb_na / nrow(cons_w2) * 100, 2)) %>%
  arrange(desc(nb_na))
write.csv(missing_cons, "outputs/tables/missing_summary_cons.csv", row.names = FALSE)

cat("Graphiques et tableaux des valeurs manquantes sauvegardés.\n")


# 1.7. IDENTIFICATION DES VARIABLES CLÉS

cat("\nVariables clés identifiées :\n")
cat("Dans sect4a_w2 : s4aq3 (maladie/blessure), s4aq6a (prestataire), s4aq9 (coût consultation),\n")
cat("                s4aq14 (médicaments OTC), s4aq17 (hospitalisation), s4aq19 (autres médicaments)\n")
cat("Dans sect1_w2  : s1q2 (sexe), s1q4 (âge), sector (milieu : 1=urbain, 2=rural)\n")
cat("Dans cons_w2   : totcons (consommation totale), hhid (identifiant ménage)\n")


# 1.8. COHÉRENCE DES IDENTIFIANTS (SANTÉ vs CONSOMMATION)

communs <- intersect(unique(sect4a_w2$hhid), unique(cons_w2$hhid))
cat("\nNombre de ménages communs entre santé et consommation :", length(communs), "\n")



# ==============================================================================
# ÉTAPE 2 : TAUX DE MORBIDITÉ GLOBAL ET PAR SOUS-GROUPES
# ==============================================================================

# 2.1. Jointure santé et démographie 
health_demo <- sect4a_w2 %>%
  left_join(sect1_w2 %>% select(hhid, indiv, s1q2, s1q4, sector),
            by = c("hhid", "indiv"))

# 2.2. Création variable morbidité (1 = maladie/blessure) 
health_demo <- health_demo %>%
  mutate(morbid = case_when(
    s4aq3 %in% c(1, 2) ~ 1,
    s4aq3 == 3         ~ 0,
    TRUE               ~ NA_real_
  ))

# 2.3. Taux global 
n_total   <- sum(!is.na(health_demo$morbid))
n_cases   <- sum(health_demo$morbid == 1, na.rm = TRUE)
prop_global <- n_cases / n_total
ic_global <- binom.test(n_cases, n_total)$conf.int

global_taux <- data.frame(
  indicateur = "Taux de morbidité global",
  effectif   = n_total,
  cas        = n_cases,
  proportion = prop_global,
  ic_inf     = ic_global[1],
  ic_sup     = ic_global[2]
)
write.csv(global_taux, "outputs/tables/taux_morbidite_global.csv", row.names = FALSE)

# 2.4. Taux par sexe 
taux_sexe <- health_demo %>%
  filter(!is.na(s1q2), !is.na(morbid)) %>%
  group_by(sexe = s1q2) %>%
  summarise(
    n       = n(),
    n_cases = sum(morbid == 1),
    prop    = n_cases / n,
    ic_low  = binom.test(n_cases, n)$conf.int[1],
    ic_high = binom.test(n_cases, n)$conf.int[2]
  ) %>%
  mutate(sexe = ifelse(sexe == 1, "Homme", "Femme"))

write.csv(taux_sexe, "outputs/tables/taux_morbidite_par_sexe.csv", row.names = FALSE)

# 2.5. Création groupes d'âge 
health_demo <- health_demo %>%
  mutate(age_group = case_when(
    s1q4 <= 14 ~ "0-14 ans",
    s1q4 <= 30 ~ "15-30 ans",
    s1q4 <= 45 ~ "31-45 ans",
    s1q4 <= 60 ~ "46-60 ans",
    s1q4 > 60  ~ "61 ans et plus",
    TRUE       ~ NA_character_
  ))

# 2.6. Taux par groupe d'âge 
taux_age <- health_demo %>%
  filter(!is.na(age_group), !is.na(morbid)) %>%
  group_by(age_group) %>%
  summarise(
    n       = n(),
    n_cases = sum(morbid == 1),
    prop    = n_cases / n,
    ic_low  = binom.test(n_cases, n)$conf.int[1],
    ic_high = binom.test(n_cases, n)$conf.int[2]
  ) %>%
  arrange(age_group)

write.csv(taux_age, "outputs/tables/taux_morbidite_par_age.csv", row.names = FALSE)

# 2.7. Graphique : Barplot par sexe 
p_sexe <- ggplot(taux_sexe, aes(x = sexe, y = prop, fill = sexe)) +
  geom_col(width = 0.6) +
  geom_errorbar(aes(ymin = ic_low, ymax = ic_high), width = 0.2) +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = "Taux de morbidité (maladie ou blessure) par sexe",
    x = NULL,
    y = "Proportion"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

ggsave("outputs/graphs/taux_morbidite_par_sexe.png", p_sexe, width = 6, height = 4, dpi = 300)

# 2.8. Graphique : Barplot par âge 
p_age <- ggplot(taux_age, aes(x = age_group, y = prop, fill = age_group)) +
  geom_col(width = 0.6) +
  geom_errorbar(aes(ymin = ic_low, ymax = ic_high), width = 0.2) +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = "Taux de morbidité par groupe d'âge",
    x = "Groupe d'âge",
    y = "Proportion"
  ) +
  theme_minimal() +
  theme(legend.position = "none", axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("outputs/graphs/taux_morbidite_par_age.png", p_age, width = 7, height = 4, dpi = 300)

# 2.9. Affichage console pour validation 
cat("\nTaux de morbidité global \n")
print(global_taux)
cat("\nTaux par sexe \n")
print(taux_sexe)
cat("\nTaux par groupe d'âge \n")
print(taux_age)




# ==============================================================================
# ÉTAPE 3 : ANALYSE DES TYPES DE PROBLÈMES DE SANTÉ (MALADIE VS BLESSURE)
# ==============================================================================

# 3.1. Filtrage des individus ayant eu un problème (morbid = 1) 
health_cases <- health_demo %>%
  filter(morbid == 1, !is.na(s4aq3)) %>%
  mutate(type = ifelse(s4aq3 == 1, "Maladie", "Blessure"))

# 3.2. Tableau des effectifs et proportions 
type_table <- health_cases %>%
  group_by(type) %>%
  summarise(
    n         = n(),
    prop      = n / nrow(health_cases)
  ) %>%
  mutate(prop_label = scales::percent(prop, accuracy = 0.1))

print(type_table)
write.csv(type_table, "outputs/tables/types_problemes_sante.csv", row.names = FALSE)

# 3.3. Graphique : Répartition 
p_type <- ggplot(type_table, aes(x = type, y = prop, fill = type)) +
  geom_col(width = 0.5) +
  geom_text(aes(label = prop_label), vjust = -0.5, size = 4) +
  scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
  labs(
    title = "Répartition maladie vs blessure",
    subtitle = "au cours des 4 dernières semaines",
    x = NULL,
    y = "Proportion"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

print(p_type)
ggsave("outputs/graphs/maladie_vs_blessure.png", p_type, width = 5, height = 4, dpi = 300)

# 3.4. Affichage console 
cat("\nRépartition maladie / blessure \n")
print(type_table)




# ==============================================================================
# ÉTAPE 4 : ANALYSE DES MOTIFS DE CONSULTATION (10 PRINCIPAUX)
# ==============================================================================

# 4.1. Filtrage des individus ayant consulté (s4aq1 == 1) 
consultants <- health_demo %>%
  filter(s4aq1 == 1, !is.na(s4aq2a))

# 4.2. Tableau des fréquences des motifs (s4aq2a) 
# Modalités : 1=Check-up, 2=Prénatal, 3=Accouchement, 4=Suivi chronique, 
#             5=Suivi accident, 6=Maladie aiguë, 7=Blessure, 8=Autre
motifs <- consultants %>%
  count(s4aq2a, name = "effectif") %>%
  mutate(
    proportion = effectif / sum(effectif),
    motif_lib  = case_when(
      s4aq2a == 1 ~ "Check-up / préventif",
      s4aq2a == 2 ~ "Consultation prénatale",
      s4aq2a == 3 ~ "Accouchement",
      s4aq2a == 4 ~ "Suivi maladie chronique",
      s4aq2a == 5 ~ "Suivi accident",
      s4aq2a == 6 ~ "Maladie aiguë",
      s4aq2a == 7 ~ "Blessure",
      s4aq2a == 8 ~ "Autre",
      TRUE        ~ as.character(s4aq2a)
    ),
    categorie = case_when(
      s4aq2a %in% c(6)        ~ "Maladie aiguë",
      s4aq2a %in% c(4, 5)     ~ "Suivi (chronique/accident)",
      s4aq2a %in% c(7)        ~ "Blessure",
      s4aq2a %in% c(1, 2, 3)  ~ "Préventif / Maternité",
      s4aq2a == 8             ~ "Autre",
      TRUE                    ~ "Autre"
    )
  ) %>%
  arrange(desc(effectif))

# 4.3. Sélection des top motifs 
top_motifs <- motifs

write.csv(top_motifs, "outputs/tables/motifs_consultation_top10.csv", row.names = FALSE)

# 4.4. Graphique : Barplot horizontal 
p_motifs <- ggplot(top_motifs, aes(x = reorder(motif_lib, effectif), y = effectif, fill = categorie)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Motifs de consultation (4 dernières semaines)",
    subtitle = "Classement par fréquence",
    x = NULL,
    y = "Nombre de consultations"
  ) +
  scale_fill_brewer(palette = "Set2", name = "Catégorie") +
  theme_minimal() +
  theme(legend.position = "bottom")

print(p_motifs)
ggsave("outputs/graphs/motifs_consultation_top10.png", p_motifs, width = 8, height = 5, dpi = 300)

# 4.5. Affichage console 
cat("\nMotifs de consultation \n")
print(top_motifs)




# ==============================================================================
# ÉTAPE 5 : RECOURS AUX SOINS – TYPE DE PRESTATAIRE
# ==============================================================================

# 5.1. Restriction aux individus malades (morbid = 1) 
malades <- health_demo %>% filter(morbid == 1)

# 5.2. Catégorisation du type de prestataire 
malades <- malades %>%
  mutate(
    prestataire = case_when(
      s4aq6a %in% c(2, 3, 4, 5, 6) ~ "Médecin / infirmier / sage-femme",
      s4aq6a %in% c(7, 8, 11)      ~ "Pharmacie / vendeur de médicaments",
      s4aq6a %in% c(1, 9, 10)      ~ "Tradipraticien / guérisseur",
      s4aq6a == 12                 ~ "Aucun",
      s4aq6a == 13                 ~ "Autre",
      is.na(s4aq6a)                ~ "Non renseigné",
      TRUE                         ~ "Autre (code non standard)"
    )
  )

# 5.3. Tableau des effectifs et proportions 
prestataire_table <- malades %>%
  filter(!is.na(prestataire)) %>%
  group_by(prestataire) %>%
  summarise(
    n         = n(),
    prop      = n / nrow(malades)
  ) %>%
  arrange(desc(n)) %>%
  mutate(prop_label = scales::percent(prop, accuracy = 0.1))

write.csv(prestataire_table, "outputs/tables/recours_soins_prestataire.csv", row.names = FALSE)

# 5.4. Graphique : Barplot horizontal 
p_prest <- ggplot(prestataire_table, aes(x = reorder(prestataire, n), y = n, fill = prestataire)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Type de prestataire consulté (premier recours)",
    subtitle = "Individus ayant déclaré une maladie ou blessure",
    x = NULL,
    y = "Nombre d'individus"
  ) +
  theme_minimal() +
  theme(legend.position = "none") +
  geom_text(aes(label = n), hjust = -0.1, size = 3)

print(p_prest)
ggsave("outputs/graphs/recours_soins_prestataire.png", p_prest, width = 8, height = 5, dpi = 300)

# 5.5. Affichage console 
cat("\nRépartition du type de prestataire consulté \n")
print(prestataire_table)




# ==============================================================================
# ÉTAPE 6 : ANALYSE DES DÉPENSES DE SANTÉ
# ==============================================================================

# 6.1. Création variable de dépense totale 
health_demo <- health_demo %>%
  mutate(
    dep_consult    = ifelse(is.na(s4aq9), 0, s4aq9),
    dep_medoc_otc  = ifelse(is.na(s4aq14), 0, s4aq14),
    dep_hosp       = ifelse(is.na(s4aq17), 0, s4aq17),
    dep_medoc_autres = ifelse(is.na(s4aq19), 0, s4aq19),
    dep_sante_totale = dep_consult + dep_medoc_otc + dep_hosp + dep_medoc_autres
  )

# 6.2. Restriction aux individus malades 
malades <- health_demo %>% filter(morbid == 1)

# 6.3. Statistiques descriptives globales 
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

write.csv(stats_globales, "outputs/tables/stats_depenses_globales.csv", row.names = FALSE)
write.csv(data.frame(decile = names(deciles), valeur = deciles), 
          "outputs/tables/deciles_depenses_sante.csv", row.names = FALSE)

# 6.4. Graphique : Histogramme (échelle log) 
p_hist <- ggplot(malades, aes(x = dep_sante_totale + 1)) +
  geom_histogram(bins = 50, fill = "steelblue", color = "white") +
  scale_x_log10(labels = scales::comma) +
  labs(
    title = "Distribution des dépenses de santé (échelle log)",
    subtitle = "Individus ayant déclaré une maladie/blessure",
    x = "Dépense totale (Naira, log10)",
    y = "Effectif"
  ) +
  theme_minimal()

print(p_hist)
ggsave("outputs/graphs/hist_depenses_sante_log.png", p_hist, width = 8, height = 5, dpi = 300)

# 6.5. Graphique : Boxplot des dépenses par prestataire 
if (!"prestataire" %in% names(malades)) {
  malades <- malades %>%
    mutate(
      prestataire = case_when(
        s4aq6a %in% c(2, 3, 4, 5, 6) ~ "Médecin / infirmier / sage-femme",
        s4aq6a %in% c(7, 8, 11)      ~ "Pharmacie / vendeur de médicaments",
        s4aq6a %in% c(1, 9, 10)      ~ "Tradipraticien / guérisseur",
        s4aq6a == 12                 ~ "Aucun",
        s4aq6a == 13                 ~ "Autre",
        is.na(s4aq6a)                ~ "Non renseigné",
        TRUE                         ~ "Autre"
      )
    )
}

p_boxplot <- ggplot(malades %>% filter(!is.na(prestataire), dep_sante_totale > 0), 
                    aes(x = reorder(prestataire, dep_sante_totale, FUN = median, na.rm = TRUE), 
                        y = dep_sante_totale + 1, fill = prestataire)) +
  geom_boxplot() +
  scale_y_log10(labels = scales::comma) +
  coord_flip() +
  labs(
    title = "Dépenses de santé selon le premier prestataire consulté",
    x = NULL,
    y = "Dépense totale (Naira, log10)"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

print(p_boxplot)
ggsave("outputs/graphs/boxplot_depenses_par_prestataire.png", p_boxplot, width = 8, height = 6, dpi = 300)

# 6.6. Identification des outliers (règle IQR) 
malades <- malades %>%
  mutate(
    Q1      = quantile(dep_sante_totale, 0.25, na.rm = TRUE),
    Q3      = quantile(dep_sante_totale, 0.75, na.rm = TRUE),
    IQR     = Q3 - Q1,
    outlier = dep_sante_totale > (Q3 + 3 * IQR)
  )

outliers_count <- sum(malades$outlier, na.rm = TRUE)
cat("\nNombre d'outliers extrêmes (dépenses > Q3 + 3*IQR) :", outliers_count, "\n")

# 6.7. Affichage des résultats 
cat("\nStatistiques globales des dépenses de santé \n")
print(stats_globales)

cat("\nDéciles \n")
print(deciles)




# ==============================================================================
# ÉTAPE 7 : RECOURS AUX SOINS ET QUINTILES DE CONSOMMATION
# ==============================================================================

# 7.1. Calcul des quintiles de consommation (cons_w2) 
quintiles <- cons_w2 %>%
  mutate(
    quintile = ntile(totcons, 5)
  ) %>%
  select(hhid, quintile)

# 7.2. Jointure aux malades 
malades <- malades %>%
  left_join(quintiles, by = "hhid")

# 7.3. Variable binaire "a consulté" 
malades <- malades %>%
  mutate(
    a_consulte = ifelse(!is.na(s4aq6a) & s4aq6a != 12, 1, 0)
  )

# 7.4. Tableau de contingence et tests 
tab_cons  <- table(malades$quintile, malades$a_consulte, dnn = c("Quintile", "Consulté"))
print(tab_cons)

chi_test  <- chisq.test(tab_cons)
print(chi_test)

cramer_v  <- cramer_v(tab_cons)
print(cramer_v)

write.csv(as.data.frame.matrix(tab_cons), "outputs/tables/recours_par_quintile.csv")

# 7.5. Graphique : Barplot 100% empilé 
props     <- prop.table(tab_cons, margin = 1)
df_props  <- as.data.frame(props)
names(df_props) <- c("recours", "quintile", "Freq")

p_cons <- ggplot(df_props, aes(x = quintile, y = Freq, fill = recours)) +
  geom_col(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = "Recours aux soins selon le quintile de consommation",
    x = "Quintile de consommation (1 = plus pauvre, 5 = plus riche)",
    y = "Proportion",
    fill = "A consulté ?"
  ) +
  theme_minimal()

print(p_cons)
ggsave("outputs/graphs/recours_soins_par_quintile.png", p_cons, width = 7, height = 5, dpi = 300)




# ==============================================================================
# ÉTAPE 8 : COMPARAISON RURAL/URBAIN DES DÉPENSES DE SANTÉ
# ==============================================================================

# 8.1. Jointure du secteur (sector) 
if (!"sector" %in% names(malades)) {
  sector_info <- sect1_w2 %>% select(hhid, indiv, sector)
  malades <- malades %>%
    left_join(sector_info, by = c("hhid", "indiv"))
}

# 8.2. Filtrage des données complètes 
malades <- malades %>% filter(!is.na(sector))

# 8.3. Statistiques descriptives par milieu 
stats_rururb <- malades %>%
  group_by(milieu = ifelse(sector == 1, "Urbain", "Rural")) %>%
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
write.csv(stats_rururb, "outputs/tables/stats_depenses_rural_urbain.csv", row.names = FALSE)

# 8.4. Test de Wilcoxon 
wilcox_test <- wilcox.test(dep_sante_totale ~ sector, data = malades)
print(wilcox_test)
capture.output(print(wilcox_test), file = "outputs/tables/wilcoxon_depenses_rural_urbain.txt")

# 8.5. Graphique : Violin plot + boxplot (échelle log) 
p_violin <- ggplot(malades, aes(x = factor(sector, labels = c("Urbain", "Rural")), 
                                y = dep_sante_totale + 1, fill = factor(sector))) +
  geom_violin(trim = FALSE, alpha = 0.6) +
  geom_boxplot(width = 0.1, fill = "white", outlier.shape = NA) +
  scale_y_log10(labels = scales::comma) +
  labs(
    title = "Distribution des dépenses de santé selon le milieu de résidence",
    x = NULL,
    y = "Dépense totale (Naira, log10)",
    fill = "Milieu"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

print(p_violin)
ggsave("outputs/graphs/violin_depenses_rural_urbain.png", p_violin, width = 6, height = 5, dpi = 300)



