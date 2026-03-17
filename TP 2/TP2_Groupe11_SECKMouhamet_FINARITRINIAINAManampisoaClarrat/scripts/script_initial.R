# ==============================================================================
# TP2 : Éducation et alphabétisation (Vague 4)
# ==============================================================================

# 0. CHARGEMENT DES LIBRAIRIES AVEC PACMAN
if (!require("pacman")) install.packages("pacman")
library(pacman)
p_load(haven, dplyr, ggplot2, naniar, tidyr, scales, rstatix, gtsummary, gt, forcats, binom)

# Création des dossiers de sortie (si pas déjà fait)
dir.create("outputs/tables", recursive = TRUE, showWarnings = FALSE)
dir.create("outputs/graphs", recursive = TRUE, showWarnings = FALSE)

# 1.1. Chargement des fichiers
sect1_w4 <- read_dta("data/raw/sect1_harvestw4.dta")
sect2_w4 <- read_dta("data/raw/sect2_harvestw4.dta")
secta_w4 <- read_dta("data/raw/secta_harvestw4.dta")

# 1.2. Examen de la structure
cat("\n--- Structure de sect1_w4 ---\n")
str(sect1_w4, max.level = 1)
glimpse(sect1_w4)

cat("\n--- Structure de sect2_w4 ---\n")
str(sect2_w4, max.level = 1)
glimpse(sect2_w4)

cat("\n--- Structure de secta_w4 ---\n")
str(secta_w4, max.level = 1)
glimpse(secta_w4)

# 1.3. Vérification des doublons (hhid, indiv) dans sect1 et sect2
doublons_s1 <- sect1_w4 %>%
  group_by(hhid, indiv) %>%
  filter(n() > 1) %>%
  nrow()
cat("\nDoublons dans sect1_w4 :", doublons_s1, "\n")

doublons_s2 <- sect2_w4 %>%
  group_by(hhid, indiv) %>%
  filter(n() > 1) %>%
  nrow()
cat("Doublons dans sect2_w4 :", doublons_s2, "\n")

# 1.4. Variables clés pour l'analyse
# Sect1 : s1q2 (sexe), s1q4 (âge), sector (milieu), state (état)
# Sect2 : s2aq5 (alphabétisation), s2aq6 (a déjà été à l'école), s2aq9 (niveau d'éducation), s2aq13a (scolarisation actuelle), s2aq2 (âge filtre)

# 1.5. Analyse des valeurs manquantes pour les variables clés
vars_cles_s1 <- c("s1q2", "s1q4", "sector", "state")
vars_cles_s2 <- c("s2aq5", "s2aq6", "s2aq9", "s2aq13a", "s2aq2")

# Graphiques gg_miss_var
p_miss_s1 <- sect1_w4 %>%
  select(any_of(vars_cles_s1)) %>%
  gg_miss_var(show_pct = TRUE) +
  labs(title = "Valeurs manquantes - sect1 (démographie)", x = "Variable", y = "% de NA") +
  theme_minimal()
print(p_miss_s1)
ggsave("outputs/graphs/missing_sect1_tp2.png", p_miss_s1, width = 8, height = 5, dpi = 300)

p_miss_s2 <- sect2_w4 %>%
  select(any_of(vars_cles_s2)) %>%
  gg_miss_var(show_pct = TRUE) +
  labs(title = "Valeurs manquantes - sect2 (éducation)", x = "Variable", y = "% de NA") +
  theme_minimal()
print(p_miss_s2)
ggsave("outputs/graphs/missing_sect2_tp2.png", p_miss_s2, width = 8, height = 5, dpi = 300)

# Tableaux récapitulatifs des NA pour tous les fichiers (en annexe)
missing_s1 <- sect1_w4 %>%
  summarise(across(everything(), ~ sum(is.na(.)))) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "nb_na") %>%
  mutate(pct_na = round(nb_na / nrow(sect1_w4) * 100, 2)) %>%
  arrange(desc(nb_na))
write.csv(missing_s1, "outputs/tables/missing_summary_sect1_tp2.csv", row.names = FALSE)

missing_s2 <- sect2_w4 %>%
  summarise(across(everything(), ~ sum(is.na(.)))) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "nb_na") %>%
  mutate(pct_na = round(nb_na / nrow(sect2_w4) * 100, 2)) %>%
  arrange(desc(nb_na))
write.csv(missing_s2, "outputs/tables/missing_summary_sect2_tp2.csv", row.names = FALSE)

missing_secta <- secta_w4 %>%
  summarise(across(everything(), ~ sum(is.na(.)))) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "nb_na") %>%
  mutate(pct_na = round(nb_na / nrow(secta_w4) * 100, 2)) %>%
  arrange(desc(nb_na))
write.csv(missing_secta, "outputs/tables/missing_summary_secta_tp2.csv", row.names = FALSE)

cat("Graphiques et tableaux des valeurs manquantes sauvegardés.\n")

# 1.6. Cohérence des identifiants entre les fichiers
communs_s1_s2 <- intersect(unique(sect1_w4$hhid), unique(sect2_w4$hhid))
communs_s1_secta <- intersect(unique(sect1_w4$hhid), unique(secta_w4$hhid))
cat("\nMénages communs entre sect1 et sect2 :", length(communs_s1_s2), "\n")
cat("Ménages communs entre sect1 et secta :", length(communs_s1_secta), "\n")

# 1.7. Identification des variables clés (rappel)
cat("\nVariables clés identifiées :\n")
cat("- s1q2 : sexe (1=Homme, 2=Femme)\n")
cat("- s1q4 : âge (années)\n")
cat("- sector : milieu (1=Urbain, 2=Rural)\n")
cat("- state : État (codes 1-37)\n")
cat("- s2aq5 : alphabétisation (1=Oui, 2=Non)\n")
cat("- s2aq6 : a déjà été à l'école (1=Oui, 2=Non)\n")
cat("- s2aq9 : niveau d'éducation le plus élevé (codes détaillés)\n")
cat("- s2aq13a : scolarisation actuelle (1=Oui, 2=Non)\n")
cat("- s2aq2 : âge de l'individu (dans sect2, mais on utilisera s1q4 pour l'âge)\n")




# Voir les labels de s2aq9
attr(sect2_w4$s2aq9, "labels")





# ==============================================================================
# ÉTAPE 2 : CONSTRUCTION DE LA VARIABLE NIVEAU D'ÉDUCATION
# ==============================================================================

# 2.1. Créer une copie de travail
sect2_educ <- sect2_w4

# 2.2. Définir les correspondances
sect2_educ <- sect2_educ %>%
  mutate(
    niveau_educ = case_when(
      s2aq9 %in% c(0, 1, 2, 3) ~ "Aucun",
      s2aq9 %in% 11:16          ~ "Primaire",
      s2aq9 %in% 21:23          ~ "Junior Secondary",
      s2aq9 %in% c(24:28, 321)  ~ "Senior Secondary",
      s2aq9 %in% c(31,33,34,35,41,43,322,411,412,421,422,423,424,51,52,61) ~ "Tertiaire",
      TRUE ~ NA_character_
    ),
    # On peut aussi créer une variable numérique ordonnée pour les analyses
    niveau_num = case_when(
      niveau_educ == "Aucun" ~ 0,
      niveau_educ == "Primaire" ~ 1,
      niveau_educ == "Junior Secondary" ~ 2,
      niveau_educ == "Senior Secondary" ~ 3,
      niveau_educ == "Tertiaire" ~ 4,
      TRUE ~ NA_real_
    )
  )

# 2.3. Vérifier les effectifs
table(sect2_educ$niveau_educ, useNA = "ifany")

# 2.4. Sauvegarder un tableau récapitulatif
niveau_summary <- sect2_educ %>%
  group_by(niveau_educ) %>%
  summarise(n = n(), .groups = "drop") %>%
  mutate(prop = n / sum(n))
write.csv(niveau_summary, "outputs/tables/niveau_educ_frequences.csv", row.names = FALSE)

# 2.5. Joindre avec sect1 pour obtenir sexe, âge, zone
# D'abord, sélectionner les variables nécessaires dans sect1
sect1_demo <- sect1_w4 %>%
  select(hhid, indiv, sexe = s1q2, age = s1q4, secteur = sector, etat = state)

# Jointure
educ_demo <- sect2_educ %>%
  left_join(sect1_demo, by = c("hhid", "indiv"))

# Vérifier le taux de jointure
cat("Taux de jointure :", nrow(educ_demo) / nrow(sect2_educ), "\n")

# Sauvegarder la base pour les analyses ultérieures
saveRDS(educ_demo, "outputs/tables/educ_demo.rds")











# ==============================================================================
# ÉTAPE 3 : ANALYSE UNIVARIÉE DU NIVEAU D'ÉDUCATION
# ==============================================================================

# 3.1. Tableau des fréquences et proportions (déjà fait, mais on peut le refaire avec la base jointe)
niveau_summary <- educ_demo %>%
  filter(!is.na(niveau_educ)) %>%
  group_by(niveau_educ) %>%
  summarise(n = n(), .groups = "drop") %>%
  mutate(
    prop = n / sum(n),
    prop_label = scales::percent(prop, accuracy = 0.1)
  ) %>%
  arrange(desc(n))

write.csv(niveau_summary, "outputs/tables/niveau_educ_frequences_final.csv", row.names = FALSE)

# 3.2. Barplot horizontal ordonné par fréquence
p_niveau <- ggplot(niveau_summary, aes(x = reorder(niveau_educ, n), y = n, fill = niveau_educ)) +
  geom_col() +
  geom_text(aes(label = paste0(n, " (", prop_label, ")")), hjust = -0.1, size = 3) +
  coord_flip() +
  labs(
    title = "Répartition du niveau d'éducation le plus élevé",
    x = NULL,
    y = "Effectif"
  ) +
  theme_minimal() +
  theme(legend.position = "none") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15)))

ggsave("outputs/graphs/niveau_educ_barplot.png", p_niveau, width = 7, height = 5, dpi = 300)
print(p_niveau)

# 3.3. Afficher le tableau
print(niveau_summary)






# ==============================================================================
# ÉTAPE 4 : COMPARAISON HOMMES/FEMMES DU NIVEAU D'ÉDUCATION (ADULTES 18+)
# ==============================================================================

# 4.1. Filtrer les adultes (âge >= 18) et garder les non manquants
adultes <- educ_demo %>%
  filter(!is.na(age), age >= 18, !is.na(sexe), !is.na(niveau_educ)) %>%
  mutate(
    sexe = factor(sexe, levels = c(1, 2), labels = c("Homme", "Femme")),
    niveau_educ = factor(niveau_educ, 
                         levels = c("Aucun", "Primaire", "Junior Secondary", 
                                    "Senior Secondary", "Tertiaire"))
  )

# 4.2. Tableau de contingence
tab_sexe_niveau <- table(adultes$sexe, adultes$niveau_educ)
print("Tableau de contingence :")
print(tab_sexe_niveau)

# Sauvegarde
write.csv(as.data.frame.matrix(tab_sexe_niveau), 
          "outputs/tables/contingence_sexe_niveau.csv")

# 4.3. Test du chi-deux
chi_test <- chisq.test(tab_sexe_niveau)
print(chi_test)

# Sauvegarde des résultats du test
capture.output(print(chi_test), file = "outputs/tables/chi2_sexe_niveau.txt")

# 4.4. V de Cramér
library(rstatix)
cramer_v <- cramer_v(tab_sexe_niveau)
cat("\nV de Cramér :", round(cramer_v, 3), "\n")
write(paste("V de Cramér :", round(cramer_v, 3)), 
      file = "outputs/tables/cramer_sexe_niveau.txt")

# 4.5. Graphique : barres 100% empilées côte à côte
# Calcul des proportions par sexe
props <- prop.table(tab_sexe_niveau, margin = 1)  # proportions en ligne (par sexe)
df_props <- as.data.frame(props)
names(df_props) <- c("sexe", "niveau_educ", "Freq")

p_sexe_niveau <- ggplot(df_props, aes(x = sexe, y = Freq, fill = niveau_educ)) +
  geom_col(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = "Niveau d'éducation des adultes (18+) par sexe",
    x = NULL,
    y = "Proportion",
    fill = "Niveau d'éducation"
  ) +
  theme_minimal()

print(p_sexe_niveau)
ggsave("outputs/graphs/education_par_sexe.png", p_sexe_niveau, width = 7, height = 5, dpi = 300)








# ==============================================================================
# ÉTAPE 5 : RELATION ENTRE ÂGE ET NIVEAU D'ÉDUCATION (ADULTES 18+)
# ==============================================================================

# 5.1. Créer des groupes d'âge pour l'analyse (en utilisant les mêmes que précédemment ou des groupes spécifiques)
adultes <- adultes %>%
  mutate(
    age_group = case_when(
      age <= 30 ~ "18-30 ans",
      age <= 45 ~ "31-45 ans",
      age <= 60 ~ "46-60 ans",
      TRUE      ~ "61 ans et plus"
    )
  )

# 5.2. Boxplot du niveau d'éducation (numérique) par groupe d'âge
# On utilise la variable niveau_num créée précédemment (0=Aucun, 1=Primaire, 2=Junior, 3=Senior, 4=Tertiaire)
# Si elle n'existe pas, on la recrée rapidement
if(!"niveau_num" %in% names(adultes)) {
  adultes <- adultes %>%
    mutate(
      niveau_num = case_when(
        niveau_educ == "Aucun" ~ 0,
        niveau_educ == "Primaire" ~ 1,
        niveau_educ == "Junior Secondary" ~ 2,
        niveau_educ == "Senior Secondary" ~ 3,
        niveau_educ == "Tertiaire" ~ 4,
        TRUE ~ NA_real_
      )
    )
}

# Boxplot
p_age_niveau <- ggplot(adultes, aes(x = age_group, y = niveau_num, fill = age_group)) +
  geom_boxplot() +
  labs(
    title = "Niveau d'éducation par groupe d'âge (adultes)",
    x = "Groupe d'âge",
    y = "Niveau d'éducation (0=Aucun, 4=Tertiaire)"
  ) +
  theme_minimal() +
  theme(legend.position = "none", axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("outputs/graphs/education_par_age_boxplot.png", p_age_niveau, width = 7, height = 5, dpi = 300)
print(p_age_niveau)

# 5.3. Test de Kruskal-Wallis
kruskal_test <- kruskal.test(niveau_num ~ age_group, data = adultes)
print(kruskal_test)
capture.output(print(kruskal_test), file = "outputs/tables/kruskal_age_niveau.txt")

# 5.4. Test post-hoc de Dunn (comparaisons multiples)
library(rstatix)
dunn_test <- adultes %>%
  dunn_test(niveau_num ~ age_group, p.adjust.method = "bonferroni")
print(dunn_test)
write.csv(dunn_test, "outputs/tables/dunn_age_niveau.csv", row.names = FALSE)

# 5.5. Tableau récapitulatif : effectifs et médiane par groupe d'âge
age_summary <- adultes %>%
  group_by(age_group) %>%
  summarise(
    n = n(),
    mediane = median(niveau_num, na.rm = TRUE),
    Q1 = quantile(niveau_num, 0.25, na.rm = TRUE),
    Q3 = quantile(niveau_num, 0.75, na.rm = TRUE)
  )
print(age_summary)
write.csv(age_summary, "outputs/tables/age_niveau_summary.csv", row.names = FALSE)







# ==============================================================================
# ÉTAPE 6 : TAUX DE SCOLARISATION DES 6-17 ANS PAR ZONE (RURAL/URBAIN)
# ==============================================================================

# 6.1. Filtrer les enfants de 6 à 17 ans
enfants <- educ_demo %>%
  filter(!is.na(age), age >= 6, age <= 17, !is.na(s2aq13a), !is.na(secteur)) %>%
  mutate(
    scolarise = ifelse(s2aq13a == 1, "Oui", "Non"),
    zone = ifelse(secteur == 1, "Urbain", "Rural")
  )

# 6.2. Calcul des effectifs, proportions et IC pour chaque combinaison zone × scolarise
scol_summary <- enfants %>%
  group_by(zone, scolarise) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(zone) %>%
  mutate(
    total_zone = sum(n),
    prop = n / total_zone,
    # Intervalle de confiance binomial exact pour chaque proportion
    ic_low = mapply(function(x, tot) binom.test(x, tot)$conf.int[1], n, total_zone),
    ic_high = mapply(function(x, tot) binom.test(x, tot)$conf.int[2], n, total_zone),
    prop_label = paste0(round(prop * 100, 1), "%")
  ) %>%
  ungroup()

# Vérifier
print(scol_summary)

# Sauvegarder le tableau
write.csv(scol_summary, "outputs/tables/taux_scolarisation_par_zone_detail.csv", row.names = FALSE)

# 6.3. Graphique en barres côte à côte avec intervalles de confiance
p_scol2 <- ggplot(scol_summary, aes(x = zone, y = prop, fill = scolarise)) +
  geom_col(position = position_dodge(0.9), width = 0.8) +
  geom_errorbar(aes(ymin = ic_low, ymax = ic_high), 
                position = position_dodge(0.9), width = 0.2) +
  geom_text(aes(label = prop_label, group = scolarise),
            position = position_dodge(0.9), vjust = -0.5, size = 3) +
  scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
  labs(
    title = "Taux de scolarisation des enfants de 6 à 17 ans par zone",
    subtitle = "Avec intervalle de confiance à 95%",
    x = NULL,
    y = "Proportion",
    fill = "Scolarisé ?"
  ) +
  theme_minimal()

ggsave("outputs/graphs/taux_scolarisation_par_zone_detail.png", p_scol2, width = 7, height = 5, dpi = 300)
print(p_scol2)

# 6.4. Test du chi-deux (optionnel, déjà fait)
tab_zone_scol <- table(enfants$zone, enfants$scolarise)
chi_scol <- chisq.test(tab_zone_scol)
print(chi_scol)
capture.output(print(chi_scol), file = "outputs/tables/chi2_scolarisation_zone.txt")






# ==============================================================================
# ÉTAPE 7 : CARTE DU TAUX D'ADULTES SANS INSTRUCTION PAR ÉTAT
# ==============================================================================

# 7.1. Charger les librairies nécessaires
library(sf)
library(dplyr)
library(ggplot2)
library(viridis)

# 7.2. Lire le shapefile des États du Nigéria (niveau 1)
shapefile_path <- "data/raw/gadm41_NGA_1.shp"
if (!file.exists(shapefile_path)) {
  stop("Shapefile non trouvé. Vérifiez le chemin.")
}
nigeria_states <- st_read(shapefile_path)

# 7.3. Préparer les données de taux par État (comme précédemment)
if (!exists("adultes")) {
  adultes <- educ_demo %>%
    filter(!is.na(age), age >= 18, !is.na(sexe), !is.na(niveau_educ)) %>%
    mutate(
      sexe = factor(sexe, levels = c(1, 2), labels = c("Homme", "Femme")),
      niveau_educ = factor(niveau_educ, 
                           levels = c("Aucun", "Primaire", "Junior Secondary", 
                                      "Senior Secondary", "Tertiaire"))
    )
}

if (!"state" %in% names(adultes)) {
  if ("state" %in% names(educ_demo)) {
    adultes <- adultes %>%
      left_join(educ_demo %>% select(hhid, indiv, state), by = c("hhid", "indiv"))
  } else {
    state_info <- sect1_w4 %>% select(hhid, indiv, state)
    adultes <- adultes %>%
      left_join(state_info, by = c("hhid", "indiv"))
  }
}

# 7.4. Correspondance code → nom d'État
state_labels <- attr(sect1_w4$state, "labels")
state_codes <- as.numeric(state_labels)
state_names <- names(state_labels)
state_names_clean <- gsub("^[0-9]+\\.\\s*", "", state_names)
code_to_name <- data.frame(
  state_code = state_codes,
  state_name = state_names_clean,
  stringsAsFactors = FALSE
)

# 7.5. Calcul du taux par État
taux_par_etat <- adultes %>%
  filter(!is.na(state), !is.na(niveau_educ)) %>%
  group_by(state_code = state) %>%
  summarise(
    total = n(),
    sans_instruction = sum(niveau_educ == "Aucun", na.rm = TRUE),
    taux = sans_instruction / total,
    .groups = "drop"
  ) %>%
  left_join(code_to_name, by = "state_code")

# 7.6. Harmonisation des noms (à ajuster si nécessaire)
# Exemple de correction manuelle (à adapter selon vos données)
taux_par_etat <- taux_par_etat %>%
  mutate(
    state_name = case_when(
      state_name == "Federal Capital Territory" ~ "FCT",
      state_name == "Akwa Ibom" ~ "Akwa Ibom",
      TRUE ~ state_name
    )
  )

# 7.7. Jointure avec le shapefile
map_data <- nigeria_states %>%
  left_join(taux_par_etat, by = c("NAME_1" = "state_name"))

# 7.8. Calculer les centroïdes pour les étiquettes
centroids <- st_centroid(map_data)

# 7.9. Créer la carte avec étiquettes
p_carte <- ggplot(data = map_data) +
  geom_sf(aes(fill = taux), color = "white", size = 0.2) +
  geom_sf_text(data = centroids, aes(label = NAME_1), 
               size = 2.5, color = "white", fontface = "bold") +
  scale_fill_viridis_c(option = "plasma", na.value = "grey50",
                       name = "Taux sans instruction") +
  labs(
    title = "Taux d'adultes sans instruction par État au Nigéria",
    subtitle = "Données GHS-Panel, Vague 4 (2018-2019)",
    caption = "Source: Calculs à partir des données LSMS-ISA"
  ) +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    legend.position = "right"
  )

# Afficher et sauvegarder
print(p_carte)
ggsave("outputs/graphs/carte_taux_sans_instruction_avec_noms.png", p_carte,
       width = 8, height = 7, dpi = 300)

# 7.10. Exporter le tableau des taux par État
write.csv(taux_par_etat, "outputs/tables/taux_sans_instruction_par_etat.csv", row.names = FALSE)