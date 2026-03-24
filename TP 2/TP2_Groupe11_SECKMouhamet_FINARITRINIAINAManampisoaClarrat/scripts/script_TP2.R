# ==============================================================================
# TP2 : Éducation et alphabétisation (Vague 4)
# ==============================================================================


# ==============================================================================
# ÉTAPE 0 : INITIALISATION – LIBRAIRIES ET DOSSIERS
# ==============================================================================


# 0.1. Chargement des librairies avec pacman

if (!require("pacman")) install.packages("pacman")
library(pacman)
p_load(haven, dplyr, ggplot2, naniar, tidyr, scales, rstatix, gtsummary, gt,
       forcats, binom, sf, viridis, survey, srvyr, Hmisc)


# 0.2. Création des dossiers de sortie

#dir.create("outputs/tables", recursive = TRUE, showWarnings = FALSE)
#dir.create("outputs/graphs", recursive = TRUE, showWarnings = FALSE)




# ==============================================================================
# ÉTAPE 1 : CHARGEMENT ET EXPLORATION DES DONNÉES (VAGUE 4)
# ==============================================================================


# 1.1. Chargement des fichiers

sect1_w4 <- read_dta("data/raw/sect1_harvestw4.dta")
sect2_w4 <- read_dta("data/raw/sect2_harvestw4.dta")
secta_w4 <- read_dta("data/raw/secta_harvestw4.dta")


# 1.2. Examen de la structure

cat("\n Structure de sect1_w4 \n")
str(sect1_w4, max.level = 1)
glimpse(sect1_w4)

cat("\n Structure de sect2_w4 \n")
str(sect2_w4, max.level = 1)
glimpse(sect2_w4)

cat("\n Structure de secta_w4 \n")
str(secta_w4, max.level = 1)
glimpse(secta_w4)


# 1.3. Vérification des doublons (hhid, indiv)

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


# 1.4. Analyse des valeurs manquantes (variables clés)

# Sect1 : démographie 

vars_cles_s1 <- c("s1q2", "s1q4", "sector", "state")

p_miss_s1 <- sect1_w4 %>%
  select(any_of(vars_cles_s1)) %>%
  gg_miss_var(show_pct = TRUE) +
  labs(
    title = "Valeurs manquantes - Sect1 (démographie)",
    x = "Variable",
    y = "% de NA"
  ) +
  theme_minimal()

print(p_miss_s1)

ggsave("outputs/graphs/missing_sect1_tp2.png", p_miss_s1, width = 8, height = 5, dpi = 300)

# Sect2 : éducation

vars_cles_s2 <- c("s2aq5", "s2aq6", "s2aq9", "s2aq13a", "s2aq2")

p_miss_s2 <- sect2_w4 %>%
  select(any_of(vars_cles_s2)) %>%
  gg_miss_var(show_pct = TRUE) +
  labs(
    title = "Valeurs manquantes - Sect2 (éducation)",
    x = "Variable",
    y = "% de NA"
  ) +
  theme_minimal()

print(p_miss_s2)

ggsave("outputs/graphs/missing_sect2_tp2.png", p_miss_s2, width = 8, height = 5, dpi = 300)

# Tableaux récapitulatifs des NA (toutes variables)

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


# 1.5. Cohérence des identifiants entre les fichiers

communs_s1_s2 <- intersect(unique(sect1_w4$hhid), unique(sect2_w4$hhid))
communs_s1_secta <- intersect(unique(sect1_w4$hhid), unique(secta_w4$hhid))
cat("\nMénages communs entre sect1 et sect2 :", length(communs_s1_s2), "\n")
cat("Ménages communs entre sect1 et secta :", length(communs_s1_secta), "\n")


# 1.6. Identification des variables clés (rappel)

cat("\nVariables clés identifiées :\n")
cat("- s1q2  : sexe (1 = Homme, 2 = Femme)\n")
cat("- s1q4  : âge (années)\n")
cat("- sector : milieu (1 = Urbain, 2 = Rural)\n")
cat("- state  : État (codes 1-37)\n")
cat("- s2aq5  : alphabétisation (1 = Oui, 2 = Non)\n")
cat("- s2aq6  : a déjà été à l'école (1 = Oui, 2 = Non)\n")
cat("- s2aq9  : niveau d'éducation le plus élevé (codes détaillés)\n")
cat("- s2aq13a : scolarisation actuelle (1 = Oui, 2 = Non)\n")
cat("- s2aq2  : âge de l'individu (dans sect2, mais on utilisera s1q4 pour l'âge)\n")

# Vérification des labels de s2aq9
attr(sect2_w4$s2aq9, "labels")     # permet de voir les attribut d'une variable tel que les labels etc. 





# ==============================================================================
# ÉTAPE 2 : CONSTRUCTION DE LA VARIABLE NIVEAU D'ÉDUCATION
# ==============================================================================


# 2.1. Copie de travail

sect2_educ <- sect2_w4


# 2.2. Définition des correspondances (codes → niveaux)

sect2_educ <- sect2_educ %>%
  mutate(
    niveau_educ = case_when(
      s2aq9 %in% c(0, 1, 2, 3)                    ~ "Aucun",
      s2aq9 %in% 11:16                             ~ "Primaire",
      s2aq9 %in% 21:23                             ~ "Junior Secondary",
      s2aq9 %in% c(24:28, 321)                     ~ "Senior Secondary",
      s2aq9 %in% c(31, 33, 34, 35, 41, 43, 322, 411, 412, 421, 422, 423, 424, 51, 52, 61) ~ "Tertiaire",
      TRUE                                         ~ NA_character_
    ),
    niveau_num = case_when(
      niveau_educ == "Aucun"            ~ 0,
      niveau_educ == "Primaire"         ~ 1,
      niveau_educ == "Junior Secondary" ~ 2,
      niveau_educ == "Senior Secondary" ~ 3,
      niveau_educ == "Tertiaire"        ~ 4,
      TRUE                              ~ NA_real_
    )
  )


# 2.3. Vérification des effectifs

table(sect2_educ$niveau_educ, useNA = "ifany")


# 2.4. Tableau récapitulatif des niveaux d'éducation (brut : avant pondération)

niveau_summary_brut <- sect2_educ %>%
  group_by(niveau_educ) %>%
  summarise(n = n(), .groups = "drop") %>%
  mutate(prop = n / sum(n))
write.csv(niveau_summary_brut, "outputs/tables/niveau_educ_frequences.csv", row.names = FALSE)


# 2.5. Jointure avec sect1 (sexe, âge, zone, état) et intégration des poids
# Left join justifié : sect2_educ est la table principale.
# wt_wave4 issu de secta_w4 (niveau ménage) propagé aux individus via hhid.
# NA sur wt_wave4 : ménages absents de secta (mécanisme MCAR) -> exclus des analyses pondérées.

sect1_demo <- sect1_w4 %>%
  select(hhid, indiv, sexe = s1q2, age = s1q4, secteur = sector, etat = state)

educ_demo <- sect2_educ %>%
  left_join(sect1_demo, by = c("hhid", "indiv")) %>%
  left_join(secta_w4 %>% select(hhid, wt_wave4), by = "hhid")

cat("Taux de jointure :", nrow(educ_demo) / nrow(sect2_educ), "\n")

# Vérification des NA sur wt_wave4
na_poids <- sum(is.na(educ_demo$wt_wave4))
cat("NA sur wt_wave4 :", na_poids,
    "(", round(na_poids / nrow(educ_demo) * 100, 2), "%)\n")

# Sauvegarde de la base jointe
saveRDS(educ_demo, "data/processed/educ_demo.rds")




# ==============================================================================
# ÉTAPE 3 : ANALYSE UNIVARIÉE DU NIVEAU D'ÉDUCATION
# ==============================================================================


# 3.1. Proportions pondérées par niveau d'éducation
# svydesign créé une seule fois, réutilisé dans toute l'étape 3

design_educ <- svydesign(
  ids     = ~hhid,
  weights = ~wt_wave4,
  data    = educ_demo %>% filter(!is.na(niveau_educ), !is.na(wt_wave4))
)

# Proportions pondérées par niveau

props_pond <- svymean(~factor(niveau_educ), design_educ, na.rm = TRUE)
props_df   <- as.data.frame(props_pond)
props_df$niveau_educ <- gsub("factor\\(niveau_educ\\)", "", rownames(props_df))

# Effectifs bruts (pour information)

n_bruts <- educ_demo %>%
  filter(!is.na(niveau_educ), !is.na(wt_wave4)) %>%
  count(niveau_educ)

niveau_summary <- props_df %>%
  left_join(n_bruts, by = "niveau_educ") %>%
  rename(prop = mean, se = SE) %>%
  mutate(
    prop_label = scales::percent(prop, accuracy = 0.1),
    ic_low     = pmax(0, prop - 1.96 * se),
    ic_high    = pmin(1, prop + 1.96 * se)
  ) %>%
  arrange(desc(prop))

print(niveau_summary)

write.csv(niveau_summary, "outputs/tables/niveau_educ_frequences_final.csv", row.names = FALSE)


# 3.2. Graphique : Barplot horizontal ordonné par proportion pondérée

p_niveau <- ggplot(niveau_summary,
                   aes(x = reorder(niveau_educ, n), y = n)) +
  geom_col(fill = "steelblue", width = 0.5) +
  geom_text(aes(label = paste0(n, " (", prop_label, ")")),
            hjust = -0.1, size = 3) +
  coord_flip() +
  labs(
    x = NULL,
    y = "Effectif"
  ) +
  theme_minimal() +
  theme(legend.position = "none") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15)))

print(p_niveau)

ggsave("outputs/graphs/niveau_educ_barplot.png", p_niveau, width = 7.08, height = 3.80, dpi = 300)



# 3.3. Affichage console

print(niveau_summary)




# ==============================================================================
# ÉTAPE 4 : COMPARAISON HOMMES/FEMMES DU NIVEAU D'ÉDUCATION (ADULTES 18+)
# ==============================================================================


# 4.1. Filtrage : adultes (âge >= 18), données complètes

adultes <- educ_demo %>%
  filter(!is.na(age), age >= 18, !is.na(sexe), !is.na(niveau_educ)) %>%
  mutate(
    sexe = factor(sexe, levels = c(1, 2), labels = c("Homme", "Femme")),
    niveau_educ = factor(
      niveau_educ,
      levels = c("Aucun", "Primaire", "Junior Secondary", "Senior Secondary", "Tertiaire")
    )
  )


# 4.2. Tableau de contingence (effectifs bruts — informatif)

tab_sexe_niveau <- table(adultes$sexe, adultes$niveau_educ)
print("Tableau de contingence (effectifs bruts) :")
print(tab_sexe_niveau)

write.csv(as.data.frame.matrix(tab_sexe_niveau),
          "outputs/tables/contingence_sexe_niveau.csv")


# 4.3. Proportions pondérées par sexe et niveau
# svydesign sur adultes avec poids disponibles

design_adultes <- svydesign(
  ids     = ~hhid,
  weights = ~wt_wave4,
  data    = adultes %>% filter(!is.na(wt_wave4))
)

# Proportions pondérées par sexe × niveau
props_sexe_niveau <- svyby(
  ~factor(niveau_educ),
  ~sexe,
  design_adultes,
  svymean,
  na.rm = TRUE
)

print("Proportions pondérées par sexe :")
print(props_sexe_niveau)

write.csv(props_sexe_niveau, "outputs/tables/props_sexe_niveau_pond.csv")


# 4.4. Test du Chi-deux (appliqué sur données brutes — pas de version pondérée standard)
# Justification : le Chi-deux pondéré (svychisq) teste l'indépendance dans la population ;
# le Chi-deux classique est ici utilisé comme test indicatif sur l'échantillon.

chi_test <- chisq.test(tab_sexe_niveau)
print(chi_test)
capture.output(print(chi_test), file = "outputs/tables/chi2_sexe_niveau.txt")


# 4.5. V de Cramér (intensité de l'association)

cramer_v <- cramer_v(tab_sexe_niveau)
cat("\nV de Cramér :", round(cramer_v, 3), "\n")
write(paste("V de Cramér :", round(cramer_v, 3)),
      file = "outputs/tables/cramer_sexe_niveau.txt")


# 4.6. Graphique : Barres 100% empilées par sexe (proportions pondérées)

# Mise en forme des proportions pondérées pour le graphique

df_props_pond <- as.data.frame(props_sexe_niveau) %>%
  pivot_longer(
    cols      = starts_with("factor"),
    names_to  = "niveau_educ",
    values_to = "prop"
  ) %>%
  mutate(
    niveau_educ = gsub("factor\\(niveau_educ\\)", "", niveau_educ),
    niveau_educ = factor(niveau_educ,
                         levels = c("Aucun", "Primaire", "Junior Secondary", "Senior Secondary", "Tertiaire"))
  )

p_sexe_niveau <- ggplot(df_props_pond, aes(x = sexe, y = prop, fill = niveau_educ)) +
  geom_col(position = "fill", width = 0.5) +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(
    values = c(
      "Aucun"            = "#E15759",  # rouge sobre
      "Primaire"         = "#76B7B2",  # vert d'eau
      "Junior Secondary" = "#F28E2B",  # orange
      "Senior Secondary" = "#4E79A7",  # bleu
      "Tertiaire"        = "#59A14F"   # vert
    )
  ) +
  labs(
    x    = NULL,
    y    = "Proportion",
    fill = "Niveau d'éducation"
  ) +
  theme_minimal()

print(p_sexe_niveau)

ggsave("outputs/graphs/education_par_sexe.png", p_sexe_niveau, width = 7, height = 5, dpi = 300)




# ==============================================================================
# ÉTAPE 5 : RELATION ENTRE ÂGE ET NIVEAU D'ÉDUCATION (ADULTES 18+)
# ==============================================================================


# 5.1. Création des groupes d'âge

adultes <- adultes %>%
  mutate(
    age_group = case_when(
      age <= 30 ~ "18-30 ans",
      age <= 45 ~ "31-45 ans",
      age <= 60 ~ "46-60 ans",
      TRUE      ~ "61 ans et plus"
    )
  )


# 5.2. Vérification/création de la variable numérique niveau_num

if (!"niveau_num" %in% names(adultes)) {
  adultes <- adultes %>%
    mutate(
      niveau_num = case_when(
        niveau_educ == "Aucun"            ~ 0,
        niveau_educ == "Primaire"         ~ 1,
        niveau_educ == "Junior Secondary" ~ 2,
        niveau_educ == "Senior Secondary" ~ 3,
        niveau_educ == "Tertiaire"        ~ 4,
        TRUE                              ~ NA_real_
      )
    )
}


# 5.3. Graphique : Boxplot du niveau d'éducation par groupe d'âge

p_age_niveau <- ggplot(adultes, aes(x = age_group, y = niveau_num)) +
  geom_boxplot(fill = "#59A14F", width = 0.5) +
  labs(
    x = "Groupe d'âge",
    y = "Niveau d'éducation (0 = Aucun, 4 = Tertiaire)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(p_age_niveau)

ggsave("outputs/graphs/education_par_age_boxplot.png", p_age_niveau, width = 7, height = 5, dpi = 300)



# 5.4. Test de Kruskal-Wallis (appliqué sur données brutes)
# Justification : pas de version pondérée standard du Kruskal-Wallis en R.
# Les résultats sont indicatifs ; les proportions pondérées par groupe d'âge
# constituent l'indicateur principal.

kruskal_test <- kruskal.test(niveau_num ~ age_group, data = adultes)
print(kruskal_test)
capture.output(print(kruskal_test), file = "outputs/tables/kruskal_age_niveau.txt")


# 5.5. Test post-hoc de Dunn (comparaisons multiples)

dunn_test <- adultes %>%
  dunn_test(niveau_num ~ age_group, p.adjust.method = "bonferroni")
print(dunn_test)
write.csv(dunn_test, "outputs/tables/dunn_age_niveau.csv", row.names = FALSE)


# 5.6. Tableau récapitulatif pondéré : proportion par niveau et par groupe d'âge

design_age <- svydesign(
  ids     = ~hhid,
  weights = ~wt_wave4,
  data    = adultes %>% filter(!is.na(wt_wave4), !is.na(age_group))
)

props_age_niveau <- svyby(
  ~factor(niveau_educ),
  ~age_group,
  design_age,
  svymean,
  na.rm = TRUE
)
print(props_age_niveau)

write.csv(props_age_niveau, "outputs/tables/age_niveau_summary.csv", row.names = FALSE)




# ==============================================================================
# ÉTAPE 6 : TAUX DE SCOLARISATION DES 6-17 ANS PAR ZONE (RURAL/URBAIN)
# ==============================================================================


# 6.1. Filtrage : enfants de 6 à 17 ans

enfants <- educ_demo %>%
  filter(!is.na(age), age >= 6, age <= 17, !is.na(s2aq13a), !is.na(secteur)) %>%
  mutate(
    scolarise  = ifelse(s2aq13a == 1, 1L, 0L),
    zone       = ifelse(secteur == 1, "Urbain", "Rural"),
    scolarise_label = ifelse(scolarise == 1, "Oui", "Non")
  )


# 6.2. Taux de scolarisation pondéré par zone (svymean + IC)
# svydesign sur enfants avec poids disponibles

design_enfants <- svydesign(
  ids     = ~hhid,
  weights = ~wt_wave4,
  data    = enfants %>% filter(!is.na(wt_wave4))
)

scol_pond <- svyby(
  ~scolarise,
  ~zone,
  design_enfants,
  svymean,
  na.rm    = TRUE,
  vartype  = "ci"
)

# Mise en forme du tableau de sortie

scol_summary <- scol_pond %>%
  rename(
    prop    = scolarise,
    ic_low  = ci_l,
    ic_high = ci_u
  ) %>%
  mutate(
    prop_label = paste0(round(prop * 100, 1), "%"),
    n_brut     = as.integer(table(enfants$zone[!is.na(enfants$wt_wave4)]))
  )

print(scol_summary)

write.csv(scol_summary, "outputs/tables/taux_scolarisation_par_zone_detail.csv", row.names = FALSE)


# 6.3. Graphique : Taux de scolarisation pondéré par zone avec IC

p_scol2 <- ggplot(scol_summary, aes(x = zone, y = prop)) +
  geom_col(fill = "steelblue", width = 0.5) +
  geom_text(aes(label = prop_label), vjust = -0.5, size = 3) +
  scale_y_continuous(labels = scales::percent, limits = c(0, 1.1)) +
  labs(
    x = NULL,
    y = "Taux de scolarisation"
  ) +
  theme_minimal()

print(p_scol2)

ggsave("outputs/graphs/taux_scolarisation_par_zone_detail.png", p_scol2, width = 7, height = 5, dpi = 300)



# 6.4. Test du Chi-deux (zone × scolarisation)
# Appliqué sur données brutes (test indicatif sur l'échantillon)

tab_zone_scol <- table(enfants$zone, enfants$scolarise_label)
chi_scol <- chisq.test(tab_zone_scol)
print(chi_scol)
capture.output(print(chi_scol), file = "outputs/tables/chi2_scolarisation_zone.txt")




# ==============================================================================
# ÉTAPE 7 : CARTE DU TAUX D'ADULTES SANS INSTRUCTION PAR ÉTAT
# ==============================================================================


# 7.1. Chargement du shapefile des États du Nigéria

shapefile_path <- "data/raw/gadm41_NGA_1.shp"
if (!file.exists(shapefile_path)) {
  stop("Shapefile non trouvé. Vérifiez le chemin.")
}
nigeria_states <- st_read(shapefile_path)


# 7.2. Préparation des données (si pas déjà fait)

if (!exists("adultes")) {
  adultes <- educ_demo %>%
    filter(!is.na(age), age >= 18, !is.na(sexe), !is.na(niveau_educ)) %>%
    mutate(
      sexe = factor(sexe, levels = c(1, 2), labels = c("Homme", "Femme")),
      niveau_educ = factor(
        niveau_educ,
        levels = c("Aucun", "Primaire", "Junior Secondary", "Senior Secondary", "Tertiaire")
      )
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


# 7.3. Correspondance code → nom d'État

state_labels     <- attr(sect1_w4$state, "labels")
state_codes      <- as.numeric(state_labels)
state_names      <- names(state_labels)
state_names_clean <- gsub("^[0-9]+\\.\\s*", "", state_names)
code_to_name     <- data.frame(
  state_code = state_codes,
  state_name = state_names_clean,
  stringsAsFactors = FALSE
)


# 7.4. Calcul du taux par État (données brutes — non pondérées)
# IMPORTANT : Le plan de sondage du GHS-Panel n'est pas conçu pour produire
# des estimations représentatives au niveau des États (représentatif au niveau
# national et par zone géopolitique uniquement). Les taux ci-dessous sont
# calculés sur données brutes et fournis à titre indicatif uniquement.

taux_par_etat <- educ_demo %>%
  filter(!is.na(age), age >= 18, !is.na(sexe), !is.na(etat)) %>%
  mutate(
    sans_instruction = case_when(
      s2aq6 == 2             ~ 1,
      niveau_educ == "Aucun" ~ 1,
      !is.na(niveau_educ)    ~ 0,
      TRUE                   ~ NA_real_
    )
  ) %>%
  filter(!is.na(sans_instruction)) %>%
  group_by(state_code = etat) %>%
  summarise(
    total            = n(),
    sans_instruction = sum(sans_instruction),
    taux             = sans_instruction / total,
    .groups          = "drop"
  ) %>%
  left_join(code_to_name, by = "state_code")


# 7.5. Harmonisation des noms d'États

taux_par_etat <- taux_par_etat %>%
  mutate(
    state_name = case_when(
      state_name == "Federal Capital Territory" ~ "FCT",
      state_name == "Akwa Ibom"                ~ "Akwa Ibom",
      TRUE                                     ~ state_name
    )
  )


# 7.6. Jointure avec le shapefile

map_data <- nigeria_states %>%
  left_join(taux_par_etat, by = c("NAME_1" = "state_name"))


# 7.7. Calcul des centroïdes pour les étiquettes

centroids <- st_centroid(map_data)


# 7.8. Graphique : Carte choroplèthe avec étiquettes

p_carte <- ggplot(data = map_data) +
  geom_sf(aes(fill = taux), color = "white", size = 0.2) +
  geom_sf_text(data = centroids, aes(label = NAME_1),
               size = 2.5, color = "white", fontface = "bold") +
  scale_fill_viridis_c(option = "plasma", na.value = "grey50",
                       name = "Taux sans instruction") +
  theme_minimal() +
  theme(
    axis.text      = element_blank(),
    axis.title     = element_blank(),
    panel.grid     = element_blank(),
    legend.position = "right"
  )

print(p_carte)

ggsave("outputs/graphs/carte_taux_sans_instruction_avec_noms.png", p_carte,
       width = 7, height = 4, dpi = 300)


# 7.9. Export du tableau des taux par État

write.csv(taux_par_etat, "outputs/tables/taux_sans_instruction_par_etat.csv", row.names = FALSE)
