# ==============================================================================
# Analyse : Éducation et alphabétisation – GHS Panel Vague 4
# ==============================================================================


# ==============================================================================
# BLOC 0 : CHARGEMENT DES PACKAGES ET CRÉATION DES RÉPERTOIRES
# ==============================================================================

if (!require("pacman")) install.packages("pacman")
library(pacman)
p_load(haven, dplyr, ggplot2, naniar, tidyr, scales, rstatix,
       gtsummary, gt, forcats, binom, sf, viridis, survey, srvyr, Hmisc)

dir.create("resultats/tableaux",  recursive = TRUE, showWarnings = FALSE)
dir.create("resultats/figures",   recursive = TRUE, showWarnings = FALSE)
dir.create("data/processed",      recursive = TRUE, showWarnings = FALSE)




# ==============================================================================
# BLOC 1 : IMPORTATION ET CONTRÔLE QUALITÉ DES DONNÉES
# ==============================================================================

# --- 1.1  Lecture des fichiers source ---

hh_section1  <- read_dta("data/raw/sect1_harvestw4.dta")
hh_section2  <- read_dta("data/raw/sect2_harvestw4.dta")
hh_sectionA  <- read_dta("data/raw/secta_harvestw4.dta")


# --- 1.2  Aperçu des structures ---

cat("\n>>> Structure : hh_section1\n");  glimpse(hh_section1)
cat("\n>>> Structure : hh_section2\n");  glimpse(hh_section2)
cat("\n>>> Structure : hh_sectionA\n");  glimpse(hh_sectionA)


# --- 1.3  Détection des doublons sur (hhid, indiv) ---

dup_s1 <- hh_section1 %>%
  group_by(hhid, indiv) %>%
  filter(n() > 1) %>%
  nrow()
cat("Doublons hh_section1 :", dup_s1, "\n")

dup_s2 <- hh_section2 %>%
  group_by(hhid, indiv) %>%
  filter(n() > 1) %>%
  nrow()
cat("Doublons hh_section2 :", dup_s2, "\n")


# --- 1.4  Visualisation et export des valeurs manquantes ---

vars_demo  <- c("s1q2", "s1q4", "sector", "state")
vars_educ  <- c("s2aq5", "s2aq6", "s2aq9", "s2aq13a", "s2aq2")

fig_na_demo <- hh_section1 %>%
  select(any_of(vars_demo)) %>%
  gg_miss_var(show_pct = TRUE) +
  labs(title = "Données manquantes – Section 1 (démographie)",
       x = NULL, y = "% de NA") +
  theme_classic()

fig_na_educ <- hh_section2 %>%
  select(any_of(vars_educ)) %>%
  gg_miss_var(show_pct = TRUE) +
  labs(title = "Données manquantes – Section 2 (éducation)",
       x = NULL, y = "% de NA") +
  theme_classic()

print(fig_na_demo)
print(fig_na_educ)

ggsave("resultats/figures/na_section1.png", fig_na_demo, width = 8, height = 5, dpi = 300)
ggsave("resultats/figures/na_section2.png", fig_na_educ, width = 8, height = 5, dpi = 300)

# Tableaux récapitulatifs des NA

resumer_na <- function(df) {
  df %>%
    summarise(across(everything(), ~ sum(is.na(.)))) %>%
    pivot_longer(everything(), names_to = "variable", values_to = "nb_manquant") %>%
    mutate(pct_manquant = round(nb_manquant / nrow(df) * 100, 2)) %>%
    arrange(desc(nb_manquant))
}

na_s1 <- resumer_na(hh_section1)
na_s2 <- resumer_na(hh_section2)
na_sA <- resumer_na(hh_sectionA)

write.csv(na_s1, "resultats/tableaux/na_section1.csv",  row.names = FALSE)
write.csv(na_s2, "resultats/tableaux/na_section2.csv",  row.names = FALSE)
write.csv(na_sA, "resultats/tableaux/na_sectionA.csv",  row.names = FALSE)

cat("Export des tableaux NA effectué.\n")


# --- 1.5  Vérification de la couverture des identifiants ---

n_commun_12 <- length(intersect(unique(hh_section1$hhid), unique(hh_section2$hhid)))
n_commun_1A <- length(intersect(unique(hh_section1$hhid), unique(hh_sectionA$hhid)))
cat("Ménages communs Section1–Section2 :", n_commun_12, "\n")
cat("Ménages communs Section1–SectionA :", n_commun_1A, "\n")


# --- 1.6  Rappel des variables retenues ---

cat("\n Variables retenues pour l'analyse :\n")
cat("  s1q2    : sexe (1 = Homme, 2 = Femme)\n")
cat("  s1q4    : âge en années\n")
cat("  sector  : milieu de résidence (1 = Urbain, 2 = Rural)\n")
cat("  state   : code État (1-37)\n")
cat("  s2aq5   : sait lire/écrire (1 = Oui, 2 = Non)\n")
cat("  s2aq6   : a fréquenté l'école (1 = Oui, 2 = Non)\n")
cat("  s2aq9   : niveau d'instruction le plus élevé\n")
cat("  s2aq13a : actuellement scolarisé (1 = Oui, 2 = Non)\n")

# Vérification des étiquettes de valeurs pour s2aq9
attr(hh_section2$s2aq9, "labels")




# ==============================================================================
# BLOC 2 : CONSTRUCTION DU NIVEAU D'INSTRUCTION
# ==============================================================================

# --- 2.1  Table de travail ---

df_instruction <- hh_section2


# --- 2.2  Recodage en catégories ---

df_instruction <- df_instruction %>%
  mutate(
    cat_instruction = case_when(
      s2aq9 %in% c(0, 1, 2, 3)                                                        ~ "Aucun",
      s2aq9 %in% 11:16                                                                 ~ "Primaire",
      s2aq9 %in% 21:23                                                                 ~ "Junior Secondary",
      s2aq9 %in% c(24:28, 321)                                                         ~ "Senior Secondary",
      s2aq9 %in% c(31,33,34,35,41,43,322,411,412,421,422,423,424,51,52,61)             ~ "Tertiaire",
      TRUE                                                                              ~ NA_character_
    ),
    score_instruction = case_when(
      cat_instruction == "Aucun"            ~ 0,
      cat_instruction == "Primaire"         ~ 1,
      cat_instruction == "Junior Secondary" ~ 2,
      cat_instruction == "Senior Secondary" ~ 3,
      cat_instruction == "Tertiaire"        ~ 4,
      TRUE                                  ~ NA_real_
    )
  )


# --- 2.3  Contrôle des effectifs ---

table(df_instruction$cat_instruction, useNA = "ifany")


# --- 2.4  Export brut ---

distrib_brute <- df_instruction %>%
  group_by(cat_instruction) %>%
  summarise(n = n(), .groups = "drop") %>%
  mutate(freq = n / sum(n))

write.csv(distrib_brute, "resultats/tableaux/distrib_instruction_brute.csv", row.names = FALSE)


# --- 2.5  Fusion avec la démographie et les pondérations ---
# Left join : df_instruction est la table principale.
# Le poids wt_wave4 est issu de hh_sectionA (niveau ménage), propagé via hhid.

bloc_demo <- hh_section1 %>%
  select(hhid, indiv,
         sexe    = s1q2,
         age     = s1q4,
         milieu  = sector,
         etat    = state)

base_complete <- df_instruction %>%
  left_join(bloc_demo, by = c("hhid", "indiv")) %>%
  left_join(hh_sectionA %>% select(hhid, wt_wave4), by = "hhid")

cat("Taux de fusion :", round(nrow(base_complete) / nrow(df_instruction) * 100, 2), "%\n")

n_na_poids <- sum(is.na(base_complete$wt_wave4))
cat("Individus sans poids :", n_na_poids,
    "(", round(n_na_poids / nrow(base_complete) * 100, 2), "%)\n")

saveRDS(base_complete, "data/processed/base_complete_education.rds")




# ==============================================================================
# BLOC 3 : DISTRIBUTION UNIVARIÉE DU NIVEAU D'INSTRUCTION
# ==============================================================================

# --- 3.1  Plan de sondage ---

plan_global <- svydesign(
  ids     = ~hhid,
  weights = ~wt_wave4,
  data    = base_complete %>% filter(!is.na(cat_instruction), !is.na(wt_wave4))
)


# --- 3.2  Proportions pondérées ---

res_pond <- svymean(~factor(cat_instruction), plan_global, na.rm = TRUE)
df_pond  <- as.data.frame(res_pond) %>%
  mutate(cat_instruction = gsub("factor\\(cat_instruction\\)", "", rownames(.)))

effectifs_bruts <- base_complete %>%
  filter(!is.na(cat_instruction), !is.na(wt_wave4)) %>%
  count(cat_instruction)

tab_distrib <- df_pond %>%
  left_join(effectifs_bruts, by = "cat_instruction") %>%
  rename(proportion = mean, erreur_std = SE) %>%
  mutate(
    etiquette  = scales::percent(proportion, accuracy = 0.1),
    ic_inf     = pmax(0, proportion - 1.96 * erreur_std),
    ic_sup     = pmin(1, proportion + 1.96 * erreur_std)
  ) %>%
  arrange(desc(proportion))

print(tab_distrib)
write.csv(tab_distrib, "resultats/tableaux/distribution_instruction_finale.csv", row.names = FALSE)


# --- 3.3  Graphique barres horizontales ---

palette_bars <- "#2C7BB6"

fig_distrib <- ggplot(tab_distrib,
                      aes(x = reorder(cat_instruction, n), y = n)) +
  geom_col(fill = palette_bars, width = 0.55) +
  geom_text(aes(label = paste0(n, "  (", etiquette, ")")),
            hjust = -0.08, size = 3.2, color = "grey30") +
  coord_flip() +
  labs(x = NULL, y = "Effectif brut") +
  theme_minimal(base_size = 11) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.18)))

print(fig_distrib)
ggsave("resultats/figures/distribution_instruction.png", fig_distrib,
       width = 7.5, height = 4, dpi = 300)




# ==============================================================================
# BLOC 4 : COMPARAISON HOMMES / FEMMES (ADULTES ≥ 18 ANS)
# ==============================================================================

# --- 4.1  Filtrage et recodage ---

pop_adultes <- base_complete %>%
  filter(!is.na(age), age >= 18,
         !is.na(sexe), !is.na(cat_instruction)) %>%
  mutate(
    sexe = factor(sexe, levels = c(1, 2), labels = c("Homme", "Femme")),
    cat_instruction = factor(
      cat_instruction,
      levels = c("Aucun", "Primaire", "Junior Secondary", "Senior Secondary", "Tertiaire")
    )
  )


# --- 4.2  Tableau de contingence brut ---

tab_contingence <- table(pop_adultes$sexe, pop_adultes$cat_instruction)
cat("Tableau de contingence (effectifs bruts) :\n")
print(tab_contingence)
write.csv(as.data.frame.matrix(tab_contingence),
          "resultats/tableaux/contingence_sexe_instruction.csv")


# --- 4.3  Proportions pondérées sexe × niveau ---

plan_adultes <- svydesign(
  ids     = ~hhid,
  weights = ~wt_wave4,
  data    = pop_adultes %>% filter(!is.na(wt_wave4))
)

props_par_sexe <- svyby(
  ~factor(cat_instruction),
  ~sexe,
  plan_adultes,
  svymean,
  na.rm = TRUE
)

cat("Proportions pondérées par sexe :\n")
print(props_par_sexe)
write.csv(props_par_sexe, "resultats/tableaux/props_instruction_par_sexe.csv")


# --- 4.4  Test du Chi-deux (indicatif) ---

test_chi2_sexe <- chisq.test(tab_contingence)
print(test_chi2_sexe)
capture.output(print(test_chi2_sexe),
               file = "resultats/tableaux/chi2_sexe_instruction.txt")


# --- 4.5  V de Cramér ---

val_cramer <- cramer_v(tab_contingence)
cat("V de Cramér :", round(val_cramer, 3), "\n")
write(paste("V de Cramér :", round(val_cramer, 3)),
      file = "resultats/tableaux/cramer_sexe_instruction.txt")


# --- 4.6  Graphique barres 100 % empilées ---

df_sexe_long <- as.data.frame(props_par_sexe) %>%
  pivot_longer(
    cols      = starts_with("factor"),
    names_to  = "cat_instruction",
    values_to = "proportion"
  ) %>%
  mutate(
    cat_instruction = gsub("factor\\(cat_instruction\\)", "", cat_instruction),
    cat_instruction = factor(cat_instruction,
                             levels = c("Aucun", "Primaire", "Junior Secondary",
                                        "Senior Secondary", "Tertiaire"))
  )

palette_niveaux <- c(
  "Aucun"            = "#D62728",
  "Primaire"         = "#AEC7E8",
  "Junior Secondary" = "#FFBB78",
  "Senior Secondary" = "#1F77B4",
  "Tertiaire"        = "#2CA02C"
)

fig_sexe <- ggplot(df_sexe_long,
                   aes(x = sexe, y = proportion, fill = cat_instruction)) +
  geom_col(position = "fill", width = 0.5) +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_fill_manual(values = palette_niveaux) +
  labs(x = NULL, y = "Proportion pondérée",
       fill = "Niveau d'instruction") +
  theme_minimal(base_size = 11)

print(fig_sexe)
ggsave("resultats/figures/instruction_par_sexe.png", fig_sexe,
       width = 7, height = 5, dpi = 300)




# ==============================================================================
# BLOC 5 : NIVEAU D'INSTRUCTION SELON LE GROUPE D'ÂGE (ADULTES ≥ 18 ANS)
# ==============================================================================

# --- 5.1  Construction des tranches d'âge ---

pop_adultes <- pop_adultes %>%
  mutate(
    tranche_age = case_when(
      age <= 30 ~ "18–30 ans",
      age <= 45 ~ "31–45 ans",
      age <= 60 ~ "46–60 ans",
      TRUE      ~ "61 ans et +"
    )
  )


# --- 5.2  Variable numérique d'instruction (si absente) ---

if (!"score_instruction" %in% names(pop_adultes)) {
  pop_adultes <- pop_adultes %>%
    mutate(
      score_instruction = case_when(
        cat_instruction == "Aucun"            ~ 0,
        cat_instruction == "Primaire"         ~ 1,
        cat_instruction == "Junior Secondary" ~ 2,
        cat_instruction == "Senior Secondary" ~ 3,
        cat_instruction == "Tertiaire"        ~ 4,
        TRUE                                  ~ NA_real_
      )
    )
}


# --- 5.3  Boxplot niveau par tranche d'âge ---

fig_age_box <- ggplot(pop_adultes,
                      aes(x = tranche_age, y = score_instruction)) +
  geom_boxplot(fill = "#F0A500", color = "#7D5A00", width = 0.5) +
  labs(x = "Tranche d'âge",
       y = "Score d'instruction (0 = Aucun, 4 = Tertiaire)") +
  theme_minimal(base_size = 11) +
  theme(axis.text.x = element_text(angle = 40, hjust = 1))

print(fig_age_box)
ggsave("resultats/figures/instruction_par_tranche_age.png", fig_age_box,
       width = 7, height = 5, dpi = 300)


# --- 5.4  Test de Kruskal-Wallis (indicatif, non pondéré) ---

kw_test <- kruskal.test(score_instruction ~ tranche_age, data = pop_adultes)
print(kw_test)
capture.output(print(kw_test),
               file = "resultats/tableaux/kruskal_age_instruction.txt")


# --- 5.5  Comparaisons post-hoc de Dunn ---

dunn_res <- pop_adultes %>%
  dunn_test(score_instruction ~ tranche_age, p.adjust.method = "bonferroni")
print(dunn_res)
write.csv(dunn_res, "resultats/tableaux/dunn_age_instruction.csv", row.names = FALSE)


# --- 5.6  Proportions pondérées par tranche d'âge ---

plan_age <- svydesign(
  ids     = ~hhid,
  weights = ~wt_wave4,
  data    = pop_adultes %>% filter(!is.na(wt_wave4), !is.na(tranche_age))
)

props_par_age <- svyby(
  ~factor(cat_instruction),
  ~tranche_age,
  plan_age,
  svymean,
  na.rm = TRUE
)

print(props_par_age)
write.csv(props_par_age,
          "resultats/tableaux/props_instruction_par_tranche_age.csv",
          row.names = FALSE)




# ==============================================================================
# BLOC 6 : TAUX DE SCOLARISATION DES 6–17 ANS PAR MILIEU
# ==============================================================================

# --- 6.1  Filtrage de la population cible ---

pop_enfants <- base_complete %>%
  filter(!is.na(age), age >= 6, age <= 17,
         !is.na(s2aq13a), !is.na(milieu)) %>%
  mutate(
    scol_bin    = ifelse(s2aq13a == 1, 1L, 0L),
    milieu_lab  = ifelse(milieu == 1, "Urbain", "Rural"),
    scol_label  = ifelse(scol_bin == 1, "Scolarisé", "Non scolarisé")
  )


# --- 6.2  Taux pondéré avec intervalle de confiance ---

plan_enfants <- svydesign(
  ids     = ~hhid,
  weights = ~wt_wave4,
  data    = pop_enfants %>% filter(!is.na(wt_wave4))
)

res_scol <- svyby(
  ~scol_bin,
  ~milieu_lab,
  plan_enfants,
  svymean,
  na.rm   = TRUE,
  vartype = "ci"
)

tab_scol <- res_scol %>%
  rename(taux = scol_bin, borne_inf = ci_l, borne_sup = ci_u) %>%
  mutate(
    taux_pct  = paste0(round(taux * 100, 1), "%"),
    n_obs     = as.integer(table(pop_enfants$milieu_lab[!is.na(pop_enfants$wt_wave4)]))
  )

print(tab_scol)
write.csv(tab_scol,
          "resultats/tableaux/scolarisation_par_milieu.csv",
          row.names = FALSE)


# --- 6.3  Graphique barres avec IC ---

fig_scol <- ggplot(tab_scol, aes(x = milieu_lab, y = taux)) +
  geom_col(fill = "#3A86FF", width = 0.45) +
  geom_errorbar(aes(ymin = borne_inf, ymax = borne_sup),
                width = 0.12, color = "grey30") +
  geom_text(aes(label = taux_pct), vjust = -0.6, size = 3.3) +
  scale_y_continuous(labels = scales::percent_format(), limits = c(0, 1.15)) +
  labs(x = NULL, y = "Taux de scolarisation (pondéré)") +
  theme_minimal(base_size = 11)

print(fig_scol)
ggsave("resultats/figures/scolarisation_par_milieu.png", fig_scol,
       width = 6.5, height = 5, dpi = 300)


# --- 6.4  Test du Chi-deux (indicatif) ---

tab_chi_scol <- table(pop_enfants$milieu_lab, pop_enfants$scol_label)
test_chi2_scol <- chisq.test(tab_chi_scol)
print(test_chi2_scol)
capture.output(print(test_chi2_scol),
               file = "resultats/tableaux/chi2_scolarisation_milieu.txt")




# ==============================================================================
# BLOC 7 : CARTE CHOROPLÈTHE – ADULTES SANS INSTRUCTION PAR ÉTAT
# ==============================================================================

# --- 7.1  Lecture du fond de carte ---

chemin_shp <- "data/raw/gadm41_NGA_1.shp"
if (!file.exists(chemin_shp)) stop("Shapefile introuvable : vérifiez le chemin.")
fonds_etats <- st_read(chemin_shp)


# --- 7.2  Reconstruction de pop_adultes si nécessaire ---

if (!exists("pop_adultes")) {
  pop_adultes <- base_complete %>%
    filter(!is.na(age), age >= 18,
           !is.na(sexe), !is.na(cat_instruction)) %>%
    mutate(
      sexe = factor(sexe, levels = c(1, 2), labels = c("Homme", "Femme")),
      cat_instruction = factor(
        cat_instruction,
        levels = c("Aucun", "Primaire", "Junior Secondary",
                   "Senior Secondary", "Tertiaire")
      )
    )
}

if (!"etat" %in% names(pop_adultes)) {
  pop_adultes <- pop_adultes %>%
    left_join(hh_section1 %>% select(hhid, indiv, etat = state),
              by = c("hhid", "indiv"))
}


# --- 7.3  Table de correspondance codes → noms d'États ---

labels_etats  <- attr(hh_section1$state, "labels")
codes_etats   <- as.numeric(labels_etats)
noms_etats    <- gsub("^[0-9]+\\.\\s*", "", names(labels_etats))
corresp_etats <- data.frame(code_etat = codes_etats,
                            nom_etat  = noms_etats,
                            stringsAsFactors = FALSE)


# --- 7.4  Calcul du taux brut par État ---
# AVERTISSEMENT : le GHS-Panel est représentatif au niveau national
# et par zone géopolitique — PAS au niveau des États.
# Ces taux sont fournis à titre indicatif uniquement.

taux_etats <- base_complete %>%
  filter(!is.na(age), age >= 18, !is.na(sexe), !is.na(etat)) %>%
  mutate(
    sans_scolarite = case_when(
      s2aq6 == 2              ~ 1,
      cat_instruction == "Aucun" ~ 1,
      !is.na(cat_instruction) ~ 0,
      TRUE                    ~ NA_real_
    )
  ) %>%
  filter(!is.na(sans_scolarite)) %>%
  group_by(code_etat = etat) %>%
  summarise(
    n_total       = n(),
    n_sans_scol   = sum(sans_scolarite),
    taux_sans     = n_sans_scol / n_total,
    .groups       = "drop"
  ) %>%
  left_join(corresp_etats, by = "code_etat")


# --- 7.5  Harmonisation des noms ---

taux_etats <- taux_etats %>%
  mutate(
    nom_etat = case_when(
      nom_etat == "Federal Capital Territory" ~ "FCT",
      TRUE                                    ~ nom_etat
    )
  )


# --- 7.6  Jointure avec le shapefile ---

donnees_carte <- fonds_etats %>%
  left_join(taux_etats, by = c("NAME_1" = "nom_etat"))


# --- 7.7  Centroïdes pour les étiquettes ---

pts_centres <- st_centroid(donnees_carte)


# --- 7.8  Carte ---

fig_carte <- ggplot(data = donnees_carte) +
  geom_sf(aes(fill = taux_sans), color = "white", linewidth = 0.2) +
  geom_sf_text(data = pts_centres, aes(label = NAME_1),
               size = 2.3, color = "white", fontface = "bold") +
  scale_fill_viridis_c(option = "magma", na.value = "grey60",
                       name = "% sans instruction",
                       labels = scales::percent_format(accuracy = 1)) +
  theme_void() +
  theme(legend.position = "right",
        legend.title    = element_text(size = 9))

print(fig_carte)
ggsave("resultats/figures/carte_sans_instruction_par_etat.png", fig_carte,
       width = 8, height = 4.5, dpi = 300)


# --- 7.9  Export du tableau ---

write.csv(taux_etats,
          "resultats/tableaux/taux_sans_instruction_etats.csv",
          row.names = FALSE)

cat("\n=== Analyse terminée. Résultats dans /resultats/ ===\n")
