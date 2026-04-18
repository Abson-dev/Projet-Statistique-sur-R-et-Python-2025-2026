# =============================================================================
# FICHIER : Import_Nettoyage.R
# PROJET  : TP4 - Analyse Foncière - GHS Nigeria Vague 4
# GROUPE  : Groupe 6 — LO Serigne Ndame & KEBJAM Jackson
# DATE    : 2025
# OBJET   : Importation, exploration préliminaire, nettoyage et pondération
# =============================================================================
# REPRODUCTIBILITÉ
#   → Ce script doit être exécuté EN PREMIER, depuis la racine du projet
#     (là où se trouve le fichier .Rproj).
#   → Ordre d'exécution : (1) Import_Nettoyage.R  →  (2) Analyse.R
#   → Les données brutes doivent être dans : data/raw/
#   → Les sorties nettoyées seront dans    : data/processed/
#   → Aucune graine aléatoire (set.seed) n'est nécessaire : toutes les
#     opérations sont déterministes.
# =============================================================================
# SOURCES DOCUMENTAIRES CONSULTÉES :
#   - GHS-Panel W4 Basic Information Document (World Bank / NBS Nigeria, 2019)
#   - GHS-Panel W4 Interviewer Manual — Section 11 (Land)
#   - General Conversion Factors to Hectares (annexe méthodologique GHS)
#   - World Bank LSMS-ISA documentation on plot-level agricultural data
# Ces documents ont guidé : l'identification des variables, le recodage des
# unités de mesure, la correspondance des régimes de tenure et le choix des
# seuils de nettoyage.
# =============================================================================

# -----------------------------------------------------------------------------
# 0. PACKAGES
# -----------------------------------------------------------------------------
library(haven)      # Lecture fichiers .dta Stata
library(dplyr)      # Manipulation de données
library(tidyr)      # Reshaping / gestion NA
library(ggplot2)    # Visualisation exploratoire
library(scales)     # Formatage axes
library(labelled)   # Labels Stata
library(janitor)    # Nettoyage noms colonnes

# -----------------------------------------------------------------------------
# 1. IMPORTATION DES DONNÉES BRUTES
# -----------------------------------------------------------------------------

# Base parcellaire : superficie déclarée, GPS, unités de mesure
sect11a1 <- read_dta("data/raw/sect11a1_plantingw4.dta")

# Base tenure : régime foncier par parcelle
sect11b1 <- read_dta("data/raw/sect11b1_plantingw4.dta")

# Base pondération : contient wt_wave4 (poids de sondage Vague 4)
# Ces poids reflètent le plan de sondage stratifié à deux degrés du GHS-Panel
# (strates = État × Milieu). Ne pas les utiliser fausse toutes les inférences
# populationnelles car l'échantillon est disproportionné entre strates.
secta_harvest <- read_dta("data/raw/secta_harvestw4.dta")

# -----------------------------------------------------------------------------
# 2. EXPLORATION PRÉLIMINAIRE — AVANT TOUT NETTOYAGE
# -----------------------------------------------------------------------------

cat("\n====== EXPLORATION : sect11a1 (parcelles) ======\n")
cat("Dimensions :", dim(sect11a1), "\n")
cat("Noms des variables :\n") ; print(names(sect11a1))
glimpse(sect11a1)
print(summary(sect11a1))

cat("\nFréquences unité de mesure (s11aq4b) :\n")
print(table(sect11a1$s11aq4b, useNA = "always"))

cat("\nSuperficie déclarée brute (s11aq4aa) :\n")
print(summary(as.numeric(sect11a1$s11aq4aa)))

cat("\nSuperficie GPS en m² (s11aq4c) :\n")
print(summary(as.numeric(sect11a1$s11aq4c)))

cat("\nFréquences Zone :\n")
print(table(sect11a1$zone, useNA = "always"))

cat("\nFréquences Secteur :\n")
print(table(sect11a1$sector, useNA = "always"))

cat("\nProportion de NA par variable (sect11a1) :\n")
na_pct_a1 <- sect11a1 %>%
  summarise(across(everything(), ~ mean(is.na(.)) * 100)) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "pct_NA") %>%
  filter(pct_NA > 0) %>%
  arrange(desc(pct_NA))
print(na_pct_a1)

cat("\n====== EXPLORATION : sect11b1 (tenure) ======\n")
cat("Dimensions :", dim(sect11b1), "\n")
glimpse(sect11b1)

cat("\nFréquences régime de tenure brut (s11b1q4) :\n")
print(table(sect11b1$s11b1q4, useNA = "always"))

cat("\n====== EXPLORATION : secta_harvest (pondération) ======\n")
cat("Dimensions :", dim(secta_harvest), "\n")
cat("Variables disponibles :\n") ; print(names(secta_harvest))
glimpse(secta_harvest)
cat("\nRésumé de wt_wave4 :\n")
print(summary(as.numeric(secta_harvest$wt_wave4)))
cat("NA dans wt_wave4 :", sum(is.na(secta_harvest$wt_wave4)), "\n")

# Vérification unicité de la clé hhid dans la base poids
dupl_poids <- secta_harvest %>%
  count(hhid) %>%
  filter(n > 1)
cat("Doublons hhid dans secta_harvest :", nrow(dupl_poids), "\n")

# -----------------------------------------------------------------------------
# 3. VÉRIFICATION DE L'UNICITÉ DES CLÉS AVANT FUSION
# -----------------------------------------------------------------------------

dupl_a1 <- sect11a1 %>% count(hhid, plotid) %>% filter(n > 1)
cat("\nDoublons (hhid, plotid) dans sect11a1 :", nrow(dupl_a1), "\n")

dupl_b1 <- sect11b1 %>% count(hhid, plotid) %>% filter(n > 1)
cat("Doublons (hhid, plotid) dans sect11b1 :", nrow(dupl_b1), "\n")

# -----------------------------------------------------------------------------
# 4. NETTOYAGE DE BASE — UNIFORMISATION DES TYPES
# -----------------------------------------------------------------------------

sect11a1 <- sect11a1 %>% clean_names()
sect11b1 <- sect11b1 %>% clean_names()
secta_harvest <- secta_harvest %>% clean_names()

# Conversion haven_labelled → types R natifs
# Le format Stata encode les valeurs avec des labels numériques ;
# sans conversion explicite, les opérations arithmétiques échouent ou
# produisent des résultats erronés sous R.
sect11a1 <- sect11a1 %>%
  mutate(
    superficie_declaree_brute = as.numeric(s11aq4aa),
    unite_code                = as.numeric(s11aq4b),
    superficie_gps_m2         = as.numeric(s11aq4c),
    zone_code                 = as.numeric(zone),
    state_code                = as.numeric(state),
    sector_code               = as.numeric(sector)
  )

sect11b1 <- sect11b1 %>%
  mutate(
    tenure_code = as.numeric(s11b1q4),
    zone_code   = as.numeric(zone),
    state_code  = as.numeric(state),
    sector_code = as.numeric(sector)
  )

secta_harvest <- secta_harvest %>%
  mutate(
    hhid     = as.numeric(hhid),
    poids    = as.numeric(wt_wave4)
  )

# On garde un seul enregistrement par hhid dans la base poids
# (en cas de doublons résiduels, on prend la première occurrence)
poids_menage <- secta_harvest %>%
  select(hhid, poids) %>%
  group_by(hhid) %>%
  slice(1) %>%
  ungroup()

cat("\nPoids disponibles pour", nrow(poids_menage), "ménages\n")
cat("NA dans poids :", sum(is.na(poids_menage$poids)), "\n")

# -----------------------------------------------------------------------------
# 5. TABLE DE CONVERSION DES SUPERFICIES EN HECTARES
# -----------------------------------------------------------------------------
# Construction basée sur :
#   (a) GHS-Panel Nigeria — General Conversion Factors to Hectares
#   (b) Examen du questionnaire Section 11 (Question s11aq4b)
#
# La variable s11aq4b contient 7 modalités :
#   1 = Heaps    2 = Ridges   3 = Stands   4 = Plots
#   5 = Acres    6 = Hectares  7 = Sq Metres
#
# Stratégie de conversion :
# → Unités métriques ou standards (Plots, Acres, Hectares, m²) :
#   facteur unique, indépendant de la localisation.
#   - Acres : on utilise 0.404686 ha (valeur exacte), non l'arrondi 0.4 du
#     tableau GHS, pour éviter une erreur systématique sur les grandes parcelles.
#
# → Unités locales (Heaps, Ridges, Stands) :
#   facteur variable selon la zone géopolitique (6 zones). Ces unités
#   désignent des modes de plantation traditionnels dont la superficie réelle
#   dépend des pratiques agricoles locales. Les facteurs sont issus du tableau
#   "Zone Specific Conversion Factors" de la documentation GHS.
#
# Correspondance zones :
#   1 = North Central  2 = North East   3 = North West
#   4 = South East     5 = South South  6 = South West

conv_zone <- tribble(
  ~zone_code, ~zone_nom,        ~heaps_cf,  ~ridges_cf, ~stands_cf,
  1L, "North Central",           0.00012,    0.0027,     0.00006,
  2L, "North East",              0.00016,    0.0040,     0.00016,
  3L, "North West",              0.00011,    0.00494,    0.00004,
  4L, "South East",              0.00019,    0.0023,     0.00004,
  5L, "South South",             0.00021,    0.0023,     0.00013,
  6L, "South West",              0.00012,    0.00001,    0.00041
)

sect11a1 <- sect11a1 %>%
  left_join(conv_zone, by = "zone_code") %>%
  mutate(
    facteur_conversion = case_when(
      unite_code == 4 ~ 0.0667,
      unite_code == 5 ~ 0.404686,
      unite_code == 6 ~ 1.0,
      unite_code == 7 ~ 0.0001,
      unite_code == 1 ~ heaps_cf,
      unite_code == 2 ~ ridges_cf,
      unite_code == 3 ~ stands_cf,
      TRUE            ~ NA_real_
    ),
    superficie_ha     = superficie_declaree_brute * facteur_conversion,
    superficie_gps_ha = superficie_gps_m2 * 0.0001
  )

cat("\n=== Vérification conversion superficie → ha ===\n")
sect11a1 %>%
  group_by(unite_code, facteur_conversion) %>%
  summarise(n = n(), moy_ha = round(mean(superficie_ha, na.rm = TRUE), 3),
            .groups = "drop") %>%
  print()

# -----------------------------------------------------------------------------
# 6. RECODAGE DU RÉGIME DE TENURE
# -----------------------------------------------------------------------------
# Construction basée sur :
#   - Questionnaire GHS-Panel W4, Section 11B1, Question 4 (s11b1q4)
#   - Tableau de correspondance fourni (codes 1 à 7 → 5 catégories analytiques)
#   - Consultation du rapport "Land Tenure Systems in Nigeria" (NBS, 2019)
#
# Regroupement justifié :
#   Codes 3, 4, 7 → "Prêt" : ces trois modalités correspondent toutes à un
#   accès sans contrepartie monétaire et sans transfert de propriété, qu'il
#   soit individuel (gratuit), communautaire ou temporaire par échange.
#
# Code 5 → "Héritage" : dominant dans les zones rurales, reflète la
#   transmission coutumière des droits fonciers au sein des familles.

sect11b1 <- sect11b1 %>%
  mutate(
    tenure_categorie = case_when(
      tenure_code == 1 ~ "Propriété pleine",
      tenure_code == 2 ~ "Location",
      tenure_code == 3 ~ "Prêt",
      tenure_code == 4 ~ "Prêt",
      tenure_code == 5 ~ "Héritage",
      tenure_code == 6 ~ "Métayage",
      tenure_code == 7 ~ "Prêt",
      is.na(tenure_code) ~ NA_character_,
      TRUE               ~ "Autre"
    ),
    tenure_categorie = factor(
      tenure_categorie,
      levels = c("Propriété pleine", "Héritage", "Location",
                 "Métayage", "Prêt", "Autre")
    ),
    milieu = factor(sector_code, levels = c(1, 2),
                    labels = c("Urbain", "Rural"))
  )

cat("\n=== Vérification recodage tenure ===\n")
sect11b1 %>% count(tenure_categorie) %>% print()

# -----------------------------------------------------------------------------
# 7. DÉTECTION ET TRAITEMENT DES VALEURS ABERRANTES
# -----------------------------------------------------------------------------
# Seuils retenus après consultation du BID (Basic Information Document) :
# → Superficie négative : impossible physiquement → NA
# → Superficie > 500 ha : dans le contexte des exploitations familiales
#   nigérianes (taille médiane ~0,3 ha), une valeur > 500 ha est
#   quasi-certainement une erreur de saisie (confusion d'unité, décimale
#   manquante, valeur en m² saisie à la place de ha…).
#   Ce seuil est cohérent avec la littérature GHS (Oseni et al., 2015).

n_neg <- sum(sect11a1$superficie_ha < 0, na.rm = TRUE)
n_ext <- sum(sect11a1$superficie_ha > 500, na.rm = TRUE)
cat("\nSuperficies négatives :", n_neg, "\n")
cat("Superficies > 500 ha  :", n_ext, "\n")

if (n_ext > 0) {
  cat("Détail des valeurs > 500 ha :\n")
  sect11a1 %>%
    filter(superficie_ha > 500) %>%
    select(hhid, plotid, superficie_declaree_brute, unite_code,
           facteur_conversion, superficie_ha) %>%
    print()
}

sect11a1 <- sect11a1 %>%
  mutate(
    superficie_ha_brute       = superficie_ha,
    superficie_gps_ha_brute   = superficie_gps_ha,
    superficie_ha     = if_else(superficie_ha < 0 | superficie_ha > 500,
                                NA_real_, superficie_ha),
    superficie_gps_ha = if_else(superficie_gps_ha < 0 | superficie_gps_ha > 500,
                                NA_real_, superficie_gps_ha)
  )

# Diagnostic des causes de NA
sect11a1 <- sect11a1 %>%
  mutate(
    raison_na = case_when(
      !is.na(superficie_ha)                                             ~ "Valide",
      is.na(superficie_declaree_brute) & is.na(unite_code)             ~ "Valeur + Unité manquantes",
      is.na(superficie_declaree_brute) & !is.na(unite_code)            ~ "Valeur manquante",
      !is.na(superficie_declaree_brute) & is.na(unite_code)            ~ "Unité manquante",
      !is.na(superficie_ha_brute) &
        (superficie_ha_brute < 0 | superficie_ha_brute > 500)          ~ "Aberrant (recodé NA)",
      TRUE                                                              ~ "Autre"
    )
  )

cat("\nDiagnostic NA superficie_ha :\n")
sect11a1 %>%
  count(raison_na) %>%
  mutate(pct = round(n / sum(n) * 100, 1)) %>%
  arrange(desc(n)) %>%
  print()

# -----------------------------------------------------------------------------
# 8. SÉLECTION, FUSION ET AJOUT DES POIDS
# -----------------------------------------------------------------------------

parcelles <- sect11a1 %>%
  select(hhid, plotid, state_code, sector_code, zone_code, zone_nom,
         superficie_declaree_brute, unite_code, facteur_conversion,
         superficie_ha, superficie_ha_brute,
         superficie_gps_ha, superficie_gps_ha_brute, raison_na)

tenure <- sect11b1 %>%
  select(hhid, plotid, tenure_code, tenure_categorie, milieu,
         state_code, sector_code, zone_code)

# Fusion parcelles ← tenure (left join : toutes les parcelles conservées)
parcelles_full <- parcelles %>%
  left_join(
    tenure %>% select(hhid, plotid, tenure_code, tenure_categorie, milieu),
    by = c("hhid", "plotid")
  ) %>%
  mutate(
    milieu = if_else(
      is.na(milieu),
      factor(sector_code, levels = c(1, 2), labels = c("Urbain", "Rural")),
      milieu
    )
  )

# Vérification que la jointure n'a pas gonflé les lignes
stopifnot("Jointure incorrecte : doublons créés !" = nrow(parcelles_full) == nrow(parcelles))
cat("\nJointure parcelles × tenure : OK (", nrow(parcelles_full), "lignes)\n")

# Ajout des poids au niveau parcelle (jointure sur hhid)
# Le poids est défini au niveau ménage — toutes les parcelles d'un même
# ménage reçoivent le même poids (logique du plan de sondage).
parcelles_full <- parcelles_full %>%
  left_join(poids_menage, by = "hhid")

cat("Parcelles sans poids :", sum(is.na(parcelles_full$poids)), "\n")

# -----------------------------------------------------------------------------
# 9. SUPERFICIE TOTALE PAR MÉNAGE + POIDS
# -----------------------------------------------------------------------------
# Règle d'agrégation :
# - na.rm = TRUE : on somme les parcelles valides
# - Si TOUTES les parcelles d'un ménage sont NA → superficie totale = NA
#   (évite de créer des superficies nulles artificielles)

superficie_menage <- parcelles_full %>%
  group_by(hhid) %>%
  summarise(
    nb_parcelles         = n(),
    superficie_totale_ha = if_else(
      all(is.na(superficie_ha)),
      NA_real_,
      sum(superficie_ha, na.rm = TRUE)
    ),
    n_parcelles_na = sum(is.na(superficie_ha)),
    state_code     = first(state_code),
    sector_code    = first(sector_code),
    zone_code      = first(zone_code),
    zone_nom       = first(zone_nom),
    poids          = first(poids),      # poids identique pour toutes les parcelles du ménage
    .groups = "drop"
  ) %>%
  mutate(
    milieu = factor(sector_code, levels = c(1, 2), labels = c("Urbain", "Rural"))
  )

cat("\n=== Superficie totale par ménage (ha) ===\n")
print(summary(superficie_menage$superficie_totale_ha))
cat("NA dans poids (ménages) :", sum(is.na(superficie_menage$poids)), "\n")

# Ajout superficie totale et poids à parcelles_full
parcelles_full <- parcelles_full %>%
  left_join(
    superficie_menage %>% select(hhid, nb_parcelles, superficie_totale_ha),
    by = "hhid"
  )

# -----------------------------------------------------------------------------
# 10. SAUVEGARDE
# -----------------------------------------------------------------------------

# Création automatique du dossier de sortie (reproductibilité)
dir.create("data/processed", recursive = TRUE, showWarnings = FALSE)

saveRDS(parcelles_full,    "data/processed/parcelles_full.rds")
saveRDS(superficie_menage, "data/processed/superficie_menage.rds")
saveRDS(tenure,            "data/processed/tenure.rds")
saveRDS(poids_menage,      "data/processed/poids_menage.rds")

cat("\n====================================================\n")
cat("  NETTOYAGE TERMINÉ\n")
cat("  parcelles_full.rds    :", nrow(parcelles_full), "obs.\n")
cat("  superficie_menage.rds :", nrow(superficie_menage), "ménages\n")
cat("  tenure.rds            :", nrow(tenure), "obs.\n")
cat("  poids_menage.rds      :", nrow(poids_menage), "ménages\n")
cat("====================================================\n")
