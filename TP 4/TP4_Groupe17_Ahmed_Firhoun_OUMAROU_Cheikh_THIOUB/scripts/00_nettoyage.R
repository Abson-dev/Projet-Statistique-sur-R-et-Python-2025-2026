# ================================================================
# PROJET ENSAE ISE 1 — GHS Nigeria Panel (W4)
# SCRIPT   : scripts/00_nettoyage.R
# OBJET    : Import, fusion et nettoyage des 4 bases brutes
# SORTIE   : data/processed/processed_tp4_w4.rds
# ORDRE    : Exécuter EN PREMIER avant tous les scripts d'analyse
# ================================================================

source("scripts/functions.R")

library(haven)
library(dplyr)
library(labelled)
library(glue)      # interpolation de chaînes de caractères pour les messages
# Créer les dossiers si nécessaires
dir.create("data/processed",  showWarnings = FALSE, recursive = TRUE)
dir.create("outputs/figures", showWarnings = FALSE, recursive = TRUE)
dir.create("outputs/tables",  showWarnings = FALSE, recursive = TRUE)


# ================================================================
# ÉTAPE 1 — IMPORT DES BASES BRUTES
# ================================================================

secta    <- read_dta("data/raw/secta_harvestw4.dta")
secta1   <- read_dta("data/raw/secta1_harvestw4.dta")
sect11a1 <- read_dta("data/raw/sect11a1_plantingw4.dta")
sect11b1 <- read_dta("data/raw/sect11b1_plantingw4.dta")

cat("Dimensions des bases importées :\n")
cat(sprintf("  secta    : %d menages x %d variables\n",    nrow(secta),    ncol(secta)))
cat(sprintf("  secta1   : %d parcelles x %d variables\n",  nrow(secta1),   ncol(secta1)))
cat(sprintf("  sect11a1 : %d parcelles x %d variables\n",  nrow(sect11a1), ncol(sect11a1)))
cat(sprintf("  sect11b1 : %d parcelles x %d variables\n",  nrow(sect11b1), ncol(sect11b1)))


# ================================================================
# ÉTAPE 2 — DÉTECTION DES CODES SPÉCIAUX
# Avant tout renommage, on vérifie les valeurs 999/9999/-99/-999
# qui encodent "NSP" ou "refus" dans les enquêtes Banque Mondiale
# ================================================================
special_codes = c(999, 9999, -99, -999)
bases <- list(sect11a1 = sect11a1, sect11b1 = sect11b1,
              secta = secta, secta1 = secta1)

for (base_name in names(bases)) {
  cat(glue("\n==============================\nBase : {base_name}\n==============================\n"))
  detect_special_codes_clean(bases[[base_name]], special_codes)
}


sect11b1 <- sect11b1 %>%
  mutate(
    s11b1q3               = ifelse(s11b1q3 == 9999, NA, s11b1q3),
    s11b1q14a1 = ifelse(s11b1q14a1 == 999, NA,
                                         s11b1q14a1)
  )

bases <- list(sect11a1 = sect11a1, sect11b1 = sect11b1,
              secta = secta, secta1 = secta1)

for (base_name in names(bases)) {
  cat(glue("\n==============================\nBase : {base_name}\n==============================\n"))
  detect_special_codes_clean(bases[[base_name]], special_codes)
}


sect11b1 <- sect11b1 %>%
  mutate(
    s11b1q3               = ifelse(s11b1q3 == 9999, NA, s11b1q3),
    s11b1q14a1 = ifelse(s11b1q14a1 == 999, NA,
                                         s11b1q14a1)
  )


# ================================================================
# ÉTAPE 3 — SÉLECTION ET RENOMMAGE DES VARIABLES
# On ne garde que les variables utiles aux tâches 19-24
# ================================================================

# --- 3.1 secta : infos ménage + poids de sondage ---
secta_propre <- secta %>%
  select(
    identifiant_menage = hhid,
    zone_code          = zone,
    code_etat          = state,
    code_lga           = lga,
    milieu             = sector,       # 1 = Urbain, 2 = Rural
    poids_vague4       = wt_wave4      # Poids section transversale W4
  )

# --- 3.2 secta1 : parcelles récolte ---
# Variables clés pour la superficie :
#   sa1q9             = Le GPS a-t-il mesuré la parcelle ? (1=Oui)
#   sa1q11            = Superficie de la parcelle (en m² si GPS, sinon unité déclarée)
#   prefilled_gps_area = GPS prérempli depuis la vague précédente (en m²)
secta1_propre <- secta1 %>%
  select(
    identifiant_menage   = hhid,
    identifiant_parcelle = plotid,
    acces_parcelle       = sa1q4,          # Ménage a encore accès ? (1=Oui)
    gps_mesure_recolte   = sa1q9,          # GPS effectué à la récolte (1=Oui)
    superficie_brute     = sa1q11,         # Superficie déclarée ou GPS récolte
    superficie_gps_prefill = prefilled_gps_area  # GPS vague précédente (m²)
  )

# --- 3.3 sect11a1 : superficie GPS mesurée au semis ---
# s11aq4c = superficie GPS en m² (mesure directe au semis)
# s11aq4aa = superficie déclarée au semis (unité dans s11aq4b)
# Note : la jointure par plotid peut ne pas matcher avec secta1
#        car les identifiants de parcelles diffèrent entre vagues
sect11a1_propre <- sect11a1 %>%
  select(
    identifiant_menage   = hhid,
    identifiant_parcelle = plotid,
    superficie_gps_semis    = s11aq4c,    # GPS semis en m² (mesure directe)
    superficie_declaree_semis = s11aq4aa, # Déclaration au semis (unité variable)
    unite_declaree_semis    = s11aq4b     # 1=Metres, 2=Acres, 3=Hectares, etc.
  )

# --- 3.4 sect11b1 : régime foncier et caractéristiques parcelle ---
sect11b1_propre <- sect11b1 %>%
  select(
    identifiant_menage    = hhid,
    identifiant_parcelle  = plotid,
    acquisition_mode      = s11b1q4,   # Mode d'acquisition (tenure)
    titre_legal           = s11b1q7,   # Titre légal (1=Oui, 2=Non)
    droit_vente           = s11b1q19,  # Droit de vendre la parcelle
    utilisation_principale = s11b1q28, # Usage principal du plot
    irrigue               = s11b1q39,  # Parcelle irriguée (1=Oui)
    qualite_sol           = s11b1q45,  # Qualité du sol (1=Bonne … 3=Mauvaise)
    erosion               = s11b1q47   # Problème d'érosion (1=Oui)
  )


# ================================================================
# ÉTAPE 4 — FUSION DES BASES
# Base principale : secta1 (récolte, parcelles cultivées W4)
# Jointures successives via identifiant_menage + identifiant_parcelle
# ================================================================

# Diagnostic de compatibilité des plot_id avant fusion
n_match_11a1 <- length(intersect(
  paste(secta1_propre$identifiant_menage, secta1_propre$identifiant_parcelle),
  paste(sect11a1_propre$identifiant_menage, sect11a1_propre$identifiant_parcelle)
))
n_match_11b1 <- length(intersect(
  paste(secta1_propre$identifiant_menage, secta1_propre$identifiant_parcelle),
  paste(sect11b1_propre$identifiant_menage, sect11b1_propre$identifiant_parcelle)
))
cat(sprintf("\nDiagnostic de jointure :\n"))
cat(sprintf("  secta1 x sect11a1 : %d parcelles en commun sur %d\n",
            n_match_11a1, nrow(secta1_propre)))
cat(sprintf("  secta1 x sect11b1 : %d parcelles en commun sur %d\n",
            n_match_11b1, nrow(secta1_propre)))

# Fusion en 4 étapes
base_fusionnee <- secta1_propre %>%
  # Poids + zone + milieu depuis secta (1 ligne par ménage → plusieurs par parcelle)
  left_join(secta_propre, by = "identifiant_menage") %>%
  # GPS semis depuis sect11a1 (la jointure peut ne produire aucun match)
  left_join(sect11a1_propre,
            by = c("identifiant_menage", "identifiant_parcelle")) %>%
  # Tenure + foncier depuis sect11b1
  left_join(sect11b1_propre,
            by = c("identifiant_menage", "identifiant_parcelle"))

cat(sprintf("\nBase fusionnee : %d parcelles x %d variables\n",
            nrow(base_fusionnee), ncol(base_fusionnee)))


# ================================================================
# ÉTAPE 5 — CONSOLIDATION DE LA SUPERFICIE GPS
#
# PRIORITÉ (justification) :
#   [1] GPS récolte  (sa1q11 si sa1q9==1) → mesure directe la plus récente
#   [2] GPS semis    (s11aq4c)            → mesure directe de la même vague
#   [3] GPS prérempli (prefilled_gps_area) → mesure vague précédente, moins récente
#
# Toutes les mesures GPS sont en m² → conversion en ha par /10000
# La superficie déclarée n'est PAS incluse ici car son unité est
# inconnue et variable (elle est gardée brute pour la tâche 20)
# ================================================================

M2_VERS_HA <- 1 / 10000

base_fusionnee <- base_fusionnee %>%
  mutate(
    # Source [1] : GPS récolte — valide uniquement si sa1q9 == 1 (mesuré par GPS)
    gps_recolte_ha  = ifelse(gps_mesure_recolte == 1,
                             superficie_brute * M2_VERS_HA,
                             NA_real_),
    # Source [2] : GPS semis (sect11a1) — déjà en m²
    gps_semis_ha    = superficie_gps_semis * M2_VERS_HA,
    # Source [3] : GPS prérempli depuis vague précédente — en m²
    gps_prefill_ha  = superficie_gps_prefill * M2_VERS_HA,

    # Variable consolidée : première source non-NA dans l'ordre de priorité
    superficie_ha = dplyr::case_when(
      !is.na(gps_recolte_ha) ~ gps_recolte_ha,
      !is.na(gps_semis_ha)   ~ gps_semis_ha,
      !is.na(gps_prefill_ha) ~ gps_prefill_ha,
      TRUE                   ~ NA_real_
    ),

    # Variable de traçabilité : quelle source a été utilisée ?
    source_superficie = dplyr::case_when(
      !is.na(gps_recolte_ha) ~ "gps_recolte",
      !is.na(gps_semis_ha)   ~ "gps_semis",
      !is.na(gps_prefill_ha) ~ "gps_prefill",
      TRUE                   ~ "aucune"
    )
  )

# Résumé de la consolidation
cat("\nRépartition des sources de superficie :\n")
base_fusionnee %>%
  dplyr::count(source_superficie) %>%
  dplyr::mutate(pct = round(n / sum(n) * 100, 1)) %>%
  print()


# ================================================================
# ÉTAPE 6 — RECODAGES ET LABELS EN FRANÇAIS
# ================================================================

base_fusionnee <- base_fusionnee %>%
  mutate(
    # Milieu de résidence
    milieu_label = dplyr::case_when(
      milieu == 1 ~ "Urbain",
      milieu == 2 ~ "Rural",
      TRUE        ~ NA_character_
    ),

    # Régime de tenure (6 catégories synthétiques)
    tenure_label = dplyr::case_when(
      acquisition_mode == 1 ~ "Propriété pleine",
      acquisition_mode == 2 ~ "Location",
      acquisition_mode == 3 ~ "Prêt",
      acquisition_mode == 4 ~ "Don communautaire",
      acquisition_mode == 5 ~ "Héritage",
      acquisition_mode == 6 ~ "Métayage",
      acquisition_mode == 7 ~ "Échange temporaire",
      TRUE                  ~ NA_character_
    ),

    # Nom de l'État extrait du label Stata (ex: "1. Abia" → "Abia")
    nom_etat = gsub("^[0-9]+\\.\\s*", "",
                    as.character(haven::as_factor(code_etat)))
  )


# ================================================================
# ÉTAPE 7 — VÉRIFICATIONS FINALES
# ================================================================

cat("\n--- Vérifications finales ---\n")
cat("Parcelles totales           :", nrow(base_fusionnee), "\n")
cat("Avec superficie_ha          :",
    sum(!is.na(base_fusionnee$superficie_ha)), "\n")
cat("Sans superficie (aucune src):",
    sum(base_fusionnee$source_superficie == "aucune"), "\n")
cat("Sans poids_vague4           :",
    sum(is.na(base_fusionnee$poids_vague4)), "\n")
cat("Sans tenure_label           :",
    sum(is.na(base_fusionnee$tenure_label)), "\n")
cat("Sans milieu_label           :",
    sum(is.na(base_fusionnee$milieu_label)), "\n")


# ================================================================
# ÉTAPE 8 — EXPORT
# ================================================================

saveRDS(base_fusionnee, "data/processed/processed_tp4_w4.rds")
cat("\n✔ Base sauvegardée : data/processed/processed_tp4_w4.rds\n")
cat(sprintf("  %d parcelles x %d variables\n",
            nrow(base_fusionnee), ncol(base_fusionnee)))
