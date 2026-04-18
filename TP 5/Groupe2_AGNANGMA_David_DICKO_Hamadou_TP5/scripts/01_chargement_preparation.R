# ===========================================================================
# Script 01 : Chargement et préparation des données
# Thème 5 — Analyse des cultures, intrants et rendements agricoles
# Enquête GHS-Panel Nigeria — Vague 4 (2018/19)
#
# Opérations réalisées :
#   (a) Récupération automatique des fichiers .dta via GitHub
#   (b) Lecture et vérification des tables de données
#   (c) Construction du référentiel de pondérations
#   (d) Codification des cultures
#   (e) Préparation de la table cultures (Q25, Q26)
#   (f) Préparation de la table intrants (Q27)
#   (g) Préparation de la table superficie (secta1) et rendement kg/ha (Q28, Q29)
#   (h) Archivage des objets traités
#
# Réalisé par : David Landry AGNANGMA SANAM | Hamadou DICKO
# ===========================================================================

library(haven)
library(dplyr)
library(tidyr)
library(forcats)

# ───────────────────────────────────────────────────────────────────────────
# (a) RÉCUPÉRATION DES DONNÉES DEPUIS LE DÉPÔT GITHUB
# ───────────────────────────────────────────────────────────────────────────
cat(">> Vérification des fichiers de données...\n")
options(timeout = 300)

url_base <- paste0(
  "https://raw.githubusercontent.com/",
  "Herman-YAMAHA/NYHP/",
  "main/",
  "TP4_raw/"
)

# Liste des fichiers nécessaires
noms_fichiers <- c(
  "secta3i_harvestw4.dta",    # données de récolte
  "secta3ii_harvestw4.dta",   # synthèse culture-ménage
  "secta_harvestw4.dta",      # pondérations et localisation
  "secta11c2_harvestw4.dta",  # données d'intrants
  "secta1_harvestw4.dta"      # superficies des parcelles (GPS + auto-déclaré)
)

dir.create("data/raw", recursive = TRUE, showWarnings = FALSE)

for (nom in noms_fichiers) {
  chemin_local <- file.path("data/raw", nom)
  if (!file.exists(chemin_local)) {
    cat("  Récupération de", nom, "...\n")
    tryCatch(
      {
        download.file(paste0(url_base, nom), destfile = chemin_local, mode = "wb")
        cat("  [OK]", nom, "\n")
      },
      error = function(err) cat("  [ERREUR]", nom, ":", conditionMessage(err), "\n")
    )
  } else {
    cat("  [OK]", nom, "(existant)\n")
  }
}

# ───────────────────────────────────────────────────────────────────────────
# (b) LECTURE DES FICHIERS BRUTS
# ───────────────────────────────────────────────────────────────────────────
cat(">> Lecture des fichiers .dta...\n")

# Table des récoltes W4
tbl_recoltes <- read_dta("data/raw/secta3i_harvestw4.dta")

# Synthèse culture par ménage W4
tbl_synthese <- read_dta("data/raw/secta3ii_harvestw4.dta")

# Données d'intrants W4
tbl_intrants_brut <- read_dta("data/raw/secta11c2_harvestw4.dta")

# Référentiel de pondérations
tbl_poids <- read_dta("data/raw/secta_harvestw4.dta")

# Superficies des parcelles (GPS + auto-déclaré)
tbl_superficie <- read_dta("data/raw/secta1_harvestw4.dta")

cat("  tbl_recoltes      :", nrow(tbl_recoltes),      "lignes x", ncol(tbl_recoltes),      "colonnes\n")
cat("  tbl_synthese      :", nrow(tbl_synthese),      "lignes x", ncol(tbl_synthese),      "colonnes\n")
cat("  tbl_intrants_brut :", nrow(tbl_intrants_brut), "lignes x", ncol(tbl_intrants_brut), "colonnes\n")
cat("  tbl_poids         :", nrow(tbl_poids),         "lignes x", ncol(tbl_poids),         "colonnes\n")
cat("  tbl_superficie    :", nrow(tbl_superficie),    "lignes x", ncol(tbl_superficie),    "colonnes\n")

# ───────────────────────────────────────────────────────────────────────────
# (c) RÉFÉRENTIEL DE PONDÉRATIONS ET LOCALISATION
# ───────────────────────────────────────────────────────────────────────────
ref_poids <- tbl_poids %>%
  select(hhid, wt_wave4, zone, state, sector) %>%
  filter(!is.na(wt_wave4)) %>%
  mutate(
    nom_zone = factor(as.numeric(zone), levels = 1:6,
                      labels = c("North Central", "North East", "North West",
                                 "South East",   "South South", "South West")),
    type_milieu = factor(as.numeric(sector), levels = c(1, 2),
                         labels = c("Urbain", "Rural")),
    nom_etat = factor(as.numeric(state), levels = 1:37,
                      labels = c("Abia","Adamawa","Akwa Ibom","Anambra","Bauchi",
                                 "Bayelsa","Benue","Borno","Cross River","Delta",
                                 "Ebonyi","Edo","Ekiti","Enugu","Gombe","Imo",
                                 "Jigawa","Kaduna","Kano","Katsina","Kebbi","Kogi",
                                 "Kwara","Lagos","Nasarawa","Niger","Ogun","Ondo",
                                 "Osun","Oyo","Plateau","Rivers","Sokoto","Taraba",
                                 "Yobe","Zamfara","FCT"))
  )

cat("\n>> Pondérations chargées :", nrow(ref_poids), "ménages\n")
cat("   Étendue wt_wave4 :", round(min(ref_poids$wt_wave4), 1),
    "—", round(max(ref_poids$wt_wave4), 1), "\n")

# ───────────────────────────────────────────────────────────────────────────
# (d) NOMENCLATURE DES CULTURES
# ───────────────────────────────────────────────────────────────────────────

nomenclature_cultures <- tribble(
  ~cropcode, ~nom_culture,              ~famille,
  1010,  "Beans/Cowpea",              "Légumineuse",
  1020,  "Cassava",                   "Tubercule",
  1040,  "Cocoyam",                   "Tubercule",
  1050,  "Cotton",                    "Culture de rente",
  1060,  "Groundnut/Peanut",          "Légumineuse",
  1070,  "Sorghum",                   "Céréale",
  1080,  "Maize",                     "Céréale",
  1090,  "Melon/Egusi",               "Légumineuse",
  1093,  "Watermelon",                "Légumineuse",
  1100,  "Millet",                    "Céréale",
  1110,  "Rice",                      "Céréale",
  1121,  "Yam (White)",               "Tubercule",
  1122,  "Yam (Yellow)",              "Tubercule",
  1123,  "Water Yam",                 "Tubercule",
  1124,  "Yam (Three-Leaved)",        "Tubercule",
  2010,  "Acha",                      "Céréale",
  2020,  "Bambara Nut",               "Légumineuse",
  2030,  "Banana",                    "Culture de rente",
  2040,  "Sesame",                    "Culture de rente",
  2050,  "Carrot",                    "Légume",
  2060,  "Cucumber",                  "Légume",
  2070,  "Cabbage",                   "Légume",
  2080,  "Garden Egg",                "Légume",
  2090,  "Garlic",                    "Légume",
  2100,  "Ginger",                    "Culture de rente",
  2120,  "Okro",                      "Légume",
  2130,  "Onion",                     "Légume",
  2141,  "Sweet Pepper",              "Légume",
  2142,  "Hot Pepper",                "Légume",
  2150,  "Pigeon Pea",                "Légumineuse",
  2160,  "Pineapple",                 "Culture de rente",
  2170,  "Plantain",                  "Culture de rente",
  2180,  "Irish Potato",              "Tubercule",
  2181,  "Sweet Potato",              "Tubercule",
  2190,  "Pumpkin",                   "Légume",
  2194,  "Green Vegetable",           "Légume",
  2220,  "Soybean",                   "Légumineuse",
  2230,  "Sugar Cane",                "Culture de rente",
  3180,  "Palm Kernel/Oil",           "Culture de rente"
)

cat("\n>> Nomenclature des cultures chargée.\n")

# ───────────────────────────────────────────────────────────────────────────
# (e) TABLE DES CULTURES W4 — pour Q25 et Q26
# ───────────────────────────────────────────────────────────────────────────
cat("\n>> Élaboration de la table des cultures W4...\n")

tbl_cultures <- tbl_recoltes %>%
  select(hhid, cropcode, zone, state, sector) %>%
  distinct(hhid, cropcode, .keep_all = TRUE) %>%
  mutate(cropcode = as.numeric(cropcode)) %>%
  left_join(nomenclature_cultures, by = "cropcode") %>%
  mutate(
    nom_culture = ifelse(is.na(nom_culture),
                         paste0("Non classée (", cropcode, ")"), nom_culture),
    famille = ifelse(is.na(famille), "Autre", famille)
  ) %>%
  left_join(ref_poids %>% select(hhid, wt_wave4, type_milieu, nom_etat, nom_zone),
            by = "hhid")

# Indice de diversification : nombre de cultures distinctes par ménage
tbl_diversification <- tbl_cultures %>%
  group_by(hhid, wt_wave4, type_milieu) %>%
  summarise(nb_cultures = n_distinct(cropcode), .groups = "drop")

effectif_pondere <- tbl_cultures %>%
  filter(!is.na(wt_wave4)) %>%
  distinct(hhid, wt_wave4) %>%
  pull(wt_wave4) %>% sum()

cat("   Ménages agricoles         :", n_distinct(tbl_cultures$hhid), "\n")
cat("   Paires ménage-culture     :", nrow(tbl_cultures), "\n")
cat("   Effectif pondéré total    :", round(effectif_pondere), "\n")

# ───────────────────────────────────────────────────────────────────────────
# (f) TABLE DES INTRANTS W4 — pour Q27
# ───────────────────────────────────────────────────────────────────────────
cat("\n>> Élaboration de la table des intrants W4...\n")

tbl_intrants_parcelle <- tbl_intrants_brut %>%
  select(hhid, plotid, zone, state, sector,
         s11dq1a, s11c2q36_1, s11c2q36_2, s11dq36, s11c2q1, s11c2q10) %>%
  mutate(
    engrais_inorg = as.numeric(s11dq1a)   == 1,
    npk           = as.numeric(s11c2q36_1) == 1,
    urea          = as.numeric(s11c2q36_2) == 1,
    engrais_org   = as.numeric(s11dq36)   == 1,
    pesticide     = as.numeric(s11c2q1)   == 1,
    herbicide     = as.numeric(s11c2q10)  == 1
  ) %>%
  left_join(ref_poids %>% select(hhid, wt_wave4, type_milieu, nom_etat, nom_zone),
            by = "hhid")

# Agrégation ménage
tbl_intrants_menage <- tbl_intrants_parcelle %>%
  group_by(hhid, wt_wave4, type_milieu, nom_etat, nom_zone) %>%
  summarise(
    engrais_inorg = any(engrais_inorg, na.rm = TRUE),
    npk           = any(npk,           na.rm = TRUE),
    urea          = any(urea,          na.rm = TRUE),
    engrais_org   = any(engrais_org,   na.rm = TRUE),
    pesticide     = any(pesticide,     na.rm = TRUE),
    herbicide     = any(herbicide,     na.rm = TRUE),
    .groups = "drop"
  )

cat("   Parcelles observées       :", nrow(tbl_intrants_parcelle), "\n")
cat("   Ménages avec intrants     :", nrow(tbl_intrants_menage), "\n")
cat("   % engrais inorganique     :",
    round(mean(tbl_intrants_menage$engrais_inorg, na.rm = TRUE) * 100, 1), "%\n")

# ───────────────────────────────────────────────────────────────────────────
# (g) TABLE SUPERFICIE + RENDEMENT MAÏS/MILLET W4 — pour Q28 et Q29
# ───────────────────────────────────────────────────────────────────────────
cat("\n>> Préparation des superficies (secta1_harvestw4)...\n")

# Construire la superficie « meilleure estimation » par parcelle :
#   1) GPS en priorité (prefilled_gps_area, en m²)
#   2) Sinon auto-déclaré (sa1q11, en m²)
# Conversion en hectares (÷ 10 000)

tbl_parcelle_surface <- tbl_superficie %>%
  select(hhid, plotid, prefilled_gps_area, sa1q11) %>%
  mutate(
    superficie_m2 = ifelse(!is.na(prefilled_gps_area),
                           as.numeric(prefilled_gps_area),
                           as.numeric(sa1q11)),
    superficie_ha = superficie_m2 / 10000,
    source_superficie = case_when(
      !is.na(prefilled_gps_area) ~ "GPS",
      !is.na(sa1q11)             ~ "Auto-déclaré",
      TRUE                       ~ NA_character_
    )
  ) %>%
  filter(!is.na(superficie_ha), superficie_ha > 0)

cat("   Parcelles avec superficie :", nrow(tbl_parcelle_surface), "/",
    nrow(tbl_superficie), "\n")
cat("   dont GPS                  :",
    sum(tbl_parcelle_surface$source_superficie == "GPS"), "\n")
cat("   dont Auto-déclaré         :",
    sum(tbl_parcelle_surface$source_superficie == "Auto-déclaré"), "\n")
cat("   Superficie (ha) — médiane :",
    round(median(tbl_parcelle_surface$superficie_ha), 3), "\n")
cat("   Superficie (ha) — moyenne :",
    round(mean(tbl_parcelle_surface$superficie_ha), 3), "\n")

# ── Construction de la table de production + rendement ────────────────────
cat("\n>> Élaboration de la table de production et rendement (maïs/millet)...\n")

tbl_production <- tbl_recoltes %>%
  mutate(cropcode = as.numeric(cropcode)) %>%
  filter(cropcode %in% c(1080, 1100),
         as.numeric(sa3iq3) == 1,
         !is.na(sa3iq6i),
         !is.na(sa3iq6_conv)) %>%
  mutate(
    culture = ifelse(cropcode == 1080, "Maïs", "Millet"),
    production_kg = as.numeric(sa3iq6i) * as.numeric(sa3iq6_conv)
  ) %>%
  filter(production_kg > 0) %>%
  # Jointure avec les superficies
  left_join(
    tbl_parcelle_surface %>% select(hhid, plotid, superficie_ha, source_superficie),
    by = c("hhid", "plotid")
  ) %>%
  # Jointure avec les intrants
  left_join(
    tbl_intrants_parcelle %>%
      select(hhid, plotid, engrais_inorg, npk, urea,
             engrais_org, pesticide, herbicide),
    by = c("hhid", "plotid")
  ) %>%
  left_join(ref_poids %>% select(hhid, wt_wave4, type_milieu, nom_etat, nom_zone),
            by = "hhid") %>%
  mutate(
    groupe_engrais = ifelse(engrais_inorg, "Avec engrais inorg.", "Sans engrais inorg."),
    # Rendement en kg/ha (uniquement si superficie disponible)
    rendement_kg_ha = ifelse(!is.na(superficie_ha) & superficie_ha > 0,
                             production_kg / superficie_ha,
                             NA_real_)
  )

n_avec_surface <- sum(!is.na(tbl_production$rendement_kg_ha))
n_total_prod   <- nrow(tbl_production)

cat("   Parcelles maïs/millet     :", n_total_prod, "\n")
cat("   dont Maïs                 :", sum(tbl_production$culture == "Maïs"), "\n")
cat("   dont Millet               :", sum(tbl_production$culture == "Millet"), "\n")
cat("   Avec superficie (→ kg/ha) :", n_avec_surface,
    "(", round(n_avec_surface / n_total_prod * 100, 1), "%)\n")
cat("   Sans superficie           :", n_total_prod - n_avec_surface, "\n")

# Statistiques descriptives du rendement
resume_rendement <- tbl_production %>%
  filter(!is.na(rendement_kg_ha)) %>%
  group_by(culture) %>%
  summarise(
    n       = n(),
    moy     = round(mean(rendement_kg_ha,            na.rm = TRUE), 1),
    med     = round(median(rendement_kg_ha,          na.rm = TRUE), 1),
    ecart_t = round(sd(rendement_kg_ha,              na.rm = TRUE), 1),
    q1      = round(quantile(rendement_kg_ha, 0.25,  na.rm = TRUE), 1),
    q3      = round(quantile(rendement_kg_ha, 0.75,  na.rm = TRUE), 1),
    .groups = "drop"
  )
cat("\n>> Résumé du rendement (kg/ha) :\n")
print(resume_rendement)

# ───────────────────────────────────────────────────────────────────────────
# (h) SAUVEGARDE DES OBJETS INTERMÉDIAIRES
# ───────────────────────────────────────────────────────────────────────────
dir.create("data/processed", showWarnings = FALSE, recursive = TRUE)

saveRDS(ref_poids,              "data/processed/ref_poids.rds")
saveRDS(nomenclature_cultures,  "data/processed/nomenclature_cultures.rds")
saveRDS(tbl_cultures,           "data/processed/tbl_cultures.rds")
saveRDS(tbl_diversification,    "data/processed/tbl_diversification.rds")
saveRDS(tbl_intrants_parcelle,  "data/processed/tbl_intrants_parcelle.rds")
saveRDS(tbl_intrants_menage,    "data/processed/tbl_intrants_menage.rds")
saveRDS(tbl_production,         "data/processed/tbl_production.rds")
saveRDS(tbl_parcelle_surface,   "data/processed/tbl_parcelle_surface.rds")
saveRDS(effectif_pondere,       "data/processed/effectif_pondere.rds")

cat("\n>> [OK] Objets sauvegardés dans data/processed/\n")
