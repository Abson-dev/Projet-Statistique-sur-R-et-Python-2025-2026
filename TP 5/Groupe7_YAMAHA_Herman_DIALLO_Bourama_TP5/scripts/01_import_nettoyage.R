# =============================================================================
# Script 01 : Import et préparation des données
# Thème 5 — Cultures pratiquées, intrants utilisés et rendements agricoles
# Nigeria GHS-Panel Wave 4 (2018/19)
#
# Ce script réalise :
#   (0) Téléchargement automatique des fichiers .dta depuis GitHub
#   (1) Chargement de tous les fichiers bruts
#   (2) Extraction des pondérations et géolocalisation (secta_harvestw4)
#   (3) Dictionnaire des cultures et facteurs de conversion zone-spécifiques
#   (4) Extraction des superficies de parcelles (secta1_harvestw4)
#   (5) Table des cultures W4 — Q25 et Q26
#   (6) Table des intrants W4 — secta11c2 — Q27
#   (7) Table des rendements maïs/millet W4 — Q28 et Q29
#   (8) Sauvegarde des objets intermédiaires
#
# ─────────────────────────────────────────────────────────────────────────────
# LOGIQUE DE CONSTRUCTION DES SUPERFICIES (secta1_harvestw4)
# ─────────────────────────────────────────────────────────────────────────────
# secta1_harvestw4 est le fichiers des parcelles W4. Deux sources de superficie
# sont disponibles, toutes deux exprimées en mètres carrés :
#
#   sa1q9 == 1 → parcelle mesurée par GPS lors de cette visite.
#                Superficie GPS actuelle : sa1q11 (m²).
#
#   sa1q9 == 2 → pas de nouvelle mesure GPS. On utilise prefilled_gps_area,
#                la superficie GPS de la visite précédente (m²).
#
# Logique de priorisation (du plus précis au moins précis) :
#   1. GPS actuel (sa1q11) si sa1q9==1 et valeur non nulle
#   2. GPS précédent (prefilled_gps_area) comme fallback
#   3. NA sinon → parcelle sans superficie → rendement non calculable
#
# Conversion : area_ha = area_m2 / 10 000 (1 m² = 0,0001 ha)
#
# Les parcelles sans superficie (~10 %) sont exclues uniquement de l'analyse
# du rendement ; ce taux est documenté dans le rapport.
#
# Auteurs : Herman YAMAHA | Bourama DIALLO
# =============================================================================

library(haven)
library(dplyr)
library(tidyr)
library(forcats)

# ─────────────────────────────────────────────────────────────────────────────
# 0. TÉLÉCHARGEMENT DEPUIS GITHUB
# ─────────────────────────────────────────────────────────────────────────────
cat("Vérification / téléchargement des données (GitHub)...\n")
options(timeout = 300)

.base_url <- paste0(
  "https://raw.githubusercontent.com/",
  "Herman-YAMAHA/NYHP/",
  "main/",
  "TP4_raw/"
)

# secta1_harvestw4 ajouté pour les superficies (Q28/Q29)
.fichiers_github <- c(
  "secta3i_harvestw4.dta",    # cultures par parcelle W4 (production + cropcode)
  "secta3ii_harvestw4.dta",   # récapitulatif culture x menage W4
  "secta_harvestw4.dta",      # pondérations wt_wave4, zone géopolitique, Etat
  "secta11c2_harvestw4.dta",  # intrants par parcelle W4
  "secta1_harvestw4.dta"      # plot roster W4 — superficies GPS des parcelles
)

dir.create("data/raw", recursive = TRUE, showWarnings = FALSE)

for (.f in .fichiers_github) {
  .dest <- file.path("data/raw", .f)
  if (!file.exists(.dest) || file.size(.dest) == 0) {
    cat("  Téléchargement :", .f, "...\n")
    tryCatch(
      {
        download.file(paste0(.base_url, .f), destfile = .dest, mode = "wb")
        cat("  \u2713", .f, "téléchargé.\n")
      },
      error = function(e) cat("  \u2717 Erreur :", .f, ":", conditionMessage(e), "\n")
    )
  } else {
    cat("  \u2713", .f, "déjà présent.\n")
  }
}

# ─────────────────────────────────────────────────────────────────────────────
# 1. CHARGEMENT DES FICHIERS BRUTS
# ─────────────────────────────────────────────────────────────────────────────
cat("\n--- Chargement des fichiers bruts ---\n")

s3i_w4   <- read_dta("data/raw/secta3i_harvestw4.dta")
s3ii_w4  <- read_dta("data/raw/secta3ii_harvestw4.dta")
s11c2_w4 <- read_dta("data/raw/secta11c2_harvestw4.dta")
secta_w4 <- read_dta("data/raw/secta_harvestw4.dta")
s1_w4    <- read_dta("data/raw/secta1_harvestw4.dta")

cat("  secta3i   :", nrow(s3i_w4),   "obs x", ncol(s3i_w4),   "vars\n")
cat("  secta3ii  :", nrow(s3ii_w4),  "obs x", ncol(s3ii_w4),  "vars\n")
cat("  secta11c2 :", nrow(s11c2_w4), "obs x", ncol(s11c2_w4), "vars\n")
cat("  secta     :", nrow(secta_w4),  "obs x", ncol(secta_w4),  "vars\n")
cat("  secta1    :", nrow(s1_w4),    "obs x", ncol(s1_w4),    "vars\n")

# ─────────────────────────────────────────────────────────────────────────────
# 2. PONDÉRATIONS ET LOCALISATION
# ─────────────────────────────────────────────────────────────────────────────
df_poids <- secta_w4 %>%
  select(hhid, wt_wave4, zone, state, sector) %>%
  filter(!is.na(wt_wave4)) %>%
  mutate(
    zone_label = factor(as.numeric(zone), levels = 1:6,
                        labels = c("North Central", "North East", "North West",
                                   "South East",   "South South", "South West")),
    milieu     = factor(as.numeric(sector), levels = c(1, 2),
                        labels = c("Urbain", "Rural")),
    state_name = factor(as.numeric(state), levels = 1:37,
                        labels = c("Abia","Adamawa","Akwa Ibom","Anambra","Bauchi",
                                   "Bayelsa","Benue","Borno","Cross River","Delta",
                                   "Ebonyi","Edo","Ekiti","Enugu","Gombe","Imo",
                                   "Jigawa","Kaduna","Kano","Katsina","Kebbi","Kogi",
                                   "Kwara","Lagos","Nasarawa","Niger","Ogun","Ondo",
                                   "Osun","Oyo","Plateau","Rivers","Sokoto","Taraba",
                                   "Yobe","Zamfara","FCT"))
  )

cat("\nPondérations :", nrow(df_poids), "ménages | wt_wave4 min/max :",
    round(min(df_poids$wt_wave4), 0), "/",
    round(max(df_poids$wt_wave4), 0), "\n")

# ─────────────────────────────────────────────────────────────────────────────
# 3. DICTIONNAIRE DES CULTURES ET FACTEURS DE CONVERSION ZONE-SPÉCIFIQUES
# ─────────────────────────────────────────────────────────────────────────────
# Les noms de cultures sont traduits en français pour les graphiques.
# Les codes cropcode correspondent au codebook GHS Wave 4 (World Bank).

cultures_dict <- tribble(
  ~cropcode, ~crop_name,                 ~crop_type,
  1010,  "Niébé / Haricot",              "Légumineuse",
  1020,  "Manioc",                       "Tubercule",
  1040,  "Macabo",                       "Tubercule",
  1050,  "Coton",                        "Culture de rente",
  1060,  "Arachide",                     "Légumineuse",
  1070,  "Sorgho",                       "Céréale",
  1080,  "Maïs",                         "Céréale",
  1090,  "Melon / Egusi",                "Légumineuse",
  1093,  "Pastèque",                     "Légumineuse",
  1100,  "Mil",                          "Céréale",
  1110,  "Riz",                          "Céréale",
  1121,  "Igname blanche",               "Tubercule",
  1122,  "Igname jaune",                 "Tubercule",
  1123,  "Igname d'eau",                 "Tubercule",
  1124,  "Igname trifoliée",             "Tubercule",
  2010,  "Acha",                         "Céréale",
  2020,  "Voandzou",                     "Légumineuse",
  2030,  "Banane",                       "Culture de rente",
  2040,  "Sésame",                       "Culture de rente",
  2050,  "Carotte",                      "Légume",
  2060,  "Concombre",                    "Légume",
  2070,  "Chou",                         "Légume",
  2080,  "Aubergine africaine",          "Légume",
  2090,  "Ail",                          "Légume",
  2100,  "Gingembre",                    "Culture de rente",
  2120,  "Gombo",                        "Légume",
  2130,  "Oignon",                       "Légume",
  2141,  "Poivron doux",                 "Légume",
  2142,  "Piment fort",                  "Légume",
  2150,  "Pois d'Angole",               "Légumineuse",
  2160,  "Ananas",                       "Culture de rente",
  2170,  "Plantain",                     "Culture de rente",
  2180,  "Pomme de terre",               "Tubercule",
  2181,  "Patate douce",                 "Tubercule",
  2190,  "Citrouille",                   "Légume",
  2194,  "Légume vert",                  "Légume",
  2220,  "Soja",                         "Légumineuse",
  2230,  "Canne à sucre",               "Culture de rente",
  3180,  "Palmier à huile",             "Culture de rente"
)

# Facteurs de conversion zone-spécifiques (unités locales -> ha)
# Source : World Bank GHS Panel documentation (Zone Specific Conversion Factors)
# Note : non utilisés directement ici car les superficies de secta1 sont
# toutes en m² (GPS). Documentés pour les analyses de superficie déclarée.
conv_zone <- tribble(
  ~zone, ~heaps,    ~ridges,  ~stands,
  1,     0.00012,   0.0027,   0.00006,
  2,     0.00016,   0.004,    0.00016,
  3,     0.00011,   0.00494,  0.00004,
  4,     0.00019,   0.0023,   0.00004,
  5,     0.00021,   0.0023,   0.00013,
  6,     0.00012,   0.00001,  0.00041
)

cat("Dictionnaire des cultures :", nrow(cultures_dict), "cultures codées (noms en français).\n")

# ─────────────────────────────────────────────────────────────────────────────
# 4. EXTRACTION DES SUPERFICIES — secta1_harvestw4
# ─────────────────────────────────────────────────────────────────────────────
# Variables mobilisées :
#   sa1q9             : parcelle mesurée par GPS lors de cette visite (1=Oui, 2=Non)
#   sa1q11            : superficie GPS actuelle (m²) — remplie si sa1q9 == 1
#   prefilled_gps_area: superficie GPS visite précédente (m²) — fallback

cat("\n--- Extraction des superficies (secta1_harvestw4) ---\n")

plot_areas <- s1_w4 %>%
  select(hhid, plotid, sa1q9, sa1q11, prefilled_gps_area) %>%
  mutate(
    gps_actuel = as.numeric(sa1q9) == 1,
    
    # Source primaire : GPS actuel (m²) — disponible quand sa1q9 == 1
    area_m2_primaire = ifelse(
      gps_actuel & !is.na(as.numeric(sa1q11)) & as.numeric(sa1q11) > 0,
      as.numeric(sa1q11), NA_real_),
    
    # Source secondaire (fallback) : GPS pré-rempli de la vague précédente
    area_m2_fallback = ifelse(
      !is.na(as.numeric(prefilled_gps_area)) & as.numeric(prefilled_gps_area) > 0,
      as.numeric(prefilled_gps_area), NA_real_),
    
    # Superficie finale en m² : source primaire puis fallback
    area_m2 = ifelse(!is.na(area_m2_primaire), area_m2_primaire, area_m2_fallback),
    
    # Conversion en hectares : 1 m² = 0,0001 ha
    area_ha_raw = area_m2 / 10000,
    
    # Filtre de plausibilité : 0,005 ha <= area <= 20 ha
    # (bornes calibrées sur la littérature smallholder nigériane)
    area_ha = ifelse(
      !is.na(area_ha_raw),
      area_ha_raw, NA_real_)
  )

cat("  Total parcelles dans secta1      :", nrow(plot_areas), "\n")
cat("  Avec GPS actuel (sa1q9==1)       :", sum(plot_areas$gps_actuel, na.rm=TRUE), "\n")
cat("  Avec superficie valide (ha)      :", sum(!is.na(plot_areas$area_ha)), "\n")
cat("  Taux de couverture               :",
    round(mean(!is.na(plot_areas$area_ha))*100, 1), "%\n")
cat("  Médiane area_ha                  :",
    round(median(plot_areas$area_ha, na.rm=TRUE), 3), "ha\n")
cat("  Q1 / Q3 area_ha                  :",
    round(quantile(plot_areas$area_ha, 0.25, na.rm=TRUE), 3), "/",
    round(quantile(plot_areas$area_ha, 0.75, na.rm=TRUE), 3), "ha\n")

plot_areas_final <- plot_areas %>%
  filter(!is.na(area_ha)) %>%
  select(hhid, plotid, area_ha)

# ─────────────────────────────────────────────────────────────────────────────
# 5. TABLE DES CULTURES W4 (Q25 et Q26)
# ─────────────────────────────────────────────────────────────────────────────
cat("\n--- Construction de la table des cultures W4 ---\n")

df_cultures_w4 <- s3i_w4 %>%
  select(hhid, cropcode, zone, state, sector) %>%
  distinct(hhid, cropcode, .keep_all = TRUE) %>%
  mutate(cropcode = as.numeric(cropcode)) %>%
  left_join(cultures_dict, by = "cropcode") %>%
  mutate(
    crop_name = ifelse(is.na(crop_name), paste0("Autre (", cropcode, ")"), crop_name),
    crop_type = ifelse(is.na(crop_type), "Autre", crop_type)
  ) %>%
  left_join(df_poids %>% select(hhid, wt_wave4, milieu, state_name, zone_label),
            by = "hhid")

df_diversif_w4 <- df_cultures_w4 %>%
  group_by(hhid, wt_wave4, milieu) %>%
  summarise(n_cultures = n_distinct(cropcode), .groups = "drop")

N_pond_total <- df_cultures_w4 %>%
  filter(!is.na(wt_wave4)) %>%
  distinct(hhid, wt_wave4) %>%
  pull(wt_wave4) %>% sum()

cat("  Ménages agricoles W4         :", n_distinct(df_cultures_w4$hhid), "\n")
cat("  Combinaisons ménage x culture:", nrow(df_cultures_w4), "\n")
cat("  Effectif pondéré total       :", round(N_pond_total), "\n")

# ─────────────────────────────────────────────────────────────────────────────
# 6. TABLE DES INTRANTS W4 — secta11c2 (Q27)
# ─────────────────────────────────────────────────────────────────────────────
cat("\n--- Construction de la table des intrants W4 ---\n")

df_intrants_w4 <- s11c2_w4 %>%
  select(hhid, plotid, zone, state, sector,
         s11dq1a, s11c2q36_1, s11c2q36_2, s11dq36, s11c2q1, s11c2q10) %>%
  mutate(
    engrais_inorg = as.numeric(s11dq1a)    == 1,
    npk           = as.numeric(s11c2q36_1) == 1,
    urea          = as.numeric(s11c2q36_2) == 1,
    engrais_org   = as.numeric(s11dq36)    == 1,
    pesticide     = as.numeric(s11c2q1)    == 1,
    herbicide     = as.numeric(s11c2q10)   == 1
  ) %>%
  left_join(df_poids %>% select(hhid, wt_wave4, milieu, state_name, zone_label),
            by = "hhid")

# Agrégation au niveau ménage : adoption = TRUE si au moins une parcelle l'utilise
df_intrants_menage <- df_intrants_w4 %>%
  group_by(hhid, wt_wave4, milieu, state_name, zone_label) %>%
  summarise(
    engrais_inorg = any(engrais_inorg, na.rm = TRUE),
    npk           = any(npk,           na.rm = TRUE),
    urea          = any(urea,          na.rm = TRUE),
    engrais_org   = any(engrais_org,   na.rm = TRUE),
    pesticide     = any(pesticide,     na.rm = TRUE),
    herbicide     = any(herbicide,     na.rm = TRUE),
    .groups = "drop"
  )

cat("  Parcelles secta11c2          :", nrow(df_intrants_w4), "\n")
cat("  Ménages secta11c2            :", nrow(df_intrants_menage), "\n")
cat("  Taux engrais inorg.          :",
    round(mean(df_intrants_menage$engrais_inorg, na.rm=TRUE)*100, 1), "%\n")
cat("  Taux herbicide               :",
    round(mean(df_intrants_menage$herbicide, na.rm=TRUE)*100, 1), "%\n")

# ─────────────────────────────────────────────────────────────────────────────
# 7. TABLE DES RENDEMENTS MAÏS/MILLET W4 (Q28 et Q29)
# ─────────────────────────────────────────────────────────────────────────────
# Production (kg) = sa3iq6i x sa3iq6_conv (facteur de conversion kg/unité)
# Superficie (ha) = issue de secta1_harvestw4 (section 4)
# Rendement (kg/ha) = prod_kg / area_ha   [NA si superficie manquante]

cat("\n--- Construction de la table des rendements maïs/millet W4 ---\n")

df_crop_intrants <- s3i_w4 %>%
  mutate(cropcode = as.numeric(cropcode)) %>%
  filter(
    cropcode %in% c(1080, 1100),  # 1080=maïs, 1100=millet
    as.numeric(sa3iq3) == 1,       # récolte effectuée
    !is.na(sa3iq6i),
    !is.na(sa3iq6_conv)
  ) %>%
  mutate(
    crop_label = ifelse(cropcode == 1080, "Maïs", "Mil"),
    prod_kg    = as.numeric(sa3iq6i) * as.numeric(sa3iq6_conv)
  ) %>%
  filter(prod_kg > 0) %>%
  left_join(
    df_intrants_w4 %>%
      select(hhid, plotid, engrais_inorg, npk, urea,
             engrais_org, pesticide, herbicide),
    by = c("hhid", "plotid")
  ) %>%
  # Jointure avec les superficies GPS de secta1_harvestw4
  left_join(plot_areas_final, by = c("hhid", "plotid")) %>%
  left_join(df_poids %>% select(hhid, wt_wave4, milieu, state_name, zone_label),
            by = "hhid") %>%
  mutate(
    # Rendement en kg/ha — NA si superficie absente ou non valide
    rendement_kgha = ifelse(!is.na(area_ha) & area_ha > 0,
                            prod_kg / area_ha, NA_real_),
    engrais_label  = ifelse(engrais_inorg,
                            "Avec engrais inorg.", "Sans engrais inorg.")
  )

n_avec_rend <- sum(!is.na(df_crop_intrants$rendement_kgha))
n_sans_rend <- sum( is.na(df_crop_intrants$rendement_kgha))
couverture  <- round(n_avec_rend / nrow(df_crop_intrants) * 100, 1)

cat("  Parcelles maïs/mil           :", nrow(df_crop_intrants), "\n")
cat("  Maïs :", sum(df_crop_intrants$crop_label == "Maïs"),
    "| Mil :", sum(df_crop_intrants$crop_label == "Mil"), "\n")
cat("  Avec rendement kg/ha         :", n_avec_rend,
    "(", couverture, "% de couverture)\n")
cat("  Sans superficie (exclus Q28) :", n_sans_rend, "\n")

stats_rend <- df_crop_intrants %>%
  filter(!is.na(rendement_kgha)) %>%
  group_by(crop_label) %>%
  summarise(
    n   = n(),
    moy = round(mean(rendement_kgha,   na.rm=TRUE), 0),
    med = round(median(rendement_kgha, na.rm=TRUE), 0),
    q1  = round(quantile(rendement_kgha, 0.25, na.rm=TRUE), 0),
    q3  = round(quantile(rendement_kgha, 0.75, na.rm=TRUE), 0),
    .groups = "drop"
  )
cat("\nRendements bruts (kg/ha) :\n"); print(stats_rend)

# ─────────────────────────────────────────────────────────────────────────────
# 8. SAUVEGARDE DES OBJETS INTERMÉDIAIRES
# ─────────────────────────────────────────────────────────────────────────────
dir.create("data/processed", showWarnings = FALSE, recursive = TRUE)

saveRDS(df_poids,           "data/processed/df_poids.rds")
saveRDS(cultures_dict,      "data/processed/cultures_dict.rds")
saveRDS(conv_zone,          "data/processed/conv_zone.rds")
saveRDS(df_cultures_w4,     "data/processed/df_cultures_w4.rds")
saveRDS(df_diversif_w4,     "data/processed/df_diversif_w4.rds")
saveRDS(df_intrants_w4,     "data/processed/df_intrants_w4.rds")
saveRDS(df_intrants_menage, "data/processed/df_intrants_menage.rds")
saveRDS(df_crop_intrants,   "data/processed/df_crop_intrants.rds")
saveRDS(plot_areas_final,   "data/processed/plot_areas.rds")
saveRDS(N_pond_total,       "data/processed/N_pond_total.rds")
saveRDS(list(n_avec=n_avec_rend, n_sans=n_sans_rend, couverture=couverture),
        "data/processed/meta_superficie.rds")

cat("\n\u2713 Tous les objets sauvegardés dans data/processed/\n")