library(haven)
library(dplyr)
library(tidyr)
library(forcats)
library(survey)

# Lecture des fichiers

# Poids de sondage, géographie
secta   <- read_dta("data/raw/secta_harvestw4.dta")

# Récoltes par parcelle-culture
secta3i <- read_dta("data/raw/secta3i_harvestw4.dta")

# Synthèse culture par ménage
secta3ii <- read_dta("data/raw/secta3ii_harvestw4.dta")

# Intrants réels par parcelle
secta11c2 <- read_dta("data/raw/secta11c2_harvestw4.dta")

cat("  secta      :", nrow(secta),    "ménages\n")
cat("  secta3i    :", nrow(secta3i),  "obs parcelle-culture\n")
cat("  secta3ii   :", nrow(secta3ii), "obs ménage-culture\n")
cat("  secta11c2  :", nrow(secta11c2),"obs parcelle-intrants\n")


# Référentiel de pondérations et localisation
wgt_data <- secta %>%
  select(hhid, wt_wave4, cluster, strata, zone, state, sector) %>%
  filter(!is.na(wt_wave4)) %>%
  mutate(
    milieu = factor(as.numeric(sector), levels = c(1, 2),
                    labels = c("Urbain", "Rural")),
    zone_label = factor(as.numeric(zone), levels = 1:6,
                        labels = c("North Central", "North East", "North West",
                                   "South East",   "South South", "South West")),
    nom_etat = factor(as.numeric(state), levels = 1:37,
                      labels = c("Abia","Adamawa","Akwa Ibom","Anambra","Bauchi",
                                 "Bayelsa","Benue","Borno","Cross River","Delta",
                                 "Ebonyi","Edo","Ekiti","Enugu","Gombe","Imo",
                                 "Jigawa","Kaduna","Kano","Katsina","Kebbi","Kogi",
                                 "Kwara","Lagos","Nasarawa","Niger","Ogun","Ondo",
                                 "Osun","Oyo","Plateau","Rivers","Sokoto","Taraba",
                                 "Yobe","Zamfara","FCT"))
  )

cat("  Ménages avec poids :", nrow(wgt_data), "\n")
cat("  Étendue wt_wave4 :", round(min(wgt_data$wt_wave4), 1),
    "—", round(max(wgt_data$wt_wave4), 1), "\n")


#  Nomenclature des cultures
# Classification agronomique : céréale, légumineuse, tubercule, culture de rente
crop_dict <- tribble(
  ~cropcode, ~crop_name,            ~crop_type,
  1010,  "Niébé / Haricot",         "Légumineuse",
  1020,  "Manioc",                   "Tubercule",
  1040,  "Macabo / Colocase",        "Tubercule",
  1050,  "Coton",                    "Rente",
  1060,  "Arachide",                 "Légumineuse",
  1070,  "Sorgho",                   "Céréale",
  1080,  "Maïs",                     "Céréale",
  1090,  "Melon / Egusi",            "Légumineuse",
  1093,  "Pastèque",                 "Rente",
  1100,  "Mil",                      "Céréale",
  1110,  "Riz",                      "Céréale",
  1121,  "Igname blanche",           "Tubercule",
  1122,  "Igname jaune",             "Tubercule",
  1123,  "Igname d'eau",             "Tubercule",
  1124,  "Igname trois feuilles",    "Tubercule",
  2010,  "Acha (fonio)",             "Céréale",
  2020,  "Voandzou",                 "Légumineuse",
  2030,  "Banane",                   "Rente",
  2040,  "Sésame",                   "Rente",
  2050,  "Carotte",                  "Légumineuse",
  2060,  "Concombre",                "Rente",
  2070,  "Chou",                     "Légumineuse",
  2071,  "Laitue",                   "Légumineuse",
  2080,  "Aubergine",                "Rente",
  2090,  "Ail",                      "Rente",
  2100,  "Gingembre",                "Rente",
  2120,  "Gombo",                    "Légumineuse",
  2130,  "Oignon",                   "Rente",
  2141,  "Poivron",                  "Rente",
  2142,  "Piment rouge",             "Rente",
  2150,  "Pois pigeon",              "Légumineuse",
  2160,  "Ananas",                   "Rente",
  2170,  "Plantain",                 "Rente",
  2180,  "Pomme de terre",           "Tubercule",
  2181,  "Patate douce",             "Tubercule",
  2190,  "Citrouille",               "Rente",
  2194,  "Légumes verts",            "Légumineuse",
  2220,  "Soja",                     "Légumineuse",
  2230,  "Canne à sucre",            "Rente",
  2250,  "Tabac",                    "Rente",
  2260,  "Tomate",                   "Rente",
  2290,  "Zobo (oseille)",           "Rente",
  3030,  "Piment chili",             "Rente",
  3180,  "Palme",                    "Rente"
)


# Table des cultures W4
# Une ligne = un ménage × une culture (distincts)
tbl_cultures <- secta3ii %>%
  select(hhid, cropcode, zone, state, sector) %>%
  distinct(hhid, cropcode, .keep_all = TRUE) %>%
  mutate(cropcode = as.numeric(cropcode)) %>%
  left_join(crop_dict, by = "cropcode") %>%
  mutate(
    crop_name = if_else(is.na(crop_name),
                        paste0("Autre (", cropcode, ")"), crop_name),
    crop_type = if_else(is.na(crop_type), "Autre", crop_type)
  ) %>%
  left_join(wgt_data %>% select(hhid, wt_wave4, milieu, nom_etat, zone_label),
            by = "hhid")

# Effectif pondéré total
effectif_pondere <- tbl_cultures %>%
  filter(!is.na(wt_wave4)) %>%
  distinct(hhid, wt_wave4) %>%
  pull(wt_wave4) %>% sum()

# Diversification : nombre de cultures distinctes par ménage
tbl_diversification <- tbl_cultures %>%
  group_by(hhid, wt_wave4, milieu) %>%
  summarise(nb_cultures = n_distinct(cropcode), .groups = "drop")

cat("  Ménages agricoles        :", n_distinct(tbl_cultures$hhid), "\n")
cat("  Paires ménage-culture    :", nrow(tbl_cultures), "\n")
cat("  Effectif pondéré total   :", round(effectif_pondere), "\n")


# Table des intrants W4
# Source : secta11c2_harvestw4 (intrants réels, pas les conseils agricoles)
# Variables :
#   s11dq1a      = engrais inorganique utilisé (1 = oui)
#   s11c2q36_1   = NPK (1 = oui)
#   s11c2q36_2   = Urée (1 = oui)
#   s11dq36      = engrais organique (1 = oui)
#   s11c2q1      = pesticide (1 = oui)
#   s11c2q10     = herbicide (1 = oui)

tbl_intrants_parcelle <- secta11c2 %>%
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
  left_join(wgt_data %>% select(hhid, wt_wave4, milieu, nom_etat, zone_label),
            by = "hhid")

# Agrégation ménage : adopte = au moins une parcelle utilise l'intrant
tbl_intrants_menage <- tbl_intrants_parcelle %>%
  group_by(hhid, wt_wave4, milieu, nom_etat, zone_label) %>%
  summarise(
    engrais_inorg = any(engrais_inorg, na.rm = TRUE),
    npk           = any(npk,           na.rm = TRUE),
    urea          = any(urea,          na.rm = TRUE),
    engrais_org   = any(engrais_org,   na.rm = TRUE),
    pesticide     = any(pesticide,     na.rm = TRUE),
    herbicide     = any(herbicide,     na.rm = TRUE),
    .groups = "drop"
  )

cat("  Parcelles observées       :", nrow(tbl_intrants_parcelle), "\n")
cat("  Ménages avec intrants     :", nrow(tbl_intrants_menage), "\n")
cat("  % engrais inorganique     :",
    round(mean(tbl_intrants_menage$engrais_inorg, na.rm = TRUE) * 100, 1), "%\n")
cat("  % NPK                    :",
    round(mean(tbl_intrants_menage$npk, na.rm = TRUE) * 100, 1), "%\n")
cat("  % Urée                   :",
    round(mean(tbl_intrants_menage$urea, na.rm = TRUE) * 100, 1), "%\n")
cat("  % engrais organique      :",
    round(mean(tbl_intrants_menage$engrais_org, na.rm = TRUE) * 100, 1), "%\n")


# Table de production maïs/millet W4
# Production en kg = sa3iq6i * sa3iq6_conv (par parcelle)
# Cultures : maïs (1080) et mil (1100), récoltées (sa3iq3 == 1)
cat("\nÉlaboration de la table de production (maïs/millet)...\n")

tbl_production <- secta3i %>%
  mutate(cropcode = as.numeric(cropcode)) %>%
  filter(
    cropcode %in% c(1080, 1100),
    as.numeric(sa3iq3) == 1,
    !is.na(sa3iq6i),
    !is.na(sa3iq6_conv)
  ) %>%
  mutate(
    culture       = if_else(cropcode == 1080, "Maïs", "Mil"),
    production_kg = as.numeric(sa3iq6i) * as.numeric(sa3iq6_conv)
  ) %>%
  filter(production_kg > 0) %>%
  # Joindre les intrants au niveau parcelle
  left_join(
    tbl_intrants_parcelle %>%
      select(hhid, plotid, engrais_inorg, npk, urea,
             engrais_org, pesticide, herbicide),
    by = c("hhid", "plotid")
  ) %>%
  left_join(wgt_data %>% select(hhid, wt_wave4, milieu, nom_etat, zone_label),
            by = "hhid") %>%
  mutate(
    groupe_engrais = if_else(engrais_inorg,
                             "Avec engrais inorg.",
                             "Sans engrais inorg.")
  )

# Résumé statistique de la production brute
resume_prod <- tbl_production %>%
  group_by(culture) %>%
  summarise(
    n       = n(),
    moy     = round(mean(production_kg,            na.rm = TRUE), 1),
    med     = round(median(production_kg,          na.rm = TRUE), 1),
    ecart_t = round(sd(production_kg,              na.rm = TRUE), 1),
    q1      = round(quantile(production_kg, 0.25,  na.rm = TRUE), 1),
    q3      = round(quantile(production_kg, 0.75,  na.rm = TRUE), 1),
    .groups = "drop"
  )

cat("  Parcelles maïs/millet     :", nrow(tbl_production), "\n")
cat("  dont Maïs                 :", sum(tbl_production$culture == "Maïs"), "\n")
cat("  dont Mil                  :", sum(tbl_production$culture == "Mil"), "\n")
cat("  Avec info engrais         :", sum(!is.na(tbl_production$engrais_inorg)), "\n")
cat("\nRésumé production (kg/parcelle) :\n")
print(resume_prod)


# Sauvegarde des objets intermédiaires
saveRDS(wgt_data,             "data/processed/wgt_data.rds")
saveRDS(crop_dict,            "data/processed/crop_dict.rds")
saveRDS(tbl_cultures,         "data/processed/tbl_cultures.rds")
saveRDS(tbl_diversification,  "data/processed/tbl_diversification.rds")
saveRDS(tbl_intrants_parcelle,"data/processed/tbl_intrants_parcelle.rds")
saveRDS(tbl_intrants_menage,  "data/processed/tbl_intrants_menage.rds")
saveRDS(tbl_production,       "data/processed/tbl_production.rds")
saveRDS(effectif_pondere,     "data/processed/effectif_pondere.rds")

cat("\nDonnées sauvegardées dans data/processed/\n")