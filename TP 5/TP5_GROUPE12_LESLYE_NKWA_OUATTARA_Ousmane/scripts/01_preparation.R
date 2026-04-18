# ==============================================================================
# TP5 - Script 01 : Préparation et exploration des données
# Analyse 5 : Cultures, intrants et rendements agricoles
# Données : Nigeria GHS Panel W4 (2018-2019), LSMS-ISA
# Auteurs  : [Votre Nom & Prénom], ENSAE ISE1 2025-2026
# Superviseur : M. Aboubacar HEMA, IFPRI
# ==============================================================================

# ---------------------------------------------------------------------------- #
# 0. NETTOYAGE DE L'ENVIRONNEMENT
# ---------------------------------------------------------------------------- #
rm(list = ls())
cat("=== Démarrage de 01_preparation.R ===\n")

# ---------------------------------------------------------------------------- #
# 1. CHARGEMENT DES PACKAGES
# ---------------------------------------------------------------------------- #
# Ceci est fait dans le main.R

# ---------------------------------------------------------------------------- #
# 2. CHARGEMENT DES DONNÉES BRUTES
# ---------------------------------------------------------------------------- #
cat("\n--- Chargement des fichiers .dta ---\n")

# secta3i  : cultures par parcelle (production réelle par plot)
secta3i  <- read_dta(here("data", "raw", "secta3i_harvestw4.dta"))
# secta3ii : production totale par culture et ménage
secta3ii <- read_dta(here("data", "raw", "secta3ii_harvestw4.dta"))
# secta11c2 : module intrants (engrais, pesticides)
sect11c2 <- read_dta(here("data", "raw", "secta11c2_harvestw4.dta"))
# sect11a1  : parcelles avec superficies GPS/déclarées
sect11a1 <- read_dta(here("data", "raw", "sect11a1_plantingw4.dta"))
# secta_ph  : poids de sondage Post-Harvest
secta_ph <- read_dta(here("data", "raw", "secta_harvestw4.dta"))

cat(sprintf("secta3i   : %d obs x %d vars\n", nrow(secta3i),  ncol(secta3i)))
cat(sprintf("secta3ii  : %d obs x %d vars\n", nrow(secta3ii), ncol(secta3ii)))
cat(sprintf("sect11c2  : %d obs x %d vars\n", nrow(sect11c2), ncol(sect11c2)))
cat(sprintf("sect11a1  : %d obs x %d vars\n", nrow(sect11a1), ncol(sect11a1)))
cat(sprintf("secta_ph  : %d obs x %d vars\n", nrow(secta_ph), ncol(secta_ph)))

# ---------------------------------------------------------------------------- #
# 3. EXTRACTION DES POIDS ET VARIABLES DE SONDAGE
# ---------------------------------------------------------------------------- #
cat("\n--- Extraction des poids de sondage ---\n")

poids <- secta_ph |>
  select(hhid, wt_wave4, cluster, strata, zone, state, sector)

cat(sprintf("Ménages avec poids : %d\n", nrow(poids)))
cat(sprintf("NA sur wt_wave4   : %d\n",  sum(is.na(poids$wt_wave4))))

# ---------------------------------------------------------------------------- #
# 4. TABLE DE CORRESPONDANCE : CODES CULTURES -> NOMS & TYPES
# ---------------------------------------------------------------------------- #
cat("\n--- Construction de la nomenclature des cultures ---\n")

# Nomenclature complète basée sur les labels du fichier .dta GHS W4
crop_labels <- tribble(
  ~cropcode, ~crop_name,           ~crop_type,
  1010,      "Beans/Niébé",        "Légumineuse",
  1020,      "Manioc",             "Tubercule",
  1040,      "Taro/Cocoyam",       "Tubercule",
  1050,      "Coton",              "Culture de rente",
  1060,      "Arachide",           "Légumineuse",
  1070,      "Sorgho",             "Céréale",
  1080,      "Maïs",               "Céréale",
  1090,      "Melon/Egusi",        "Légumineuse",
  1093,      "Pastèque",           "Légumineuse",
  1100,      "Mil",                "Céréale",
  1110,      "Riz",                "Céréale",
  1121,      "Igname blanche",     "Tubercule",
  1122,      "Igname jaune",       "Tubercule",
  1123,      "Igname eau",         "Tubercule",
  1124,      "Igname 3 feuilles",  "Tubercule",
  2010,      "Acha (Fonio)",       "Céréale",
  2020,      "Bambara nut",        "Légumineuse",
  2030,      "Banane",             "Fruit",
  2040,      "Sésame",             "Culture de rente",
  2050,      "Carotte",            "Légume",
  2060,      "Concombre",          "Légume",
  2070,      "Chou",               "Légume",
  2071,      "Laitue",             "Légume",
  2080,      "Aubergine africaine","Légume",
  2090,      "Ail",                "Légume",
  2100,      "Gingembre",          "Culture de rente",
  2120,      "Gombo",              "Légume",
  2130,      "Oignon",             "Légume",
  2141,      "Poivron",            "Légume",
  2142,      "Piment",             "Légume",
  2150,      "Pois pigeon",        "Légumineuse",
  2160,      "Ananas",             "Fruit",
  2170,      "Plantain",           "Tubercule",
  2180,      "Pomme de terre",     "Tubercule",
  2181,      "Patate douce",       "Tubercule",
  2190,      "Citrouille",         "Légume",
  2194,      "Légumes feuilles",   "Légume",
  2220,      "Soja",               "Légumineuse",
  2230,      "Canne à sucre",      "Culture de rente",
  2250,      "Tabac",              "Culture de rente",
  2260,      "Tomate",             "Légume",
  2280,      "Blé",                "Céréale",
  3020,      "Cajou",              "Culture de rente",
  3040,      "Cacao",              "Culture de rente",
  3050,      "Noix de coco",       "Fruit",
  3060,      "Café",               "Culture de rente",
  3110,      "Kola",               "Culture de rente",
  3160,      "Mangue",             "Fruit",
  3170,      "Orange",             "Fruit",
  3180,      "Palmier à huile",    "Culture de rente",
  3230,      "Hévéa",              "Culture de rente",
  9001,      "Baka",               "Autre",
  9002,      "Néré",               "Autre",
  9003,      "Moringa",            "Légume",
  9007,      "Karité",             "Culture de rente",
  9999,      "Autre",              "Autre"
)

# ---------------------------------------------------------------------------- #
# 5. PRÉPARATION DE secta3i : CULTURES ET RÉCOLTES PAR PARCELLE
# ---------------------------------------------------------------------------- #
cat("\n--- Préparation de secta3i (cultures par parcelle) ---\n")

# Variables clés :
#   cropcode    : code de la culture
#   sa3iq3      : ménage a-t-il récolté ? (1=oui, 2=non)
#   sa3iq6i     : quantité récoltée (brute)
#   sa3iq6ii    : unité de mesure
#   sa3iq6_conv : facteur de conversion -> kg

df_cultures_plot <- secta3i |>
  select(
    zone, state, sector, hhid, plotid, cropcode,
    harvested = sa3iq3,
    qty_raw   = sa3iq6i,
    unit      = sa3iq6ii,
    kg_conv   = sa3iq6_conv
  ) |>
  mutate(
    cropcode  = as.integer(cropcode),
    harvested = as.integer(harvested),
    qty_kg    = if_else(!is.na(qty_raw) & !is.na(kg_conv),
                        qty_raw * kg_conv, NA_real_)
  ) |>
  left_join(crop_labels, by = "cropcode")

cat(sprintf("Observations cultures (parcelle) : %d\n", nrow(df_cultures_plot)))
cat(sprintf("Cultures récoltées (sa3iq3=1)    : %d (%.1f%%)\n",
            sum(df_cultures_plot$harvested == 1, na.rm = TRUE),
            100 * mean(df_cultures_plot$harvested == 1, na.rm = TRUE)))

# ---------------------------------------------------------------------------- #
# 6. PRÉPARATION DE secta3ii : PRODUCTION TOTALE PAR CULTURE ET MÉNAGE
# ---------------------------------------------------------------------------- #
cat("\n--- Préparation de secta3ii (production par ménage) ---\n")

# Variables clés :
#   cropcode     : code culture
#   sa3iiq1a     : quantité totale récoltée
#   sa3iiq1c     : unité
#   sa3iiq1_conv : facteur conversion -> kg

df_production <- secta3ii |>
  select(
    zone, state, sector, hhid, cropcode,
    qty_raw = sa3iiq1a,
    unit    = sa3iiq1c,
    kg_conv = sa3iiq1_conv
  ) |>
  mutate(
    cropcode = as.integer(cropcode),
    qty_kg   = if_else(!is.na(qty_raw) & !is.na(kg_conv),
                       qty_raw * kg_conv, NA_real_)
  ) |>
  left_join(crop_labels, by = "cropcode")

cat(sprintf("Observations production (ménage) : %d\n", nrow(df_production)))

# ---------------------------------------------------------------------------- #
# 7. PRÉPARATION DES SUPERFICIES (sect11a1)
# ---------------------------------------------------------------------------- #
cat("\n--- Préparation des superficies (sect11a1) ---\n")

# Facteurs de conversion BID Appendix 2 (World Bank, LSMS-ISA Nigeria)
# Zones : 1=North Central, 2=North East, 3=North West,
#         4=South East,    5=South South, 6=South West
conv_heaps  <- c(0.00012, 0.00016, 0.00011, 0.00019, 0.00021, 0.00012)
conv_ridges <- c(0.00270, 0.00400, 0.00494, 0.00230, 0.00230, 0.00001)
conv_stands <- c(0.00006, 0.00016, 0.00004, 0.00004, 0.00013, 0.00041)

df_sup <- sect11a1 |>
  select(
    zone, state, sector, hhid, plotid,
    gps_m2       = s11aq4c,   # Superficie GPS en m²
    sup_declaree = s11aq4aa,  # Superficie déclarée
    unite_decl   = s11aq4b    # Unité (1=Heaps, 2=Ridges, 3=Stands, 5=Acres, 6=Ha, 7=m²)
  ) |>
  mutate(
    zone       = as.integer(zone),
    # GPS -> hectares (prioritaire)
    sup_gps_ha = gps_m2 * 0.0001,
    # Facteur de conversion déclaration selon zone × unité
    facteur = case_when(
      unite_decl == 1 ~ conv_heaps[zone],
      unite_decl == 2 ~ conv_ridges[zone],
      unite_decl == 3 ~ conv_stands[zone],
      unite_decl == 5 ~ 0.4047,
      unite_decl == 6 ~ 1.0,
      unite_decl == 7 ~ 0.0001,
      TRUE            ~ NA_real_
    ),
    sup_decl_ha = sup_declaree * facteur,
    # Stratégie GPS-first avec fallback sur la déclaration (Carletto et al. 2015)
    sup_ha     = if_else(!is.na(sup_gps_ha), sup_gps_ha, sup_decl_ha),
    source_sup = if_else(!is.na(sup_gps_ha), "GPS", "Declaree")
  ) |>
  # Winsorisation : > 50 ha = aberrant (seuil standard LSMS-ISA)
  mutate(sup_ha = if_else(sup_ha > 50, NA_real_, sup_ha))

cat(sprintf("Parcelles avec superficie : %d / %d\n",
            sum(!is.na(df_sup$sup_ha)), nrow(df_sup)))
cat(sprintf("Source GPS               : %.1f%%\n",
            100 * mean(df_sup$source_sup == "GPS", na.rm = TRUE)))

# ---------------------------------------------------------------------------- #
# 8. PRÉPARATION DES INTRANTS (sect11c2)
# ---------------------------------------------------------------------------- #
cat("\n--- Préparation des intrants (sect11c2) ---\n")

# Variables clés :
#   s11dq1a    : engrais inorganique utilisé (1=oui, 2=non)
#   s11c2q36_1 : NPK (1=oui)
#   s11c2q36_2 : Urée (1=oui)
#   s11dq36    : engrais organique (1=oui, 2=non)
#   s11c2q1    : pesticide (1=oui, 2=non)

df_intrants <- sect11c2 |>
  select(
    zone, state, sector, hhid, plotid,
    inorg_any = s11dq1a,
    npk       = s11c2q36_1,
    uree      = s11c2q36_2,
    organique = s11dq36,
    pesticide = s11c2q1
  ) |>
  mutate(
    npk_bin       = as.integer(npk       == 1),
    uree_bin      = as.integer(uree      == 1),
    organique_bin = as.integer(organique == 1),
    pesticide_bin = as.integer(pesticide == 1),
    inorg_bin     = as.integer(inorg_any == 1),
    # Engrais chimique = NPK OU Urée
    chimique_bin  = as.integer(npk_bin == 1 | uree_bin == 1)
  )

cat(sprintf("Parcelles avec données intrants      : %d\n", nrow(df_intrants)))
cat(sprintf("Taux utilisation engrais inorganique : %.1f%%\n",
            100 * mean(df_intrants$inorg_bin,     na.rm = TRUE)))
cat(sprintf("Taux utilisation NPK                 : %.1f%%\n",
            100 * mean(df_intrants$npk_bin,       na.rm = TRUE)))
cat(sprintf("Taux utilisation Urée                : %.1f%%\n",
            100 * mean(df_intrants$uree_bin,      na.rm = TRUE)))
cat(sprintf("Taux utilisation organique           : %.1f%%\n",
            100 * mean(df_intrants$organique_bin, na.rm = TRUE)))

# ---------------------------------------------------------------------------- #
# 9. CONSTRUCTION DE LA BASE PRINCIPALE (NIVEAU PARCELLE-CULTURE)
# ---------------------------------------------------------------------------- #
cat("\n--- Construction de la base principale ---\n")

# Jointure cultures + superficies + intrants + poids
# NB : df_cultures_plot contient déjà zone/state/sector
#      -> on ne joint depuis df_sup, df_intrants et poids
#         que les variables supplémentaires pour éviter les doublons
df_main <- df_cultures_plot |>
  left_join(
    df_sup |> select(hhid, plotid, sup_ha, sup_gps_ha, sup_decl_ha, source_sup),
    by = c("hhid", "plotid")
  ) |>
  left_join(
    df_intrants |> select(hhid, plotid,
                          npk_bin, uree_bin, organique_bin,
                          pesticide_bin, inorg_bin, chimique_bin),
    by = c("hhid", "plotid")
  ) |>
  left_join(
    poids |> select(hhid, wt_wave4, cluster, strata),
    by = "hhid"
  ) |>
  mutate(
    milieu = factor(as.integer(sector),
                    levels = c(1, 2),
                    labels = c("Urbain", "Rural")),
    zone_f = factor(as.integer(zone),
                    levels = 1:6,
                    labels = c("North Central", "North East", "North West",
                               "South East",    "South South", "South West")),
    state_name = case_when(
      state == 1  ~ "Abia",        state == 2  ~ "Adamawa",
      state == 3  ~ "Akwa Ibom",   state == 4  ~ "Anambra",
      state == 5  ~ "Bauchi",      state == 6  ~ "Bayelsa",
      state == 7  ~ "Benue",       state == 8  ~ "Borno",
      state == 9  ~ "Cross River", state == 10 ~ "Delta",
      state == 11 ~ "Ebonyi",      state == 12 ~ "Edo",
      state == 13 ~ "Ekiti",       state == 14 ~ "Enugu",
      state == 15 ~ "Gombe",       state == 16 ~ "Imo",
      state == 17 ~ "Jigawa",      state == 18 ~ "Kaduna",
      state == 19 ~ "Kano",        state == 20 ~ "Katsina",
      state == 21 ~ "Kebbi",       state == 22 ~ "Kogi",
      state == 23 ~ "Kwara",       state == 24 ~ "Lagos",
      state == 25 ~ "Nasarawa",    state == 26 ~ "Niger",
      state == 27 ~ "Ogun",        state == 28 ~ "Ondo",
      state == 29 ~ "Osun",        state == 30 ~ "Oyo",
      state == 31 ~ "Plateau",     state == 32 ~ "Rivers",
      state == 33 ~ "Sokoto",      state == 34 ~ "Taraba",
      state == 35 ~ "Yobe",        state == 36 ~ "Zamfara",
      state == 37 ~ "FCT",
      TRUE        ~ NA_character_
    ),
    rendement_ha = if_else(
      !is.na(qty_kg) & !is.na(sup_ha) & sup_ha > 0,
      qty_kg / sup_ha,
      NA_real_
    )
  )

cat(sprintf("Base principale : %d obs x %d vars\n", nrow(df_main), ncol(df_main)))

# ---------------------------------------------------------------------------- #
# 10. ÉLIMINATION DES OUTLIERS DE RENDEMENTS — méthode IQR × 3
# ---------------------------------------------------------------------------- #
cat("\n--- Détection et élimination des outliers de rendement (IQR x 3) ---\n")

iqr_seuils <- df_main |>
  filter(!is.na(rendement_ha), harvested == 1) |>
  group_by(cropcode) |>
  summarise(
    Q1        = quantile(rendement_ha, 0.25),
    Q3        = quantile(rendement_ha, 0.75),
    IQR_v     = Q3 - Q1,
    borne_inf = Q1 - 3 * IQR_v,
    borne_sup = Q3 + 3 * IQR_v,
    .groups   = "drop"
  )

df_main <- df_main |>
  left_join(iqr_seuils |> select(cropcode, borne_inf, borne_sup),
            by = "cropcode") |>
  mutate(
    outlier_rdt        = !is.na(rendement_ha) &
                         (rendement_ha < borne_inf | rendement_ha > borne_sup),
    rendement_ha_clean = if_else(outlier_rdt, NA_real_, rendement_ha)
  )

n_out <- sum(df_main$outlier_rdt, na.rm = TRUE)
cat(sprintf("Outliers écartés : %d (%.1f%% des obs avec rendement)\n",
            n_out,
            100 * n_out / sum(!is.na(df_main$rendement_ha))))

# ---------------------------------------------------------------------------- #
# 11. BASE AGRÉGÉE NIVEAU MÉNAGE : DIVERSIFICATION CULTURALE
# ---------------------------------------------------------------------------- #
cat("\n--- Agrégation au niveau ménage ---\n")

df_menage <- df_main |>
  filter(harvested == 1) |>
  group_by(hhid) |>
  summarise(
    n_cultures     = n_distinct(cropcode),
    n_cereales     = sum(crop_type == "Céréale",     na.rm = TRUE),
    n_tubercules   = sum(crop_type == "Tubercule",   na.rm = TRUE),
    n_legumineuses = sum(crop_type == "Légumineuse", na.rm = TRUE),
    .groups = "drop"
  ) |>
  left_join(poids, by = "hhid") |>
  mutate(
    milieu = factor(as.integer(sector),
                    levels = c(1, 2),
                    labels = c("Urbain", "Rural")),
    zone_f = factor(as.integer(zone),
                    levels = 1:6,
                    labels = c("North Central", "North East", "North West",
                               "South East",    "South South", "South West"))
  )

cat(sprintf("Ménages avec données cultures : %d\n",  nrow(df_menage)))
cat(sprintf("Nb moyen cultures/ménage      : %.2f\n", mean(df_menage$n_cultures)))
cat(sprintf("Médiane cultures/ménage       : %.0f\n", median(df_menage$n_cultures)))

# ---------------------------------------------------------------------------- #
# 12. DIAGNOSTICS FINAUX
# ---------------------------------------------------------------------------- #
cat("\n--- Diagnostics finaux ---\n")

cat("NA sup_ha          :", sum(is.na(df_main$sup_ha)),          "/", nrow(df_main), "\n")
cat("NA qty_kg          :", sum(is.na(df_main$qty_kg)),          "/", nrow(df_main), "\n")
cat("NA rendement_ha    :", sum(is.na(df_main$rendement_ha)),    "/", nrow(df_main), "\n")
cat("NA wt_wave4        :", sum(is.na(df_main$wt_wave4)),        "/", nrow(df_main), "\n")

cat("\nTop 10 cultures (W4, pondéré) :\n")
df_main |>
  filter(harvested == 1, !is.na(crop_name)) |>
  count(crop_name, crop_type, wt = wt_wave4) |>
  arrange(desc(n)) |>
  head(10) |>
  print()

# ---------------------------------------------------------------------------- #
# 13. SAUVEGARDE
# ---------------------------------------------------------------------------- #
cat("\n--- Sauvegarde des données traitées ---\n")

saveRDS(df_main,       here("data", "processed", "df_main.rds"))
saveRDS(df_menage,     here("data", "processed", "df_menage.rds"))
saveRDS(df_intrants,   here("data", "processed", "df_intrants.rds"))
saveRDS(df_sup,        here("data", "processed", "df_sup.rds"))
saveRDS(df_production, here("data", "processed", "df_production.rds"))
saveRDS(crop_labels,   here("data", "processed", "crop_labels.rds"))
saveRDS(poids,         here("data", "processed", "poids.rds"))

cat(sprintf("\ndf_main       : %d obs x %d vars\n",   nrow(df_main),    ncol(df_main)))
cat(sprintf("df_menage     : %d ménages\n",            nrow(df_menage)))
cat(sprintf("df_intrants   : %d parcelles-intrants\n", nrow(df_intrants)))

cat("\n=== 01_preparation.R terminé avec succès ===\n")
