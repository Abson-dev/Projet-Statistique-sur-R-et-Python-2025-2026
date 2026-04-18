# =============================================================================
# TP5 - Cultures, Intrants et Rendements Agricoles
# Script 02 : Chargement et Nettoyage des Données
# =============================================================================
# Bases utilisées (W4 = 2018/2019) :
#   - secta3i_harvestw4   : cultures récoltées par parcelle
#   - secta11c2_harvestw4 : engrais (NPK, Urée, organique), pesticides, herbicides
#   - secta11c3_harvestw4 : intrants achetés (quantité, coût)
#   - sect11f_plantingw4  : cultures plantées + type de semence
#   - sect11e1_plantingw4 : acquisition des semences
#   - sect11a1_plantingw4 : superficie des parcelles
#   - secta_harvestw4     : identifiants ménage, rural/urbain, poids
# =============================================================================

message("--- Chargement des données ---")

# --- 1. Lecture des bases .dta -----------------------------------------------

# Cultures récoltées (W4)
df_recolte <- read_dta(file.path(DATA_DIR, "secta3i_harvestw4.dta")) %>%
  zap_labels() %>%
  rename_with(tolower)

# Engrais, pesticides, herbicides (W4)
df_intrants <- read_dta(file.path(DATA_DIR, "secta11c2_harvestw4.dta")) %>%
  zap_labels() %>%
  rename_with(tolower)

# Intrants achetés (W4)
df_intrants_achat <- read_dta(file.path(DATA_DIR, "secta11c3_harvestw4.dta")) %>%
  zap_labels() %>%
  rename_with(tolower)

# Cultures plantées + semences (W4 post-planting)
df_plante <- read_dta(file.path(DATA_DIR, "sect11f_plantingw4.dta")) %>%
  zap_labels() %>%
  rename_with(tolower)

# Acquisition semences (W4 post-planting)
df_semences <- read_dta(file.path(DATA_DIR, "sect11e1_plantingw4.dta")) %>%
  zap_labels() %>%
  rename_with(tolower)

# Superficie des parcelles (W4 post-planting)
df_superficie <- read_dta(file.path(DATA_DIR, "sect11a1_plantingw4.dta")) %>%
  zap_labels() %>%
  rename_with(tolower)

# Identifiants ménage + rural/urbain (W4)
df_menage <- read_dta(file.path(DATA_DIR, "secta_harvestw4.dta")) %>%
  zap_labels() %>%
  rename_with(tolower) %>%
  select(hhid, sector, state, zone, wt_wave4) %>%
  mutate(
    milieu = case_when(
      sector == 1 ~ "Urbain",
      sector == 2 ~ "Rural",
      TRUE        ~ NA_character_
    )
  )

message("Bases chargées avec succès.")

# --- 2. Nettoyage : superficie des parcelles ---------------------------------

df_superficie_clean <- df_superficie %>%
  select(hhid, plotid, s11aq4aa, s11aq4b, s11aq4c, s11b1q27) %>%
  rename(
    superficie_declaree = s11aq4aa,
    unite_superficie    = s11aq4b,
    superficie_gps_m2   = s11aq4c,
    plot_cultive        = s11b1q27
  ) %>%
  mutate(
    # Conversion GPS en hectares (1 ha = 10 000 m²)
    superficie_gps_ha = superficie_gps_m2 / 10000,
    
    # Conversion superficie déclarée en hectares selon l'unité
    # Codes unités questionnaire : 5=acres, 6=hectares, 7=m²
    superficie_declaree_ha = case_when(
      unite_superficie == 5 ~ superficie_declaree * 0.404686,  # acres -> ha
      unite_superficie == 6 ~ superficie_declaree,              # déjà en ha
      unite_superficie == 7 ~ superficie_declaree / 10000,      # m² -> ha
      TRUE                  ~ NA_real_
    ),
    
    # Superficie finale : GPS en priorité, sinon déclarée convertie
    superficie_ha = if_else(
      !is.na(superficie_gps_ha) & superficie_gps_ha > 0,
      superficie_gps_ha,
      superficie_declaree_ha
    ),
    
    # Nettoyage valeurs aberrantes (< 0 ou > 500 ha)
    superficie_ha = if_else(
      superficie_ha <= 0 | superficie_ha > 500,
      NA_real_,
      superficie_ha
    )
  ) %>%
  filter(plot_cultive == 1 | is.na(plot_cultive))  # garder parcelles cultivées

# --- 3. Nettoyage : cultures récoltées ---------------------------------------

# Codes cultures selon le questionnaire PDF
codes_cultures <- tribble(
  ~cropcode, ~nom_culture,      ~type_culture,
  1010,      "Niébé/Haricot",   "Légumineuse",
  1020,      "Manioc",          "Tubercule",
  1040,      "Colocase",        "Tubercule",
  1050,      "Coton",           "Culture de rente",
  1060,      "Arachide",        "Légumineuse",
  1070,      "Sorgho",          "Céréale",
  1080,      "Maïs",            "Céréale",
  1090,      "Egusi/Melon",     "Autre",
  1100,      "Mil",             "Céréale",
  1110,      "Riz",             "Céréale",
  1121,      "Igname blanche",  "Tubercule",
  1122,      "Igname jaune",    "Tubercule",
  1123,      "Igname d'eau",    "Tubercule",
  1124,      "Igname 3 feuilles","Tubercule",
  2020,      "Bambara",         "Légumineuse",
  2030,      "Banane",          "Fruit",
  2160,      "Ananas",          "Fruit",
  2170,      "Plantain",        "Fruit",
  2180,      "Pomme de terre",  "Tubercule",
  2181,      "Patate douce",    "Tubercule",
  2220,      "Soja",            "Légumineuse",
  2230,      "Canne à sucre",   "Culture de rente",
  2260,      "Tomate",          "Légume",
  2280,      "Blé",             "Céréale",
  3180,      "Palmier à huile", "Culture de rente"
)

df_recolte_clean <- df_recolte %>%
  select(hhid, plotid, cropcode,
         sa3iq3,      # récolté ?
         sa3iq6i,     # quantité récoltée
         sa3iq6ii,    # unité
         sa3iq6_conv, # facteur conversion kg
         sa3iq5,      # % parcelle récoltée
         sa3iq6a,     # valeur estimée
         state, sector, zone) %>%
  rename(
    recolte       = sa3iq3,
    qte_recoltee  = sa3iq6i,
    unite_recolte = sa3iq6ii,
    conv_kg       = sa3iq6_conv,
    pct_recolte   = sa3iq5,
    valeur_recolte = sa3iq6a
  ) %>%
  filter(recolte == 1) %>%   # garder seulement les cultures effectivement récoltées
  mutate(
    # Conversion en kg
    qte_kg = if_else(!is.na(conv_kg) & conv_kg > 0,
                     qte_recoltee * conv_kg,
                     NA_real_)
  ) %>%
  left_join(codes_cultures, by = "cropcode") %>%
  left_join(df_menage, by = "hhid")

# --- 4. Nettoyage : intrants (engrais, pesticides) ---------------------------

df_intrants_clean <- df_intrants %>%
  select(hhid, plotid,
         # Engrais inorganiques
         s11dq1a,      # utilisé engrais inorganique ?
         s11c2q36_1,   # NPK ?
         s11c2q36_2,   # Urée ?
         s11c2q37a, s11c2q37a_conv,   # quantité NPK + conversion
         s11c2q38a, s11c2q38a_conv,   # quantité Urée + conversion
         # Engrais organique
         s11dq36,      # utilisé engrais organique ?
         s11dq37a,     # quantité organique
         # Pesticides
         s11c2q1,      # utilisé pesticide ?
         s11c2q2a, s11c2q2_conv,      # quantité pesticide + conversion
         # Herbicides
         s11c2q10,     # utilisé herbicide ?
         s11c2q11a, s11c2q11_conv,    # quantité herbicide + conversion
         # Traction animale
         s11c2q19     # traction animale ?
  ) %>%
  rename(
    engrais_inorg  = s11dq1a,
    npk            = s11c2q36_1,
    uree           = s11c2q36_2,
    qte_npk        = s11c2q37a,
    conv_npk       = s11c2q37a_conv,
    qte_uree       = s11c2q38a,
    conv_uree      = s11c2q38a_conv,
    engrais_org    = s11dq36,
    qte_org        = s11dq37a,
    pesticide      = s11c2q1,
    qte_pesticide  = s11c2q2a,
    conv_pest      = s11c2q2_conv,
    herbicide      = s11c2q10,
    qte_herbicide  = s11c2q11a,
    conv_herb      = s11c2q11_conv,
    traction_anim  = s11c2q19
  ) %>%
  mutate(
    across(c(engrais_inorg, npk, uree, engrais_org,
             pesticide, herbicide, traction_anim),
           ~ case_when(. == 1 ~ 1L, . == 2 ~ 0L, TRUE ~ NA_integer_)),
    # Quantités en kg
    qte_npk_kg       = if_else(!is.na(conv_npk)  & conv_npk  > 0, qte_npk  * conv_npk,  NA_real_),
    qte_uree_kg      = if_else(!is.na(conv_uree) & conv_uree > 0, qte_uree * conv_uree, NA_real_),
    qte_pest_kg      = if_else(!is.na(conv_pest) & conv_pest > 0, qte_pesticide * conv_pest, NA_real_),
    qte_herb_kg      = if_else(!is.na(conv_herb) & conv_herb > 0, qte_herbicide * conv_herb, NA_real_)
  ) %>%
  left_join(df_menage %>% select(hhid, wt_wave4, milieu, state, zone), by = "hhid") %>%
  rename_with(~ sub("\\.x$", "", .), ends_with(".x")) %>%
  select(-ends_with(".y"))

# --- 5. Nettoyage : cultures plantées + semences -----------------------------

df_plante_clean <- df_plante %>%
  select(hhid, plotid, cropcode,
         s11fq0,    # type : culture de champ (1) ou arbre (2)
         s11fq3b,   # semence améliorée (1) ou traditionnelle (2)
         s11fq3aa,  # certifiée ?
         s11fq1,    # % parcelle plantée
         s11fq2a,   # purestand (1) ou intercropping (2)
         sector, state, zone) %>%
  rename(
    type_culture_champ = s11fq0,
    type_semence       = s11fq3b,
    semence_certifiee  = s11fq3aa,
    pct_plante         = s11fq1,
    methode_culture    = s11fq2a
  ) %>%
  filter(type_culture_champ == 1) %>%   # garder uniquement cultures de champ
  mutate(
    semence_amelioree = case_when(
      type_semence == 1 ~ "Améliorée",
      type_semence == 2 ~ "Traditionnelle",
      TRUE              ~ NA_character_
    ),
    milieu = case_when(sector == 1 ~ "Urbain", sector == 2 ~ "Rural", TRUE ~ NA_character_)
  ) %>%
  left_join(codes_cultures, by = "cropcode") %>%
  left_join(df_menage %>% select(hhid, wt_wave4), by = "hhid")

message("=== Données nettoyées avec succès ===")
message("Observations - Récoltes    : ", nrow(df_recolte_clean))
message("Observations - Intrants    : ", nrow(df_intrants_clean))
message("Observations - Plantations : ", nrow(df_plante_clean))