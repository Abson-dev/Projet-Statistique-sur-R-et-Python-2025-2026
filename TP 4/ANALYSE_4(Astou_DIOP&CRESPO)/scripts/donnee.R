# =============================================================================
# TP4 - Script 02 : Chargement  des données
# =============================================================================
# Bases utilisées (W4 = 2018/2019) :
#   - sect11a1_plantingw4  : roster parcelles, superficie déclarée + GPS
#   - sect11b1_plantingw4  : tenure foncière, sol, pente, irrigation
#   - secta_harvestw4      : identifiants ménage, rural/urbain, poids
#   - secta1_harvestw4     : données parcelles post-récolte (GPS prefilled)
# =============================================================================

message("--- Chargement des données TP4 ---")

# --- 1. Lecture des bases ----------------------------------------------------
DATA_DIR <- here("data", "raw")
# Roster parcelles + superficie (post-planting W4)
df_parcelles <- sect11a1_clean %>%
  zap_labels() %>%
  rename_with(tolower)

# Tenure foncière + caractéristiques parcelles (post-planting W4)
df_tenure <- read_dta(file.path(DATA_DIR, "sect11b1_plantingw4.dta")) %>%
  zap_labels() %>%
  rename_with(tolower)

# Identifiants ménage + rural/urbain (post-harvest W4)
df_menage <- read_dta(file.path(DATA_DIR, "secta_harvestw4.dta")) %>%
  zap_labels() %>%
  rename_with(tolower) %>%
  select(hhid, sector, state, zone, wt_wave4) %>%
  mutate(
    milieu   = case_when(sector == 1 ~ "Urbain", sector == 2 ~ "Rural",
                         TRUE ~ NA_character_),
    nom_etat = etats_nigeria[as.character(state)],
    nom_etat = if_else(is.na(nom_etat), paste0("État ", state), nom_etat)
  )

# Données GPS post-récolte W4 (prefilled_gps_area)
df_gps <- read_dta(here("data", "raw", "Base_vague", "NGA_2018_GHSP-W4_v03_M_Stata12", "secta1_harvestw4.dta")) %>%
  zap_labels() %>%
  rename_with(tolower) %>%
  select(hhid, plotid, prefilled_gps_area, sa1q9, sa1q11) %>%
  rename(
    gps_mesure_ph    = sa1q9,           # mesuré par GPS post-récolte ?
    superficie_ph    = sa1q11           # superficie déclarée post-récolte
  )

message("Bases chargées.")

# --- 2. Nettoyage : superficie des parcelles ---------------------------------

# Codes unités superficie (questionnaire PDF section 11A) :
# 1=Heaps, 2=Ridges, 3=Stands, 5=Acres, 6=Hectares, 7=Square meters

# Facteurs de conversion zone-spécifiques vers hectares (source : World Bank GHS)
# Les unités traditionnelles (Heaps, Ridges, Stands) varient selon la zone géographique
# zone : 1=North Central, 2=North East, 3=North West,
#         4=South East,   5=South South, 6=South West
facteurs_conversion <- tribble(
  ~zone, ~heaps,   ~ridges,  ~stands,
  1,     0.00012,  0.0027,   0.00006,   # North Central
  2,     0.00016,  0.0040,   0.00016,   # North East
  3,     0.00011,  0.00494,  0.00004,   # North West
  4,     0.00019,  0.0023,   0.00004,   # South East
  5,     0.00021,  0.0023,   0.00013,   # South South
  6,     0.00012,  0.00001,  0.00041    # South West
)

df_sup_clean <- df_parcelles %>%
  select(hhid, plotid, s11aq4a, s11aq4aa, s11aq4b, s11aq4c, s11b1q27,
         sector, state, zone) %>%
  rename(
    gps_mesure       = s11aq4a,      # mesuré par GPS ?
    sup_declaree     = s11aq4aa,     # superficie déclarée (nombre)
    unite_sup        = s11aq4b,      # unité
    sup_gps_m2       = s11aq4c,      # GPS en m²
    plot_cultive     = s11b1q27      # parcelle cultivée ?
  ) %>%
  # Joindre GPS post-récolte
  left_join(df_gps, by = c("hhid", "plotid")) %>%
  # Joindre les facteurs de conversion selon la zone
  left_join(facteurs_conversion, by = "zone") %>%
  mutate(
    # GPS W4 post-planting en hectares
    # Mesure physique faite par l'enquêteur lors de la visite post-planting
    # Priorité 1 : mesure directe, la plus fiable
    sup_gps_ha = sup_gps_m2 / 10000,
    
    # GPS W4 post-récolte en hectares (prefilled depuis visite précédente)
    # Priorité 2 : substitut si GPS post-planting manquant
    sup_gps_ph_ha = prefilled_gps_area / 10000,
    
    # Superficie déclarée convertie en hectares selon unité et zone
    # Priorité 3 : déclaration de l'agriculteur, moins fiable (biais de surestimation)
    sup_declaree_ha = case_when(
      unite_sup == 1 ~ sup_declaree * heaps,       # Heaps  -> ha (zone-spécifique)
      unite_sup == 2 ~ sup_declaree * ridges,      # Ridges -> ha (zone-spécifique)
      unite_sup == 3 ~ sup_declaree * stands,      # Stands -> ha (zone-spécifique)
      unite_sup == 5 ~ sup_declaree * 0.404686,    # Acres  -> ha (standard)
      unite_sup == 6 ~ sup_declaree,               # Hectares (pas de conversion)
      unite_sup == 7 ~ sup_declaree / 10000,       # m²     -> ha
      TRUE           ~ NA_real_                    # unité inconnue
    ),
    
    # Superficie finale : GPS post-planting > GPS post-récolte > déclarée
    superficie_ha = case_when(
      !is.na(sup_gps_ha)    & sup_gps_ha    > 0 ~ sup_gps_ha,
      !is.na(sup_gps_ph_ha) & sup_gps_ph_ha > 0 ~ sup_gps_ph_ha,
      !is.na(sup_declaree_ha)                    ~ sup_declaree_ha,
      TRUE                                        ~ NA_real_
    ),
    
    # Valeurs aberrantes -> NA
    superficie_ha = if_else(superficie_ha <= 0 | superficie_ha > 500,
                            NA_real_, superficie_ha),
    
    # Milieu et État
    milieu   = case_when(sector == 1 ~ "Urbain", sector == 2 ~ "Rural",
                         TRUE ~ NA_character_),
    nom_etat = etats_nigeria[as.character(state)],
    nom_etat = if_else(is.na(nom_etat), paste0("État ", state), nom_etat)
  )

message("Valeurs manquantes superficie : ",
        sum(is.na(df_sup_clean$superficie_ha)))
message("Valeurs aberrantes écartées   : ",
        sum(df_parcelles$s11aq4aa > 500, na.rm = TRUE))

# --- 3. Superficie agrégée par ménage ----------------------------------------

df_sup_menage <- df_sup_clean %>%
  filter(!is.na(superficie_ha)) %>%
  group_by(hhid) %>%
  summarise(
    sup_totale_ha  = sum(superficie_ha, na.rm = TRUE),
    nb_parcelles   = n(),
    sup_moy_ha     = mean(superficie_ha, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(df_menage, by = "hhid")

# --- 4. Nettoyage : tenure foncière ------------------------------------------

# s11b1q4 : mode d'acquisition
# 1=Achat, 2=Location cash/nature, 3=Usage gratuit, 4=Distribution communauté,
# 5=Héritage familial, 6=Métayage, 7=Échange temporaire

df_tenure_clean <- df_tenure %>%
  select(hhid, plotid, s11b1q4, s11b1q44, s11b1q45, s11b1q46,
         s11b1q39, s11b1q27, sector, state, zone) %>%
  rename(
    mode_acquisition = s11b1q4,
    type_sol         = s11b1q44,
    qualite_sol      = s11b1q45,
    pente            = s11b1q46,
    irrigation       = s11b1q39,
    plot_cultive     = s11b1q27
  ) %>%
  mutate(
    tenure_label = case_when(
      mode_acquisition == 1 ~ "Achat",
      mode_acquisition == 2 ~ "Location",
      mode_acquisition == 3 ~ "Usage gratuit",
      mode_acquisition == 4 ~ "Distribution communauté",
      mode_acquisition == 5 ~ "Héritage familial",
      mode_acquisition == 6 ~ "Métayage",
      mode_acquisition == 7 ~ "Échange temporaire",
      TRUE                  ~ NA_character_
    ),
    milieu   = case_when(sector == 1 ~ "Urbain", sector == 2 ~ "Rural",
                         TRUE ~ NA_character_),
    nom_etat = etats_nigeria[as.character(state)],
    nom_etat = if_else(is.na(nom_etat), paste0("État ", state), nom_etat)
  ) %>%
  left_join(df_menage %>% select(hhid, wt_wave4), by = "hhid")

# Jointure tenure + superficie
df_tenure_sup <- df_tenure_clean %>%
  left_join(df_sup_clean %>% select(hhid, plotid, superficie_ha),
            by = c("hhid", "plotid"))

message("=== Données TP4 nettoyées avec succès ===")
message("Parcelles total      : ", nrow(df_sup_clean))
message("Ménages avec données : ", nrow(df_sup_menage))
message("Parcelles tenure     : ", nrow(df_tenure_clean))