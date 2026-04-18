# =============================================================================
# 02_nettoyage.R — Nettoyage et préparation des données
# Projet  : Analyse des parcelles agricoles — Nigeria GHS Panel (W1–W4)
# Auteur  : ENSAE ISE 1 | 2025-2026
# Corrections : Q19 (NA + outliers explicites), déciles pondérés, multi-vagues
# =============================================================================


library(dplyr)
library(haven)
library(labelled)

# Charger les bases
secta_w4 <- read_dta(file.path(data_path, "secta_harvestw4.dta"))
secta1_w4 <- read_dta(file.path(data_path, "secta1_harvestw4.dta"))
sect11a1_w4 <- read_dta(file.path(data_path, "sect11a1_plantingw4.dta"))
sect11b1_w4 <- read_dta(file.path(data_path, "sect11b1_plantingw4.dta"))

library(DataExplorer)
create_report(secta1_w4)

library(skimr)
skim(secta1_w4)

library(summarytools)
dfSummary(secta1_w4)
view(dfSummary(secta1_w4))


# Construction de la base finale
parcelles_agricoles <- secta_w4 %>%
  select(hhid, zone, state, lga, sector, ea, cluster, strata, wt_wave4) %>%
  left_join(
    secta1_w4 %>% select(hhid, plotid, sa1q11, prefilled_gps_area),
    by = "hhid"
  ) %>%
  left_join(
    sect11a1_w4 %>% select(hhid, plotid, s11aq4aa, s11aq4b),
    by = c("hhid", "plotid")
  ) %>%
  left_join(
    sect11b1_w4 %>% select(hhid, plotid, s11b1q4),
    by = c("hhid", "plotid")
  )

# Renommer les variables
parcelles_agricoles <- parcelles_agricoles %>%
  rename(
    identifiant_menage = hhid,
    identifiant_parcelle = plotid,
    region = zone,
    etat = state,
    zone_residence = sector,
    lga = lga,
    ea = ea,
    cluster = cluster,
    strate = strata,
    poids_menage = wt_wave4,
    superficie_declaree = sa1q11,
    superficie_gps_m2 = prefilled_gps_area,
    superficie_parcelle = s11aq4aa,
    unite_superficie = s11aq4b,
    mode_acquisition = s11b1q4
  )

# Convertir unite_superficie en numeric si nécessaire
if(is.character(parcelles_agricoles$unite_superficie)) {
  parcelles_agricoles <- parcelles_agricoles %>%
    mutate(unite_superficie = as.numeric(unite_superficie))
}

# 1. AJOUTER LES FACTEURS DE CONVERSION PAR ZONE (D'APRÈS L'IMAGE)
parcelles_agricoles <- parcelles_agricoles %>%
  mutate(
    # Conversion en hectares pour les unités standard (en utilisant les valeurs numériques)
    superficie_ha_standard = case_when(
      unite_superficie == 6 ~ superficie_parcelle,  # 6 = Hectares
      unite_superficie == 5 ~ superficie_parcelle * 0.404686,  # 5 = Acres
      unite_superficie == 7 ~ superficie_parcelle / 10000,  # 7 = Square meters
      TRUE ~ NA_real_
    ),
    
    # Conversion des Heaps (unite_superficie == 1) par zone
    superficie_ha_heaps = case_when(
      unite_superficie == 1 & region == 1 ~ superficie_parcelle * 0.00012,
      unite_superficie == 1 & region == 2 ~ superficie_parcelle * 0.00016,
      unite_superficie == 1 & region == 3 ~ superficie_parcelle * 0.00011,
      unite_superficie == 1 & region == 4 ~ superficie_parcelle * 0.00019,
      unite_superficie == 1 & region == 5 ~ superficie_parcelle * 0.00021,
      unite_superficie == 1 & region == 6 ~ superficie_parcelle * 0.00012,
      TRUE ~ NA_real_
    ),
    
    # Conversion des Ridges (unite_superficie == 2) par zone
    superficie_ha_ridges = case_when(
      unite_superficie == 2 & region == 1 ~ superficie_parcelle * 0.0027,
      unite_superficie == 2 & region == 2 ~ superficie_parcelle * 0.004,
      unite_superficie == 2 & region == 3 ~ superficie_parcelle * 0.00494,
      unite_superficie == 2 & region == 4 ~ superficie_parcelle * 0.0023,
      unite_superficie == 2 & region == 5 ~ superficie_parcelle * 0.0023,
      unite_superficie == 2 & region == 6 ~ superficie_parcelle * 0.00001,
      TRUE ~ NA_real_
    ),
    
    # Conversion des Stands (unite_superficie == 3) par zone
    superficie_ha_stands = case_when(
      unite_superficie == 3 & region == 1 ~ superficie_parcelle * 0.00006,
      unite_superficie == 3 & region == 2 ~ superficie_parcelle * 0.00016,
      unite_superficie == 3 & region == 3 ~ superficie_parcelle * 0.00004,
      unite_superficie == 3 & region == 4 ~ superficie_parcelle * 0.00004,
      unite_superficie == 3 & region == 5 ~ superficie_parcelle * 0.00013,
      unite_superficie == 3 & region == 6 ~ superficie_parcelle * 0.00041,
      TRUE ~ NA_real_
    ),
    
    # SUPERFICIE FINALE EN HECTARES (toutes unités confondues)
    superficie_ha = coalesce(
      superficie_ha_standard,
      superficie_ha_heaps,
      superficie_ha_ridges,
      superficie_ha_stands
    ),
    
    # Conversion GPS en hectares
    superficie_gps_ha = superficie_gps_m2 / 10000,
    
    # Identifier les valeurs aberrantes
    outlier_superficie = case_when(
      is.na(superficie_ha) ~ NA,
      superficie_ha <= 0 ~ TRUE,
      superficie_ha > 500 ~ TRUE,
      TRUE ~ FALSE
    ),
    
    # Superficie corrigée sans outliers
    superficie_ha_propre = if_else(outlier_superficie == TRUE, NA_real_, superficie_ha)
  )

# 2. VÉRIFIER LES DOUBLONS
duplicates <- parcelles_agricoles %>%
  count(identifiant_menage, identifiant_parcelle) %>%
  filter(n > 1)

if(nrow(duplicates) > 0) {
  cat("Attention ! Doublons détectés :", nrow(duplicates), "\n")
  parcelles_agricoles <- parcelles_agricoles %>%
    distinct(identifiant_menage, identifiant_parcelle, .keep_all = TRUE)
}


# 4. RENOMMER LES MODALITÉS
parcelles_agricoles <- parcelles_agricoles %>%
  mutate(
    # Renommer les états
    etat = factor(etat,
                  levels = 1:37,
                  labels = c("Abia", "Adamawa", "Akwa Ibom", "Anambra", "Bauchi", "Bayelsa",
                             "Benue", "Borno", "Cross River", "Delta", "Ebonyi", "Edo", "Ekiti",
                             "Enugu", "FCT", "Gombe", "Imo", "Jigawa", "Kaduna", "Kano", "Katsina",
                             "Kebbi", "Kogi", "Kwara", "Lagos", "Nasarawa", "Niger", "Ogun",
                             "Ondo", "Osun", "Oyo", "Plateau", "Rivers", "Sokoto", "Taraba",
                             "Yobe", "Zamfara")
    ),
    
    # Renommer les régions
    region = factor(region,
                    levels = 1:6,
                    labels = c("North Central", "North East", "North West", 
                               "South East", "South South", "South West")
    ),
    
    # Renommer zone résidence
    zone_residence = factor(zone_residence,
                            levels = 1:2,
                            labels = c("Urbain", "Rural")
    ),
    
    # Renommer mode acquisition
    mode_acquisition = factor(mode_acquisition,
                              levels = 1:7,
                              labels = c("propriété pleine", "location", "prêt", "communautaire",
                                         "héritage", "métayage", "échange temporaire")
    )
  )

# 5. ENREGISTRER
saveRDS(parcelles_agricoles, file.path(processed_path, "parcelles_agricoles_w4.rds"))

write.csv(parcelles_agricoles, file.path(processed_path, "parcelles_agricoles_w4.csv"), row.names = FALSE)

# Vérification finale
cat("\n✅ Base prête pour les analyses !\n")
cat("Nombre de parcelles :", nrow(parcelles_agricoles), "\n")
cat("Nombre de ménages :", n_distinct(parcelles_agricoles$identifiant_menage), "\n")
cat("\nSuperficie moyenne (ha) :", mean(parcelles_agricoles$superficie_ha_propre, na.rm = TRUE), "\n")

