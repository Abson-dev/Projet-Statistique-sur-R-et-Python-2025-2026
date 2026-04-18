# ==================== 1. Chargement ====================
library(haven)
library(survey)      # ou srvyr
library(dplyr)

# Fichiers clés (dans votre dossier dézippé)
geo     <- read_dta("NGA_2018_GHSP-W4_v03_M_Stata12/nga_householdgeovars_y4.dta")
secta   <- read_dta("NGA_2018_GHSP-W4_v03_M_Stata12/secta_harvestw4.dta")   # ou plantingw4 selon votre analyse

# ==================== 2. Préparation du data.frame design ====================
data_design <- geo %>%
  left_join(secta %>% select(hhid, wt_wave4), by = "hhid") %>%
  mutate(
    stratum = zone,                    # stratification officielle = zone géopolitique
    # OU si vous voulez plus fin : stratum = interaction(zone, sector, drop = TRUE)
    cluster = ea,                      # PSU officiel = Enumeration Area
    weight  = wt_wave4                 # poids cross-sectionnel officiel
  ) %>%
  filter(!is.na(stratum) & !is.na(cluster) & !is.na(weight) & weight > 0)

# Vérification (doit donner ~ 4 976 lignes et 519 clusters)
nrow(data_design)
data_design %>% summarise(n_ea = n_distinct(cluster))

# ==================== 3. Création du plan de sondage OFFICIEL ====================
design <- svydesign(
  id      = ~ cluster,     # EA = PSU
  strata  = ~ stratum,     # zone (ou zone x sector)
  weights = ~ weight,      # wt_wave4
  data    = data_design,
  nest    = TRUE           # obligatoire pour ce type de plan cluster
)

# OU avec srvyr (style tidyverse)
library(srvyr)
design_srvyr <- as_survey_design(data_design, 
                                 id = cluster, 
                                 strata = stratum, 
                                 weights = weight, 
                                 nest = TRUE)

# ==================== 4. Vérification du plan ====================
summary(design)
# Doit afficher :
# Stratified 2-Stage Cluster Sampling design
# Avec ~6-36 strates et 519 PSUs (et non 1000 !)

print(design)