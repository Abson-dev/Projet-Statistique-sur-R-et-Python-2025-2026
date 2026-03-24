library(haven)
library(dplyr)
library(naniar)
library(survey)

# chemins vers les fichiers bruts
path_data <- "data/raw/sect1_harvestw4.dta"
path_wgt  <- "data/raw/secta_harvestw4.dta"

# Chargement de la base individus
data <- read_dta(path_data)
cat("  Dimensions sect1 :", nrow(data), "x", ncol(data), "\n")

# Chargement de la base pondérations (secta_harvestw4)
# Variables clés : hhid, wt_wave4, strata, cluster (unité primaire de sondage)
wgt_data <- read_dta(path_wgt) %>%
  select(hhid, wt_wave4, strata, cluster)
cat("  Dimensions secta (poids) :", nrow(wgt_data), "x", ncol(wgt_data), "\n")

# Identifiant unique ménage × individu
data$hhid_indiv <- paste(data$hhid, data$indiv, sep = "_")

# Identification des doublons
doublons <- data[duplicated(data$hhid_indiv), ]
cat("  Doublons hhid×indiv :", nrow(doublons), "\n")

# Valeurs manquantes
n_miss_total <- n_miss(data)
cat("  Total valeurs manquantes :", n_miss_total, "\n")

# Recodage des variables numériques en facteurs
data <- data %>%
  mutate(
    age     = as.numeric(s1q4),
    sexe    = factor(as.numeric(s1q2), levels = c(1, 2),
                     labels = c("Homme", "Femme")),
    secteur = factor(as.numeric(sector), levels = c(1, 2),
                     labels = c("Urbain", "Rural")),
    zone_geo = factor(as.numeric(zone), levels = 1:6,
                      labels = c("North Central", "North East", "North West",
                                 "South East", "South South", "South West")),
    # lien de parenté ramené à 4 catégories principales
    s1q3_num = as.numeric(as.character(s1q3)),
    lien_parente = case_when(
      s1q3_num == 1  ~ "Chef de ménage",
      s1q3_num == 2  ~ "Conjoint(e)",
      s1q3_num == 3  ~ "Enfant",
      TRUE           ~ "Autre"
    ),
    lien_parente = factor(lien_parente,
                          levels = c("Chef de ménage","Conjoint(e)","Enfant","Autre")),
    # découpage en groupes d'âge quinquennaux
    grp_age = cut(age,
                  breaks = c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,Inf),
                  labels = c("0-4","5-9","10-14","15-19","20-24","25-29","30-34",
                             "35-39","40-44","45-49","50-54","55-59","60-64",
                             "65-69","70-74","75-79","80+"),
                  right  = FALSE, include.lowest = TRUE)
  )

# Jointure des pondérations ménage → individus
# wt_wave4 est un poids ménage ; chaque individu hérite du poids de son ménage
data <- data %>%
  left_join(wgt_data, by = "hhid")

n_miss_wgt <- sum(is.na(data$wt_wave4))
cat("  Individus sans poids :", n_miss_wgt, "\n")

# Base ménages enrichie des poids
taille_menage <- data %>%
  group_by(hhid) %>%
  summarise(taille_menage = n(), .groups = "drop")

base_menage <- data %>%
  group_by(hhid) %>%
  summarise(
    taille   = n(),
    secteur  = first(secteur),
    zone_geo = first(zone_geo),
    wt_wave4 = first(wt_wave4),   # poids identique pour tous les membres du ménage
    strata   = first(strata),
    cluster  = first(cluster),
    .groups  = "drop"
  )

cat("\n  Ménages total  :", nrow(base_menage), "\n")
cat("  Ménages Urbain :", sum(base_menage$secteur == "Urbain"), "\n")
cat("  Ménages Rural  :", sum(base_menage$secteur == "Rural"),  "\n")

# Données individuelles enrichies (pour gtsummary)
data_indiv <- data %>%
  left_join(taille_menage, by = "hhid") %>%
  filter(!is.na(secteur))

# Objet plan de sondage individus — strate + cluster + poids Wave 4
# (utilisé pour les estimations pondérées dans 02_analyses.R)
plan_indiv <- svydesign(
  ids     = ~cluster,
  strata  = ~strata,
  weights = ~wt_wave4,
  data    = data %>% filter(!is.na(wt_wave4)),
  nest    = TRUE
)

# Objet plan de sondage ménages
plan_menage <- svydesign(
  ids     = ~cluster,
  strata  = ~strata,
  weights = ~wt_wave4,
  data    = base_menage %>% filter(!is.na(wt_wave4)),
  nest    = TRUE
)

# Sauvegarde
dir.create("data/processed", showWarnings = FALSE, recursive = TRUE)
saveRDS(data,        "data/processed/data_brute.rds")
saveRDS(data_indiv,  "data/processed/data_indiv.rds")
saveRDS(base_menage, "data/processed/base_menage.rds")
saveRDS(plan_indiv,  "data/processed/plan_indiv.rds")
saveRDS(plan_menage, "data/processed/plan_menage.rds")

cat("\n Données et plan de sondage sauvegardés dans data/processed/\n")