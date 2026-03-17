library(haven)
library(dplyr)
library(naniar)

# chemin vers le fichier brut
path_data <- "data/raw/sect1_harvestw4.dta"

# Chargement de la base
data <- read_dta(path_data)
cat("  Dimensions :", nrow(data), "x", ncol(data), "\n")

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

# Base ménages
taille_menage <- data %>%
  group_by(hhid) %>%
  summarise(taille_menage = n(), .groups = "drop")

base_menage <- data %>%
  group_by(hhid) %>%
  summarise(
    taille   = n(),
    secteur  = first(secteur),
    zone_geo = first(zone_geo),
    .groups  = "drop"
  )

cat("\n  Ménages total  :", nrow(base_menage), "\n")
cat("  Ménages Urbain :", sum(base_menage$secteur == "Urbain"), "\n")
cat("  Ménages Rural  :", sum(base_menage$secteur == "Rural"),  "\n")

# Données individuelles enrichies
data_indiv <- data %>%
  left_join(taille_menage, by = "hhid") %>%
  filter(!is.na(secteur))

# Sauvegarde 
dir.create("data/processed", showWarnings = FALSE, recursive = TRUE)
saveRDS(data,        "data/processed/data_brute.rds")
saveRDS(data_indiv,  "data/processed/data_indiv.rds")
saveRDS(base_menage, "data/processed/base_menage.rds")

cat("\n Données sauvegardées dans data/processed/\n")