library(haven)
library(dplyr)
library(forcats)
library(survey)

# chemins vers les trois fichiers bruts
path_sect1  <- "data/raw/sect1_harvestw4.dta"
path_sect2  <- "data/raw/sect2_harvestw4.dta"
path_secta  <- "data/raw/secta_harvestw4.dta"

# Chargement des fichiers
cat("Chargement de sect1_harvestw4.dta ...\n")
sect1 <- read_dta(path_sect1)

cat("Chargement de sect2_harvestw4.dta ...\n")
sect2 <- read_dta(path_sect2)

cat("Chargement de secta_harvestw4.dta ...\n")
secta <- read_dta(path_secta)

# Extraction des poids de sondage depuis secta
# Variables : wt_wave4 (poids Wave 4), cluster (UPE), strata (strate)
wgt_data <- secta %>%
  select(hhid, wt_wave4, cluster, strata)
cat("  Ménages avec poids :", nrow(wgt_data), "\n")

# Jointure sect2 + sect1 (sexe, âge)
cat("\nJointure sect2 x sect1 sur hhid + indiv ...\n")

sect1_small <- sect1 %>%
  select(hhid, indiv,
         sexe = s1q2,   # 1=Male, 2=Female
         age  = s1q4    # âge en années
  )

df_educ <- sect2 %>%
  left_join(sect1_small, by = c("hhid", "indiv"))

# Valeurs manquantes du niveau d'éducation
cat("\nInspection des valeurs manquantes :\n")
cat("  s2aq15 (niveau atteint) manquants :", sum(is.na(df_educ$s2aq15)), "\n")
cat("  s2aq9  (classe actuelle) manquants :", sum(is.na(df_educ$s2aq9)),  "\n")
cat("  s2aq10 (diplôme obtenu)  manquants :", sum(is.na(df_educ$s2aq10)), "\n")
cat("  s2aq13 (scolarisé ?)     manquants :", sum(is.na(df_educ$s2aq13)), "\n")
cat("  age                      manquants :", sum(is.na(df_educ$age)),    "\n")

# Construction de la variable niveau_educ (5 catégories)
construire_niveau_educ <- function(s2aq13, s2aq15, s2aq9, s2aq10) {
  niveau_brut <- ifelse(!is.na(s2aq15), s2aq15,
                        ifelse(!is.na(s2aq9),  s2aq9, NA_real_))
  
  dplyr::case_when(
    s2aq13 == 2 | (is.na(niveau_brut) & is.na(s2aq10)) ~ "Aucun",
    niveau_brut %in% c(34,35,41,43,61,322,411,412,421,422,423,424) |
      s2aq10 %in% c(8,9,11,12) ~ "Tertiaire",
    niveau_brut %in% c(24,25,26,27,28,31,33,321) |
      s2aq10 %in% c(6,7,41,42) ~ "Senior Secondary",
    niveau_brut %in% c(21,22,23) | s2aq10 == 5 ~ "Junior Secondary",
    niveau_brut %in% c(11,12,13,14,15,16) |
      s2aq10 %in% c(2,3) ~ "Primaire",
    niveau_brut %in% c(1,2,3,51,52) ~ "Aucun",
    TRUE ~ "Aucun"
  )
}

df_educ <- df_educ %>%
  mutate(
    niveau_educ = construire_niveau_educ(
      as.numeric(s2aq13),
      as.numeric(s2aq15),
      as.numeric(s2aq9),
      as.numeric(s2aq10)
    ),
    niveau_educ = factor(niveau_educ,
                         levels = c("Aucun","Primaire","Junior Secondary",
                                    "Senior Secondary","Tertiaire"),
                         ordered = TRUE),
    sexe_label = factor(ifelse(as.numeric(sexe) == 1, "Homme", "Femme")),
    zone_label = factor(ifelse(as.numeric(sector) == 1, "Urbain", "Rural")),
    groupe_age = cut(age,
                     breaks = c(18, 30, 45, 60, Inf),
                     labels = c("18-30","31-45","46-60","60+"),
                     right  = TRUE, include.lowest = FALSE),
    state_num  = as.numeric(state)
  )

# table de correspondance numéro et nom d'État
state_labels <- c(
  "1"="Abia","2"="Adamawa","3"="Akwa Ibom","4"="Anambra","5"="Bauchi",
  "6"="Bayelsa","7"="Benue","8"="Borno","9"="Cross River","10"="Delta",
  "11"="Ebonyi","12"="Edo","13"="Ekiti","14"="Enugu","15"="Gombe",
  "16"="Imo","17"="Jigawa","18"="Kaduna","19"="Kano","20"="Katsina",
  "21"="Kebbi","22"="Kogi","23"="Kwara","24"="Lagos","25"="Nasarawa",
  "26"="Niger","27"="Ogun","28"="Ondo","29"="Osun","30"="Oyo",
  "31"="Plateau","32"="Rivers","33"="Sokoto","34"="Taraba","35"="Yobe",
  "36"="Zamfara","37"="FCT"
)

df_educ <- df_educ %>%
  mutate(state_name = state_labels[as.character(state_num)])

# Jointure des poids sur les individus via hhid
df_educ <- df_educ %>%
  left_join(wgt_data, by = "hhid")

n_miss_wgt <- sum(is.na(df_educ$wt_wave4))
cat("  Individus sans poids :", n_miss_wgt, "\n")

# Sous-ensemble adultes (18+)
df_adultes <- df_educ %>%
  filter(!is.na(age), age >= 18, !is.na(sexe_label), !is.na(niveau_educ),
         !is.na(wt_wave4))

cat("\nDimensions df_educ    :", nrow(df_educ),    "x", ncol(df_educ),    "\n")
cat("Dimensions df_adultes :", nrow(df_adultes), "x", ncol(df_adultes), "\n")

# Données pour taux de scolarisation 6-17 ans
df_scol <- df_educ %>%
  filter(!is.na(age), age >= 6, age <= 17, !is.na(wt_wave4)) %>%
  mutate(
    scolarise = as.numeric(s2aq13) == 1,
    zone_scol = as.character(zone_label)
  ) %>%
  filter(!is.na(zone_scol), !is.na(scolarise))

cat("Données scolarisation 6-17 ans :", nrow(df_scol), "individus\n")

# Plans de sondage
# plan individus adultes (18+)
plan_adultes <- svydesign(
  ids     = ~cluster,
  strata  = ~strata,
  weights = ~wt_wave4,
  data    = df_adultes,
  nest    = TRUE
)

# plan enfants scolarisation (6-17 ans)
plan_scol <- svydesign(
  ids     = ~cluster,
  strata  = ~strata,
  weights = ~wt_wave4,
  data    = df_scol,
  nest    = TRUE
)

# Sauvegarde
dir.create("data/processed", showWarnings = FALSE, recursive = TRUE)

saveRDS(df_educ,      file = "data/processed/df_educ.rds")
saveRDS(df_adultes,   file = "data/processed/df_adultes.rds")
saveRDS(df_scol,      file = "data/processed/df_scol.rds")
saveRDS(plan_adultes, file = "data/processed/plan_adultes.rds")
saveRDS(plan_scol,    file = "data/processed/plan_scol.rds")

cat("\n✓ Données et plans de sondage sauvegardés dans data/processed/\n")
