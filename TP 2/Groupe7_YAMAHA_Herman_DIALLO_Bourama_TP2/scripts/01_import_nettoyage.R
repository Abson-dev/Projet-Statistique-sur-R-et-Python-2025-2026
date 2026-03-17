# =============================================================================
# Script 01 : Import et nettoyage des données
# Thème 2 – Éducation et alphabétisation des membres des ménages
# Enquête GHS Nigeria 2018 (Wave 4)
# Réalisé par : Herman YAMAHA, Bourama DIALLO 
# =============================================================================

library(haven)
library(dplyr)
library(forcats)

# Localisation des fichiers bruts
chemin_s1  <- "data/raw/sect1_harvestw4.dta"
chemin_s2  <- "data/raw/sect2_harvestw4.dta"
chemin_sa  <- "data/raw/secta_harvestw4.dta"

# ---- 0. Téléchargement des données depuis GitHub (si absentes) ----

cat("Vérification / téléchargement des données (GitHub)...\n")

options(timeout = 300)

base_url <- paste0(
  "https://raw.githubusercontent.com/",
  "Herman-YAMAHA/NYHP/",
  "001fdd702f41b836d5c03d7682b819593842e7c5/",
  "TP2_raw/"
)

fichiers <- c(
  "sect1_harvestw4.dta",
  "sect2_harvestw4.dta",
  "secta_harvestw4.dta"
)

dir.create("data/raw", recursive = TRUE, showWarnings = FALSE)

for (f in fichiers) {
  dest <- file.path("data/raw", f)
  
  if (!file.exists(dest)) {
    url <- paste0(base_url, f)
    cat("  Téléchargement :", f, "...\n")
    
    tryCatch(
      {
        download.file(url, destfile = dest, mode = "wb")
        cat("  ✓", f, "téléchargé.\n")
      },
      error = function(e) {
        cat("  ✗ Erreur pour", f, ":", conditionMessage(e), "\n")
      }
    )
    
  } else {
    cat("  ✓", f, "déjà présent.\n")
  }
}

cat("Téléchargement terminé.\n\n")

# ---- 1. Lecture des fichiers Stata ----
cat("Lecture de sect1_harvestw4.dta ...\n")
sect1 <- read_dta(chemin_s1)

cat("Lecture de sect2_harvestw4.dta ...\n")
sect2 <- read_dta(chemin_s2)

cat("Lecture de secta_harvestw4.dta ...\n")
secta <- read_dta(chemin_sa)

# ---- 2. Fusion sect2 + sect1 (caractéristiques démographiques) ----
# Remarque : sect2 contient déjà la variable sector (milieu de résidence)
cat("\nFusion de sect2 et sect1 via hhid + indiv ...\n")

sect1_reduit <- sect1 %>%
  select(hhid, indiv,
         sexe = s1q2,   # 1=Masculin, 2=Féminin
         age  = s1q4    # âge révolu
  )

df_educ <- sect2 %>%
  left_join(sect1_reduit, by = c("hhid", "indiv"))

# ---- 3. Diagnostic des données manquantes ----
cat("\nDiagnostic des valeurs manquantes :\n")
cat("  s2aq15 (dernier niveau)     :", sum(is.na(df_educ$s2aq15)), "\n")
cat("  s2aq9  (classe en cours)    :", sum(is.na(df_educ$s2aq9)),  "\n")
cat("  s2aq10 (diplôme le + élevé) :", sum(is.na(df_educ$s2aq10)), "\n")
cat("  s2aq13 (fréquente l'école?) :", sum(is.na(df_educ$s2aq13)), "\n")
cat("  age                         :", sum(is.na(df_educ$age)),    "\n")

# ---- 4. Création de la variable niveau_educ en 5 modalités ----
# Principe :
#   s2aq15 = dernier niveau fréquenté (pour ceux ayant quitté l'école)
#   s2aq9  = classe actuelle (pour ceux encore scolarisés)
#   s2aq10 = diplôme le plus élevé obtenu
# Correspondance des codes :
#   0-3 = Pré-primaire/maternelle ; 11-16 = Primaire ;
#   21-23 = Junior Secondary ; 24-28,31-35,321 = Senior Secondary ;
#   34,35,41-43,322,411-424 = Supérieur

attribuer_niveau_educ <- function(s2aq13, s2aq15, s2aq9, s2aq10) {
  code_brut <- ifelse(!is.na(s2aq15), s2aq15,
               ifelse(!is.na(s2aq9),  s2aq9, NA_real_))

  dplyr::case_when(
    # N'a jamais fréquenté l'école
    s2aq13 == 2 | (is.na(code_brut) & is.na(s2aq10)) ~ "Aucun",
    # Enseignement supérieur
    code_brut %in% c(34,35,41,43,61,322,411,412,421,422,423,424) |
      s2aq10 %in% c(8,9,11,12) ~ "Tertiaire",
    # Lycée / Senior Secondary
    code_brut %in% c(24,25,26,27,28,31,33,321) |
      s2aq10 %in% c(6,7,41,42) ~ "Senior Secondary",
    # Collège / Junior Secondary
    code_brut %in% c(21,22,23) | s2aq10 == 5 ~ "Junior Secondary",
    # École primaire
    code_brut %in% c(11,12,13,14,15,16) |
      s2aq10 %in% c(2,3) ~ "Primaire",
    # Pré-primaire / école coranique sans poursuite
    code_brut %in% c(1,2,3,51,52) ~ "Aucun",
    TRUE ~ "Aucun"
  )
}

df_educ <- df_educ %>%
  mutate(
    niveau_educ = attribuer_niveau_educ(
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
    # sector figure déjà dans sect2
    zone_label = factor(ifelse(as.numeric(sector) == 1, "Urbain", "Rural")),
    groupe_age = cut(age,
                     breaks = c(18, 30, 45, 60, Inf),
                     labels = c("18-30","31-45","46-60","60+"),
                     right  = TRUE, include.lowest = FALSE),
    state_num  = as.numeric(state)
  )

etiquettes_etats <- c(
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
  mutate(state_name = etiquettes_etats[as.character(state_num)])

# ---- 5. Restriction aux adultes de 18 ans et plus ----
df_adultes <- df_educ %>%
  filter(!is.na(age), age >= 18, !is.na(sexe_label), !is.na(niveau_educ))

cat("\nDimensions df_educ    :", nrow(df_educ),    "x", ncol(df_educ),    "\n")
cat("Dimensions df_adultes :", nrow(df_adultes), "x", ncol(df_adultes), "\n")

# ---- 6. Extraction des enfants de 6 à 17 ans (scolarisation) ----
# s2aq13 = fréquente actuellement l'école (1=Oui, 2=Non)
# sector est déjà disponible dans sect2 (1=Urbain, 2=Rural)
df_scol <- df_educ %>%
  filter(!is.na(age), age >= 6, age <= 17) %>%
  mutate(
    scolarise = as.numeric(s2aq13) == 1,
    zone_scol = as.character(zone_label)
  ) %>%
  filter(!is.na(zone_scol), !is.na(scolarise))

cat("Enfants 6-17 ans retenus :", nrow(df_scol), "individus\n")

# ---- 7. Export des tables intermédiaires ----
dir.create("data/processed", showWarnings = FALSE, recursive = TRUE)

saveRDS(df_educ,    file = "data/processed/df_educ.rds")
saveRDS(df_adultes, file = "data/processed/df_adultes.rds")
saveRDS(df_scol,    file = "data/processed/df_scol.rds")

cat("\n✓ Tables sauvegardées dans data/processed/\n")
