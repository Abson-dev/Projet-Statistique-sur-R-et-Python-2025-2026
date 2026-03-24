# =============================================================================
# Script 01 : Import et nettoyage des données
# Thème 2 – Éducation et alphabétisation des membres des ménages
# Enquête GHS Nigeria 2018 (Wave 4) – avec pondérations transversales wt_wave4
# Réalisé par : Herman YAMAHA, Bourama DIALLO
#
# NOTE MÉTHODOLOGIQUE :
#   Les pondérations transversales wt_wave4 issues de secta_harvestw4.dta
#   permettent de redresser l'échantillon pour qu'il soit représentatif de
#   la population nigériane. Elles sont jointes au niveau ménage (hhid) et
#   propagées à chaque individu.
# =============================================================================

library(haven)
library(dplyr)
library(forcats)

# Localisation des fichiers bruts
chemin_s1  <- "data/raw/sect1_harvestw4.dta"
chemin_s2  <- "data/raw/sect2_harvestw4.dta"
chemin_sa  <- "data/raw/secta_harvestw4.dta"

# ---- 0. Téléchargement des données depuis GitHub (si absentes) ----
# Ce bloc garantit le fonctionnement si le script est exécuté directement
# sans passer par main.R.
options(timeout = 300)

.base_url <- paste0(
  "https://raw.githubusercontent.com/",
  "Herman-YAMAHA/NYHP/",
  "bf1173ced39831e18d8e21c3b2880e597bbc6300/",
  "TP1_raw/"
)

dir.create("data/raw", recursive = TRUE, showWarnings = FALSE)

for (.f in c("sect1_harvestw4.dta", "sect2_harvestw4.dta", "secta_harvestw4.dta")) {
  .dest <- file.path("data/raw", .f)
  if (!file.exists(.dest)) {
    cat("  Téléchargement :", .f, "...\n")
    tryCatch(
      download.file(paste0(.base_url, .f), destfile = .dest, mode = "wb"),
      error = function(e) cat("  Erreur :", conditionMessage(e), "\n")
    )
  }
}

# ---- 1. Lecture des fichiers Stata ----
cat("Lecture de sect1_harvestw4.dta ...\n")
sect1 <- read_dta(chemin_s1)

cat("Lecture de sect2_harvestw4.dta ...\n")
sect2 <- read_dta(chemin_s2)

cat("Lecture de secta_harvestw4.dta ...\n")
secta <- read_dta(chemin_sa)

cat("  sect1  :", nrow(sect1),  "obs x", ncol(sect1),  "vars\n")
cat("  sect2  :", nrow(sect2),  "obs x", ncol(sect2),  "vars\n")
cat("  secta  :", nrow(secta),  "obs x", ncol(secta),  "vars\n")

# ---- 2. Extraction des pondérations depuis secta ----
# wt_wave4 : pondération transversale Wave 4 (une valeur par ménage)
df_poids <- secta %>%
  select(hhid, wt_wave4) %>%
  filter(!is.na(wt_wave4))

cat("\nPondérations wt_wave4 :\n")
cat("  Ménages avec poids     :", nrow(df_poids), "\n")
cat("  Min / Max              :", round(min(df_poids$wt_wave4), 1),
    "/", round(max(df_poids$wt_wave4), 1), "\n")
cat("  Moyenne                :", round(mean(df_poids$wt_wave4), 1), "\n")

# ---- 3. Fusion sect2 + sect1 + pondérations ----
cat("\nFusion de sect2, sect1 et pondérations via hhid + indiv ...\n")

sect1_reduit <- sect1 %>%
  select(hhid, indiv,
         sexe = s1q2,   # 1=Masculin, 2=Féminin
         age  = s1q4    # âge révolu
  )

df_educ <- sect2 %>%
  left_join(sect1_reduit, by = c("hhid", "indiv")) %>%
  left_join(df_poids,     by = "hhid")

n_sans_poids <- sum(is.na(df_educ$wt_wave4))
cat("  Individus sans pondération :", n_sans_poids,
    "(", round(n_sans_poids / nrow(df_educ) * 100, 2), "%)\n")

# ---- 4. Diagnostic des données manquantes ----
cat("\nDiagnostic des valeurs manquantes :\n")
cat("  s2aq15 (dernier niveau)     :", sum(is.na(df_educ$s2aq15)), "\n")
cat("  s2aq9  (classe en cours)    :", sum(is.na(df_educ$s2aq9)),  "\n")
cat("  s2aq10 (diplôme le + élevé) :", sum(is.na(df_educ$s2aq10)), "\n")
cat("  s2aq13 (fréquente l'école?) :", sum(is.na(df_educ$s2aq13)), "\n")
cat("  age                         :", sum(is.na(df_educ$age)),    "\n")
cat("  wt_wave4                    :", sum(is.na(df_educ$wt_wave4)), "\n")

# ---- 5. Construction de la variable niveau_educ en 5 modalités ----
attribuer_niveau_educ <- function(s2aq13, s2aq15, s2aq9, s2aq10) {
  code_brut <- ifelse(!is.na(s2aq15), s2aq15,
               ifelse(!is.na(s2aq9),  s2aq9, NA_real_))

  dplyr::case_when(
    s2aq13 == 2 | (is.na(code_brut) & is.na(s2aq10)) ~ "Aucun",
    code_brut %in% c(34,35,41,43,61,322,411,412,421,422,423,424) |
      s2aq10 %in% c(8,9,11,12) ~ "Tertiaire",
    code_brut %in% c(24,25,26,27,28,31,33,321) |
      s2aq10 %in% c(6,7,41,42) ~ "Senior Secondary",
    code_brut %in% c(21,22,23) | s2aq10 == 5 ~ "Junior Secondary",
    code_brut %in% c(11,12,13,14,15,16) |
      s2aq10 %in% c(2,3) ~ "Primaire",
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
                         levels = c("Aucun", "Primaire", "Junior Secondary",
                                    "Senior Secondary", "Tertiaire"),
                         ordered = TRUE),
    sexe_label = factor(ifelse(as.numeric(sexe) == 1, "Homme", "Femme")),
    zone_label = factor(ifelse(as.numeric(sector) == 1, "Urbain", "Rural")),
    groupe_age = cut(age,
                     breaks = c(18, 30, 45, 60, Inf),
                     labels = c("18-30", "31-45", "46-60", "60+"),
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

# ---- 6. Restriction aux adultes de 18 ans et plus ----
df_adultes <- df_educ %>%
  filter(!is.na(age), age >= 18, !is.na(sexe_label), !is.na(niveau_educ))

cat("\nDimensions df_educ    :", nrow(df_educ),    "x", ncol(df_educ),    "\n")
cat("Dimensions df_adultes :", nrow(df_adultes), "x", ncol(df_adultes), "\n")
cat("  dont avec poids     :", sum(!is.na(df_adultes$wt_wave4)), "\n")

# ---- 7. Extraction des enfants de 6 à 17 ans (scolarisation) ----
df_scol <- df_educ %>%
  filter(!is.na(age), age >= 6, age <= 17) %>%
  mutate(
    scolarise = as.numeric(s2aq13) == 1,
    zone_scol = as.character(zone_label)
  ) %>%
  filter(!is.na(zone_scol), !is.na(scolarise))

cat("Enfants 6-17 ans retenus :", nrow(df_scol), "individus\n")
cat("  dont avec poids        :", sum(!is.na(df_scol$wt_wave4)), "\n")

# ---- 8. Export des tables intermédiaires ----
dir.create("data/processed", showWarnings = FALSE, recursive = TRUE)

saveRDS(df_educ,    file = "data/processed/df_educ.rds")
saveRDS(df_adultes, file = "data/processed/df_adultes.rds")
saveRDS(df_scol,    file = "data/processed/df_scol.rds")

cat("\n\u2713 Tables sauvegardées dans data/processed/ (wt_wave4 inclus)\n")
