# =============================================================================
# Script 01 : Import et préparation des données
# TP1 — Profil démographique des ménages nigérians
# Nigeria GHS-Panel Wave 4 (2018/19) — sect1_harvestw4.dta
#
# Ce script réalise :
#   (1) Le chargement du fichier .dta brut
#   (2) La vérification des doublons sur la clé hhid × indiv
#   (3) Le diagnostic des valeurs manquantes
#   (4) La construction des variables dérivées (âge, sexe, milieu, etc.)
#   (5) La construction de la base ménages agrégée
#   (6) La sauvegarde des objets intermédiaires en .rds
#
# Auteurs  : Herman YAMAHA | Bourama DIALLO
# =============================================================================

library(haven)    # read_dta()
library(dplyr)    # manipulation tabulaire
library(naniar)   # n_miss()

# --------------------------------------------------------------------------
# Chemin vers le fichier source
# --------------------------------------------------------------------------
fichier_source <- "data/raw/sect1_harvestw4.dta"

# --------------------------------------------------------------------------
# 1. CHARGEMENT DU FICHIER BRUT
# --------------------------------------------------------------------------
cat("Chargement de", fichier_source, "...\n")
df_brut <- read_dta(fichier_source)
cat("  ->", nrow(df_brut), "observations x", ncol(df_brut), "variables\n")

# --------------------------------------------------------------------------
# 2. IDENTIFIANT UNIQUE MÉNAGE × INDIVIDU
# --------------------------------------------------------------------------
# Chaque individu est identifié de façon unique par la combinaison
# de l'identifiant ménage (hhid) et du numéro d'individu dans le ménage (indiv)
df_brut$cle_unique <- paste(df_brut$hhid, df_brut$indiv, sep = "_")

# --------------------------------------------------------------------------
# 3. VÉRIFICATION DES DOUBLONS
# --------------------------------------------------------------------------
n_doublons <- sum(duplicated(df_brut$cle_unique))
cat("  Doublons sur hhid x indiv :", n_doublons, "\n")

# --------------------------------------------------------------------------
# 4. DIAGNOSTIC DES VALEURS MANQUANTES
# --------------------------------------------------------------------------
total_na <- n_miss(df_brut)
cat("  Valeurs manquantes totales :", total_na, "\n")

# --------------------------------------------------------------------------
# 5. CONSTRUCTION DES VARIABLES DÉRIVÉES
# --------------------------------------------------------------------------
# Recodage des variables brutes en facteurs lisibles
# Les codes sources sont ceux du codebook GHS Wave 4 :
#   s1q4  : âge en années (continu)
#   s1q2  : sexe (1 = Homme, 2 = Femme)
#   sector: milieu de résidence (1 = Urbain, 2 = Rural)
#   zone  : zone géopolitique (1 à 6, de North Central à South West)
#   s1q3  : lien de parenté au chef de ménage

df_brut <- df_brut %>%
  mutate(
    # Âge (variable continue)
    age = as.numeric(s1q4),

    # Sexe — facteur à deux niveaux
    genre = factor(
      as.numeric(s1q2),
      levels = c(1, 2),
      labels = c("Homme", "Femme")
    ),

    # Milieu de résidence
    milieu = factor(
      as.numeric(sector),
      levels = c(1, 2),
      labels = c("Urbain", "Rural")
    ),

    # Zone géopolitique
    zone_geo = factor(
      as.numeric(zone),
      levels = 1:6,
      labels = c("North Central", "North East", "North West",
                 "South East",   "South South", "South West")
    ),

    # Lien de parenté simplifié en 4 modalités
    # Code 1 = Chef, 2 = Conjoint(e), 3 = Enfant, autres = Autre membre
    lien_code = as.numeric(as.character(s1q3)),
    lien_parente = factor(
      case_when(
        lien_code == 1 ~ "Chef de ménage",
        lien_code == 2 ~ "Conjoint(e)",
        lien_code == 3 ~ "Enfant",
        TRUE           ~ "Autre membre"
      ),
      levels = c("Chef de ménage", "Conjoint(e)", "Enfant", "Autre membre")
    ),

    # Tranche d'âge quinquennale pour la pyramide des âges
    # Intervalles fermés à gauche : [0, 5), [5, 10), ...
    tranche_age = cut(
      age,
      breaks = c(0, 5, 10, 15, 20, 25, 30, 35, 40,
                 45, 50, 55, 60, 65, 70, 75, 80, Inf),
      labels = c("0-4",  "5-9",  "10-14", "15-19", "20-24",
                 "25-29", "30-34", "35-39", "40-44", "45-49",
                 "50-54", "55-59", "60-64", "65-69", "70-74",
                 "75-79", "80+"),
      right          = FALSE,
      include.lowest = TRUE
    )
  )

# --------------------------------------------------------------------------
# 6. CONSTRUCTION DE LA BASE MÉNAGES
# --------------------------------------------------------------------------
# Chaque ligne = un ménage, avec sa taille et son milieu de résidence
# La taille est calculée comme le nombre de membres observés (individus)

effectifs_menage <- df_brut %>%
  group_by(hhid) %>%
  summarise(taille_menage = n(), .groups = "drop")

df_menages <- df_brut %>%
  group_by(hhid) %>%
  summarise(
    taille   = n(),
    milieu   = first(milieu),
    zone_geo = first(zone_geo),
    .groups  = "drop"
  )

cat("\n  Ménages au total  :", nrow(df_menages), "\n")
cat("  dont Urbain       :", sum(df_menages$milieu == "Urbain"), "\n")
cat("  dont Rural        :", sum(df_menages$milieu == "Rural"),  "\n")

# --------------------------------------------------------------------------
# 7. JEU DE DONNÉES INDIVIDUEL ENRICHI
# --------------------------------------------------------------------------
# On joint la taille du ménage à chaque individu
# et on exclut les individus sans milieu renseigné

df_individus <- df_brut %>%
  left_join(effectifs_menage, by = "hhid") %>%
  filter(!is.na(milieu))

cat("  Individus retenus :", nrow(df_individus), "\n")

# --------------------------------------------------------------------------
# 8. SAUVEGARDE DES OBJETS INTERMÉDIAIRES
# --------------------------------------------------------------------------
dir.create("data/processed", showWarnings = FALSE, recursive = TRUE)

saveRDS(df_brut,       "data/processed/df_brut.rds")
saveRDS(df_individus,  "data/processed/df_individus.rds")
saveRDS(df_menages,    "data/processed/df_menages.rds")

cat("\n  -> Objets sauvegardés dans data/processed/\n")
