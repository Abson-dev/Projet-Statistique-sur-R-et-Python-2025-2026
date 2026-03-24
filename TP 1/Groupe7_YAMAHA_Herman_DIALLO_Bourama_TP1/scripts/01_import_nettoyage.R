# =============================================================================
# Script 01 : Import et préparation des données
# TP1 — Profil démographique des ménages nigérians
# Nigeria GHS-Panel Wave 4 (2018/19) — sect1_harvestw4.dta
#
# Ce script réalise :
#   (1) Le chargement du fichier .dta brut (démographie) et de la base de pondérations
#   (2) La jointure avec secta_harvestw4.dta pour récupérer les poids de sondage (wt_wave4)
#   (3) La vérification des doublons sur la clé hhid × indiv
#   (4) Le diagnostic des valeurs manquantes
#   (5) La construction des variables dérivées (âge, sexe, milieu, etc.)
#   (6) La construction de la base ménages agrégée avec poids
#   (7) La sauvegarde des objets intermédiaires en .rds
#
# NOTE MÉTHODOLOGIQUE SUR LES PONDÉRATIONS :
#   Le GHS-Panel repose sur un plan de sondage à plusieurs degrés.
#   Les pondérations transversales (wt_wave4) issues de secta_harvestw4.dta
#   permettent de redresser l'échantillon pour qu'il soit représentatif de la
#   population nigériane à la Wave 4. Toutes les statistiques descriptives
#   (proportions, moyennes, médianes, tests) doivent être calculées en tenant
#   compte de ces poids afin que les résultats soient généralisables.
#
# Auteurs  : Herman YAMAHA | Bourama DIALLO
# =============================================================================


# =============================================================================
# Installation automatique des packages manquants
# =============================================================================

# Liste des packages requis
packages_requis <- c(
  "haven", "dplyr", "naniar"
)

# Vérification et installation des packages manquants
for (pkg in packages_requis) {
  if (!require(pkg, character.only = TRUE)) {
    message(paste("Installation du package:", pkg))
    install.packages(pkg, dependencies = TRUE)
    library(pkg, character.only = TRUE)
  }
}


# --------------------------------------------------------------------------
# Chemins vers les fichiers sources
# --------------------------------------------------------------------------
fichier_demographie   <- "data/raw/sect1_harvestw4.dta"
fichier_ponderation   <- "data/raw/secta_harvestw4.dta"

# --------------------------------------------------------------------------
# 0. TÉLÉCHARGEMENT DEPUIS GITHUB (si non lancé depuis main.R directement)
# --------------------------------------------------------------------------
# Ce bloc garantit que les données sont disponibles même si le script est
# exécuté de façon indépendante (sans passer par main.R).
.base_url <- paste0(
  "https://raw.githubusercontent.com/",
  "Herman-YAMAHA/NYHP/",
  "bf1173ced39831e18d8e21c3b2880e597bbc6300/",
  "TP1_raw/"
)
dir.create("data/raw", recursive = TRUE, showWarnings = FALSE)
options(timeout = 300)
for (.f in c("sect1_harvestw4.dta", "secta_harvestw4.dta")) {
  .dest <- file.path("data/raw", .f)
  if (!file.exists(.dest)) {
    cat("  Téléchargement :", .f, "...\n")
    tryCatch(
      download.file(paste0(.base_url, .f), destfile = .dest, mode = "wb"),
      error = function(e) cat("  Erreur :", conditionMessage(e), "\n")
    )
  }
}

# --------------------------------------------------------------------------
# 1. CHARGEMENT DES FICHIERS BRUTS
# --------------------------------------------------------------------------
cat("Chargement de", fichier_demographie, "...\n")
df_brut <- read_dta(fichier_demographie)
cat("  ->", nrow(df_brut), "observations x", ncol(df_brut), "variables\n")

cat("Chargement de", fichier_ponderation, "...\n")
df_poids <- read_dta(fichier_ponderation) %>%
  select(hhid, wt_wave4, zone, state, lga, sector)
cat("  ->", nrow(df_poids), "ménages avec pondérations\n")
cat("  wt_wave4 : min =", round(min(df_poids$wt_wave4, na.rm=TRUE),1),
    "| max =", round(max(df_poids$wt_wave4, na.rm=TRUE),1),
    "| NA =", sum(is.na(df_poids$wt_wave4)), "\n")

# --------------------------------------------------------------------------
# 2. IDENTIFIANT UNIQUE MÉNAGE × INDIVIDU
# --------------------------------------------------------------------------
df_brut$cle_unique <- paste(df_brut$hhid, df_brut$indiv, sep = "_")

# --------------------------------------------------------------------------
# 3. VÉRIFICATION DES DOUBLONS
# --------------------------------------------------------------------------
n_doublons <- sum(duplicated(df_brut$cle_unique))
cat("  Doublons sur hhid x indiv :", n_doublons, "\n")

# --------------------------------------------------------------------------
# 4. JOINTURE AVEC LES PONDÉRATIONS
# --------------------------------------------------------------------------
# On joint la pondération transversale wt_wave4 au niveau ménage (hhid)
# Chaque individu d'un même ménage reçoit le même poids de sondage.
# On utilise sector depuis secta (source officielle) pour le milieu de résidence.
df_brut <- df_brut %>%
  left_join(df_poids, by = "hhid", suffix = c("", "_secta"))

n_poids_manquants <- sum(is.na(df_brut$wt_wave4))
cat("  Individus sans pondération :", n_poids_manquants,
    "(", round(n_poids_manquants/nrow(df_brut)*100,2), "% )\n")

# --------------------------------------------------------------------------
# 5. DIAGNOSTIC DES VALEURS MANQUANTES
# --------------------------------------------------------------------------
total_na <- n_miss(df_brut)
cat("  Valeurs manquantes totales :", total_na, "\n")

# --------------------------------------------------------------------------
# 6. CONSTRUCTION DES VARIABLES DÉRIVÉES
# --------------------------------------------------------------------------
# Recodage des variables brutes en facteurs lisibles
# Les codes sources sont ceux du codebook GHS Wave 4 :
#   s1q4  : âge en années (continu)
#   s1q2  : sexe (1 = Homme, 2 = Femme)
#   sector: milieu de résidence (1 = Urbain, 2 = Rural) — issu de secta
#   zone  : zone géopolitique (1 à 6, de North Central à South West)
#   s1q3  : lien de parenté au chef de ménage
#   wt_wave4 : poids de sondage transversal Wave 4

# On utilise 'sector' depuis secta (plus fiable que dans sect1)
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

    # Milieu de résidence (sector issu de secta_harvestw4)
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
    ),

    # Indicateur de présence du poids (pour diagnostic)
    a_poids = !is.na(wt_wave4)
  )

# --------------------------------------------------------------------------
# 7. CONSTRUCTION DE LA BASE MÉNAGES AVEC PONDÉRATION
# --------------------------------------------------------------------------
# La taille est calculée comme le nombre de membres observés
# Le poids ménage = wt_wave4 (unique par hhid, issu de secta)
# Note : on prend first() pour les variables constantes au niveau ménage

effectifs_menage <- df_brut %>%
  group_by(hhid) %>%
  summarise(taille_menage = n(), .groups = "drop")

df_menages <- df_brut %>%
  group_by(hhid) %>%
  summarise(
    taille   = n(),
    milieu   = first(milieu),
    zone_geo = first(zone_geo),
    wt_wave4 = first(wt_wave4),   # poids transversal ménage
    .groups  = "drop"
  )

cat("\n  Ménages au total  :", nrow(df_menages), "\n")
cat("  dont Urbain       :", sum(df_menages$milieu == "Urbain", na.rm=TRUE), "\n")
cat("  dont Rural        :", sum(df_menages$milieu == "Rural", na.rm=TRUE),  "\n")
cat("  dont poids non NA :", sum(!is.na(df_menages$wt_wave4)), "\n")

# --------------------------------------------------------------------------
# 8. JEU DE DONNÉES INDIVIDUEL ENRICHI
# --------------------------------------------------------------------------
df_individus <- df_brut %>%
  left_join(effectifs_menage, by = "hhid") %>%
  filter(!is.na(milieu))

cat("  Individus retenus :", nrow(df_individus), "\n")
cat("  dont avec poids   :", sum(!is.na(df_individus$wt_wave4)), "\n")

# --------------------------------------------------------------------------
# 9. SAUVEGARDE DES OBJETS INTERMÉDIAIRES
# --------------------------------------------------------------------------
dir.create("data/processed", showWarnings = FALSE, recursive = TRUE)

saveRDS(df_brut,       "data/processed/df_brut.rds")
saveRDS(df_individus,  "data/processed/df_individus.rds")
saveRDS(df_menages,    "data/processed/df_menages.rds")

cat("\n  -> Objets sauvegardés dans data/processed/\n")
cat("     (poids wt_wave4 intégrés dans les trois objets)\n")
