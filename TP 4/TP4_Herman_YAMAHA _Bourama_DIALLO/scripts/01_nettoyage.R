# =============================================================================
# Script 01 : Import et préparation des données
# TP4 — Analyse des parcelles agricoles: superficie
# Nigeria GHS-Panel Wave 4 (2018/19) — secta1_harvestw4.dta
#
# Ce script réalise :
#   (1) Le chargement du fichier .dta brut secta1_harvestw4.dta et de la base de pondérations
#   (2) La jointure avec secta_harvestw4.dta pour récupérer les poids de sondage (wt_wave4)
#   (3) La construction de la base ménages agrégée avec poids
#   (4) Construire la variable superficie en hectares 
#   (5) Calculer la superficie par ménage (sum par hhid)
#   (6) Décrire les valeurs manquantes et les valeurs aberrantes(superificie <0 ou > 500 ha)
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
# Installation automatique des packages manquants (si nécessaire)
# =============================================================================
# Packages installables via CRAN normalement
packages_cran <- c(
  "haven",        # Importation des fichiers
  "tidyverse",    # Manipulation des données
  "naniar",       # Valeurs manquantes
  "here",         # Gestion des chemins
  "survey",       # Gestion de la pondération
  "scales",       # Mise en forme des échelles dans les graphiques
  "wCorr",         # Corrélation de Spearman pondérée (utilisée en Q20)
  "labelled",     # Gestion des labels
  "writexl",       # Création et exportation de fichiers excels
  "Hmisc"          # Outils pour la gestion, description et analyse des données
)

# Installation CRAN et importation
for(pkg in packages_cran) {
  if(!require(pkg, character.only = TRUE)) {
    message(paste("Installation CRAN:", pkg))
    install.packages(pkg, dependencies = TRUE)
    library(pkg, character.only = TRUE)
  }
}

# --------------------------------------------------------------------------
# Chemins vers les fichiers sources
# --------------------------------------------------------------------------

fichier_ponderation <- "data/raw/secta_harvestw4.dta"
fichier_parcelle1 <- "data/raw/sect11a1_plantingw4.dta"
fichier_parcelle2 <- "data/raw/sect11b1_plantingw4.dta"

# --------------------------------------------------------------------------
# 0. TÉLÉCHARGEMENT DEPUIS GITHUB (si non lancé depuis main.R directement)
# --------------------------------------------------------------------------
# Ce bloc garantit que les données sont disponibles même si le script est
# exécuté de façon indépendante (sans passer par main.R).

.base_url <- paste0(
  "https://raw.githubusercontent.com/",
  "diallobourama851-oss/Donn-es-des-TP/",
  "main/",
  "TP4/"
)

dir.create("data/raw", showWarnings =  FALSE, recursive =  TRUE)
options(timeout = 300)
for (.f in c("secta_harvestw4.dta","sect11a1_plantingw4.dta","sect11b1_plantingw4.dta")) {
  .dest <- file.path("data/raw", .f)
  if(!file.exists(.dest)) {
    cat(" Téléchargement :", .f, "...\n")
    tryCatch(
      download.file(paste0(.base_url, .f), destfile = .dest, mode = "wb"),
      error = function(e) cat("  Erreur :", conditionMessage(e), "\n")
    )
  }
}

# --------------------------------------------------------------------------
# 1. CHARGEMENT DES FICHIERS BRUTS
# --------------------------------------------------------------------------


# Fichier sect11a1_plantingw4.dta sur les parcelles contenant les informations sur la superficie
cat("Chargement de ", fichier_parcelle1)
parc1_brut <- read_dta(fichier_parcelle1)
cat(" --> ", nrow(parc1_brut), "observations", ncol(parc1_brut), "Variables\n")


#Fichier sect11b1_plantingw4.dta sur les parcelles contenant les informations sur la tenure,
#l'irrigation, la pente et le type de sol.
cat("Chargement de ", fichier_parcelle2)
parc2_brut <- read_dta(fichier_parcelle2)
cat(" --> ", nrow(parc2_brut), "observations", ncol(parc2_brut), "Variables\n")


# Fichier secta_harvestw4.dta contenant les informations sur le ménage et le poids
cat("Chargement de ", fichier_ponderation, "...\n")
df_poids <- read_dta(fichier_ponderation) %>%
  select(hhid,wt_wave4,zone,state,lga,sector,ea)
hh_avec_poids <- sum(!is.na(df_poids$wt_wave4))
hh_sans_poids <- sum(is.na(df_poids$wt_wave4))
cat(" --> ",hh_avec_poids, "ménages avec pondération\n")
cat("-->", hh_sans_poids, "ménages sans pondération(NA)\n")
cat(" wt_wave4 : min =", round(min(df_poids$wt_wave4, na.rm = TRUE),1),
    "| max =", round(max(df_poids$wt_wave4, na.rm = TRUE),1))

#======================================================================

# Extraire les variables et leurs labels de la base sect11a1_plantingw4
meta <- data.frame(
  variable = names(parc1_brut),
  label    = sapply(parc1_brut, function(x) {
    lab <- var_label(x)
    if (is.null(lab)) NA else lab
  })
)

# Exporter vers Excel
dir.create(here("outputs", "tables"), recursive = TRUE, showWarnings = FALSE)
write_xlsx(meta, here("outputs", "tables", "labels_sect11a1_plantingw4.xlsx"))


#Extraire les variables et leurs labels de la base sect11b1_plantingw4

meta1 <- data.frame(
  variable = names(parc2_brut),
  label = sapply(parc2_brut , function(x) {
    lab <- var_label(x)
    if(is.null(lab)) NA else lab
  }
  )
)

#Exporter vers Excel
dir.create(here("outputs","tables"), recursive = TRUE, showWarnings = FALSE)
write_xlsx(meta1,here("outputs","tables","labels_sect11b1_planting.xlsx"))
#==============================================================================

#Jointure des deux bases de parcelles sect11a1_plantingw4.dta et sect11b1_plantingw4.dta
#Afin d'avoir toutes les variables dans une seule base.
#===============================================================================
base_parcelle <- parc1_brut |>
  left_join(parc2_brut, by = c("hhid","plotid"), suffix = c("_11a1","_11b1"))
cat("\nbase_parcelle: ","observations : ",nrow(base_parcelle),"| variables :",ncol(base_parcelle))
#===============================================================================


#------------------------------------------------------------
# Question 19.
#------------------------------------------------------------

# Construction de la variable superficie en hectares

# ================================
# 1. Tables de conversion
# ================================
conv_generale <- c("5" = 0.4, "6" = 1, "7" = 0.0001)

conv_zone <- matrix(
  c(0.00012, 0.0027,  0.00006,
    0.00016, 0.004,   0.00016,
    0.00011, 0.00494, 0.00004,
    0.00019, 0.0023,  0.00004,
    0.00021, 0.0023,  0.00013,
    0.00012, 0.00001, 0.00041),
  nrow = 6, byrow = TRUE,
  dimnames = list(1:6, c("1","2","3"))
)

unit_names <- c("1"="heap","2"="ridge","3"="stand",
                "5"="acres","6"="hectares","7"="sqm")

zone_names <- c("1"="North Central","2"="North East","3"="North West",
                "4"="South East",  "5"="South South","6"="South West")

# ================================
# 2. Fonction de conversion
# ================================
convert_ha_single <- function(valeur, unite, zone) {
  if (is.na(valeur) | is.na(unite)) return(NA_real_)
  u <- as.character(unite)
  if (u %in% names(conv_generale)) return(valeur * conv_generale[u])
  if (u %in% c("1","2","3") & !is.na(zone))
    return(valeur * conv_zone[as.character(zone), u])
  return(NA_real_)
}

# ================================
# 3. Appliquer sur base_parcelle
# ================================
base_parcelle <- base_parcelle %>%
  mutate(
    zone_11a1     = as.integer(zone_11a1),
    s11aq4b       = as.integer(s11aq4b),
    s11aq4aa      = as.numeric(s11aq4aa),
    superficie_ha = mapply(convert_ha_single, s11aq4aa, s11aq4b, zone_11a1),
    unit_name     = unit_names[as.character(s11aq4b)],
    zone_name     = zone_names[as.character(zone_11a1)]
  )

# ================================
# 4. Contrôles et warnings
# ================================
bad_units <- base_parcelle %>%
  filter(is.na(unit_name)) %>%
  pull(s11aq4b) %>% unique()
if (length(bad_units) > 0)
  message("Unités non reconnues (s11aq4b) : ", paste(bad_units, collapse = ", "))

bad_zones <- base_parcelle %>%
  filter(s11aq4b %in% c(1,2,3) & is.na(zone_name)) %>%
  pull(zone_11a1) %>% unique()
if (length(bad_zones) > 0)
  message("Zones manquantes pour unités locales (zone_11a1) : ", paste(bad_zones, collapse = ", "))

cat("NA dans superficie_ha :", sum(is.na(base_parcelle$superficie_ha)), "\n")


# ================================
# 1. Superficie totale par ménage
# ================================
superficie_menage <- base_parcelle %>%
  group_by(hhid) %>%
  summarise(
    superficie_totale_ha = sum(superficie_ha, na.rm = TRUE),
    n_parcelles          = n(),
    n_manquantes         = sum(is.na(superficie_ha))
  )

print(head(superficie_menage))

# ================================
# 2. Valeurs manquantes
# ================================
na_count <- sum(is.na(base_parcelle$superficie_ha))
na_pct   <- round(na_count / nrow(base_parcelle) * 100, 2)
cat("Valeurs manquantes :", na_count, "(", na_pct, "% )\n")

# Afficher les lignes concernées
base_parcelle %>% filter(is.na(superficie_ha))

# ================================
# 3. Valeurs aberrantes
# ================================
aberrantes <- base_parcelle %>%
  filter(superficie_ha < 0 | superficie_ha > 500)

cat("Valeurs aberrantes :", nrow(aberrantes), "\n")

# Afficher les lignes concernées
print(aberrantes %>% select(hhid, superficie_ha, unit_name, zone_name))


# Nombre de ménages sans poids

menage_sans_poids <- sum(is.na(df_poids$wt_wave4))
menage_sans_poids

# Proportion de valeurs manquantes dans la superficie GPS
super_manq <- sum(is.na(base_parcelle$s11aq4c))
super_manq


# =============================================================================
# Sauvegarde des objets intermédiaires pour le script 02
# =============================================================================
dir.create(here("data", "processed"), showWarnings = FALSE, recursive = TRUE)

# =============================================================================
# IMPORTANT : Ajouter les pondérations (wt_wave4) à base_parcelle
# =============================================================================


# Convertir hhid en caractère pour éviter les problèmes de type
base_parcelle <- base_parcelle %>%
  mutate(hhid = as.character(hhid))

df_poids <- df_poids %>%
  mutate(hhid = as.character(hhid))

# Joindre les poids aux parcelles
base_parcelle <- base_parcelle %>%
  left_join(df_poids %>% select(hhid, wt_wave4), by = "hhid")

# Vérification de la jointure
cat("\n--- Vérification de la jointure des poids ---\n")
cat("wt_wave4 présent dans base_parcelle :", "wt_wave4" %in% names(base_parcelle), "\n")
cat("Nombre de ménages uniques dans base_parcelle :", length(unique(base_parcelle$hhid)), "\n")
cat("Valeurs manquantes dans wt_wave4 :", sum(is.na(base_parcelle$wt_wave4)), "\n")
cat("Premières valeurs de wt_wave4 :\n")
print(head(base_parcelle$wt_wave4, 10))


# =============================================================================
# Sauvegarde des objets intermédiaires pour le script 02
# =============================================================================

saveRDS(df_poids,       here("data", "processed", "df_poids.rds"))
saveRDS(base_parcelle,  here("data", "processed", "base_parcelle.rds"))

cat("\n Fichiers sauvegardés dans data/processed/ :\n")
cat("  - df_poids.rds\n")
cat("  - base_parcelle.rds (avec wt_wave4 inclus)\n")
