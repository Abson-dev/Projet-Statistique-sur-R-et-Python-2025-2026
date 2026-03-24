# =============================================================================
# TP2 GHS Nigeria Panel Wave 4 (2018-2019)
# Script 01 : Import, exploration, nettoyage et construction des variables
# Thème    : Capital humain | Éducation et alphabétisation
# Données  : NBS Nigeria / World Bank LSMS-ISA
#
# Ce script couvre entièrement la TÂCHE 7 du sujet :
# "Charger sect2_harvestw4 et sect1_harvestw4. Joindre les deux fichiers
#  sur hhid + indiv. Inspecter les valeurs manquantes du niveau d'éducation."
#
# Note structurelle importante : évolution du module éducation W1 à W4 :
# W1-W2 : deux fichiers séparés selon le statut scolaire de l'individu
#   - sect2a : membres ayant quitté l'école (niveau atteint, diplôme)
#   - sect2b : membres actuellement scolarisés (inscription, dépenses)
#   Le filtre d'entrée était s2aq1 (W1) ou s2aq2 (W2) : "5 ans et plus"
# W3-W4 : fusion en un seul fichier sect2_harvestwX
#   Les variables s2bq* ont disparu, leur contenu absorbé dans s2aq*.
#   W3 : filtre s2aq2 = "5 ans et plus"
#   W4 : filtre s2aq2 = "3 ans et plus" , élargissement de la population
#   Ce changement de seuil sera mentionné dans le rapport pour justifier
#   les différences d'effectifs entre vagues dans le TP10.
# =============================================================================

# Packages et graine chargés par main.R — ce script ne charge rien.


# =============================================================================
# BLOC 1 — Création des dossiers de sortie
# =============================================================================
# dir.create() avec showWarnings = FALSE ne génère pas d'erreur si le
# dossier existe déjà
dir.create(here("data", "processed"),  showWarnings = FALSE, recursive = TRUE)
dir.create(here("outputs", "figures"), showWarnings = FALSE, recursive = TRUE)
dir.create(here("outputs", "tables"),  showWarnings = FALSE, recursive = TRUE)


# =============================================================================
# BLOC 2 — Import des données
# =============================================================================
# Trois fichiers sont nécessaires :
#
# sect2_harvestw4 : module éducation — niveau individu, membres 3 ans et plus.
#   Contient toutes les variables éducation (s2aq*) ainsi que zone, state,
#   sector directement — pas besoin de joindre secta pour ces variables.
#
# sect1_harvestw4 : roster ménage — niveau individu, TOUS les membres
#   (actifs résidents + absents partis). Fournit âge, sexe, lien de parenté
#   et surtout le statut de membership (s1q4a) pour filtrer les actifs.
#
# secta_harvestw4 : fichier de couverture post-harvest — niveau ménage,
#   une ligne par ménage. En W4, ce fichier joue le rôle de fichier Root
#   général (contrairement aux vagues précédentes où il était spécifique
#   au module agricole). Il contient les poids de sondage (wt_wave4) et
#   les variables administratives d'interview (interview_result, hhidyn...).
#   Chargé ici pour vérification de cohérence et attrition.
#   Sera mobilisé lors du passage aux analyses pondérées.

sect2 <- read_dta(here("data", "raw", "NGA_2018_GHSP-W4_v03_M_Stata12",
                       "sect2_harvestw4.dta"))
sect1 <- read_dta(here("data", "raw", "NGA_2018_GHSP-W4_v03_M_Stata12",
                       "sect1_harvestw4.dta"))
secta <- read_dta(here("data", "raw", "NGA_2018_GHSP-W4_v03_M_Stata12",
                       "secta_harvestw4.dta"))

cat("sect2 :", nrow(sect2), "lignes x", ncol(sect2), "colonnes\n")
cat("sect1 :", nrow(sect1), "lignes x", ncol(sect1), "colonnes\n")
cat("secta :", nrow(secta), "lignes x", ncol(secta), "colonnes\n")

# sect2 : 26 557 lignes — tous membres pour lesquels le module éducation
#   a été ouvert, c'est-à-dire présents dans le ménage au moment de
#   l'enquête. Inclut les moins de 3 ans (s2aq2 == 2, NA sur tout le reste).
# sect1 : 30 337 lignes — roster complet incluant membres partis (s1q4a == 2).
#   L'écart brut sect1 - sect2 (30 337 - 26 557 = 3 780) s'explique
#   principalement par les membres partis (s1q4a == 2), confirmé au TP1.
#   OBSERVATION : sect2 contient aussi 790 membres NON actifs (s1q4a != 1),
#   probablement des membres temporairement absents mais encore considérés
#   comme faisant partie du ménage. Calcul : sect1_actifs (25 767, voir
#   BLOC 6) vs sect2 (26 557) = écart de 790 membres non actifs dans sect2.
#   Ces 790 individus seront exclus lors du left_join depuis sect1_actifs.
# secta : 5 025 lignes , cadre de sondage complet (ménages ciblés).
#   L'écart avec sect2 (5 025 - 4 980 = 45) correspond à l'attrition totale
#   documentée ci-dessous (BLOC 4).
# Rapport 26 557 / 5 025 ≈ 5.3 membres éligibles éducation par ménage 
#   cohérent avec la taille médiane de 5 membres observée au TP1.


# =============================================================================
# BLOC 3 — Exploration des variables clés
# =============================================================================
# on ne recode jamais sans avoir d'abord vérifié
# les labels Stata originaux, le nombre de modalités et leur distribution.
# La fonction que nous creeons ici, inspecter_var() centralise cette vérification pour chaque
# variable qualitative clé du module éducation.

inspecter_var <- function(df, var_name) {
  x     <- df[[var_name]]
  label <- attr(x, "label")
  labs  <- attr(x, "labels")
  cat("\n", strrep("=", 65), "\n", sep = "")
  cat("Variable :", var_name, "\n")
  cat("Intitulé :", ifelse(is.null(label), "(aucun label)", label), "\n")
  if (!is.null(labs)) {
    cat("Modalités :", length(labs), "catégories\n")
    for (i in seq_along(labs))
      cat(sprintf("  %5g -> %s\n", labs[i], names(labs)[i]))
  } else {
    cat("Variable numérique sans labels Stata\n")
  }
  freq <- table(x, useNA = "always")
  prop <- round(prop.table(freq) * 100, 1)
  print(data.frame(Modalite = names(freq),
                   Effectif = as.integer(freq),
                   Pct      = as.numeric(prop)),
        row.names = FALSE)
}

# s2aq2 — filtre d'entrée du module éducation
# "Cette personne a-t-elle 3 ans ou plus ?"
# s2aq2 == 1 : oui -> questionnaire administré
# s2aq2 == 2 : non -> questionnaire sauté, toutes variables s2aq* à NA
# Absence de NA sur s2aq2 = le filtre a été renseigné pour chaque individu.
inspecter_var(sect2, "s2aq2")

# s2aq5 — alphabétisation
# "Cette personne sait-elle lire et écrire dans une langue ?"
# Variable centrale du TP2. Posée à tous les individus éligibles (s2aq2 == 1).
# NA attendus = exactement les individus avec s2aq2 == 2 (moins de 3 ans).
# Un NA sur un individu éligible serait une vraie non-réponse à documenter.
inspecter_var(sect2, "s2aq5")

# Vérification directe : parmi les individus avec s2aq2 == 2,
# combien ont NA sur s2aq5 ?
cat("s2aq2 == 2 ET s2aq5 NA      :",
    sum(sect2$s2aq2 == 2 & is.na(sect2$s2aq5), na.rm = TRUE), "\n")


# Vérification inverse : parmi les individus avec NA sur s2aq5,
# combien ont s2aq2 == 2 ?
cat("s2aq5 NA ET s2aq2 == 2      :",
    sum(is.na(sect2$s2aq5) & sect2$s2aq2 == 2, na.rm = TRUE), "\n")

# s2aq6 — a déjà fréquenté l'école
# "Cette personne a-t-elle jamais été à l'école ?"
# Filtre secondaire interne au questionnaire :
#   s2aq6 == 1 (oui) -> s2aq9 renseigné (niveau atteint)
#   s2aq6 == 2 (non) -> s2aq9 = NA par construction , PAS une non-réponse
# Ce mécanisme est crucial pour interpréter les NA sur s2aq9.
inspecter_var(sect2, "s2aq6")

# s2aq9 — niveau d'éducation le plus élevé complété
# Variable centrale pour construire niveau_educ (tâche 8 dans 02_analyses.R).
# 35 modalités avec codes non contigus reflétant le système éducatif nigérian :
#   0        : aucun niveau formel
#   1-3      : préscolaire (nursery, pré-nursery, maternelle)
#   11-16    : primaire — Primary 1 à Primary 6 (6 ans)
#   21-23    : Junior Secondary School (JSS) — collège, 3 ans
#   24-26    : Senior Secondary School (SSS) — lycée, 3 ans
#   27-28    : Lower/Upper 6 — classes préparatoires université
#   31-35    : formations professionnelles courtes (NCE, nursing...)
#   41, 43   : université et post-graduate
#   51-52    : école coranique (Quaranic) — éducation islamique formelle
#   61       : éducation des adultes (alphabétisation tardive)
#   321-322  : vocational/technique secondaire et tertiaire
#   411-412  : OND/HND (Ordinary/Higher National Diploma)
#   421-424  : université par niveau d'année (100L à 600L)
# Les NA sur s2aq9 peuvent avoir quatre origines distinctes qu'on
# décompose rigoureusement en BLOC 5 après la jointure.
inspecter_var(sect2, "s2aq9")

# s2aq10 — diplôme le plus élevé obtenu
# Complément de s2aq9 : 13 certifications officielles du système nigérian.
# FSLC = First School Leaving Certificate (fin primaire)
# JSS  = Junior Secondary Certificate (fin collège)
# SSS/O Level = baccalauréat nigérian (WAEC/NECO)
# NCE  = National Certificate of Education (enseignants)
# HND  = Higher National Diploma (polytechnique)
# Note : le label du code 6 contient "â€˜O LEVELâ€" — apostrophes mal
# encodées dans le fichier Stata original (UTF-8). Sans impact analytique.
inspecter_var(sect2, "s2aq10")

# s2aq13 — a fréquenté l'école en 2017/2018 (année passée)
# Posée avant s2aq13a dans le questionnaire.
# Permet de construire les trois profils de scolarisation :
#   s2aq13 == 1 et s2aq13a == 1 : scolarisé les deux années (persévérant)
#   s2aq13 == 1 et s2aq13a == 2 : décrocheur récent (présent puis absent)
#   s2aq13 == 2 et s2aq13a == 1 : retour à l'école après interruption
#   s2aq13 == 2 et s2aq13a == 2 : non scolarisé les deux années
# La comparaison des distributions des deux variables révèle aussi
# si des individus ont été interrogés sur une année mais pas l'autre.
inspecter_var(sect2, "s2aq13")
inspecter_var(sect2, "s2aq13a")

# s2aq13c — type d'établissement fréquenté en 2018/2019
# Renseigné uniquement pour les scolarisés actuellement (s2aq13a == 1).
# Les NA représentent les non-scolarisés + non-éligibles — attendu.
# Variable nécessaire pour la tâche 11 (taux de scolarisation rural/urbain).
inspecter_var(sect2, "s2aq13c")


# =============================================================================
# BLOC 4 — Cohérence inter-fichiers et attrition
# =============================================================================
# Avant toute jointure, on vérifie trois points :
# 1. Tous les ménages de sect2 existent dans sect1 et secta
# 2. Les variables géographiques (zone, sector) sont identiques entre
#    sect2 et secta — ce qui permettra de ne pas joindre secta
# 3. Les ménages présents dans secta mais absents de sect2 sont expliqués
#    par l'attrition et non par une incohérence de la base

hhid_sect2 <- unique(sect2$hhid)
hhid_sect1 <- unique(sect1$hhid)
hhid_secta <- unique(secta$hhid)

cat("\n=== Couverture des ménages ===\n")
cat("sect2 :", length(hhid_sect2), "| sect1 :", length(hhid_sect1),
    "| secta :", length(hhid_secta), "\n")
cat("Ménages sect2 absents de sect1 :", sum(!hhid_sect2 %in% hhid_sect1), "\n")
cat("Ménages sect2 absents de secta :", sum(!hhid_sect2 %in% hhid_secta), "\n")
# 0 absent : tous les ménages de sect2 existent dans sect1 et secta.
# L'écart secta/sect2 (5 025 - 4 980 = 45) vient de ménages dans secta
# mais absents de sect2 — investigués ci-dessous.

# Cohérence zone et sector entre sect2 et secta
sect2_geo <- sect2 |> distinct(hhid, zone, sector)
secta_geo  <- secta |> select(hhid, zone_s = zone, sector_s = sector)
comp <- inner_join(sect2_geo, secta_geo, by = "hhid")
cat("Incohérences zone   :", sum(comp$zone   != comp$zone_s,   na.rm = TRUE), "\n")
cat("Incohérences sector :", sum(comp$sector != comp$sector_s, na.rm = TRUE), "\n")
rm(sect2_geo, secta_geo, comp)
# 0 incohérence : zone et sector dans sect2 sont fiables et utilisables
# directement sans joindre secta — même conclusion qu'au TP1 pour sect1.

# Investigation des 45 ménages dans secta mais absents de sect2
# Trois hypothèses initiales :
# H1 : tous membres < 3 ans -> sect2 ouvert mais s2aq2 == 2 partout
# H2 : module éducation non administré (refus partiel ou défaillance)
# H3 : ménage absent de sect1 également -> attrition totale
hhid_absents <- setdiff(hhid_secta, hhid_sect1)
cat("\nMénages absents de sect1 ET sect2 :", length(hhid_absents), "\n")
# H3 confirmée : ces 45 ménages sont absents des deux fichiers individuels.
# La cause est documentée dans secta via interview_result :

statut <- secta |>
  filter(hhid %in% hhid_absents) |>
  count(Statut = as_factor(interview_result)) |>
  mutate(Pct = round(n / sum(n) * 100, 1))
print(statut)

# 19 introuvables (UNABLE TO IDENTIFY) — ménage déménagé sans trace
# 12 indisponibles longue durée (LONG TERM UNAVAILABLE) — migration saisonnière
# 14 refus (REFUSED) — droit du répondant, protocole éthique NBS/LSMS
# -> Aucune interview complète -> absents de sect1 et sect2 par construction
# -> Attrition TOTALE (pas partielle) : 45 / 5 025 = 0.9%

# -> Taux de complétion post-harvest calculé : 4 980 / 5 025 = 99.1%
#    (ménages avec au moins une ligne dans sect1_harvestw4)
#
# -> Taux de complétion officiel Wave 4 : 4 976 / 5 025 = 98.9%
#    (NBS/World Bank, Basic Information Document GHS-Panel Wave 4)
#    Légèrement inférieur car il exige la complétion des DEUX visites
#    (post-planting juillet-septembre 2018 ET post-harvest janvier-
#    février 2019). Les 4 ménages d'écart (4 980 - 4 976 = 4) ont été
#    interviewés en post-harvest mais pas en post-planting.
#
# -> Impact négligeable sur les estimateurs descriptifs du TP2 :
#    45 ménages non-interviewés représentent 0.9% de l'échantillon,
#    répartis entre trois causes indépendantes (introuvables, indisponibles,
#    refus) sans lien avec le niveau d'éducation des membres.


# H1 réfutée : si tous les membres avaient moins de 3 ans, le ménage
# aurait quand même des lignes dans sect2 avec s2aq2 == 2.
# Or ces ménages sont totalement absents de sect2 — aucune ligne.
# -> L'interview n'a jamais été ouverte, pas seulement le module éducation.
#
# H2 réfutée : un refus partiel du module éducation laisserait aussi
# des lignes dans sect2 pour les autres modules administrés.
# Aucune ligne = aucun module administré = interview non complète.
#
# H3 confirmée : ces ménages sont absents de sect1 ET sect2.
# interview_result dans secta le prouve sans ambiguïté.
rm(hhid_sect2, hhid_sect1, hhid_secta, hhid_absents, statut)


# =============================================================================
# BLOC 5 — Doublons
# =============================================================================
# La clé unique individuelle est hhid + indiv :
# hhid seul n'est pas unique (plusieurs membres par ménage)
# indiv seul n'est pas unique (1, 2, 3... se répète dans chaque ménage)
# C'est la combinaison des deux qui identifie un individu de façon unique.

doublons <- sect2 |> group_by(hhid, indiv) |> filter(n() > 1)
cat("\nDoublons hhid + indiv :", nrow(doublons), "\n")
rm(doublons)
# 0 doublon -> intégrité de la base confirmée


# =============================================================================
# BLOC 6 — Jointure sect1_actifs + sect2
# =============================================================================
# DÉCISION MÉTHODOLOGIQUE : comment construire la population analytique ?
#
# Trois populations sont en jeu :
#   A. sect1 brut        : 30 337 membres (actifs + absents)
#   B. sect1_actifs      : ~25 767 membres (filtre s1q4a == 1)
#   C. sect2             : 26 557 membres (3 ans et plus, actifs et absents)
#
# La population analytique cible est B ∩ C :
# membres résidents actifs avec données éducation potentiellement disponibles.
#
# CHOIX DE JOINTURE : left_join depuis sect1_actifs vers sect2
#
# On part de sect1_actifs (B) comme table de gauche — c'est notre référence
# démographique. On joint sect2 dessus avec left_join.
#
# left_join garantit que TOUS les membres actifs sont conservés dans le
# résultat, qu'ils aient ou non une ligne dans sect2 :
#   - Membres actifs avec s2aq2 == 1 (3 ans+) : données éducation complètes
#   - Membres actifs avec s2aq2 == 2 (<3 ans) : NA sur variables éducation,
#     mais s2aq2 == 2 identifiable -> absence expliquée, non silencieuse
#   - Membres actifs absents de sect2 (cas rare) : s2aq2 == NA
#
# Pourquoi PAS inner_join ?
# inner_join ne garderait que les individus présents dans les DEUX tables.
# Les membres actifs de moins de 3 ans disparaîtraient silencieusement
# sans qu'on puisse le détecter ni l'expliquer dans le rapport.
#
# Pourquoi retire-t-on zone, state, sector, lga, ea de sect2 ?
# Ces 5 colonnes existent déjà dans sect1_actifs avec les mêmes valeurs
# (vérifié en BLOC 4 — 0 incohérence). Les dupliquer provoquerait la
# création automatique de suffixes par R (zone.x / zone.y) qui alourdissent
# la base et créent de la confusion dans les analyses suivantes.

sect1_actifs <- sect1 |>
  filter(s1q4a == 1) |>
  select(hhid, indiv, s1q2, s1q4, s1q3, sector, zone, state)

cat("sect1_actifs :", nrow(sect1_actifs), "membres résidents\n")
cat("Retirés      :", nrow(sect1) - nrow(sect1_actifs), "membres absents\n")

#3 780 membres partis du ménage (s1q4a == 2, 12.5%)
#   790 membres avec s1q4a NA (2.6%)
#     -> individus dont le statut de membership n'a pas été renseigné.
#        Incohérence mineure de collecte documentée au TP1.

df_educ <- sect1_actifs |>
  left_join(
    sect2 |> select(-zone, -state, -sector, -lga, -ea),
    by = c("hhid", "indiv")
  )

cat("\n=== Résultat de la jointure ===\n")
cat("Dimensions df_educ  :", nrow(df_educ), "x", ncol(df_educ), "\n")
cat("s2aq2 == 1 (3 ans+) :", sum(df_educ$s2aq2 == 1, na.rm = TRUE), "\n")
cat("s2aq2 == 2 (<3 ans) :", sum(df_educ$s2aq2 == 2, na.rm = TRUE), "\n")
cat("s2aq2 NA (absents)  :", sum(is.na(df_educ$s2aq2)), "\n")

cat("\n=== Valeurs manquantes variables clés post-jointure ===\n")
cat("s1q2  (sexe)            :", sum(is.na(df_educ$s1q2)),    "\n")
cat("s1q4  (âge)             :", sum(is.na(df_educ$s1q4)),    "\n")
cat("s2aq5 (alphabétisation) :", sum(is.na(df_educ$s2aq5)),   "\n")
cat("s2aq9 (niveau éduc)     :", sum(is.na(df_educ$s2aq9)),   "\n")
cat("s2aq13a (scolarisé)     :", sum(is.na(df_educ$s2aq13a)), "\n")

# Décomposition rigoureuse des NA sur s2aq9 — variable centrale
# Les NA sur s2aq9 ont quatre origines qu'on peut quantifier exactement :
#   1. s2aq2 == 2 : moins de 3 ans -> filtre entrée non franchi
#   2. s2aq6 == 2 : jamais scolarisé -> logique questionnaire (saut)
#   3. s2aq2 NA   : membre actif absent de sect2 (attrition individuelle)
#   4. Résidu     : vraies non-réponses (problématique si > 0)
# Si la somme 1+2+3 = total NA s2aq9, il n'y a aucune vraie non-réponse
# -> qualité de collecte excellente sur cette variable.

na_filtre  <- sum(df_educ$s2aq2 == 2, na.rm = TRUE)
na_jamais  <- sum(df_educ$s2aq6 == 2, na.rm = TRUE)
na_absents <- sum(is.na(df_educ$s2aq2))
na_total   <- sum(is.na(df_educ$s2aq9))
na_vraies  <- na_total - na_filtre - na_jamais - na_absents

cat("\nDécomposition NA s2aq9 :\n")
cat("  1. Moins de 3 ans (filtre s2aq2)    :", na_filtre,  "\n")
cat("  2. Jamais scolarisés (s2aq6 == 2)   :", na_jamais,  "\n")
cat("  3. Absents de sect2 (s2aq2 NA)      :", na_absents, "\n")
cat("  4. Vraies non-réponses (résidu)     :", na_vraies,  "\n")
cat("  Total NA s2aq9                      :", na_total,   "\n")

rm(na_filtre, na_jamais, na_absents, na_total, na_vraies, sect1_actifs)

# Décomposition des 6 866 NA sur s2aq9 :
#   1 438 : moins de 3 ans (s2aq2 == 2) — filtre d'entrée non franchi
#   5 428 : jamais scolarisés (s2aq6 == 2) — saut logique du questionnaire
#       0 : absents de sect2 — aucun membre actif sans ligne dans sect2
#       0 : vraies non-réponses — qualité de collecte excellente
#   Total : 1 438 + 5 428 = 6 866 — les NA s'expliquent intégralement.
#
# Même décomposition confirmée pour s2aq13a (6 866 NA identiques) :
# les deux variables obéissent aux mêmes filtres logiques du questionnaire.
# Aucune intervention de nettoyage n'est nécessaire sur ces variables.

# visualisation des valeurs manquante 
# Diagnostic visuel des valeurs manquantes — variables analytiques clés
# Confirme que les NA sont structurés (logique questionnaire) et non aléatoires
vars_cles <- df_educ |>
  select(s1q2, s1q4, sector, zone,
         s2aq2, s2aq5, s2aq6, s2aq9, s2aq10, s2aq13a)

p_miss <- vis_miss(vars_cles, sort_miss = TRUE) +
  labs(
    title   = "Diagnostic des valeurs manquantes — variables clés TP2",
    subtitle = "Nigeria GHS Panel W4 | Membres actifs (n = 25 767)",
    caption  = source_ghs
  )

ggsave(here("outputs", "figures", "fig00_diagnostic_NA_educ.png"),
       plot = p_miss, width = 12, height = 7, dpi = 300)
# =============================================================================
# BLOC 7 — Recodages et construction des variables analytiques
# =============================================================================
# Ce bloc prépare les variables qui seront utilisées dans 02_analyses.R.
# Toutes les variables recodées sont construites à partir des labels
# vérifiés en BLOC 3 — aucun recodage n'est fait à l'aveugle.

# =============================================================================
# BLOC 7 — Recodages et construction des variables analytiques
# =============================================================================
# Variables construites depuis les labels vérifiés en BLOC 3.
# Toutes les décisions de regroupement sont documentées ci-dessous.
#
# sexe        : s1q2 (1=Homme, 2=Femme) -> factor
# milieu      : sector (1=Urban, 2=Rural) -> factor
# alphabetise : s2aq5 (1=YES, 2=NO) -> binaire 1/0
#               NA = moins de 3 ans uniquement (0 vraie non-réponse confirmé)
#
# niveau_educ : s2aq9 (35 modalités) -> factor ordonné 6 niveaux
#   Aucun            : code 0 + jamais scolarisés (s2aq6 == 2)
#                      Séparés explicitement des NA (moins de 3 ans)
#   Primaire         : codes 1-3 (préscolaire) + 11-16 (Primary 1-6)
#                      Préscolaire rattaché au primaire (<5% échantillon)
#   Secondaire       : codes 21-28 (JSS 1-3 + SSS 1-3 + Lower/Upper 6)
#   Technique/Prof   : codes 31-35 + 321-322 + 411-412 (NCE, OND, HND...)
#   Tertiaire        : codes 41-43 + 421-424 (université + post-graduate)
#                      421-424 spécifiques à W4 (niveaux 100L à 600L)
#   Coranique/Adulte : codes 51-52 + 61 (système éducatif islamique parallèle)
#                      Maintenu distinct — fusion dans "Aucun" serait
#                      statistiquement et culturellement inexact
#
# scolarise_actuel : s2aq13a (1=YES, 2=NO) -> binaire 1/0
# groupe_age_large : s1q4 -> 5 tranches (tâche 10)
# adulte_18plus    : s1q4 >= 18 -> booléen (tâches 9-10)
# enfant_6_17      : 6 <= s1q4 <= 17 -> booléen (tâche 11)
# =============================================================================

df_educ <- df_educ |>
  mutate(
    sexe             = factor(s1q2, levels = c(1, 2),
                              labels = c("Homme", "Femme")),
    milieu           = factor(sector, levels = c(1, 2),
                              labels = c("Urbain", "Rural")),
    alphabetise      = case_when(s2aq5 == 1 ~ 1L,
                                 s2aq5 == 2 ~ 0L,
                                 TRUE       ~ NA_integer_),
    niveau_educ      = case_when(
      s2aq6 == 2                               ~ "Aucun",
      s2aq9 == 0                               ~ "Aucun",
      s2aq9 %in% c(1:3, 11:16)                ~ "Primaire",
      s2aq9 %in% c(21:28)                      ~ "Secondaire",
      s2aq9 %in% c(31:35, 321, 322, 411, 412)  ~ "Technique/Prof",
      s2aq9 %in% c(41, 43, 421:424)            ~ "Tertiaire",
      s2aq9 %in% c(51, 52, 61)                 ~ "Coranique/Adulte",
      TRUE                                     ~ NA_character_
    ) |> factor(levels = c("Aucun", "Primaire", "Secondaire",
                           "Technique/Prof", "Tertiaire",
                           "Coranique/Adulte"),
                ordered = TRUE),
    scolarise_actuel = case_when(s2aq13a == 1 ~ 1L,
                                 s2aq13a == 2 ~ 0L,
                                 TRUE         ~ NA_integer_),
    groupe_age_large = cut(s1q4,
                           breaks = c(0, 18, 31, 46, 61, Inf),
                           labels = c("0-17", "18-30", "31-45",
                                      "46-60", "61+"),
                           right  = FALSE,
                           include.lowest = TRUE),
    adulte_18plus    = if_else(s1q4 >= 18, TRUE, FALSE, missing = NA),
    enfant_6_17      = if_else(s1q4 >= 6 & s1q4 <= 17,
                               TRUE, FALSE, missing = NA)
  )

# Vérification des recodages
cat("\n=== Vérification des recodages ===\n")
cat("sexe :\n");            print(table(df_educ$sexe,             useNA = "always"))
cat("milieu :\n");          print(table(df_educ$milieu,           useNA = "always"))
cat("alphabetise :\n");     print(table(df_educ$alphabetise,      useNA = "always"))
cat("niveau_educ :\n");     print(table(df_educ$niveau_educ,      useNA = "always"))
cat("scolarise_actuel :\n");print(table(df_educ$scolarise_actuel, useNA = "always"))
cat("groupe_age_large :\n");print(table(df_educ$groupe_age_large, useNA = "always"))
cat("adulte_18plus :\n");   print(table(df_educ$adulte_18plus,    useNA = "always"))
cat("enfant_6_17 :\n");     print(table(df_educ$enfant_6_17,      useNA = "always"))

# RÉSULTATS ET VÉRIFICATIONS CROISÉES :
#
# sexe : 12 828 H / 12 939 F / 0 NA — équilibre quasi parfait (49.8%/50.2%)
#   Cohérent avec TP1. 0 NA confirme l'intégrité après jointure.
#
# milieu : 7 324 Urbain / 18 443 Rural / 0 NA
#   Cohérent avec TP1. Surreprésentation rurale volontaire du GHS Panel.
#
# alphabetise : 15 847 (1) / 8 482 (0) / 1 438 NA
#   1 438 NA = exactement les moins de 3 ans (s2aq2 == 2). Cohérent.
#   Taux brut sur éligibles : 15 847 / (15 847 + 8 482) = 65.1%
#
# niveau_educ : 1 438 NA = uniquement les moins de 3 ans.
#   Aucun        : 6 053 — UNION de jamais scolarisés (s2aq6==2, 5428) ET code 0 (s2aq9==0, 637)
#                  Note : chevauchement possible (individus avec s2aq6==2 ET s2aq9==0)
#   Primaire     : 8 120 — niveau le plus fréquent, cohérent avec s2aq9
#   Secondaire   : 6 568
#   Technique    : 1 225
#   Tertiaire    :   869
#   Coranique    : 1 494 — important, reflète le Nord nigérian
#   Total renseigné : 6053+8120+6568+1225+869+1494 = 24 329 = s2aq2==1 .ok
#
# Vérification arithmétique décomposition "Aucun" :
cat("\n=== Décomposition 'Aucun' ===\n")
cat("s2aq6 == 2 (jamais scolarisé) :", sum(df_educ$s2aq6 == 2, na.rm=TRUE), "\n")
cat("s2aq9 == 0 (aucun niveau)      :", sum(df_educ$s2aq9 == 0, na.rm=TRUE), "\n")
cat("Intersection (les deux)        :",
    sum(df_educ$s2aq6 == 2 & df_educ$s2aq9 == 0, na.rm=TRUE), "\n")
cat("Union (niveau_educ == 'Aucun') :",
    sum(df_educ$niveau_educ == "Aucun", na.rm=TRUE), "\n")
#
# scolarise_actuel : 9 069 scolarisés / 9 832 non / 6 866 NA
#   6 866 NA = moins de 3 ans (1 438) + jamais scolarisés (5 428). Cohérent.
#
# groupe_age_large : 0 NA — tous les membres actifs ont un âge renseigné.
#   0-17 : 12 576 (48.8%) — population très jeune, signature nigériane
#
# adulte_18plus : 13 191 adultes / 12 576 mineurs / 0 NA
#   Vérification : 12 576 (FALSE) = groupe 0-17 de groupe_age_large .ok
#
# enfant_6_17 : 8 952 enfants 6-17 ans — population cible tâche 11
#   Vérification : 8 952 < 12 576 (0-17) car les 0-5 ans sont exclus .ok

# =============================================================================
# Vérifications de cohérence supplémentaires
# =============================================================================

# Vérif 1 : Incohérence scolarisation actuelle vs passée
# Un individu scolarisé actuellement (s2aq13a == 1) devrait avoir
# s2aq6 == 1 (a déjà fréquenté l'école)
incoherent <- df_educ |>
  filter(s2aq13a == 1 & s2aq6 == 2)
cat("\n=== Vérification cohérence scolarisation ===\n")
cat("Scolarisés actuellement MAIS jamais allés à l'école :", nrow(incoherent), "\n")
cat("(Attendu : 0 incohérence)\n")
rm(incoherent)

# Vérif 2 : Distribution âge des moins de 3 ans
# Les s2aq2 == 2 (moins de 3 ans) devraient avoir s1q4 < 3
cat("\n=== Distribution âge des < 3 ans (s2aq2 == 2) ===\n")
print(summary(df_educ$s1q4[df_educ$s2aq2 == 2]))
cat("(Attendu : Max = 2)\n")

# =============================================================================
# BLOC 8 — Sauvegarde
# =============================================================================
# Format .rds : plus rapide que .dta ou .csv, conserve tous les attributs R
# (facteurs ordonnés, labels) et ne nécessite que base R pour être relu.

saveRDS(df_educ, here("data", "processed", "df_educ_clean.rds"))

cat("\n=== Fichier sauvegardé ===\n")
cat("Chemin  :", here("data", "processed", "df_educ_clean.rds"), "\n")
cat("Dimensions finales :", nrow(df_educ), "x", ncol(df_educ), "\n")
cat("Variables construites : sexe, milieu, alphabetise, niveau_educ,\n")
cat("                        scolarise_actuel, groupe_age_large,\n")
cat("                        adulte_18plus, enfant_6_17\n")

# =============================================================================
# La tâche 7 est complète.
# Les tâches 8 à 12 sont traitées dans 02_analyses.R.
# =============================================================================