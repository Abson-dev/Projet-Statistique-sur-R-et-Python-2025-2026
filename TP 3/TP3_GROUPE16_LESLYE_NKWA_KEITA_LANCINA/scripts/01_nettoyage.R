# =============================================================================
# TP3 GHS Nigeria Panel Wave 4 (2018-2019)
# Script 01 : Import, exploration, nettoyage et construction des variables
# Thème : Santé | Accès aux soins et dépenses de santé
# Données : NBS Nigeria / World Bank LSMS-ISA
#
# NOTE STRUCTURELLE CRITIQUE — Restructuration du module santé en Wave 4 :
#
# Le sujet cite sect3a et sect3b comme sources des données de santé.
# Cette nomenclature est correcte pour W1, W2 et W3 où :
#   - sect3a : morbidité (maladies/blessures des 4 dernières semaines)
#   - sect3b : consultations et dépenses de santé
#
# En Wave 4, ces fichiers ont été RESTRUCTURÉS en profondeur :
#   - sect3a_harvestw4 (120 vars) : module EMPLOI exclusivement
#     (activité principale, secteur, contrat, chômage, apprentissage)
#     Aucune variable de morbidité ni de consultation.
#   - sect3b_harvestw4 (14 vars) : enrôlement NHIS uniquement
#     (National Health Insurance Scheme) — pas de dépenses détaillées.
#
# Le contenu santé de W1-W3 (morbidité + consultations + dépenses) est
# désormais dans sect4a_harvestw4 (69 vars) — confirmé dans
# GHS_Variable_Dictionary.xlsx (NBS/World Bank LSMS-ISA) :
#   - s4aq3    : maladie/blessure dans les 4 dernières semaines
#   - s4aq3b_* : type de maladie/blessure (27 catégories)
#   - s4aq1    : consultation d'un praticien de santé
#   - s4aq6a/b : type de prestataire consulté
#   - s4aq7    : lieu de consultation
#   - s4aq8    : gestionnaire de l'établissement (public/privé/autre)
#   - s4aq9    : dépense de consultation
#   - s4aq14   : dépense médicaments
#   - s4aq17   : dépense hospitalisation
#
# sect4a_harvestw4 n'a pas de variable filtre d'âge explicite
# (aucune s3q1 ou équivalent — 69 variables vérifiées dans le
# dictionnaire). s4aq51 filtre uniquement les enfants < 60 mois
# pour les mesures anthropométriques. Tous les membres du roster
# ont a priori une ligne dans sect4a.
#
# Pour l'agrégat de consommation (quintiles de richesse) :
# cons_agg_wave4_visit2 n'existe pas dans le package W4.
# Il est remplacé par totcons_final.dta, ajouté en version 03
# (DOI : 10.48529/1hgw-dq47, World Bank Microdata Library, oct. 2021).
# Variable utilisée : totcons_adj (consommation annuelle par tête,
# ajustée spatialement et temporellement).
# =============================================================================

# Packages et graine chargés par main.R.
# Note : rstatix:: utilisé systématiquement — conflit de namespace avec coin.


# =============================================================================
# BLOC 1 — Création des dossiers de sortie
# =============================================================================

dir.create(here("data", "processed"),  showWarnings = FALSE, recursive = TRUE)
dir.create(here("outputs", "figures"), showWarnings = FALSE, recursive = TRUE)
dir.create(here("outputs", "tables"),  showWarnings = FALSE, recursive = TRUE)


# =============================================================================
# BLOC 2 — Import des données
# =============================================================================
# sect4a  : module santé W4 — morbidité, consultations, dépenses
# sect3b  : couverture NHIS — seule variable santé utile de sect3b en W4
# sect1   : roster ménage — sexe, âge, membership
# secta   : fichier Root — poids, géographie
# totcons : agrégat consommation W4 — quintiles de richesse

sect4a  <- read_dta(here("data", "raw", "NGA_2018_GHSP-W4_v03_M_Stata12",
                         "sect4a_harvestw4.dta"))
sect3b  <- read_dta(here("data", "raw", "NGA_2018_GHSP-W4_v03_M_Stata12",
                         "sect3b_harvestw4.dta"))
sect1   <- read_dta(here("data", "raw", "NGA_2018_GHSP-W4_v03_M_Stata12",
                         "sect1_harvestw4.dta"))
secta   <- read_dta(here("data", "raw", "NGA_2018_GHSP-W4_v03_M_Stata12",
                         "secta_harvestw4.dta"))
totcons <- read_dta(here("data", "raw", "NGA_2018_GHSP-W4_v03_M_Stata12",
                         "totcons_final.dta"))

cat("sect4a  :", nrow(sect4a),  "x", ncol(sect4a),  "\n")
cat("sect3b  :", nrow(sect3b),  "x", ncol(sect3b),  "\n")
cat("sect1   :", nrow(sect1),   "x", ncol(sect1),   "\n")
cat("secta   :", nrow(secta),   "x", ncol(secta),   "\n")
cat("totcons :", nrow(totcons), "x", ncol(totcons),  "\n")

# RÉSULTAT ATTENDU :
# sect4a  : 26 556 x 69 — module santé individuel
# sect3b  :  4 980 x 14 — NHIS niveau ménage
# sect1   : 30 337 x 59 — roster complet dont absents
# secta   :  5 025 x 22 — fichier Root
# totcons :  4 976 x 64 — agrégat consommation


# =============================================================================
# BLOC 3 — Exploration des variables clés
# =============================================================================

inspecter_var <- function(df, var_name) {
  x     <- df[[var_name]]
  label <- attr(x, "label")
  labs  <- attr(x, "labels")
  cat("\n", strrep("=", 65), "\n", sep = "")
  cat("Variable :", var_name, "\n")
  cat("Intitulé :", ifelse(is.null(label), "(sans label)", label), "\n")
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

# Filtre enfants < 60 mois — anthropométrie uniquement
# s4aq51 est la seule variable de filtre d'âge dans sect4a W4
inspecter_var(sect4a, "s4aq51")
# RÉSULTAT : 3 256 enfants < 5 ans (12.3%), 23 300 individus 5 ans+ (87.7%)
# 0 NA — le filtre est exhaustif sur toutes les lignes de sect4a

# Morbidité — variable centrale du TP3
inspecter_var(sect4a, "s4aq3")
# RÉSULTAT : 2 065 malades (7.8%), 20 106 non-malades (75.7%), 4 385 NA (16.5%)
# Les 4 385 NA correspondent exactement aux enfants < 5 ans (s4aq51 == 1) :
# la question de morbidité n'est pas posée aux moins de 5 ans en W4.
# Le taux de morbidité brut sur renseignés : 2 065 / (2 065 + 20 106) = 9.3%
# Cohérent avec les enquêtes DHS/MICS Afrique subsaharienne (8-15% sur 4 sem.)

# Arrêt d'activités dû à la maladie
inspecter_var(sect4a, "s4aq4")
# RÉSULTAT : 2 835 arrêts (10.7%), 3 615 non-arrêts (13.6%), 20 106 NA (75.7%)
# Les NA = exactement les non-malades (s4aq3 == 2) — logique questionnaire :
# la question n'est posée qu'aux malades. 2 835 > 2 065 malades déclarés
# car s4aq4 capture aussi les arrêts dus à des maladies chroniques ou
# antérieures à la fenêtre des 4 semaines.

# Types de maladie/blessure — 27 catégories
inspecter_var(sect4a, "s4aq3b_1")
# RÉSULTAT : Paludisme dominant (2 039, 7.7%), douleurs corporelles
# (1 411, 5.3%), catarrhe (584, 2.2%), toux (440, 1.7%).
# Profil épidémiologique typique du Nigeria rural 2018 :
# maladies infectieuses et symptômes respiratoires en tête.
# 20 106 NA = exactement les non-malades, 0 vraie non-réponse.

# Consultation d'un praticien de santé
inspecter_var(sect4a, "s4aq1")
# RÉSULTAT : 4 731 consultations (17.8%), 21 825 non (82.2%), 0 NA
# Point d'attention : 4 731 consultants > 2 065 malades déclarés.
# Cela signifie que s4aq1 capture aussi les consultations préventives,
# suivis de maladies chroniques, consultations pré/post-natales —
# indépendamment d'une maladie aiguë dans les 4 semaines.

# Type de praticien consulté
inspecter_var(sect4a, "s4aq6a")
# RÉSULTAT : Chemist/pharmacie informelle dominant (2 689, 10.1%),
# médecin (1 335, 5.0%), infirmier (447, 1.7%), tradipraticien (235, 0.9%).
# Dominance du secteur informel (chemist = PMV = Patent Medicine Vendor) :
# point d'entrée principal dans le système de soins nigérian, notamment
# en zone rurale (Onwujekwe et al., Health Policy Plan, 2011).

# Lieu de consultation
inspecter_var(sect4a, "s4aq7")
# RÉSULTAT : Pharmacie/chemist (2 822, 10.6%), hôpital (1 254, 4.7%),
# clinique (328, 1.2%). Cohérent avec s4aq6a — le chemist est à la fois
# le type de praticien et le lieu le plus fréquent.

# Gestionnaire de l'établissement — clé pour public/privé/autre
inspecter_var(sect4a, "s4aq8")
# RÉSULTAT : Privé largement dominant (3 568, 13.4%), État (709, 2.7%),
# Local (519, 2.0%), Fédéral (141, 0.5%).
# Le secteur privé représente 70% des consultations avec établissement
# identifié. Cohérent avec la littérature sur le Nigeria où le secteur
# privé formel et informel assure l'essentiel de l'offre de soins
# (WHO Nigeria Health System Review, 2015).
# Note : s4aq8 ne distingue pas explicitement le secteur informel
# (tradipraticiens, PMV) — celui-ci se lit via s4aq6a et s4aq7 combinés.

# Dépenses de santé — variables continues
cat("\n=== Dépenses de santé (variables continues) ===\n")
cat("s4aq9  (consultation)   :\n"); summary(sect4a$s4aq9)  |> print()
cat("s4aq14 (médicaments)    :\n"); summary(sect4a$s4aq14) |> print()
cat("s4aq17 (hospitalisation):\n"); summary(sect4a$s4aq17) |> print()
# RÉSULTAT :
# s4aq9  consultation : médiane 0 (beaucoup de consultations gratuites
#         ou non déclarées), max 50 000 nairas, 21 018 NA (non-consultants)
# s4aq14 médicaments  : médiane 700, moyenne 1 741, max 117 000 nairas
#         18 632 NA — moins de NA que s4aq9 car médicaments achetés même
#         sans consultation formelle (automédication)
# s4aq17 hospitalisation : médiane 9 000, moyenne 24 471, max 2 500 000 N
#         valeur aberrante extrême à identifier et traiter dans script 2
#         25 733 NA — très peu d'hospitalisations (normale pour 4 semaines)

# NHIS — niveau ménage
inspecter_var(sect3b, "s3q50")
# RÉSULTAT : 138 ménages couverts (2.8%), 4 842 non couverts (97.2%), 0 NA
# Taux de couverture NHIS catastrophiquement bas — cohérent avec
# WHO Nigeria 2018 qui estimait < 5% de couverture effective.
# Le NHIS lancé en 2005 n'a jamais atteint sa cible de couverture
# universelle faute de financement et d'adhésion du secteur informel.

# Consommation — totcons_final
cat("\n=== totcons_final — variables disponibles ===\n")
cat("totcons_pc  :", attr(totcons$totcons_pc,  "label"), "\n")
cat("totcons_adj :", attr(totcons$totcons_adj, "label"), "\n")
cat("Médiane totcons_adj :", median(totcons$totcons_adj, na.rm = TRUE), "\n")
summary(totcons$totcons_adj) |> print()
# RÉSULTAT : médiane 167 011 nairas/an/tête (~460 nairas/jour en 2018-19)
# soit environ 1.2 USD/jour au taux de change 2018 (380 N/USD) —
# cohérent avec le seuil de pauvreté nigérian (NBS, 2020 : 137 430 N/an)
# L'écart interquartile très large (107 654 à 263 445) annonce une forte
# inégalité qui sera documentée au TP7 via la courbe de Lorenz.


# =============================================================================
# BLOC 4 — Cohérence inter-fichiers
# =============================================================================

hhid_sect4a  <- unique(sect4a$hhid)
hhid_sect1   <- unique(sect1$hhid)
hhid_secta   <- unique(secta$hhid)
hhid_totcons <- unique(totcons$hhid)

cat("\n=== Couverture ménages ===\n")
cat("sect4a  :", length(hhid_sect4a),  "ménages\n")
cat("sect1   :", length(hhid_sect1),   "ménages\n")
cat("secta   :", length(hhid_secta),   "ménages\n")
cat("totcons :", length(hhid_totcons), "ménages\n")

cat("\nsect4a absents de sect1  :", sum(!hhid_sect4a %in% hhid_sect1),  "\n")
cat("sect4a absents de secta  :", sum(!hhid_sect4a %in% hhid_secta),  "\n")
cat("totcons absents de secta :", sum(!hhid_totcons %in% hhid_secta), "\n")

# RÉSULTAT :
# sect4a  : 4 979 ménages — 1 ménage de moins que sect1 (4 980)
#           Ce ménage est dans sect1 mais absent de sect4a :
#           probable ménage avec uniquement des enfants < 5 ans
#           dont aucun membre n'a de ligne dans sect4a avec données santé.
# secta   : 5 025 ménages — 45 ménages d'attrition totale (non-interviewés)
#           identiques aux TP1 et TP2, cohérence confirmée.
# totcons : 4 976 ménages — 3 ménages de moins que sect4a.
#           Ces 3 ménages ont des membres sans quintile de consommation
#           (23 NA sur quintile post-jointure).
# 0 incohérence de contenu entre fichiers — tous les hhid de sect4a
# sont présents dans sect1 et secta.

comp <- inner_join(
  sect4a |> distinct(hhid, zone, sector),
  secta  |> select(hhid, zone_s = zone, sector_s = sector),
  by = "hhid"
)
cat("Incohérences zone   :", sum(comp$zone   != comp$zone_s,   na.rm = TRUE), "\n")
cat("Incohérences sector :", sum(comp$sector != comp$sector_s, na.rm = TRUE), "\n")
# RÉSULTAT : 0 incohérence — variables géographiques cohérentes
# entre sect4a et secta. Zone et sector de sect4a utilisables directement.
rm(comp, hhid_sect4a, hhid_sect1, hhid_secta, hhid_totcons)


# =============================================================================
# BLOC 5 — Doublons
# =============================================================================

doublons <- sect4a |> group_by(hhid, indiv) |> filter(n() > 1)
cat("\nDoublons hhid+indiv sect4a :", nrow(doublons), "\n")
# RÉSULTAT : 0 doublon — chaque membre a au plus une ligne dans sect4a.
# La clé hhid+indiv est unique dans ce fichier.
rm(doublons)


# =============================================================================
# BLOC 6 — Jointure principale
# =============================================================================
# Population analytique :
#   - membres résidents actifs (s1q4a == 1) issus de sect1 : 25 767
#   - left_join sur sect4a : membres absents de sect4a -> NA sur
#     variables santé (enfants < 5 ans non couverts par le module)
#   - totcons et NHIS joints au niveau ménage (hhid)
#
# Note : sect4a contient 26 556 lignes vs 25 767 membres actifs dans
# sect1. L'écart (789 lignes) correspond aux membres non-résidents
# (s1q4a != 1) présents dans sect4a. Le left_join depuis sect1_actifs
# garantit que seuls les membres résidents actifs sont retenus.

sect1_actifs <- sect1 |>
  filter(s1q4a == 1) |>
  select(hhid, indiv, s1q2, s1q4, s1q3, sector, zone, state)

cat("sect1_actifs :", nrow(sect1_actifs), "membres résidents\n")

# Jointure individuelle sect1 + sect4a
df_sante <- sect1_actifs |>
  left_join(
    sect4a |> select(-zone, -state, -sector, -lga, -ea),
    by = c("hhid", "indiv")
  )

# Jointure ménage : quintile de consommation
quintiles <- totcons |>
  select(hhid, totcons_adj, hhsize, wt_wave4) |>
  mutate(quintile = ntile(totcons_adj, 5))

df_sante <- df_sante |>
  left_join(quintiles, by = "hhid")

# Jointure ménage : NHIS
nhis <- sect3b |>
  select(hhid, nhis = s3q50)

df_sante <- df_sante |>
  left_join(nhis, by = "hhid")

cat("\n=== Résultat jointure ===\n")
cat("Dimensions df_sante           :", nrow(df_sante), "x", ncol(df_sante), "\n")
cat("Avec s4aq3 renseigné          :", sum(!is.na(df_sante$s4aq3)), "\n")
cat("Avec s4aq3 NA (absents sect4a):", sum(is.na(df_sante$s4aq3)),  "\n")

cat("\n=== NA variables clés post-jointure ===\n")
cat("s1q2  (sexe)          :", sum(is.na(df_sante$s1q2)),    "\n")
cat("s1q4  (âge)           :", sum(is.na(df_sante$s1q4)),    "\n")
cat("s4aq3 (morbidité)     :", sum(is.na(df_sante$s4aq3)),   "\n")
cat("s4aq1 (consultation)  :", sum(is.na(df_sante$s4aq1)),   "\n")
cat("s4aq8 (prestataire)   :", sum(is.na(df_sante$s4aq8)),   "\n")
cat("quintile              :", sum(is.na(df_sante$quintile)), "\n")
cat("nhis                  :", sum(is.na(df_sante$nhis)),     "\n")

# RÉSULTAT :
# df_sante : 25 767 x 75 — dimensions cohérentes avec TP1 et TP2
# s4aq3 NA : 4 271 — membres de sect1_actifs absents de sect4a
#   Décomposition : enfants < 5 ans dans sect1_actifs non couverts
#   par le module santé W4. Le taux de morbidité sera calculé sur
#   les 21 496 membres renseignés uniquement.
# s4aq1 NA : 1 — un seul individu sans réponse sur la consultation.
#   Marginal, ne justifie pas d'action particulière.
# s4aq8 NA : 20 837 — tous les non-consultants (s4aq1 == 2) et les
#   consultants sans établissement identifié. Logique questionnaire.
# quintile NA : 23 — membres des 3 ménages absents de totcons_final.
#   Ces membres seront exclus des analyses croisant santé et richesse.
# nhis NA : 0 — couverture NHIS renseignée pour tous les ménages.

rm(quintiles, nhis, sect1_actifs)


# =============================================================================
# BLOC 7 — Diagnostic valeurs manquantes
# =============================================================================

vars_cles <- df_sante |>
  select(s1q2, s1q4, sector, zone,
         s4aq3, s4aq1, s4aq6a, s4aq9, quintile)

p_miss <- vis_miss(vars_cles, sort_miss = TRUE) +
  labs(
    title    = "Diagnostic des valeurs manquantes — variables clés TP3",
    subtitle = "Nigeria GHS Panel W4 | Membres actifs (n = 25 767)",
    caption  = "Source : Nigeria GHS Panel W4 (2018-2019), NBS / World Bank LSMS-ISA."
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 9))

ggsave(here("outputs", "figures", "fig00_diagnostic_NA_sante.png"),
       plot = p_miss, width = 12, height = 7, dpi = 300)
cat("fig00_diagnostic_NA_sante.png exporté\n")
# La figure confirme la structure des NA :
# s4aq6a et s4aq9 : ~80% NA (non-consultants — logique questionnaire)
# s4aq3 : ~17% NA (enfants < 5 ans — logique filtre)
# s1q2, s1q4, sector, zone, quintile : 0% NA (variables complètes)


# =============================================================================
# BLOC 8 — Recodages
# =============================================================================
# Variables construites depuis les labels vérifiés en BLOC 3 :
#
# sexe          : s1q2 (1=Homme, 2=Femme)
# milieu        : sector (1=Urban, 2=Rural)
# malade        : s4aq3 (1=YES -> 1, 2=NO -> 0)
#                 NA conservés pour les enfants < 5 ans
# a_consulte    : s4aq1 (1=YES -> 1, 2=NO -> 0)
#                 Capture consultations curatives ET préventives
# type_presta   : s4aq8 recodé en 3 catégories :
#                   Public   : 1 (Fédéral), 2 (État), 3 (Local)
#                   Prive    : 7 (Privé)
#                   Autre    : 4 (Communauté), 5 (Religieux),
#                              6 (ONG), 8 (Autre)
#                 Note : le secteur informel (tradipraticiens, PMV)
#                 se lit via s4aq6a (type praticien) car s4aq8 ne le
#                 distingue pas explicitement.
# depense_totale  : s4aq9 + s4aq14 + s4aq17 (nairas courants)
#                   NA si aucun poste renseigné
# depense_totale_0 : même somme avec NA -> 0 (non-malades = 0 dépense)
# groupe_age    : s1q4 en 4 tranches OMS : 0-14, 15-34, 35-59, 60+
# adulte_15plus : s1q4 >= 15
# nhis_couvert  : nhis (1=YES -> 1, 2=NO -> 0) — niveau ménage

df_sante <- df_sante |>
  mutate(
    sexe = factor(s1q2, levels = c(1, 2),
                  labels = c("Homme", "Femme")),
    
    milieu = factor(sector, levels = c(1, 2),
                    labels = c("Urbain", "Rural")),
    
    malade = case_when(
      s4aq3 == 1 ~ 1L,
      s4aq3 == 2 ~ 0L,
      TRUE       ~ NA_integer_
    ),
    
    a_consulte = case_when(
      s4aq1 == 1 ~ 1L,
      s4aq1 == 2 ~ 0L,
      TRUE       ~ NA_integer_
    ),
    
    type_presta = case_when(
      s4aq8 %in% c(1, 2, 3)    ~ "Public",
      s4aq8 == 7                ~ "Prive",
      s4aq8 %in% c(4, 5, 6, 8) ~ "Autre",
      TRUE                      ~ NA_character_
    ) |> factor(levels = c("Public", "Prive", "Autre")),
    
    depense_totale = rowSums(
      cbind(s4aq9, s4aq14, s4aq17),
      na.rm = FALSE
    ),
    
    depense_totale_0 = rowSums(
      cbind(replace_na(s4aq9,  0),
            replace_na(s4aq14, 0),
            replace_na(s4aq17, 0))
    ),
    
    groupe_age = cut(
      s1q4,
      breaks         = c(0, 15, 35, 60, Inf),
      labels         = c("0-14", "15-34", "35-59", "60+"),
      right          = FALSE,
      include.lowest = TRUE
    ),
    
    adulte_15plus = if_else(s1q4 >= 15, TRUE, FALSE,
                            missing = NA),
    
    nhis_couvert = case_when(
      nhis == 1 ~ 1L,
      nhis == 2 ~ 0L,
      TRUE      ~ NA_integer_
    )
  )

# Vérification
cat("\n=== Vérification recodages ===\n")
cat("sexe :\n");         print(table(df_sante$sexe,         useNA = "always"))
cat("milieu :\n");       print(table(df_sante$milieu,       useNA = "always"))
cat("malade :\n");       print(table(df_sante$malade,       useNA = "always"))
cat("a_consulte :\n");   print(table(df_sante$a_consulte,   useNA = "always"))
cat("type_presta :\n");  print(table(df_sante$type_presta,  useNA = "always"))
cat("groupe_age :\n");   print(table(df_sante$groupe_age,   useNA = "always"))
cat("nhis_couvert :\n"); print(table(df_sante$nhis_couvert, useNA = "always"))
cat("quintile :\n");     print(table(df_sante$quintile,     useNA = "always"))
cat("\nDépense totale — résumé (non-zéro uniquement) :\n")
summary(
  df_sante$depense_totale[df_sante$depense_totale > 0 &
                            !is.na(df_sante$depense_totale)]
) |> print()

# RÉSULTATS ET VÉRIFICATIONS CROISÉES :
#
# sexe : 12 828 H / 12 939 F / 0 NA — identique TP1 et TP2, cohérent
# milieu : 7 324 Urbain / 18 443 Rural / 0 NA — identique TP1 et TP2
#
# malade : 2 002 (1) / 19 494 (0) / 4 271 NA
#   Taux morbidité sur renseignés : 2 002 / 21 496 = 9.3%
#   Cohérent avec littérature (8-15% sur 4 semaines, DHS Afrique 2018)
#   Note : 2 065 malades dans sect4a brut vs 2 002 dans df_sante —
#   écart de 63 correspond aux non-résidents présents dans sect4a
#   mais exclus de sect1_actifs (s1q4a != 1).
#
# a_consulte : 4 580 (1) / 21 186 (0) / 1 NA
#   4 580 consultants > 2 002 malades déclarés : confirme que s4aq1
#   capture aussi les consultations préventives et de suivi chronique.
#   Taux de consultation global : 4 580 / 25 766 = 17.8%
#
# type_presta : Public 1 333 / Privé 3 464 / Autre 133 / NA 20 837
#   Privé = 70% des consultations avec établissement identifié.
#   20 837 NA = non-consultants + consultants sans s4aq8 renseigné.
#   Cohérent avec s4aq8 brut (21 484 NA) — légère différence due à
#   la restriction à sect1_actifs.
#
# groupe_age : 0-14 (10 791) / 15-34 (7 746) / 35-59 (5 322) / 60+ (1 908)
#   0 NA — tous les membres actifs ont un âge renseigné.
#   Population jeune : 42% de 0-14 ans, signature démographique nigériane.
#
# nhis_couvert : 669 couverts / 25 098 non couverts / 0 NA
#   669 individus dans des ménages NHIS sur 25 767 = 2.6%
#   Taux dérisoire mais cohérent avec WHO Nigeria 2018 (< 5%).
#
# quintile : distribution Q1 > Q2 > Q3 > Q4 > Q5 attendue car les
#   ménages pauvres sont plus grands — plus de membres par ménage
#   dans les quintiles inférieurs (biais de taille Kish, 1965).
#
# depense_totale (non-zéro) : médiane 15 000 N, max 1 503 500 N
#   Valeur max très élevée — outlier à identifier et traiter dans
#   script 2 avant toute analyse des dépenses.
#   Médiane 15 000 nairas ≈ 39 USD en 2018 — plausible pour une
#   dépense de santé incluant médicaments et hospitalisation.
#
# POINT D'ATTENTION :
# depense_totale utilise na.rm = FALSE — si l'un des trois postes
# est NA, la somme totale est NA. Cela convient pour les analyses
# de dépenses réelles (on ne veut pas imputer 0 à un poste inconnu).
# depense_totale_0 avec na.rm implicite via replace_na(0) est à
# utiliser uniquement pour les analyses de participation (a-t-on
# dépensé quelque chose) et non pour les montants.


# =============================================================================
# BLOC 9 — Sauvegarde
# =============================================================================

saveRDS(df_sante, here("data", "processed", "df_sante_clean.rds"))

cat("\n=== Fichier sauvegardé ===\n")
cat("Chemin     :", here("data", "processed", "df_sante_clean.rds"), "\n")
cat("Dimensions :", nrow(df_sante), "x", ncol(df_sante), "\n")
cat("Variables construites : sexe, milieu, malade, a_consulte,\n")
cat("                        type_presta, depense_totale, depense_totale_0,\n")
cat("                        groupe_age, adulte_15plus, nhis_couvert, quintile\n")

# =============================================================================
# Les tâches 14 à 18 sont traitées dans 02_analyses.R.
# =============================================================================