# =============================================================================
# TP1 GHS Nigeria Panel Wave 4 (2018-2019)
# Script 01 : Import, exploration, nettoyage et construction des variables
# Thème : Démographie ║ Profil démographique des ménages 
# Données : Nigeria General Household Survey Panel W4 — NBS / World Bank LSMS-ISA
# =============================================================================

# Les packages et la graine aléatoire sont chargés par main.R.
# Ce script suppose que main.R a été exécuté au préalable.

# Création automatique des dossiers de sortie
# showWarnings = FALSE : aucune erreur si le dossier existe déjà
dir.create(here("data", "processed"),  showWarnings = FALSE, recursive = TRUE)
dir.create(here("outputs", "figures"), showWarnings = FALSE, recursive = TRUE)
dir.create(here("outputs", "tables"),  showWarnings = FALSE, recursive = TRUE)
# =============================================================================
# TÂCHE 1.1 — Import des données
# =============================================================================
# haven::read_dta() est utilisé plutôt que foreign::read.dta() car il conserve
# les labels Stata sous forme d'attributs R consultables avec attr(x, "labels"),
# produit des colonnes de type dbl+lbl compatibles avec le tidyverse, et gère
# correctement les fichiers Stata 13 et supérieurs que foreign ne lit pas.

sect1 <- read_dta(here("data", "raw", "NGA_2018_GHSP-W4_v03_M_Stata12",
                       "sect1_harvestw4.dta"))

secta <- read_dta(here("data", "raw", "NGA_2018_GHSP-W4_v03_M_Stata12",
                       "secta_harvestw4.dta"))

# sect1 : 30 337 lignes x 59 colonnes — niveau individu (une ligne par membre)
# secta :  5 025 lignes x 22 colonnes — niveau ménage (une ligne par ménage)
# Le rapport 30 337 / 5 025 ≈ 6 membres par ménage en moyenne, cohérent
# avec la taille typique des ménages nigérians.
# sector et zone sont déjà présentes dans sect1 : la jointure avec secta
# n'est pas nécessaire pour les analyses du TP 1.

# --- Vérification de cohérence entre sect1 et secta ---
# Les deux fichiers proviennent de la même enquête. On vérifie que :
# 1. Tous les ménages de sect1 existent dans secta (et inversement)
# 2. Les variables communes (sector, zone) ont les mêmes valeurs

# 1. Couverture des ménages
hhid_sect1 <- unique(sect1$hhid)
hhid_secta <- unique(secta$hhid)

cat("=== Vérification de cohérence sect1 / secta ===\n")
cat("Ménages dans sect1 :", length(hhid_sect1), "\n")
cat("Ménages dans secta :", length(hhid_secta), "\n")
cat("Ménages de sect1 absents de secta :", sum(!hhid_sect1 %in% hhid_secta), "\n")
cat("Ménages de secta absents de sect1 :", sum(!hhid_secta %in% hhid_sect1), "\n")

# 2. Cohérence des variables sector et zone
# On extrait une ligne par ménage dans sect1 pour comparer au niveau ménage
sect1_menage <- sect1 %>%
  distinct(hhid, .keep_all = TRUE) %>%
  select(hhid, sector_sect1 = sector, zone_sect1 = zone)

secta_menage <- secta %>%
  select(hhid, sector_secta = sector, zone_secta = zone)

comparaison <- inner_join(sect1_menage, secta_menage, by = "hhid")

# Nombre de ménages où sector diffère entre les deux fichiers
incoherences_sector <- sum(comparaison$sector_sect1 != comparaison$sector_secta, na.rm = TRUE)
incoherences_zone   <- sum(comparaison$zone_sect1 != comparaison$zone_secta, na.rm = TRUE)

cat("Incohérences sur sector :", incoherences_sector, "\n")
cat("Incohérences sur zone   :", incoherences_zone, "\n")

# Si 0 incohérence : les deux fichiers sont parfaitement alignés.
# Cela confirme que sector et zone dans sect1 sont fiables et qu'on peut
# les utiliser directement sans devoir joindre secta.
# Si des incohérences existaient, il faudrait privilégier secta (fichier
# ménage de référence) et joindre ses variables à sect1.

rm(sect1_menage, secta_menage, comparaison, hhid_sect1, hhid_secta)  # nettoyage mémoire


# =============================================================================
# TÂCHE 1.2 — Exploration structurée
# =============================================================================
# str() révèle les types bruts et les attributs Stata (dbl+lbl, labels).
# glimpse() montre les valeurs réelles sur une ligne par variable.
# Ces deux fonctions sont complémentaires : str() pour la structure,
# glimpse() pour un aperçu rapide des valeurs.
# summary() ne sera appliqué qu'à s1q4 (âge), seule variable quantitative
# continue pertinente à ce stade. L'appliquer au sexe ou au lien de parenté
# n'aurait aucun sens statistique : on ne calcule pas de moyenne sur des codes
# numériques représentant des catégories.

str(sect1, list.len = 20)
glimpse(sect1)
summary(sect1$s1q4)

# Variable quantitative — summary a du sens ici

# Résultats summary(s1q4) :
# Min=0, Q1=8, Médiane=18, Moyenne=23.98, Q3=36, Max=130, NA=3780
# La médiane à 18 ans confirme une population très jeune.
# La moyenne (23.98) supérieure à la médiane (18) indique une asymétrie
# positive : des personnes très âgées tirent la moyenne vers le haut.
# Le maximum à 130 ans est biologiquement impossible et sera traité ci-dessous.
# Les 3780 NA correspondent aux membres qui ont quitté le ménage (s1q4a == 2).

table(sect1$s1q4[sect1$s1q4 > 90], useNA = "always")
attr(sect1$s1q4, "labels")
#cela nous montre NULL ce qui signifie que la variable n'a pas de labels Stata attachés 
#cest une variable numérique brute. Les valeurs 98 et 99 ne sont donc pas des codes conventionnels encodés dans le fichier
#ce sont des âges déclarés, et seul 130 est un peu extreme.

# Variables qualitatives — fréquences absolues et relatives
# On n'utilise jamais summary() sur ces variables : calculer une moyenne
# sur des codes numériques représentant des catégories (1=Homme, 2=Femme)
# n'a aucun sens statistique. On utilise table() + prop.table().
# useNA = "always" force l'affichage des NA même si leur effectif est zéro.

table(sect1$s1q2, useNA = "always")
round(prop.table(table(sect1$s1q2, useNA = "always")) * 100, 1)

# 1 (Homme) : 13 189 (43.5%), 2 (Femme) : 13 368 (44.1%), NA : 3 780 (12.5%)
# Les NA représentent exactement les membres partis — cohérent avec s1q4a.
# Hors NA : 13 189 / (13 189 + 13 368) = 49.7% hommes, 50.3% femmes.
# Équilibre quasi parfait, typique de la démographie nigériane.

table(sect1$s1q3, useNA = "always")
round(prop.table(table(sect1$s1q3, useNA = "always")) * 100, 1)
# Chef (1) : 16.4%, Conjoint (2) : 14.1%, Enfant biologique (3) : 48.2%
# Beau-fils/belle-fille (4) : 0.6%, Adopté (5) : 0.3%
# Petit-enfant (6) : 3.4%, Frère/Sœur (7) : 1.6%
# Les enfants biologiques seuls représentent 48.2% — signature d'un pays
# à forte fécondité (ISF ≈ 5.3 en 2018). La modalité 13 est absente :
# supprimée lors d'une révision du questionnaire GHS.
# 12 (Domestic Help) : 0.1% — employés de maison résidants, réalité
# courante dans les ménages aisés nigérians urbains.

table(sect1$s1q4a, useNA = "always")
round(prop.table(table(sect1$s1q4a, useNA = "always")) * 100, 1)
# Membres actifs (1) : 25 767 (84.9%)
# Membres partis (2) : 3 780 (12.5%)
# NA sur s1q4a lui-même : 790 (2.6%) — ces individus n'ont pas de réponse
# à la question de membership, ce qui est une incohérence mineure dans
# la collecte. Ils seront exclus par le filtre s1q4a == 1.

table(sect1$sector, useNA = "always")
round(prop.table(table(sect1$sector, useNA = "always")) * 100, 1)
# Urbain (1) : 8 719 (28.7%), Rural (2) : 21 618 (71.3%), NA : 0
# Surreprésentation rurale volontaire du GHS Panel. Le taux d'urbanisation
# réel du Nigeria en 2018 était d'environ 52% (Banque Mondiale).
# Aucun NA : la stratification géographique est complète.

table(sect1$zone, useNA = "always")
round(prop.table(table(sect1$zone, useNA = "always")) * 100, 1)
# North Central (1) : 16.5%, North East (2) : 21.0%, North West (3) : 21.8%
# South East (4) : 13.3%, South South (5) : 15.4%, South West (6) : 11.9%
# Le Nord représente 59.3% de l'échantillon contre 40.7% pour le Sud.
# Cela reflète la réalité démographique : le Nord est la région la plus
# peuplée et la plus rurale du Nigeria, avec des États comme Kano et
# Kaduna qui comptent parmi les plus peuplés du pays.
# Aucun NA : toutes les observations ont une zone géopolitique renseignée.

# =============================================================================
# TÂCHE 1.3 — Vérification des labels des variables clés
# =============================================================================
# Avant tout recodage, on vérifie les modalités réelles encodées dans le fichier.
# On ne recode jamais sans avoir d'abord vérifié les labels originaux.

print(attr(sect1$s1q2,   "labels"))   # sexe       : 1=MALE, 2=FEMALE
print(attr(sect1$s1q3,   "labels"))   # parenté    : 14 modalités
print(attr(sect1$s1q4a,  "labels"))   # actif      : 1=YES, 2=NO
print(attr(sect1$sector, "labels"))   # milieu     : 1=Urban, 2=Rural
print(attr(sect1$zone,   "labels"))   # géopolitique: 6 zones

# s1q3 a 14 modalités dont 12=DOMESTIC HELP (employé de maison résident).
# La modalité 13 est absente — supprimée lors d'une révision du questionnaire.
# ATTENTION : zone encode les 6 zones géopolitiques du Nigeria (North Central,
# North East, North West, South East, South South, South West). Elle ne
# distingue PAS rural et urbain. Pour toute analyse rural/urbain, utiliser
# exclusivement sector (1=Urban, 2=Rural).


# =============================================================================
# TÂCHE 1.4 — Filtre des membres actifs
# =============================================================================
# s1q4a == 2 identifie les personnes qui figuraient dans le panel lors d'une
# vague précédente mais qui ont quitté le ménage avant 2018 : décès, mariage,
# migration pour travail ou études. Leurs variables démographiques (âge, sexe,
# parenté) sont toutes NA car l'enquêteur ne les a pas interrogées.
# Les inclure biaiserait la taille des ménages, la distribution des âges
# et toutes les proportions calculées.

n_avant <- nrow(sect1)
sect1_clean <- sect1 %>% filter(s1q4a == 1)
n_apres    <- nrow(sect1_clean)

# Résultat : 30 337 → 25 767 membres actifs. 4 570 individus retirés (15%).
# Au Nigeria, cette mobilité s'explique principalement par la migration des
# jeunes hommes vers Lagos et Abuja pour le travail, les mariages qui font
# quitter les filles de leur ménage d'origine, et la mortalité.

cat("Membres retirés :", n_avant - n_apres, "sur", n_avant,
    "(", round((n_avant - n_apres) / n_avant * 100, 1), "%)\n")


# =============================================================================
# TÂCHE 1.5 — Vérification des doublons
# =============================================================================
# La clé unique d'identification d'un individu dans cette base est hhid + indiv.
# Un doublon non détecté gonflerait artificiellement la taille des ménages
# et fausserait toutes les distributions et les tests statistiques.

doublons <- sect1_clean %>%
  group_by(hhid, indiv) %>%
  filter(n() > 1)

cat("Doublons détectés :", nrow(doublons), "\n")
# Résultat : 0 doublon. La clé hhid + indiv est bien unique dans ce fichier.


# =============================================================================
# TÂCHE 1.6 — Traitement de la valeur aberrante sur l'âge
# =============================================================================
# Le summary a révélé un maximum de 130 ans. On identifie l'individu concerné.

sect1_clean %>%
  filter(s1q4 > 100) %>%
  select(hhid, indiv, s1q4, sector, zone, s1q3)

# Résultat : 1 seul individu. hhid=169068, indiv=1, âge=130, Rural,
# South East, HEAD (chef de ménage).
# 130 ans est biologiquement impossible (record mondial certifié : 122 ans).
# Il s'agit très probablement d'une erreur de saisie de l'enquêteur.
# On ne supprime pas cet individu car il est chef de ménage — le supprimer
# ferait disparaître l'ensemble de son ménage de l'analyse.
# On remplace son âge par NA et on le signale dans le rapport.

sect1_clean <- sect1_clean %>%
  mutate(s1q4 = ifelse(s1q4 > 100, NA_real_, s1q4))

cat("Individus avec âge > 100 après correction :",
    sum(sect1_clean$s1q4 > 100, na.rm = TRUE), "\n")
# retourne 0.


# =============================================================================
# TÂCHE 1.7 — Diagnostic des valeurs manquantes
# =============================================================================
# vis_miss() produit une carte visuelle où chaque cellule est colorée selon
# qu'elle est présente ou manquante. Elle révèle si les NA sont aléatoirement
# distribués (MCAR) ou structurés (par exemple, tous sur les mêmes lignes),
# ce qu'un simple sum(is.na()) ne permet pas de voir.

vars_cles <- sect1_clean %>%
  select(s1q2, s1q3, s1q4, sector, zone)

p_miss <- vis_miss(vars_cles, sort_miss = TRUE) +
  labs(title   = "Diagnostic des valeurs manquantes : Variables clés",
       subtitle = "GHS Panel W4 (2018-2019) — Membres actifs uniquement",
       caption  = "Source : NBS Nigeria / World Bank LSMS-ISA")

ggsave(here("outputs", "figures", "fig00_diagnostic_NA.png"),
       plot = p_miss, width = 10, height = 7, dpi = 300)

cat("NA s1q2 (sexe)    :", sum(is.na(sect1_clean$s1q2)),    "\n")
cat("NA s1q3 (parenté) :", sum(is.na(sect1_clean$s1q3)),    "\n")
cat("NA s1q4 (âge)     :", sum(is.na(sect1_clean$s1q4)),    "\n")
cat("NA sector          :", sum(is.na(sect1_clean$sector)),  "\n")
cat("NA zone            :", sum(is.na(sect1_clean$zone)),    "\n")

#Dans le fichier brut, il y avait 3 780 NA sur l'âge, le sexe, la parenté.
#En voyant ça, on pourrait penser que les enquêteurs ont mal fait leur travail, 
#ils ont oublié de remplir ces cases
#Mais non. Ces NA existaient parce que ces personnes avaient quitté le ménage (s1q4a == 2).
#L'enquêteur ne les a pas interrogées car elles n'étaient plus là physiquement.
#Il n'a pas oublié, il n'avait personne à interroger.
#Final point a retenir : C'est le fonctionnement normal d'une enquête panel :
#on garde la trace des anciens membres mais on ne remplit pas leurs informations actuelles.
#La preuve : une fois quon retires ces membres partis (filtre s1q4a == 1), il reste 0 NA sur le sexe
#0 NA sur la parenté, et 1 seul NA sur l'âge (celui créé par la correction de la valeur aberrante de 130 ans).

#Conclusion:: les enquêteurs ont bien rempli 100% des cases pour les personnes qu'ils ont effectivement rencontrées.
#La collecte est de bonne qualité.
# =============================================================================
# TÂCHE 1.8 — Recodages et construction des variables analytiques
# =============================================================================

sect1_clean <- sect1_clean %>%
  mutate(
    
    sexe = factor(s1q2, levels = c(1, 2), labels = c("Homme", "Femme")),
    
    groupe_age = cut(s1q4,
                     breaks          = seq(0, 100, by = 5),
                     right           = FALSE,
                     include.lowest  = TRUE),
    
    milieu = factor(sector, levels = c(1, 2), labels = c("Urbain", "Rural")),
    
    parente = case_when(
      s1q3 == 1            ~ "Chef de menage",
      s1q3 == 2            ~ "Conjoint(e)",
      s1q3 %in% c(3, 4)   ~ "Enfant",
      !is.na(s1q3)         ~ "Autre"
    ),
    
    parente = factor(parente,
                     levels = c("Chef de menage", "Conjoint(e)",
                                "Enfant", "Autre"))
  )

# Vérification des recodages par fréquences
table(sect1_clean$sexe,    useNA = "always")
table(sect1_clean$milieu,  useNA = "always")
table(sect1_clean$parente, useNA = "always")

# Sexe     : Homme 12 828 (49.8%), Femme 12 939 (50.2%), NA 0
# Équilibre quasi parfait, cohérent avec la démographie nigériane.
# Les femmes sont légèrement majoritaires dans les ménages car les hommes
# migrent davantage vers les villes ou l'étranger.

# Milieu   : Urbain 7 324 (28.4%), Rural 18 443 (71.6%), NA 0
# Surreprésentation rurale volontaire du GHS Panel qui cible les ménages
# agricoles. Le taux d'urbanisation réel du Nigeria était d'environ 52%
# en 2018 (Banque Mondiale). Les résultats ne sont pas directement
# extrapolables à l'ensemble de la population sans pondération.

# Parenté  : Chef 4 968 (19.3%), Conjoint 4 214 (16.4%),
#            Enfant 14 328 (55.6%), Autre 2 257 (8.8%), NA 0
# Les enfants représentent plus de la moitié des membres — signature
# démographique d'un pays à fort taux de fécondité (ISF ≈ 5.3 en 2018).
# 754 chefs sans conjoint résident : veufs, ménages monoparentaux, ou
# conjoints en migration. La catégorie Autre (8.8%) reflète la famille
# élargie caractéristique du modèle nigérian.


# =============================================================================
# TÂCHE 1.9 — Calcul de la taille des ménages
# =============================================================================
# La taille du ménage est une variable de niveau ménage, pas individuel.
# Elle se calcule par agrégation (n() par hhid) puis jointure sur le fichier
# individuel pour que chaque ligne affiche la taille de son ménage.

taille_par_menage <- sect1_clean %>%
  group_by(hhid) %>%
  summarise(taille_menage = n(), .groups = "drop")

cat("Nombre de ménages :", nrow(taille_par_menage), "\n")
summary(taille_par_menage$taille_menage)

# 4 978 ménages.
# Min=1, Q1=3, Médiane=5, Moyenne=5.18, Q3=7, Max=29
# Taille médiane de 5 personnes. Le maximum de 29 membres correspond à un
# ménage polygame élargi typique du Nord nigérian où plusieurs épouses et
# leurs enfants cohabitent sous le même toit. Le Q3 à 7 signifie que 25%
# des ménages ont au moins 7 membres — charge démographique élevée avec
# des implications directes sur la pauvreté et l'accès aux services.

sect1_clean <- sect1_clean %>%
  left_join(taille_par_menage, by = "hhid")

cat("NA dans taille_menage :", sum(is.na(sect1_clean$taille_menage)), "\n")
# Doit retourner 0.


# =============================================================================
# NOTE — Attrition totale : ménages dans secta mais absents de sect1
# =============================================================================
# secta contient 5 025 ménages (cadre de sondage complet).
# sect1 contient 4 980 ménages (ménages effectivement interviewés).
# Les 45 ménages manquants constituent une attrition totale documentée :
#
#   19 — UNABLE TO IDENTIFY HOUSEHOLD : ménage introuvable sur le terrain
#   12 — LONG TERM UNAVAILABLE        : aucun membre disponible sur
#                                       toute la durée de la collecte
#   14 — REFUSED                      : refus explicite de participer
#
# Ces 45 ménages (0.9% de l'échantillon) sont légitimement absents de
# toutes les analyses grace au filtre sur létat dactivité  du membre.
#Leur exclusion n'introduit pas de biais notable
# sur les variables descriptives du TP1.

hhid_absents <- setdiff(unique(secta$hhid), unique(sect1$hhid))
cat("Ménages non-interviewés (attrition totale) :", length(hhid_absents), "\n")
rm(hhid_absents)

# =============================================================================
# TÂCHE 1.10 — Sauvegarde
# =============================================================================
# Le format .rds est plus rapide que .dta ou .csv, conserve tous les attributs
# R (facteurs, labels) et ne nécessite que base R pour être relu avec readRDS().

saveRDS(sect1_clean, here("data", "processed", "sect1_clean.rds"))

cat("Dimensions finales :", nrow(sect1_clean), "x", ncol(sect1_clean), "\n")
cat("Variables ajoutées : sexe, groupe_age, milieu, parente, taille_menage\n")
# 25 767 lignes x 64 colonnes.
