# README — TP1 : Analyse du profil démographique des ménages nigérians

## Traitement statistique avec R · ENSAE ISE1 · 2025-2026

\---

## 1\. Description du projet

Ce projet analyse les **caractéristiques démographiques des ménages nigérians** à partir des données du **Nigeria General Household Survey Panel (GHS Panel)**, couvrant quatre vagues d'enquête :

|Vague|Année|Période|
|-|-|-|
|Wave 1 (W1)|2010|Post-Harvest + Post-Planting|
|Wave 2 (W2)|2012|Post-Harvest + Post-Planting|
|Wave 3 (W3)|2015|Post-Harvest + Post-Planting|
|Wave 4 (W4)|2018|Post-Harvest + Post-Planting|

Les variables étudiées sont : l'âge, le sexe, le statut matrimonial, le lien de parenté avec le chef de ménage, la taille du ménage et le milieu de résidence (urbain/rural).

\---

## 2\. Structure du projet

```
TP1/
├── TP1.Rproj                          ← Projet RStudio (ouvrir avant d'exécuter)
├── script\_pondere.R                   ← Script principal avec pondérations
├── rapport\_TP1.Rmd                    ← Rapport R Markdown → sortie Word
├── README.md                          ← Ce fichier
│
├── data/                              ← Données brutes (non incluses dans le dépôt)
│   ├── sect1\_harvestw1.dta            ← Membres des ménages W1 Post-Harvest
│   ├── sect1\_harvestw2.dta            ← Membres des ménages W2 Post-Harvest
│   ├── sect1\_harvestw3.dta            ← Membres des ménages W3 Post-Harvest
│   ├── sect1\_harvestw4.dta            ← Membres des ménages W4 Post-Harvest
│   ├── sect1\_plantingw1.dta           ← Membres des ménages W1 Post-Planting
│   ├── sect1\_plantingw2.dta           ← Membres des ménages W2 Post-Planting
│   ├── sect1\_plantingw3.dta           ← Membres des ménages W3 Post-Planting
│   ├── sect1\_plantingw4.dta           ← Membres des ménages W4 Post-Planting
│   ├── secta\_harvestw1.dta            ← Ménages W1 (milieu + poids wt\_wave1)
│   ├── secta\_harvestw2.dta            ← Ménages W2 (milieu + poids wt\_wave2)
│   ├── secta\_harvestw3.dta            ← Ménages W3 (milieu + poids wt\_wave3)
│   └── secta\_harvestw4.dta            ← Ménages W4 (milieu + poids wt\_wave4)
│
├── images/                            ← Images pour le rapport Rmd
│   ├── image1.png                     ← Logo ANSD
│   ├── image2.png                     ← Logo ENSAE
│   ├── image3.png                     ← Heatmap valeurs manquantes
│   ├── image4.png                     ← Histogramme + boxplot âge (W4)
│   ├── image5\_camembert.png           ← Camembert répartition par sexe (W4)
│   ├── image6\_hist.png                ← Histogramme statut matrimonial (W4)
│   ├── image7\_barplot.png             ← Barplot lien de parenté + IC95% (W4)
│   ├── image8.png                     ← Pyramide des âges pondérée (W4)
│   ├── image9\_violin.png              ← Violin plot taille des ménages (W4)
│   ├── image10.png                    ← 4 pyramides des âges W1-W4
│   ├── image11.png                    ← Évolution taux de féminisation W1-W4
│   ├── image12.png                    ← Évolution statut matrimonial W1-W4
│   ├── image13.png                    ← Évolution lien de parenté W1-W4
│   └── image14.png                    ← Évolution taille médiane W1-W4
│
└── outputs/
    ├── graphiques/                    ← Graphiques générés par script\_pondere.R
    │   ├── T1\_qualite\_valeurs\_manquantes.png      ← Heatmap % NA harvest+planting W1–W4
    │   ├── T2\_age\_histogramme\_boxplot.png         ← Histogramme + boxplot âge pondérés (W4)
    │   ├── T3\_sexe\_camembert\_evolution.png        ← Camembert sexe + évolution féminisation
    │   ├── T4\_statut\_matrimonial\_hist\_evolution.png ← Statut mat. + évolution W1–W4
    │   ├── T5\_pyramide\_ages\_w4.png                ← Pyramide des âges pondérée (W4)
    │   ├── T6\_pyramides\_4\_vagues.png              ← 4 pyramides pondérées côte à côte W1–W4
    │   ├── T7\_lien\_parente\_IC95\_evolution.png     ← Lien parenté IC95% + évolution W1–W4
    │   └── T8\_taille\_menage\_violin\_evolution.png  ← Violin pondéré + évolution taille W1–W4
    └── tableaux/                      ← Tableaux générés par script\_pondere.R
        └── T9\_tableau\_gtsummary\_milieu.html       ← Tableau pondéré stratifié Urban/Rural (W4)
```

\---

## 3\. Données utilisées

### Source

**Nigeria General Household Survey Panel (GHS Panel)**
Producteur : National Bureau of Statistics (NBS) Nigeria / World Bank LSMS
Disponible sur : https://microdata.worldbank.org/

### Fichiers clés et variables

#### `sect1\_harvestwX.dta` — Variables individuelles (une ligne par individu)

|Variable|Description|Modalités|
|-|-|-|
|`hhid`|Identifiant du ménage|—|
|`indiv`|Identifiant de l'individu|—|
|`s1q2`|Sexe|1 = Male · 2 = Female|
|`s1q3`|Lien de parenté avec le chef|1=Chef · 2=Conjoint · 3-5=Enfant · autres|
|`s1q4`|Âge (années révolues)|Numérique continu|
|`s1q7`|Statut matrimonial|1=Marié monogame · 2=Polygame · 3=Union informelle · 4=Divorcé · 5=Séparé · 6=Veuf · 7=Jamais marié|
|`sector`|Milieu de résidence|1 = Urban · 2 = Rural|
|`zone`|Zone géographique|1=North Central · … · 6=South West|
|`state`|État nigérian|1 à 37|

#### `secta\_harvestwX.dta` — Variables ménage + **poids de sondage**

|Variable|Description|
|-|-|
|`hhid`|Identifiant du ménage (clé de jointure)|
|`sector`|Milieu de résidence (1=Urban · 2=Rural)|
|`wt\_wave1` / `wt\_wave2` / `wt\_wave3` / `wt\_wave4`|**Poids de sondage cross-sectionnel**|
|`wt\_combined` / `wt\_longpanel` / `wt\_w1\_w2\_w3`|Poids longitudinal (panel)|

\---

## 4\. Pondérations — Explication détaillée

### Deux types de poids disponibles

Dans les fichiers `secta\_harvestwX`, deux variables de pondération sont disponibles :

|Vague|Poids cross-sectionnel|Poids longitudinal|
|-|-|-|
|W1|`wt\_wave1`|`wt\_combined`|
|W2|`wt\_wave2`|`wt\_combined`|
|W3|`wt\_wave3`|`wt\_w1\_w2\_w3`|
|W4|`wt\_wave4`|`wt\_longpanel`|

### Signification de chaque poids

**`wt\_waveX` — poids cross-sectionnel (choix retenu)**
Ce poids est conçu pour rendre l'échantillon représentatif de la **population nigériane à une date donnée** (une vague). Il permet de répondre à la question : *"Quelle est la structure démographique du Nigeria en 2018 ?"*. Il est calibré pour représenter l'ensemble de la population nationale au moment de la collecte, y compris les nouveaux ménages ajoutés à chaque vague.

**`wt\_longpanel` / `wt\_combined` / `wt\_w1\_w2\_w3` — poids longitudinal**
Ce poids est conçu pour les **analyses longitudinales** qui suivent les **mêmes ménages** sur plusieurs vagues. Il corrige les biais d'attrition (ménages perdus de vue entre deux vagues). Il est adapté à la question : *"Comment ont évolué les conditions de vie des mêmes ménages entre 2010 et 2018 ?"*

### Pourquoi on a choisi `wt\_waveX`

Toutes les analyses de ce TP sont **cross-sectionnelles** : on décrit la population à chaque vague indépendamment, sans suivre les mêmes individus dans le temps. Même les graphiques d'évolution W1–W4 comparent des vagues séparées, pas les mêmes ménages.

Utiliser `wt\_longpanel` aurait restreint l'analyse aux seuls ménages présents dans **toutes** les vagues du panel long, ce qui aurait exclu les nouveaux ménages ajoutés en W3 et W4 et **sous-représenté** certaines catégories de la population.

> \*\*Règle à retenir :\*\*
> - `wt\_waveX` → analyse \*\*cross-sectionnelle\*\* (décrire la population à une date)
> - `wt\_longpanel` / `wt\_combined` → analyse \*\*longitudinale\*\* (suivre les mêmes unités dans le temps)

### Comment les poids sont appliqués

Les poids sont au **niveau ménage** (dans `secta\_harvestwX`) et sont joints aux données individuelles via `hhid`. Ainsi, chaque individu reçoit le poids de son ménage.

```r
# Jointure des poids sur les données individuelles
df\_harvest <- df\_harvest |>
  left\_join(df\_secta |> select(hhid, vague, milieu, poids),
            by = c("hhid", "vague"))

# Création du design de sondage (package srvyr)
design\_w4 <- df\_harvest |>
  filter(vague == "W4 (2018)", !is.na(poids)) |>
  as\_survey\_design(weights = poids, ids = hhid)
```

\---

## 5\. Description des analyses réalisées

### T1 — Vérification de la qualité des données (non pondérée)

Heatmap du pourcentage de valeurs manquantes pour les 4 variables clés (sexe, lien de parenté, âge, statut matrimonial), sur Post-Harvest et Post-Planting, pour les 4 vagues. Cette analyse n'est **pas pondérée** car les valeurs manquantes sont une propriété de la collecte (qualité de l'échantillon), pas de la population.

### T2 — Distribution de l'âge (W4, pondérée)

Histogramme pondéré (classes de 5 ans) + boîte à moustaches. Statistiques descriptives pondérées via `srvyr` (moyenne, médiane, quartiles, écart-type). Test de Shapiro-Wilk sur un sous-échantillon pour tester la normalité (non pondéré — les tests de rang ne se pondèrent pas).

### T3 — Distribution par sexe (W4, pondérée) + évolution W1–W4

Camembert avec proportions pondérées. Évolution du taux de féminisation pondéré par milieu (Rural/Urbain) avec intervalles de confiance à 95% via `srvyr`.

### T4 — Statut matrimonial (W4, pondérée) + évolution W1–W4

Barplot horizontal avec proportions pondérées et IC 95%. Graphique 100% empilé de l'évolution W1–W4 avec proportions pondérées.

### T5 — Pyramide des âges W4 (pondérée)

Pyramide construite manuellement avec `geom\_col` (hommes à gauche en négatif, femmes à droite) à partir des effectifs pondérés agrégés par groupe quinquennal × sexe. Les effectifs représentent la population estimée (en millions).

### T6 — 4 pyramides des âges W1–W4 (pondérées)

Même méthode que T5, appliquée aux 4 vagues côte à côte pour visualiser l'évolution de la structure démographique.

### T7 — Lien de parenté (W4, pondéré) + évolution W1–W4

Barplot horizontal des proportions pondérées avec IC à 95% (`srvyr`). Graphique d'évolution des proportions pondérées W1–W4.

### T8 — Taille des ménages Rural/Urbain (W4)

Violin plot pondéré + boîte à moustaches. Test de Wilcoxon-Mann-Whitney (sur données non pondérées — les tests de rang ne se pondèrent pas) avec taille d'effet r de rang. Évolution de la taille moyenne pondérée W1–W4 par milieu.

### T9 — Tableau récapitulatif stratifié par milieu (W4, pondéré)

Tableau `tbl\_svysummary()` (package `gtsummary`) avec statistiques pondérées pour l'âge, le sexe, la taille du ménage et le statut matrimonial, stratifié par milieu (Urban/Rural). Tests pondérés : Wilcoxon pour les variables continues, chi-deux pour les catégorielles.

\---

## 6\. Packages R utilisés

|Package|Rôle|
|-|-|
|`haven`|Lecture des fichiers `.dta` Stata|
|`dplyr`|Manipulation des données|
|`tidyr`|Restructuration (pivot)|
|`forcats`|Gestion des facteurs|
|`ggplot2`|Visualisation|
|`srvyr`|Analyses statistiques pondérées (design enquête)|
|`survey`|Backend pour `srvyr`|
|`apyramid`|Pyramides des âges (T5, T6)|
|`naniar`|Visualisation des valeurs manquantes (T1)|
|`gtsummary`|Tableaux descriptifs pondérés (T9)|
|`rstatix`|Tests statistiques tidy (Wilcoxon, taille d'effet)|
|`PropCIs`|Intervalles de confiance sur proportions (T7)|
|`e1071`|Asymétrie et kurtosis (T2)|
|`patchwork`|Assemblage de graphiques|
|`scales`|Formatage des axes|
|`gt`|Export des tableaux|
|`officedown`|Rapport R Markdown → Word|
|`officer`|Mise en forme Word (page de garde)|
|`flextable`|Tableaux dans le rapport Word|

\---

## 7\. Comment reproduire les résultats

### Étape 1 — Prérequis

```r
install.packages(c(
  "haven", "dplyr", "tidyr", "forcats", "ggplot2",
  "srvyr", "survey", "apyramid", "naniar", "gtsummary",
  "rstatix", "PropCIs", "e1071", "patchwork", "scales",
  "gt", "officedown", "officer", "flextable"
))
```

### Étape 2 — Données

Télécharger les données GHS Panel W1–W4 sur le portail World Bank LSMS et les placer dans le dossier `data/`.

### Étape 3 — Exécuter le script

1. Ouvrir `TP1.Rproj` dans RStudio
2. Ouvrir `TP1.R`
3. Exécuter le script complet (`Ctrl+Shift+Enter`)
4. Les graphiques et tableaux sont sauvegardés dans `outputs/`

### Étape 4 — Générer le rapport

1. Copier les graphiques souhaités dans `images/`
2. Ouvrir `rapport\_TP1.Rmd`
3. Cliquer **Knit → Knit to Word**

\---

## 8\. Auteurs

* **Diop Astou**
* **Mamadou Lamine Diabang**

Filière : ISE1 – CL  
Professeur : Monsieur Aboubacar Hema  
Année académique : 2025/2026  
École Nationale de la Statistique et de l'Analyse Économique (ENSAE)

