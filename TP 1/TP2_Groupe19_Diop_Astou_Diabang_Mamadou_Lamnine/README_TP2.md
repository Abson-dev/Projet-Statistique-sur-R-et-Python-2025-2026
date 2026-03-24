# README — TP2 : Analyse du niveau d'éducation et de l'alphabétisation des ménages nigérians

## Traitement statistique avec R · ENSAE ISE1 · 2025-2026

\---

## 1\. Description du projet

Ce projet analyse la **distribution du niveau d'éducation et de l'alphabétisation des ménages nigérians** à partir des données du **Nigeria General Household Survey Panel (GHS Panel)**, Wave 4 (2018) uniquement.

L'objectif est d'identifier les caractéristiques du système éducatif et de mettre en évidence les disparités selon :

* le **sexe** (inégalités de genre dans l'accès à l'éducation)
* l'**âge** (différences intergénérationnelles)
* la **zone de résidence** (urbain vs rural)
* l'**État nigérian** (disparités géographiques)

\---

## 2\. Structure du projet

```
TP2/
├── TP2.Rproj                            ← Projet RStudio (ouvrir avant d'exécuter)
├── TP2.R                                ← Script principal avec pondérations
├── rapport\_TP2.Rmd                      ← Rapport R Markdown → sortie Word
├── README.md                            ← Ce fichier
│
├── data/                                ← Données brutes (non incluses dans le dépôt)
│   ├── sect2\_harvestw4.dta              ← Éducation des membres W4 (variable principale)
│   ├── sect1\_harvestw4.dta              ← Démographie des membres W4 (sexe, âge)
│   └── secta\_harvestw4.dta              ← Ménages W4 (zone, état, poids wt\_wave4)
│
├── images/                              ← Images pour le rapport Rmd
│   ├── logo\_ansd.png                    ← Logo ANSD
│   ├── logo\_ensae.png                   ← Logo ENSAE
│   ├── 01\_valeurs\_manquantes.png        ← Barplot % valeurs manquantes
│   ├── 02\_niveau\_education.png          ← Distribution globale niveau éducation
│   ├── 03\_education\_par\_sexe.png        ← Éducation × sexe (barres empilées 100%)
│   ├── 04\_education\_par\_age.png         ← Éducation × groupe d'âge (violin+boxplot)
│   ├── 05\_scolarisation\_zone.png        ← Taux scolarisation 6-17 ans (urbain/rural)
│   ├── 06\_heatmap\_analphabetisme.png    ← Heatmap analphabétisme par État
│   ├── 07\_education\_age\_tendance.png    ← Évolution éducation avec l'âge (continu)
│   └── 08\_matrice\_sexe\_education.png    ← Matrice proportions sexe × éducation
│
└── outputs/
    ├── graphiques/                      ← Graphiques générés par script\_pondere.R
    │   ├── 01\_valeurs\_manquantes.png    ← Barplot % NA par variable (non pondéré)
    │   ├── 02\_niveau\_education.png      ← Barplot pondéré + IC 95% niveau éducation
    │   ├── 03\_education\_par\_sexe.png    ← Barres 100% empilées pondérées × sexe
    │   ├── 04\_education\_par\_age.png     ← Violin+boxplot par groupe d'âge
    │   ├── 05\_scolarisation\_zone.png    ← Barplot pondéré scolarisation + IC 95%
    │   ├── 06\_heatmap\_analphabetisme.png← Heatmap taux pondéré par État
    │   ├── 07\_education\_age\_tendance.png← Violons dégradé viridis + courbe pondérée
    │   └── 08\_matrice\_sexe\_education.png← Matrice heatmap proportions pondérées
    └── tableaux/                        ← Tableaux générés par script\_pondere.R
        ├── 01\_tableau\_zone.html         ← Tableau pondéré caractéristiques par zone
        ├── 02\_tableau\_sexe.html         ← Tableau pondéré éducation par sexe
        ├── 03\_tableau\_etats.html        ← Taux d'analphabétisme pondéré par État
        └── 04\_dunn\_results.csv          ← Résultats post-hoc Dunn (Tâche 4)
```

\---

## 3\. Données utilisées

### Source

**Nigeria General Household Survey Panel (GHS Panel) — Wave 4 (2018)**
Producteur : National Bureau of Statistics (NBS) Nigeria / World Bank LSMS
Disponible sur : https://microdata.worldbank.org/

### Fichiers et variables clés

#### `sect2\_harvestw4.dta` — Variables d'éducation

|Variable|Description|Modalités / Codage|
|-|-|-|
|`hhid`|Identifiant du ménage|—|
|`indiv`|Identifiant de l'individu|—|
|`s2aq2`|Scolarisé actuellement ?|1 = Oui · 2 = Non|
|`s2aq15`|Niveau d'éducation le plus élevé atteint|Voir codage ci-dessous|

**Codage de `s2aq15` → `niveau\_educ` (5 catégories ordonnées) :**

|Code(s)|Catégorie|
|-|-|
|0, 1, 2, 51, 52, 61|**Aucun** (aucune instruction formelle, coranique, adulte)|
|11 à 16|**Primaire** (Primary 1 à 6)|
|21 à 23|**Junior Secondary** (JS1 à JS3)|
|24 à 28, 321|**Senior Secondary** (SS1 à SS3, Lower/Upper 6, voc. secondaire)|
|31 à 35, 322, 41, 411, 412, 42, 421–424, 43|**Tertiaire** (NCE, polytechnique, université, post-grad)|

#### `sect1\_harvestw4.dta` — Variables démographiques

|Variable|Description|Modalités|
|-|-|-|
|`hhid`|Identifiant du ménage|—|
|`indiv`|Identifiant de l'individu|—|
|`s1q2`|Sexe|1 = Homme · 2 = Femme|
|`s1q4`|Âge (années révolues)|Numérique continu|

#### `secta\_harvestw4.dta` — Variables ménage + **poids de sondage**

|Variable|Description|
|-|-|
|`hhid`|Identifiant du ménage (clé de jointure)|
|`sector`|Milieu de résidence (1 = Urban · 2 = Rural)|
|`state`|État nigérian (1 à 37)|
|`wt\_wave4`|**Poids de sondage cross-sectionnel W4** ← utilisé|
|`wt\_longpanel`|Poids longitudinal (non utilisé)|

\---

## 4\. Pondérations — Explication détaillée

### Deux variables disponibles dans `secta\_harvestw4`

|Variable|Type|Usage|
|-|-|-|
|`wt\_wave4`|Poids **cross-sectionnel**|Représente la population nigériane en 2018|
|`wt\_longpanel`|Poids **longitudinal**|Suivi des mêmes ménages sur plusieurs vagues|

### Pourquoi `wt\_wave4` a été retenu

Toutes les analyses du TP2 portent **uniquement sur Wave 4 (2018)** et sont de nature **cross-sectionnelle** : on décrit la population à un moment donné, sans suivre les mêmes individus dans le temps.

`wt\_wave4` est le poids adapté car il calibre l'échantillon W4 pour être représentatif de l'ensemble de la **population nigériane en 2018**, y compris les nouveaux ménages. Utiliser `wt\_longpanel` aurait restreint l'analyse aux seuls ménages présents dans plusieurs vagues du panel, introduisant un biais de sélection.

> \*\*Règle générale :\*\*
> - `wt\_wave4` → analyse \*\*cross-sectionnelle\*\* (décrire la population à une date)
> - `wt\_longpanel` → analyse \*\*longitudinale\*\* (suivre les mêmes ménages dans le temps)

### Comment les poids sont appliqués

Les poids sont au **niveau ménage** (dans `secta\_harvestw4`) et joints aux données individuelles via `hhid`. Chaque individu reçoit le poids de son ménage.

```r
# Jointure des poids
secta <- secta %>%
  mutate(poids = wt\_wave4) %>%
  select(hhid, zone, state, poids)

df <- sect2 %>%
  left\_join(sect1, by = c("hhid", "indiv")) %>%
  left\_join(secta, by = "hhid")

# Création du design de sondage
design\_complet <- df %>%
  filter(!is.na(poids)) %>%
  as\_survey\_design(weights = poids, ids = hhid)
```

\---

## 5\. Description des analyses réalisées

### Tâche 1 — Valeurs manquantes (non pondérée)

Barplot du pourcentage de valeurs manquantes pour les 5 variables clés (niveau d'éducation, âge, scolarisation, sexe, zone). **Non pondérée** car les valeurs manquantes sont une propriété de la collecte, pas de la population.

### Tâche 2 — Distribution du niveau d'éducation (pondérée)

Barplot horizontal des proportions pondérées avec IC à 95% via `srvyr::survey\_mean()`. Montre la répartition de la population nigériane en 2018 selon les 5 niveaux d'éducation.

### Tâche 3 — Éducation par sexe — Adultes 18+ (pondérée)

Barres 100% empilées avec proportions pondérées par sexe via `srvyr`. Test du chi-deux et V de Cramér calculés sur les données brutes (les tests de rang ne se pondèrent pas directement).

### Tâche 4 — Éducation par groupe d'âge (tests sur données brutes)

Violin plot + boxplot par groupe d'âge (18-30, 31-45, 46-60, 60+). Test de Kruskal-Wallis + post-hoc Dunn avec correction de Bonferroni sur les données brutes. Les résultats Dunn sont exportés en CSV.

### Tâche 5 — Scolarisation des 6-17 ans par zone (pondérée)

Barplot avec proportions pondérées et IC à 95% (srvyr) pour le taux de scolarisation Urbain vs Rural. Test du chi-deux sur les données brutes.

### Tâche 6 — Heatmap analphabétisme par État (pondérée)

Taux d'analphabétisme pondéré (`weighted.mean()`) par État nigérian pour les adultes 18+. Visualisation en heatmap ordonnée du taux le plus élevé au plus bas.

### Tâche 7 — Évolution éducation avec l'âge (pondérée)

Violons colorés en dégradé viridis selon l'âge + courbe loess pondérée (`weights = poids` dans `geom\_smooth()`). Montre la tendance intergénérationnelle du niveau d'éducation.

### Tâche 8 — Matrice Sexe × Éducation (pondérée)

Heatmap des proportions pondérées (palette plasma) croisant le sexe et le niveau d'éducation. Lecture en ligne : chaque ligne somme à 100%.

\---

## 6\. Packages R utilisés

|Package|Rôle|
|-|-|
|`haven`|Lecture des fichiers `.dta` Stata|
|`dplyr`|Manipulation des données|
|`tidyr`|Restructuration|
|`forcats`|Gestion des facteurs|
|`ggplot2`|Visualisation|
|`srvyr`|Analyses statistiques pondérées|
|`survey`|Backend pour `srvyr`|
|`naniar`|Valeurs manquantes|
|`gtsummary`|Tableaux descriptifs pondérés (`tbl\_svysummary`)|
|`rstatix`|Kruskal-Wallis, Dunn, V de Cramér|
|`viridis`|Palettes de couleurs|
|`scales`|Formatage des axes|
|`PropCIs`|IC sur proportions (Wilson)|
|`gt`|Export tableaux HTML|
|`officedown`|Rapport R Markdown → Word|
|`officer`|Mise en forme Word (page de garde)|
|`flextable`|Tableaux dans le rapport Word|

\---

## 7\. Comment reproduire les résultats

### Étape 1 — Prérequis

```r
install.packages(c(
  "haven", "dplyr", "tidyr", "forcats", "ggplot2",
  "srvyr", "survey", "naniar", "gtsummary", "rstatix",
  "viridis", "scales", "PropCIs", "gt", "RColorBrewer",
  "gridExtra", "officedown", "officer", "flextable"
))
```

### Étape 2 — Données

Télécharger les données GHS Panel W4 sur le portail World Bank LSMS et placer les 3 fichiers dans `data/`.

### Étape 3 — Exécuter le script

1. Ouvrir `TP2.Rproj` dans RStudio
2. Ouvrir `script\_pondere.R`
3. Exécuter le script complet (`Ctrl+Shift+Enter`)
4. Les graphiques sont dans `outputs/graphiques/` et les tableaux dans `outputs/tableaux/`

### Étape 4 — Générer le rapport

1. Copier les graphiques dans `images/` (avec les logos ANSD et ENSAE)
2. Ouvrir `rapport\_TP2.Rmd`
3. Cliquer **Knit → Knit to Word**

\---

## 8\. Auteurs

* **Diop Astou**
* **Mamadou Lamine Diabang**

Filière : ISE1 – CL
Professeur : Monsieur Aboubacar Hema
Année académique : 2025/2026
École Nationale de la Statistique et de l'Analyse Économique (ENSAE)

