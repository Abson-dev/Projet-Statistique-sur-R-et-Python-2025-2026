# TP1 — Profil Démographique des Ménages Nigérians (avec Pondérations)

---

## Membres de l'équipe

- **Herman YAMAHA**
- **Bourama DIALLO**

Étudiants en ISE1 à l'ENSAE de Dakar

**Superviseur :** M. Aboubacar HEMA, Data Scientist

**Année académique : 2025 - 2026**

---

## Mise à jour principale — Intégration des pondérations transversales

Cette version du projet intègre les **pondérations transversales `wt_wave4`** issues du fichier `secta_harvestw4.dta`. Toutes les statistiques descriptives, proportions, quantiles et tests statistiques sont désormais calculés en tenant compte de ces poids, afin que les résultats soient **représentatifs de la population nigériane** et non seulement descriptifs de l'échantillon.

**Autres changements par rapport à la version précédente :**
- Le tableau de synthèse est exporté en **Excel** (`.xlsx`) via `openxlsx` (plus de sortie HTML)
- Le rapport R Markdown génère uniquement un **document Word** (`.docx`) avec mise en page soignée via `flextable` et `officer`
- Les graphiques intègrent `aes(weight = wt_wave4)` pour des visualisations pondérées

---

## 1. Description du projet

Ce projet s'inscrit dans le cadre du cours de **Projet Statistique sous R et Python** (ENSAE ISE 1, 2025-2026). Il porte sur l'**Analyse 1 : Profil démographique des ménages nigérians**, à partir des données du Nigeria General Household Survey (GHS) Panel, programme LSMS-ISA de la Banque Mondiale.

**Objectif principal** : Explorer la structure sociodémographique des ménages nigérians à partir de la section 1 du GHS-Panel Wave 4 (2018/19), en produisant des estimations représentatives de la population grâce aux pondérations transversales `wt_wave4`.

**Questions analytiques traitées :**

1. Quelle est la structure du jeu de données et quelles variables clés en sont issues ?
2. Le fichier présente-t-il des doublons ou des valeurs manquantes sur les variables clés ?
3. Comment se distribue l'âge des membres des ménages (pondéré) et suit-il une loi normale ?
4. Quelle est la fréquence pondérée des différents liens de parenté au sein des ménages ?
5. La taille des ménages diffère-t-elle significativement entre zones urbaines et rurales (pondéré) ?
6. Quelle synthèse pondérée peut-on dresser des caractéristiques démographiques selon le milieu de résidence ?

---

## 2. Sources de données

| Fichier `.dta` | Contenu | Variables clés | Vague |
|:---|:---|:---|:---|
| `sect1_harvestw4.dta` | Section démographie individuelle | `s1q2` (sexe), `s1q4` (âge), `s1q3` (lien parenté), `sector` (milieu) | W4 |
| `secta_harvestw4.dta` | Caractéristiques ménage + **pondérations** | `hhid`, **`wt_wave4`** (poids transversal), `sector`, `zone`, `state` | W4 |

**Source officielle :** World Bank LSMS-ISA — Nigeria GHS Panel, Wave 4 (2018/19), Post-Harvest  
**Lien :** `https://microdata.worldbank.org/index.php/catalog/3557`

> **Les fichiers `.dta` ne sont pas inclus dans ce dépôt.** Ils sont hébergés sur GitHub et téléchargés automatiquement dans `data/raw/` à la première exécution de `main.R` :
> ```
> https://github.com/Herman-YAMAHA/NYHP/tree/bf1173ced39831e18d8e21c3b2880e597bbc6300/TP1_raw
> ```

---

## 3. Structure du projet

```
Groupe7_YAMAHA_Herman_DIALLO_Bourama_TP1/
│
├── main.R                                       # Point d'entrée — orchestre l'exécution
│
├── Groupe7_YAMAHA_Herman_DIALLO_Bourama_TP1.Rproj
│
├── scripts/
│   ├── 01_import_nettoyage.R                    # Import + jointure pondérations + recodages
│   └── 02_exploration_visualisation.R           # Statistiques pondérées + graphiques + Excel
│
├── data/
│   ├── raw/                                     # VIDE — fichiers téléchargés automatiquement depuis GitHub
│   └── processed/                               # Objets .rds intermédiaires (avec wt_wave4)
│
├── rapport/
│   └── Rapport_TP1_profil_demographique.Rmd     # Rapport R Markdown → génère un .docx
│
├── outputs/
│   ├── fig01_histogramme_age.png                # Distribution pondérée de l'âge
│   ├── fig02_boxplot_age.png                    # Boîte à moustaches de l'âge
│   ├── fig03_pyramide_ages.png                  # Pyramide des âges pondérée par sexe
│   ├── fig04_lien_parente.png                   # Proportions pondérées du lien de parenté
│   ├── fig05_boxplot_menages.png                # Taille des ménages pondérée Urbain vs Rural
│   └── tableau_gtsummary.xlsx                   # Tableau récapitulatif pondéré (Excel)
│
└── README.md
```

---

## 4. Méthodologie

### Pondérations — Note méthodologique

Le GHS-Panel repose sur un **plan de sondage stratifié à plusieurs degrés**. Les pondérations transversales `wt_wave4` issues de `secta_harvestw4.dta` permettent de corriger les biais de représentativité liés au plan de sondage. Sans ces poids, les estimations obtenues sur l'échantillon brut ne seraient valables que pour l'échantillon lui-même, et non pour la population nigériane.

En pratique, les poids sont intégrés via :
- `aes(weight = wt_wave4)` dans les graphiques ggplot2
- `as_survey_design()` du package `srvyr` pour les statistiques descriptives
- `svydesign()` du package `survey` pour les tests statistiques
- `tbl_svysummary()` du package `gtsummary` pour le tableau de synthèse

### Tâche 1 — Exploration de la structure du dataset

Chargement de `sect1_harvestw4.dta` (via `haven`) et `secta_harvestw4.dta` (pondérations). Jointure sur `hhid`. Construction des variables dérivées : âge, sexe, milieu de résidence, zone géopolitique, lien de parenté (4 catégories), tranches d'âge quinquennales. Intégration de `wt_wave4` dans tous les objets de travail.

### Tâche 2 — Doublons et valeurs manquantes

Vérification de l'unicité de la clé `hhid × indiv`. Comptage des valeurs manquantes sur les variables clés incluant `wt_wave4`. Visualisation avec `vis_miss()` du package `naniar`.

### Tâche 3 — Analyse univariée pondérée de l'âge

Statistiques descriptives **pondérées** (moyenne, médiane, quartiles, écart-type) via `srvyr`. Histogramme pondéré (`aes(weight = wt_wave4)`, binwidth = 5 ans). Boîte à moustaches avec moyenne pondérée annotée. Test de normalité de **Shapiro-Wilk** sur sous-échantillon de 5 000 individus (graine = 2025). **Pyramide des âges pondérée** : effectifs agrégés par `sum(wt_wave4)` par tranche d'âge et sexe.

### Tâche 4 — Lien de parenté et IC à 95 % pondérés

Proportions pondérées avec **intervalles de confiance à 95%** calculés par le plan de sondage (`survey_mean(vartype = "ci")`). Diagramme en barres horizontales avec barres d'erreur sur les IC.

### Tâche 5 — Taille des ménages et test de Wilcoxon-Mann-Whitney

Construction de la base ménages avec poids `wt_wave4`. Statistiques descriptives pondérées par milieu. Test de **Wilcoxon-Mann-Whitney** bilatéral. Taille d'effet $r$ de Rosenthal. Boxplot pondéré avec `aes(weight = wt_wave4)` et annotation de la moyenne pondérée par milieu.

### Tâche 6 — Tableau `tbl_svysummary` stratifié → Excel

Tableau pondéré via `tbl_svysummary()` stratifié par milieu de résidence. Tests pondérés associés (`svy.wilcox.test`, `svy.chisq.test`). Export en **Excel** via `openxlsx` avec mise en forme soignée (en-têtes colorés, lignes alternées, note méthodologique).

---

## 5. Packages R requis

```r
install.packages(c(
  "haven",       # lecture des fichiers .dta Stata
  "dplyr",       # manipulation des données
  "ggplot2",     # visualisation
  "naniar",      # diagnostic des valeurs manquantes
  "PropCIs",     # intervalles de confiance exacts (Clopper-Pearson)
  "gtsummary",   # tableaux de synthèse statistique
  "gt",          # export des tableaux gtsummary
  "apyramid",    # pyramide des âges
  "forcats",     # réordonner les facteurs
  "scales",      # formatage des axes
  "survey",      # plan de sondage complexe (pondérations)
  "srvyr",       # interface dplyr pour survey
  "openxlsx",    # export Excel du tableau gtsummary
  "flextable",   # tableaux professionnels dans les documents Word
  "officer",     # gestion des documents Word
  "knitr"        # compilation du rapport R Markdown
))
```

---

## 6. Reproduction des résultats

### Étape 1 — Exécuter l'analyse

Ouvrir RStudio à partir du `.Rproj` du projet, puis exécuter :

```r
source("main.R")
```

Les scripts sont exécutés séquentiellement. Les figures PNG sont sauvegardées dans `outputs/` et le tableau Excel dans `outputs/tableau_gtsummary.xlsx`.

> **Note :** `data/raw/` est **vide par défaut** — les fichiers `.dta` sont téléchargés automatiquement depuis GitHub à la première exécution via `raw.githubusercontent.com`. Une connexion Internet est requise lors du premier lancement.

### Étape 2 — Compiler le rapport Word

Ouvrir `rapport/Rapport_TP1_profil_demographique.Rmd` dans RStudio et cliquer sur **Knit → Knit to Word**.

> **Important :** Le rapport génère uniquement un fichier **`.docx`** (Word). Aucune sortie HTML n'est produite.

---

## 7. Références

- World Bank (2019). *Nigeria General Household Survey Panel 2018-2019, Wave 4*. LSMS-ISA. Washington DC : The World Bank.
- Lumley, T. (2010). *Complex Surveys: A Guide to Analysis Using R*. Hoboken : Wiley.
- Deaton, A. (1997). *The Analysis of Household Surveys: A Microeconometric Approach to Development Policy*. Baltimore : Johns Hopkins University Press.
- Clopper, C. J. & Pearson, E. S. (1934). The use of confidence or fiducial limits illustrated in the case of the binomial. *Biometrika*, 26(4), 404-413.
- Wilcoxon, F. (1945). Individual comparisons by ranking methods. *Biometrics Bulletin*, 1(6), 80-83.
