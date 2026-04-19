# 🌾 TP5 — Cultures, Intrants et Rendements Agricoles au Nigeria

**ENSAE Pierre NDIAYE | ISE1 2025-2026**  
**Cours : Projet Statistique sous R et Python | Encadrant : Aboubacar HEMA**  
**Auteurs : DIOP Marème & BA Abdoul Aziz**

---

## 📋 Description du projet

Ce projet analyse le module agriculture du **Nigeria General Household Survey (GHS) Panel Wave 4 (2018-2019)**, enquête longitudinale du programme LSMS-ISA de la Banque Mondiale couvrant **4 976 ménages** répartis sur l'ensemble du territoire nigérian.

Il couvre les **questions 25 à 29** du TP5 (la Q30 est exclue) :

| Question | Thème                                    | Méthode principale                         |
| -------- | ---------------------------------------- | ------------------------------------------ |
| Q25      | Top 15 des cultures les plus fréquentes  | Fréquences pondérées, barplot horizontal   |
| Q26      | Diversification culturale par ménage     | Histogramme, violin plot, test de Wilcoxon |
| Q27      | Utilisation des engrais par type et zone | Barplot groupé IC 95%, test chi-deux       |
| Q28      | Rendements agricoles par État            | Boxplots, statistiques pondérées           |
| Q29      | Lien engrais chimique × rendement        | Boxplot groupé, Wilcoxon, taille d'effet   |

---

## 📁 Structure du projet

```
TP5/
│
├── README.md                        ← Ce fichier
├── reference.docx                   ← Template Word (mise en forme du rapport)
├── tp5_rapport_final.Rmd            ← Rapport R Markdown final
│
├── R/
│   ├── 00_setup.R                   ← Packages, chemins, design de sondage
│   └── 01_analyses_tp5.R            ← Scripts d'analyse Q25 à Q29
│
├── data/
│   ├── raw/                         ← Fichiers .dta bruts (non versionnés)
│   └── processed/                   ← Données nettoyées .rds
│       ├── cultures_w4.rds
│       ├── cultures_hh_w4.rds
│       └── rendements_w4.rds
│
└── outputs/
    ├── figures/                     ← Graphiques ggplot2 exportés (.png)
    │   ├── Q25_top15_cultures.png
    │   ├── Q26_diversification.png
    │   ├── Q27_engrais.png
    │   ├── Q28_rendements_etat.png
    │   └── Q29_engrais_rendement.png
    └── rapport/
        └── tp5_rapport_final.docx   ← Rapport Word compilé
```

---

## 📦 Données utilisées

Les données sont issues du **Nigeria GHS Panel Wave 4 (2018-2019)**, disponibles sur le portail de la Banque Mondiale ([LSMS-ISA](https://www.worldbank.org/en/programs/lsms/initiatives/lsms-ISA)).

> ⚠️ Les fichiers `.dta` bruts **ne sont pas versionnés** (trop volumineux). Ils doivent être téléchargés séparément et placés dans `data/raw/`.

| Fichier `.dta`        | Contenu                                            | Questions          |
| --------------------- | -------------------------------------------------- | ------------------ |
| `secta3i_harvestw4`   | Cultures de champ récoltées (field crops)          | Q25, Q26, Q28, Q29 |
| `secta3ii_harvestw4`  | Cultures arborées récoltées (tree/permanent crops) | Q25                |
| `sect11a1_plantingw4` | Superficie GPS des parcelles                       | Q28, Q29           |
| `secta11c2_harvestw4` | Utilisation des engrais par parcelle               | Q27, Q29           |
| `secta_plantingw4`    | Poids de sondage (`wt_wave4`), EA, secteur         | Toutes             |

---

## ⚙️ Prérequis et installation

### Version R recommandée

```
R ≥ 4.4.0
```

### Packages R nécessaires

```r
install.packages(c(
  "haven",      # Lecture des fichiers .dta Stata
  "dplyr",      # Manipulation des données
  "tidyr",      # Transformation des données
  "ggplot2",    # Visualisations
  "forcats",    # Réordonner les facteurs
  "scales",     # Formatage des axes
  "stringr",    # Manipulation de chaînes
  "patchwork",  # Assembler plusieurs graphiques
  "srvyr",      # Plan de sondage (version tidyverse)
  "survey",     # Tests pondérés (svychisq, svyranktest)
  "rstatix",    # Tests statistiques (wilcox_effsize)
  "gtsummary",  # Tableaux récapitulatifs
  "Hmisc",      # Statistiques pondérées (wtd.quantile, wtd.mean)
  "readxl",     # Lecture du dictionnaire Excel
  "conflicted", # Gestion des conflits de packages
  "knitr",      # Compilation du rapport
  "rmarkdown"   # Rapport R Markdown
))
```

---

## Reproductibilité

### Étape 1 — Configurer les chemins

Ouvrir `R/00_setup.R` et adapter les chemins à votre machine :

```r
path_raw  <- "data/raw/"       # Dossier des fichiers .dta bruts
path_proc <- "data/processed/" # Dossier des données nettoyées
path_fig  <- "outputs/figures/"# Dossier des graphiques
```

### Étape 2 — Lancer les analyses

```r
source("R/00_setup.R")
source("R/01_analyses_tp5.R")
```

Les graphiques sont automatiquement sauvegardés dans `outputs/figures/` et les données nettoyées dans `data/processed/`.

### Étape 3 — Compiler le rapport

```r
rmarkdown::render("tp5_rapport_final.Rmd",
                  output_file = "outputs/rapport/tp5_rapport_final.docx")
```

---

## Méthodologie

### Plan de sondage

Le GHS Panel W4 utilise un **sondage stratifié à deux degrés** :

- **Strates** : État × zone (rural/urbain) → 408 grappes
- **Unités primaires** : Enumeration Areas (EA)
- **Poids** : `wt_wave4` (cross-section weight)

```r
design <- data %>%
  as_survey_design(ids = ea, strata = sector,
                   weights = wt_wave4, nest = TRUE)
```

### Construction de la superficie (Q28-Q29)

Trois niveaux de priorité pour la superficie des parcelles :

1. **GPS** (`s11aq4c ÷ 10 000`) → fiable, 62% de couverture globale, 85% pour maïs/millet
2. **Déclaré standard** (acres, m², hectares) → biais de surestimation de +28% documenté
3. **Unités locales** (Heaps, Ridges, Stands) → convertis via grille zone-spécifique GHS-Panel

### Traitement des outliers (Q28-Q29)

Exclusion selon la règle **IQR × 3 pondéré** par culture :

```
Outlier si : rendement < Q1_w - 3×IQR_w  ou  rendement > Q3_w + 3×IQR_w
```

- Maïs : 93 parcelles exclues (3,9%)
- Millet : 32 parcelles exclues (3,3%)

### Skip pattern engrais (Q27-Q29)

Les variables NPK (`s11c2q36_1`) et Urée (`s11c2q36_2`) sont issues d'une question conditionnelle posée uniquement aux ménages utilisant des engrais inorganiques (`s11dq1a = 1`, 33,6% des parcelles). Les NA sont recodés à 0 conformément au skip pattern.

---

## Principaux résultats

| Indicateur                              | Résultat                         |
| --------------------------------------- | -------------------------------- |
| 1ère culture (% ménages)                | Maïs — 49,8%                     |
| 2ème culture (% ménages)                | Cassava — 46,3%                  |
| Nb médian de cultures/ménage            | 3 (IQR : 2-4)                    |
| Différence rural/urbain diversification | Non significative (p = 0,131)    |
| Taux adoption engrais inorganiques      | 33,6% des parcelles              |
| Taux adoption NPK                       | 34,9%                            |
| Rendement médian maïs                   | 823 kg/ha (CV = 99%)             |
| Rendement médian millet                 | 613 kg/ha (CV = 96%)             |
| Gain rendement avec engrais             | +59% (p < 0,001, r = 0,198)      |
| Zone la plus productive                 | Nord-Ouest (1 066 kg/ha médiane) |
| Zone la moins productive                | Sud-Sud (297 kg/ha médiane)      |

---

## 📝 Livrables

| Livrable     | Fichier                                     | Description                     |
| ------------ | ------------------------------------------- | ------------------------------- |
| Rapport Word | `outputs/rapport/tp5_rapport_final.docx`    | Rapport complet 13 pages        |
| Figure Q25   | `outputs/figures/Q25_top15_cultures.png`    | Barplot top 15 cultures         |
| Figure Q26   | `outputs/figures/Q26_diversification.png`   | Histogramme + violin plot       |
| Figure Q27   | `outputs/figures/Q27_engrais.png`           | Barplot engrais par zone/État   |
| Figure Q28   | `outputs/figures/Q28_rendements_etat.png`   | Boxplots rendements par État    |
| Figure Q29   | `outputs/figures/Q29_engrais_rendement.png` | Violin plot engrais × rendement |

---

## 📚 Références

- **Banque Mondiale (2019).** Nigeria General Household Survey, Panel 2018-2019, Wave 4. LSMS-ISA. Washington, DC.
- **Banque Mondiale (2019).** Nigeria GHS-Panel W4 — Basic Information Document (BID). Méthodologie du plan de sondage.
- **Kilic, T. et al. (2017).** "Covariate Selection and Poverty Measurement." LSMS Working Paper.
- **Hema, A. (2025-2026).** Énoncé TP5 — Statistiques Descriptives, Tests & Visualisation. ENSAE Pierre NDIAYE.

---

## 🗂️ `.gitignore` recommandé

```gitignore
# Données brutes (volumineuses, confidentielles)
data/raw/

# Fichiers R temporaires
.Rhistory
.RData
.Rproj.user/

# Outputs (optionnel : décommenter si on ne les versionne pas)
# outputs/

# Fichiers système
.DS_Store
Thumbs.db
```

---

_Dernière mise à jour : Avril 2026 | R version 4.4.3_
