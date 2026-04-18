# TP 4 — Analyse des Parcelles Agricoles au Nigeria
### Nigeria General Household Survey Panel (vague 4, 2018)
**ENSAE ISE 1 | Groupe 18 : KEBJAM Jackson & NGOYI Parfait Jemmy Prodige | 2025–2026**

---

## 📋 Description du projet

Ce projet réalise une analyse statistique complète des superficies agricoles nigérianes à partir du **Nigeria General Household Survey (GHS) Panel** de la Banque mondiale. Il couvre quatre vagues d'enquête longitudinales (W1 à W4, 2010–2018) et s'appuie sur la vague 4 (2018–2019) pour les analyses principales.

### Objectifs
1. Évaluer la **qualité des données** déclaratives (valeurs manquantes et aberrantes)
2. Réaliser une **analyse univariée pondérée** des superficies agricoles
3. Comparer les **superficies déclarées vs. mesures GPS**
4. Analyser les **régimes de tenure foncière** et leur lien avec la zone de résidence
5. Explorer la **fragmentation des exploitations** (superficie totale × nombre de parcelles)
6. Cartographier les **disparités géographiques** entre États du Nigeria

---

## 🗂️ Structure du projet

```
TP_4_Groupe_18_KEBJAM_Jackson_NGOYI_Parfait_Jemmy_Prodige/
│
├── README.md                          # Ce fichier
├── rapport_final.Rmd                  # Rapport principal (R Markdown → PDF)
│
├── R/
│   └── fonctions.R                    # Fonctions utilitaires du projet
│
├── data/
│   ├── raw/                           # Données brutes (non modifiées)
│   │   ├── secta_harvestw4.dta
│   │   ├── secta1_harvestw4.dta
│   │   ├── sect11a1_plantingw4.dta
│   │   └── sect11b1_plantingw4.dta
│   └── processed/                     # Données nettoyées et transformées
│       ├── parcelles_agricoles_w4.rds
│
└── output/
    ├── figures/                        # Graphiques exportés
    │   ├── Fig01_histogrammes.pdf
    │   ├── Fig02_boxplots.pdf
    │   ├── Fig03_scatter_gps.pdf
    │   ├── Fig04_tenure_barplot.pdf
    │   ├── Fig05_loess_parcelles.pdf
    │   ├── Fig06_heatmap_etats.pdf
    │   └── Fig07_carte_nigeria.pdf
    └── tables/                         # Tableaux exportés
        ├── T01_sources_donnees.tex
        ├── T02_conversion_unites.tex
        ├── T03_valeurs_manquantes.tex
        ├── T04_valeurs_aberrantes.tex
        ├── T05_stats_descriptives.tex
        ├── T06_deciles.tex
        ├── T07_biais_gps.tex
        └── T08_tenure.tex
```

---

## 📦 Packages R requis


```r
# Packages de manipulation de données
library(dplyr)
library(tidyr)
library(readr)

# Packages de visualisation
library(ggplot2)
library(scales)
library(patchwork)
library(viridis)
library(forcats)
library(maps)

# Package de rendu de tableaux
library(knitr)   # knitr::kable uniquement — PAS kableExtra

# Packages de reporting
# (rmarkdown est nécessaire pour compiler le .Rmd)
```

### Installation des packages

```r
install.packages(c(
  "dplyr", "tidyr", "readr",
  "ggplot2", "scales", "patchwork",
  "viridis", "forcats", "maps",
  "knitr", "rmarkdown"
))
```


### Vérifier l'installation de XeLaTeX

```r
# Dans la console R
tinytex::tlmgr_install(c(
  "booktabs", "longtable", "colortbl",
  "xcolor", "tcolorbox", "fancyhdr",
  "caption", "enumitem", "makecell"
))
```

---

## ▶️ Compilation du rapport

### Méthode 1 — Via RStudio
1. Ouvrir `rapport_final.Rmd` dans RStudio
2. Cliquer sur le bouton **Knit** (ou `Ctrl + Shift + K`)
3. Le PDF est généré dans le même répertoire

### Méthode 2 — Via la console R

```r
# Définir le répertoire de travail
setwd("chemin/vers/TP_4_Groupe_18_KEBJAM_Jackson_NGOYI_Parfait_Jemmy_Prodige/rapport")

# Compiler le rapport
rmarkdown::render(
  input       = "rapport_final.Rmd",
  output_format = "pdf_document",
  output_file = "rapport_final.pdf"
)
```

---

## 📊 Sources de données

| Fichier                    | Contenu                                      | Variables clés                              |
|:---------------------------|:---------------------------------------------|:--------------------------------------------|
| `secta_harvestw4`          | Stratification, poids de sondage             | `hhid`, `zone`, `state`, `sector`, `wt_wave4` |
| `secta1_harvestw4`         | Superficie déclarée, mesure GPS              | `plotid`, `sa1q11`, `prefilled_gps_area`    |
| `sect11a1_plantingw4`      | Superficie en unités locales                 | `s11aq4aa`, `s11aq4b`                       |
| `sect11b1_plantingw4`      | Mode d'acquisition foncière                  | `s11b1q4`                                   |


---

## 🔑 Méthodologie clé

### Construction de la variable superficie (ha)

Les superficies déclarées en unités hétérogènes sont converties selon la hiérarchie suivante :

```
superficie_ha = coalesce(
  conversion_standard(ha / acres / m²),
  heaps  × facteur_régional,
  ridges × facteur_régional,
  stands × facteur_régional
)
```

### Facteurs de conversion régionaux (source : LSMS-ISA)

| Zone            | Heaps (ha) | Ridges (ha) | Stands (ha) |
|:----------------|-----------:|------------:|------------:|
| North Central   | 0.00012    | 0.0027      | 0.00006     |
| North East      | 0.00016    | 0.0040      | 0.00016     |
| North West      | 0.00011    | 0.00494     | 0.00004     |
| South East      | 0.00019    | 0.0023      | 0.00004     |
| South South     | 0.00021    | 0.0023      | 0.00013     |
| South West      | 0.00012    | 0.00001     | 0.00041     |

### Règles d'exclusion (valeurs aberrantes)

- Superficies **≤ 0 ha** → exclues (erreur de saisie)
- Superficies **> 500 ha** → exclues (erreur de conversion)
- Observations conservées dans la base brute pour analyses de sensibilité

---

## 📈 Principaux résultats

| Indicateur                             | Valeur          |
|:---------------------------------------|:----------------|
| Superficie médiane / parcelle (W4)     | ~0,32 ha        |
| Superficie médiane / ménage (W4)       | ~0,88 ha        |
| Coefficient de variation (ménage)      | ~404 %          |
| Corrélation Spearman (déclaré vs GPS)  | ~0,70           |
| Part de sous-déclaration               | ~38,7 %         |
| Tenure dominante                       | Héritage (~58 %)|
| Corrélation Spearman (nb parcelles / superficie) | ~0,43 |


---

## 👥 Auteurs

| Nom                          | Rôle                     |
|:-----------------------------|:-------------------------|
| KEBJAM Jackson               | Analyse, rédaction, code |
| NGOYI Parfait Jemmy Prodige  | Analyse, rédaction, code |

**Encadrement : Aboubacar HEMA ** chargé du Cours R et Python, ISE 1, 2025–2026

---

