# TP1 — Profil démographique des ménages nigérians

## Informations générales

| | |
|---|---|
| **Cours** | Projet Statistique sous R et Python 2025-2026 |
| **Établissement** | ENSAE Pierre Ndiaye, Dakar |
| **Programme** | ISE1-CL |
| **Enseignant** | Aboubacar HEMA |
| **Auteurs** | David Landry AGNANGMA SANAM & Cheikh THIOUB |

## Description

Analyse descriptive du profil démographique des ménages nigérians à partir du
**Nigeria General Household Survey (GHS) Panel** (4 vagues, 2010–2018, LSMS-ISA).

### Tâches réalisées

1. Exploration et vérification de la qualité des données (doublons, valeurs manquantes).
2. Analyse univariée de l'âge : histogramme, boîte à moustaches, statistiques descriptives, test de Shapiro-Wilk.
3. Pyramide des âges par sexe (classes quinquennales) pour la vague 4.
4. Fréquences du lien de parenté avec le chef de ménage, proportions avec IC à 95 %.
5. Comparaison de la taille des ménages rural/urbain (boxplot, test de Wilcoxon, taille d'effet r).
6. Tableau récapitulatif `gtsummary` stratifié par zone avec p-valeurs.
7. Analyses complémentaires : évolution inter-vagues (W1–W4), chefs de ménage féminins.

## Structure du projet

```
TP1/
├── data/                                  <- Données brutes (.dta)
│   ├── sect1_harvestw1.dta ... w4.dta
│   ├── sect1_plantingw1.dta ... w4.dta
│   └── secta_harvestw1.dta ... w4.dta
├── scripts/                               <- Code R modulaire
│   ├── 00_fonctions.R                     <- Packages, chemins, thème, fonctions utilitaires
│   ├── 01_import.R                        <- Chargement et exploration des données
│   ├── 02_nettoyage.R                     <- Nettoyage, recodage, création de variables
│   └── 03_analyse.R                       <- Analyses, tests, graphiques, export des livrables
├── outputs/                               <- Livrables générés automatiquement
│   ├── pyramide_ages_w4.png               <- Pyramide des âges annotée (W4)
│   ├── tableau_gtsummary_zone.docx        <- Tableau gtsummary exportable
│   ├── histogramme_age_w4.png
│   ├── boxplot_taille_zone.png
│   ├── barplot_parente.png
│   └── figure_synthese_tp1.png
├── docs/                                  <- Rapport compilé
│   ├── TP1_Rapport.pdf                    <- Rapport PDF (8 pages)
│   └── TP1_Rapport.Rmd                    <- Source du rapport R Markdown
├── main.R                                 <- Script principal (exécute tout le pipeline)
├
└── README.md                              <- Ce fichier
```

## Livrables

| Livrable demandé | Fichier produit |
|---|---|
| Pyramide des âges annotée par vague (W4) | `outputs/pyramide_ages_w4.png` |
| Tableau gtsummary exportable : statistiques démographiques par zone | `outputs/tableau_gtsummary_zone.docx` |
| Rapport R Markdown de 8 pages commentant chaque analyse | `docs/TP1_Rapport.pdf` |

## Instructions de reproduction

### 1. Préparer les données

Placer tous les fichiers `.dta` dans le dossier `data/`.

### 2. Exécuter le pipeline complet

```r
setwd("chemin/vers/TP1")
source("main.R")
```

Le script `main.R` exécute séquentiellement :

1. `scripts/00_fonctions.R` — Charge les packages, définit les chemins et le thème graphique.
2. `scripts/01_import.R` — Importe et explore les données (doublons, NA).
3. `scripts/02_nettoyage.R` — Nettoie et recode les variables (âge, sexe, parenté, zone, taille ménage).
4. `scripts/03_analyse.R` — Réalise toutes les analyses et génère les livrables dans `outputs/`.

### 3. Compiler le rapport seul

```r
rmarkdown::render("TP1_Rapport.Rmd", output_dir = "docs")
```

### Prérequis

- **R** ≥ 4.0 | **RStudio** recommandé
- **TinyTeX** ou distribution LaTeX (pour le PDF)
- Packages R (installés automatiquement par `00_fonctions.R`) :

`haven`, `dplyr`, `ggplot2`, `tidyr`, `naniar`, `gtsummary`, `rstatix`,
`PropCIs`, `patchwork`, `scales`, `moments`, `forcats`, `knitr`, `kableExtra`,
`flextable`, `rmarkdown`

Si des packages LaTeX manquent :

```r
tinytex::tlmgr_install(c("titlesec", "caption", "fancyhdr",
                          "booktabs", "float", "xcolor"))
```

## Tests statistiques

| Test | Objectif | Tâche |
|---|---|---|
| Shapiro-Wilk | Normalité de la distribution de l'âge | 2 |
| Wilcoxon-Mann-Whitney | Comparaison taille ménage rural/urbain | 5 |
| Chi-deux (via gtsummary) | Association sexe × zone | 6 |
| Test binomial exact | IC à 95 % des proportions de parenté | 4 |

## Source des données

Nigeria General Household Survey (GHS) Panel, Waves 1–4 (2010–2018).
Programme LSMS-ISA, Banque Mondiale.
https://microdata.worldbank.org/index.php/catalog/lsms
