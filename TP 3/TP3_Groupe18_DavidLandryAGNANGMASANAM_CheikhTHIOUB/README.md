# TP3 -- Acces aux soins et depenses de sante des menages nigerians

## Informations generales

| | |
|---|---|
| **Cours** | Projet Statistique sous R et Python 2025-2026 |
| **Etablissement** | ENSAE Pierre Ndiaye, Dakar |
| **Programme** | ISE1-CL |
| **Enseignant** | Aboubacar HEMA |
| **Auteurs** | David Landry AGNANGMA SANAM & Cheikh THIOUB |

## Description

Analyse descriptive de l'acces aux services de sante et des depenses de sante
des menages nigerians a partir de la **section 4a** (sante) du GHS Panel,
vague 4 (2018), programme LSMS-ISA de la Banque Mondiale.

### Taches realisees

13. Taux de morbidite par sexe et groupe d'age, barplots avec IC a 95%.
14. Distribution des types de maladies declarees (top 10), barplot colore par categorie.
15. Recours aux soins : frequence de consultation par type de prestataire.
16. Distribution des depenses de sante : histogramme log, statistiques par decile.
17. Test d'independance recours aux soins x quintile de richesse (chi-deux, V de Cramer).
18. Comparaison des depenses medianes par zone et par quintile (Wilcoxon, Kruskal-Wallis, violin plots).

## Structure du projet

```
TP3/
+-- data/                                          <- Donnees brutes (.dta)
|   +-- sect4a_harvestw4.dta                       <- Section sante (morbidite, soins, depenses)
|   +-- sect1_harvestw4.dta                        <- Composition menage (sexe, age)
|   +-- totcons_final.dta                          <- Agregats de consommation (quintiles)
+-- scripts/                                       <- Code R modulaire
|   +-- 00_fonctions.R                             <- Packages, chemins, theme, fonctions
|   +-- 01_import.R                                <- Chargement et exploration
|   +-- 02_nettoyage.R                             <- Nettoyage, recodage, jointures
|   +-- 03_analyse.R                               <- Analyses, tests, generation des livrables
+-- outputs/                                       <- Livrables generes par les scripts
|   +-- barplot_maladies_prestataires.png           <- Barplot maladies + prestataires
|   +-- violin_depenses_zone_quintile.png           <- Violin plots depenses par zone/quintile
|   +-- tableau_contingence_soins_quintile.csv      <- Tableau de contingence (CSV)
|   +-- tableau_contingence_soins_quintile_tests.txt <- Resultats des tests
|   +-- barplot_morbidite_sexe_age.png
|   +-- barplot_recours_quintile.png
|   +-- histogramme_depenses_log.png
+-- docs/                                          <- Rapport
|   +-- TP3_Rapport.Rmd                            <- Source du rapport
|   +-- TP3_Rapport.pdf                            <- Rapport compile (8-10 pages)
+-- main.R                                         <- Script principal
+-- README.md                                      <- Ce fichier
```

## Livrables

| Livrable demande | Fichier produit |
|---|---|
| Barplot des types de maladies et des prestataires consultes | `outputs/barplot_maladies_prestataires.png` |
| Violin plots des depenses de sante par zone et par quintile | `outputs/violin_depenses_zone_quintile.png` |
| Tableau de contingence recours aux soins x quintile avec tests | `outputs/tableau_contingence_soins_quintile.csv` + `.txt` |
| Rapport de 8-10 pages | `docs/TP3_Rapport.pdf` |

## Instructions de reproduction

### 1. Preparer les donnees

Placer les trois fichiers `.dta` dans le dossier `data/` :
- `sect4a_harvestw4.dta`
- `sect1_harvestw4.dta`
- `totcons_final.dta`

### 2. Executer le pipeline (scripts + outputs)

```r
setwd("chemin/vers/TP3")
source("main.R")
```

### 3. Compiler le rapport PDF (separement)

```r
rmarkdown::render("docs/TP3_Rapport.Rmd", output_dir = "docs")
```

### Prerequis

- **R** >= 4.0 | **RStudio** recommande
- **TinyTeX** ou distribution LaTeX pour le PDF
- Packages R (installes automatiquement) :
  `haven`, `dplyr`, `ggplot2`, `tidyr`, `naniar`, `gtsummary`, `rstatix`,
  `patchwork`, `scales`, `moments`, `forcats`, `knitr`, `kableExtra`,
  `flextable`, `rmarkdown`

## Correspondance variables enonce / fichiers reels

L'enonce du TP3 fait reference aux fichiers `sect3a/sect3b` avec des variables
`s3aq1`, `s3aq3`, `s3bq1`, `s3bq8`. Dans la Wave 4 du GHS Panel, la section
sante est en realite dans `sect4a_harvestw4.dta` avec les variables :

| Variable enonce | Variable reelle | Description |
|---|---|---|
| s3aq1 (maladie) | s4aq3 | Maladie/blessure dans les 4 dernieres semaines |
| s3aq3 (type maladie) | s4aq3b_1 | Type de maladie (27 modalites) |
| s3bq1 (consultation) | s4aq1 + s4aq6a | A consulte + type de prestataire |
| s3bq8 (depense sante) | s4aq9 + s4aq14 + s4aq17 | Consultation + medicaments + hospitalisation |

## Tests statistiques

| Test | Objectif | Tache |
|---|---|---|
| Chi-deux | Independance recours aux soins x quintile | 17 |
| V de Cramer | Force de l'association | 17 |
| Wilcoxon-Mann-Whitney | Depenses medianes rural vs urbain | 18 |
| Kruskal-Wallis | Depenses medianes entre quintiles | 18 |

## Source des donnees

Nigeria General Household Survey (GHS) Panel, Wave 4 (2018).
Programme LSMS-ISA, Banque Mondiale.
https://microdata.worldbank.org/index.php/catalog/lsms
