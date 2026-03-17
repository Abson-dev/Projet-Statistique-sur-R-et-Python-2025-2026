# TP3 : Accès aux services de santé et chocs sanitaires des ménages

Ce projet analyse les données de santé de l'Enquête Générale des Ménages du Nigeria (GHS) - vague 2 (2012-2013). Il étudie la morbidité, le recours aux soins, les dépenses de santé et leurs liens avec les caractéristiques démographiques et socio-économiques.

## Structure du travail

TP3/
│ 
│── data/                      # Données 
│     ├── raw/                 # Données brutes (fichiers .dta de l'enquête GHS)
│     └── processed/           # Données traitées (non utilisées dans ce TP)
│ 
│── outputs/
│     ├── graphs/              # Visualisations générées (graphiques)
│     └── tables/              # Tableaux générés (CSV et TXT)
│ 
│── scripts/
│     └── script_TP3.R         # Script principal R pour analyses descriptives et tests
│
│── docs/
│     ├── Rapport.Rmd          # Rapport R Markdown
│     ├── Rapport.pdf          # Rapport compilé (généré)
│     └── autres fichiers LaTeX
│ 
│── README.md                  # Documentation du TP
│── TP3.Rproj                  # Projet RStudio



## Données utilisées
- `sect4a_harvestw2.dta` : Module santé
- `sect1_harvestw2.dta` : Composition des ménages
- `cons_agg_wave2_visit2.dta` : Dépenses de consommation

## Analyses réalisées
- Statistiques descriptives sur la morbidité, le recours aux soins et les dépenses
- Tests statistiques (Wilcoxon) pour comparer rural/urbain
- Visualisations et tableaux exportés dans `outputs/`
- etc.

## Comment exécuter
1. Ouvrir `TP3.Rproj` dans RStudio
2. Installer les dépendances : `install.packages(c("haven", "dplyr", "ggplot2", "naniar", "tidyr", "scales", "rstatix"))`
3. Exécuter `scripts/script_TP3.R`
4. Compiler le rapport : `docs/Rapport.Rmd` (nécessite knitr, kableExtra)

## Auteur
[Nom de l'étudiant]                  

