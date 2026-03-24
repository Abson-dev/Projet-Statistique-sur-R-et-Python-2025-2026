# TP2: Analyse statistique de l'éducation et de l'alphabétisation au Nigéria

## Description
Ce projet analyse les données de l'enquête LSMS (Living Standards Measurement Study) vague 4 sur l'éducation et l'alphabétisation au Nigéria. Il explore les niveaux d'éducation, les disparités par sexe, âge et région, ainsi que les taux de scolarisation.

## Structure du projet
```
TP2/
├── data/
│   ├── raw/                 # Données brutes (.dta)
│   └── processed/           
├── outputs/
│   ├── graphs/              # Graphiques générés 
│   └── tables/              # Tableaux CSV
├── scripts/
│   └── script_TP2.R         # Script principal d'analyse
├── docs/
│   ├── Rapport.Rmd          # Rapport détaillé en R Markdown
│   └── Rapport.docx         # Rapport compilé en word
├── README.md                # Ce fichier
└── TP2.Rproj                # Projet R
```

## Dépendances
- Packages R : haven, dplyr, ggplot2, naniar, tidyr, scales, rstatix,survey, Hmisc, gtsummary, gt, forcats, binom, sf, viridis

## Installation et exécution
1. Ouvrir le projet `TP2.Rproj` dans RStudio
2. Exécuter le script principal : `source("scripts/script_TP2.R")`

## Analyses réalisées
- Chargement et exploration des données (sect1, sect2, secta)
- Gestion des valeurs manquantes et vérification de la cohérence
- Construction de la variable "niveau d'éducation"
- Analyses univariées (fréquences des niveaux d'éducation)
- Comparaisons bivariées : sexe vs niveau d'éducation (Chi-deux, V de Cramér)
- Comparaisons par âge (Kruskal-Wallis, test de Dunn)
- Analyses géographiques : taux de scolarisation par état et zone
- Visualisations : barplots, cartes choroplèthes

Les résultats sont sauvegardés dans `outputs/` pour consultation.                  

