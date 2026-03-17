# TP2: Éducation et alphabétisation des membres des ménages  nigérians

## Description
Ce projet analyse les données de l'enquête LSMS (Living Standards Measurement Study) vague 4 sur l'éducation et l'alphabétisation au Nigéria. Il explore les niveaux d'éducation, les disparités par sexe, âge et région, ainsi que les taux de scolarisation.

## Structure du projet
- `data/raw/`: Données brutes (fichiers Stata .dta et shapefiles pour les cartes)
- `data/processed/`: Données traitées (optionnel)
- `scripts/`: Scripts R pour le traitement et l'analyse
- `outputs/tables/`: Tableaux de résultats (fréquences, tests statistiques, etc.)
- `outputs/graphs/`: Graphiques et cartes géographiques

## Dépendances
- R (version 4.0+ recommandée)
- Packages R : haven, dplyr, ggplot2, naniar, tidyr, scales, rstatix, gtsummary, gt, forcats, binom, sf, viridis

## Installation et exécution
1. Ouvrir le projet `TP2.Rproj` dans RStudio
2. Installer les packages si nécessaire : `install.packages(c("haven", "dplyr", "ggplot2", ...))`
3. Exécuter le script principal : `source("scripts/script_TP2.R")`

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

