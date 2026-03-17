# TP1 : Profil Démographique des Ménages (Nigeria GHS Panel - Vague 4)

Ce projet analyse le profil démographique des ménages nigérians à partir des données de la Nigeria General Household Survey (GHS) Panel, vague 4 (2018-2019).

## Structure du Projet

```
TP1/
├── data/
│   ├── raw/                 # Données brutes (.dta)
│   └── processed/           # Données traitées (non utilisées ici)
├── outputs/
│   ├── graphs/              # Graphiques générés (histogrammes, pyramide des âges, etc.)
│   └── tables/              # Tableaux CSV et HTML (statistiques, fréquences, etc.)
├── scripts/
│   └── script_TP1.R         # Script principal d'analyse
├── docs/
│   ├── Rapport.Rmd          # Rapport détaillé en R Markdown
│   ├── Rapport.pdf          # Rapport compilé (si généré)
│   └── header.tex           # En-tête LaTeX pour le rapport
├── README.md                # Ce fichier
└── TP1.Rproj                # Projet R
```

## Analyses Réalisées

- **Exploration des données** : Chargement, structure, doublons et valeurs manquantes.
- **Analyse univariée de l'âge** : Statistiques descriptives, test de normalité (Shapiro-Wilk), histogramme et boîte à moustaches.
- **Pyramide des âges** : Répartition par sexe et groupes d'âge quinquennaux.
- **Liens de parenté** : Fréquences des relations (chef, conjoint, enfant, autre) avec intervalles de confiance.
- **Taille des ménages** : Comparaison rural/urbain avec test de Wilcoxon-Mann-Whitney et taille d'effet.
- **Tableau récapitulatif** : Caractéristiques démographiques stratifiées par zone.

## Données

- **Sources** : `sect1_harvestw4.dta` (démographie individuelle) et `secta_harvestw4.dta` (couverture géographique).
- **Variables clés** : âge (`s1q4`), sexe (`s1q2`), lien de parenté (`s1q3`), secteur (`sector`).

## Dépendances

- R version >= 4.0
- Packages : `haven`, `dplyr`, `ggplot2`, `naniar`, `tidyr`, `scales`, `rstatix`, `moments`, `coin`, `gtsummary`, `gt`

## Exécution

1. Ouvrir `TP1.Rproj` dans RStudio.
2. Exécuter `scripts/script_TP1.R` pour reproduire les analyses.
3. Les outputs sont sauvegardés dans `outputs/`.

## Rapport

Consulter `docs/Rapport.Rmd` pour une présentation détaillée des résultats.                  

