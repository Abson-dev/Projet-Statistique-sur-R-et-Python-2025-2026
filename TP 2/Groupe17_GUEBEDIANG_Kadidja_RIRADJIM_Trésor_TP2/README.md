# Analyse des déterminants de l'éducation au Nigeria

## Description du projet

Ce projet analyse les déterminants de l'éducation au Nigeria à partir des données de la Nigeria General Household Survey (GHS), vague 4 (2018-2019).

L’objectif est d’identifier et de mesurer les principales inégalités éducatives selon le sexe, l’âge, la zone de résidence (urbaine/rurale) et la localisation géographique (État).

Cette étude vise à fournir des éléments d’analyse statistique utiles à la compréhension des disparités éducatives et à l’orientation des politiques publiques.

---

## Objectifs

* Décrire la structure du niveau d'éducation de la population
* Analyser les inégalités de genre
* Étudier les différences intergénérationnelles
* Évaluer la scolarisation des enfants
* Identifier les disparités régionales

---

## Structure du projet

```
projet/
│
├── rapport/
│   ├── rapor.Rmd          # Script RMarkdown
│   ├── rapor.pdf          # Rapport final
│   ├── logo1.png
│   └── logo2.jpeg
│
├── data/
│   └── raw/               # Données sources (.dta)
│
├── output/
│   ├── figures/           # Graphiques générés
│   └── tables/            # Tableaux générés
│
└── README.md
```

---

## Méthodologie

L’analyse repose sur des méthodes statistiques descriptives et inférentielles :

* Statistiques descriptives (effectifs, proportions)
* Tableaux de contingence
* Test du Chi² d’indépendance
* Test de Kruskal-Wallis
* Test post-hoc de Dunn
* Visualisation des données avec ggplot2

---

## Packages utilisés

* dplyr
* ggplot2
* haven
* kableExtra
* rstatix
* DescTools
* viridis
* patchwork
* gtsummary
* openxlsx

---

## Exécution du projet

1. Placer les fichiers de données (.dta) dans le dossier :

```
data/raw/
```

2. Ouvrir le fichier :

```
rapport/rapor.Rmd
```

3. Compiler le rapport :

```r
rmarkdown::render("rapport/rapor.Rmd")
```

---

## Prérequis

* R (version 4.0 ou supérieure)
* RStudio recommandé

Installation des packages nécessaires :

```r
install.packages(c(
  "dplyr", "ggplot2", "haven", "kableExtra",
  "rstatix", "DescTools", "viridis",
  "patchwork", "gtsummary", "openxlsx"
))
```

---

## Principaux résultats

* Une part importante de la population présente un faible niveau d’éducation
* Des inégalités significatives entre hommes et femmes
* Une amélioration du niveau éducatif chez les jeunes générations
* Des écarts marqués entre zones rurales et urbaines
* De fortes disparités régionales, notamment au Nord

---

## Limites

* Présence de données manquantes
* Variable d’éducation auto-déclarée
* Absence d’indicateurs de qualité de l’éducation
* Nature transversale des données (pas d’inférence causale)

---

## Auteurs

* Kadidja GUEBEDIANG A NKEN
* Tresor RIRADJIM NGARMOUNDOU

---

## Contexte académique

Projet réalisé dans le cadre de la formation ISE1-CL
ENSAE Sénégal
Année académique : 2025-2026

---

## Licence

Ce projet est destiné à un usage académique.
