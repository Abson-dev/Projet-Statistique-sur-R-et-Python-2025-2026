# Analyse des déterminants de l'éducation au Nigeria

## Description du projet

Ce projet analyse les déterminants du niveau d’éducation au Nigeria à partir des données de la Nigeria General Household Survey (GHS), vague 4 (2018-2019).

L’analyse intègre les pondérations d’enquête afin de produire des estimations représentatives de la population.

L’objectif est d’identifier et de mesurer les principales inégalités éducatives selon le sexe, l’âge, la zone de résidence (urbaine/rurale) et la localisation géographique (État).

---

## Objectifs

* Décrire la structure du niveau d'éducation de la population (pondérée)
* Analyser les inégalités de genre
* Étudier les différences intergénérationnelles
* Évaluer la scolarisation des enfants
* Identifier les disparités régionales

---

## Structure du projet

projet/
│
├── rapport/
│   ├── rapor.Rmd
│   ├── rapor.pdf
│   ├── logo1.png
│   └── logo2.jpeg
│
├── data/
│   ├── raw/
│   └── processed/
│
├── output/
│   ├── figures/
│   └── tables/
│
└── README.md

---

## Méthodologie

### 1. Préparation des données
* Importation des fichiers
* Nettoyage des poids (wt_wave4)
* Fusion des bases
* suppression de 49 observations de la base secta_w4 car n'ayant pas compléter le questionnaire
* 4 observations de sect2_w4 et sect1_W4 ont également été supprimée car la base secta_w4 mentionnent que ces observation n'ont pas terminé l'enquête
selon la variable interview_result de la base secta_w4

### 2. Variables
* Recodage du niveau d’éducation
* Création de l’âge et des groupes d’âge

### 3. Pondération
Utilisation du package survey

### 4. Analyses
* Statistiques descriptives pondérées
* Chi² pondéré
* V de Cramer
* Kruskal-Wallis
* Visualisation

---

## Exécution

1. Placer les données dans data/raw/
2. Ouvrir rapport/rapor.Rmd
3. Lancer :

rmarkdown::render("rapport/rapor.Rmd")

---

## Sorties

Graphiques : output/figures/
Tableaux : output/tables/resultats_analyse.xlsx

---

## Auteurs

Kadidja GUEBEDIANG A NKEN  
Trésor RIRADJIM NGARMOUNDOU
