
---

# Nigeria GHS-Panel Analysis | TP 5 : Cultures, Intrants et Rendements

Ce projet réalise une analyse statistique approfondie du secteur agricole au Nigeria, en se basant sur les données de la **vague 4 (2018-2019)** de l'enquête *General Household Survey (GHS) Panel* de la Banque Mondiale (LSMS-ISA). 

L'étude se concentre sur trois piliers : la diversité des cultures, l'adoption des intrants chimiques et l'analyse de la productivité (rendements) du maïs et du millet.

## 📌 Objectifs du TP
* **Cartographie des cultures** : Identifier et classifier les 15 cultures prédominantes.
* **Diversification** : Mesurer l'indice de diversification culturale par ménage et comparer les milieux urbains et ruraux.
* **Modernisation** : Analyser les taux d'utilisation des engrais (organiques vs chimiques) et leur impact réel sur la productivité.
* **Analyse de performance** : Calculer les rendements à l'hectare, traiter les valeurs aberrantes et tester statistiquement les différences entre États.
* **Évolution** : Comparer l'adoption des intrants entre la vague 1 (2010) et la vague 4 (2018).

---

## 📁 Structure du Projet
Le projet est organisé de manière modulaire pour garantir la reproductibilité :

```text
PROJET_TP5_NIGERIA/
├── data/
│   ├── raw/             # Fichiers .dta bruts (secta3i, sect11a1, secta11c2, etc.)
│   └── processed/       # Bases de données nettoyées et fichiers .rds pour le rapport
├── scripts/
│   ├── 01_preparation.R    # Import, nettoyage, jointures logiques et gestion des outliers
│   ├── 02_analyses.R       # Calculs statistiques, tests de Wilcoxon/Chi-deux et graphiques
├── outputs/
│   ├── figures/         # Barplots, Boxplots, Violin plots (format PNG)
│   └── tables/          # Tableaux gtsummary exportés
├── rapport/
│   ├── rapport.Rmd      # Source RMarkdown du rapport final
│   └── reference.docx   # Document de style pour la mise en forme Word
└── README.md            # Documentation du projet (ce fichier)
└── main.R               # Script maître (chef d'orchestre) pour tout exécuter
```

---

## 🛠️ Installation et Prérequis

### 1. Logiciels nécessaires
* **R** (version 4.0 ou supérieure)
* **RStudio**
* **Pandoc** (généralement inclus avec RStudio pour le RMarkdown)

### 2. Librairies R utilisées
Le projet s'appuie sur la suite `tidyverse` et des packages statistiques spécialisés :
```R
install.packages(c("tidyverse", "haven", "here", "labelled", "rstatix", 
                   "ggpubr", "scales", "patchwork", "gtsummary", "rmarkdown"))
```

---

## 🚀 Comment exécuter le projet ?

Il n'est pas nécessaire de lancer les scripts un par un. Tout a été automatisé :

1.  **Placer les données** : Copiez vos fichiers Stata (`.dta`) dans le dossier `data/raw/`.
2.  **Ouvrir le projet** : Ouvrez votre session RStudio dans le dossier racine du projet.
3.  **Lancer le script main.R ** : pour lancer, tapez dans la console de R : source("main.R) et tout le projet va s'exécuter.

**Ce que fait le script `main.R` :**
1.  Il initialise l'environnement et charge les librairies.
2.  Il appelle `01_preparation.R` pour fusionner les fichiers de récolte (`harvest`) et de plantation (`planting`).
3.  Il exécute `02_analyses.R` pour générer tous les indicateurs et graphiques.
4.  Il compile automatiquement le fichier `rapport.Rmd` pour générer le rapport final au format Word dans le dossier `outputs/`.

---

## 📊 Méthodologie Statistique
* **Nettoyage des données** : Les rendements extrêmes sont traités par la méthode de l'Écart Interquartile (**IQR × 3**) pour éviter de biaiser les moyennes.
* **Tests de Comparaison** : 
    * **Test de Wilcoxon-Mann-Whitney** pour comparer les rendements et la diversification (distributions non normales).
    * **Test du Chi-deux de Pearson** pour l'association entre la zone géographique et l'utilisation d'engrais.
* **Visualisation** : Utilisation de `ggplot2` avec des palettes de couleurs harmonisées (normes FAO/IFPRI).

---

## ✍️ Auteurs
* **OUATTARA Ousmane** - Élève Ingénieur Statisticien Économiste (ENSAE Dakar)
* **LESLYE NKWA** - Élève Ingénieur Statisticien Économiste (ENSAE Dakar)

---
*Dernière mise à jour : Avril 2026*