# TP1 — Projet Statistique sous R et Python

Groupe 4 | ENSAE Pierre Ndiaye, ISE1, 2025-2026  
Auteurs : TEVOEDJRE Michel, DICKO Hamadou  
Encadrant : Aboubacar HEMA

---

## Objet du travail

Ce dépôt contient les scripts, données traitées et rapport du premier travail pratique du cours de Projet Statistique sous R et Python. L'analyse porte sur les données de l'enquête Nigeria General Household Survey-Panel Wave 4 (NBS, 2018/19), section 1 relative aux caractéristiques des membres des ménages.

Les objectifs sont : vérifier la qualité des données, décrire la structure démographique de la population enquêtée, estimer des proportions avec intervalles de confiance, et comparer la taille des ménages urbains et ruraux à l'aide d'un test non paramétrique.

---

## Structure du dépôt

```
.
├── data/
│   ├── raw/
│   │   └── sect1_harvestw4.dta       # Données brutes (NBS Nigeria, GHSP-W4)
│   └── processed/
│       ├── data_brute.rds            # Données brutes recodées
│       ├── data_indiv.rds            # Données individuelles enrichies
│       ├── base_menage.rds           # Agrégats au niveau ménage
│       ├── proportions_IC.rds        # Proportions et IC 95% (lien de parenté)
│       └── resultats_wilcoxon.rds    # Résultats du test de Wilcoxon
├── scripts/
│   ├── 01_preparation_donnees.R      # Chargement, recodage, création des bases
│   └── 02_analyses.R                 # Analyses statistiques et visualisations
├── outputs/
│   ├── fig00_vis_miss.png            # Carte des valeurs manquantes
│   ├── fig01_histogramme_age.png     # Histogramme de l'âge
│   ├── fig02_boxplot_age.png         # Boîte à moustaches de l'âge
│   ├── fig03_pyramide_ages.png       # Pyramide des âges par sexe
│   ├── fig04_lien_parente.png        # Fréquence du lien de parenté
│   ├── fig05_boxplot_menages.png     # Taille des ménages Urbain vs Rural
│   └── tableau_gtsummary.html        # Tableau de statistiques stratifié (gtsummary)
├── rapport/
│   ├── TP1_Groupe4_TEVOEDJRE_Michel_DICKO_Hamadou.Rmd                 # Rapport principal (R Markdown)
│   ├── TP1_Groupe4_TEVOEDJRE_Michel_DICKO_Hamadou.pdf                 # Rapport principal (pdf)
│   └── logos/
│       ├── ENSAE.PNG
│       ├── ANSD.png
│       └── SN.PNG
├── main.R                            # Script principal (orchestre tout le pipeline)
└── README.md
```

---

## Reproductibilité

Pour reproduire l'intégralité du travail, ouvrir le projet RStudio (`*.Rproj`) puis exécuter :

```r
source("main.R")
```

Le script `main.R` installe les packages manquants, télécharge les données depuis GitHub si elles sont absentes, puis enchaîne les deux scripts d'analyse. Les figures et tableaux sont enregistrés dans `outputs/`.

Pour compiler le rapport, ouvrir `rapport/rapport.Rmd` dans RStudio et cliquer sur **Knit**, ou exécuter :

```r
rmarkdown::render("rapport/rapport.Rmd")
```

La compilation requiert une distribution LaTeX (TinyTeX recommandé : `tinytex::install_tinytex()`).

---

## Packages R utilisés

| Package     | Usage principal                                    |
|-------------|---------------------------------------------------|
| haven       | Lecture des fichiers Stata (.dta)                 |
| dplyr       | Manipulation et transformation des données        |
| ggplot2     | Visualisations graphiques                         |
| naniar      | Analyse et visualisation des valeurs manquantes   |
| apyramid    | Pyramide des âges                                 |
| PropCIs     | Intervalles de confiance exacts (Clopper-Pearson) |
| gtsummary   | Tableau de statistiques stratifié                 |
| gt          | Export du tableau gtsummary                       |
| forcats     | Manipulation des variables catégorielles          |
| scales      | Formatage des axes et étiquettes                  |
| knitr / kableExtra | Tableaux dans le rapport R Markdown        |

---

## Principaux résultats

- La base couvre environ 38 000 individus dans plus de 5 000 ménages, avec très peu de valeurs manquantes sur les variables clés.
- La distribution de l'âge est significativement non normale (test de Shapiro-Wilk, p < 2,2e-16), avec une structure démographique jeune typique d'un pays en développement.
- Les enfants représentent la catégorie de parenté la plus fréquente, devant les chefs de ménage et les conjoints.
- Les ménages ruraux sont significativement plus grands que les ménages urbains (test de Wilcoxon-Mann-Whitney, p < 2,2e-16 ; taille d'effet r jugée petite à modérée).

---

## Source des données

National Bureau of Statistics (NBS) Nigeria — General Household Survey Panel, Wave 4, 2018/2019.  
Section 1 : Caractéristiques des membres du ménage (`sect1_harvestw4.dta`).