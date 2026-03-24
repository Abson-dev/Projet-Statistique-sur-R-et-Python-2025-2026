# TP3 -- Projet Statistique sous R et Python

Groupe 4 | ENSAE Pierre Ndiaye, ISE1, 2025-2026  
Auteurs : TEVOEDJRE Michel, DICKO Hamadou  
Encadrant : Aboubacar HEMA

---

## Objet du travail

Ce depot contient les scripts, donnees traitees et rapport du troisieme travail pratique du cours de Projet Statistique sous R et Python. L'analyse porte sur la sante, la morbidite et les depenses de soins des membres des menages nigerians, a partir de la section 4a de l'enquete Nigeria General Household Survey-Panel Wave 4 (NBS, 2018/19).

Les objectifs sont : analyser les taux de morbidite par sexe et groupe d'age, identifier les types d'affections declarees, decrire le recours aux soins par type de prestataire, etudier la distribution des depenses de sante, et tester les liens entre richesse, milieu de residence et comportements de sante (Chi-deux, Wilcoxon-Mann-Whitney).

---

## Structure du depot

```
.
├── data/
│   ├── raw/
│   │   ├── sect1_harvestw4.dta       # Caracteristiques individuelles (sexe, age, milieu)
│   │   ├── sect3a_harvestw4.dta      
│   │   ├── sect3b_harvestw4.dta      
│   │   ├── sect4a_harvestw4.dta      # Sante : maladie, recours, depenses
│   │   ├── secta_harvestw4.dta       # Menages (zone de residence)
│   │   └── totcons_final.dta         # Consommation totale par menage (quintiles)
│   └── processed/
│       ├── df_health_base.rds        # Base individuelle complete apres jointure
│       └── params_depenses.rds       # Parametres (seuil outlier, labels prestataires)
├── scripts/
│   ├── 01_preparation_donnees.R      # Chargement, jointure, recodage, quintiles
│   └── 02_analyses_visualisations.R  # Analyses statistiques et visualisations
├── output/
│   ├── figures/
│   │   ├── 01a_morbidite_sexe.png             # Taux de morbidite par sexe (IC 95%)
│   │   ├── 01b_morbidite_age.png              # Taux de morbidite par groupe d'age
│   │   ├── 01c_morbidite_sexe_age.png         # Taux croise sexe x groupe d'age
│   │   ├── 02_types_maladies.png              # Top 10 affections declarees
│   │   ├── 03_recours_prestataires.png        # Recours aux soins par prestataire
│   │   ├── 04a_depenses_histogramme.png       # Distribution des depenses (echelle log)
│   │   ├── 04b_depenses_boxplot_prestataire.png  # Depenses par prestataire
│   │   ├── 05_recours_quintile.png            # Recours aux soins x quintile
│   │   ├── 06a_violin_rural_urbain.png        # Violin depenses rural vs urbain
│   │   ├── 06b_violin_quintiles.png           # Violin depenses par quintile
│   │   └── 06c_violin_combined.png            # Violin combine (rural/urbain + quintiles)
│   └── tables/
│       ├── 02_top10_maladies.csv              # Top 10 affections avec effectifs et %
│       ├── 03_recours_prestataires.csv        # Distribution du recours aux soins
│       ├── 04_depenses_decile.csv             # Statistiques des depenses par decile
│       ├── 05_contingence_recours_quintile.csv # Table de contingence recours x quintile
│       └── 06_wilcoxon_rural_urbain.csv       # Resultats du test de Wilcoxon
├── rapport/
│   ├── TP3_Groupe4_TEVOEDJRE_Michel_DICKO_Hamadou.Rmd                 # Rapport principal (R Markdown)
│   ├── TP3_Groupe4_TEVOEDJRE_Michel_DICKO_Hamadou.pdf                 # Rapport principal (pdf)
│   └── logos/
│       ├── ENSAE.PNG
│       ├── ANSD.png
│       └── SN.PNG
├── main.R                            # Script principal (orchestre tout le pipeline)
└── README.md
```

---

## Reproductibilite

Pour reproduire l'integralite du travail, ouvrir le projet RStudio (`*.Rproj`) puis executer :

```r
source("main.R")
```

Le script `main.R` installe les packages manquants, telecharge les donnees depuis GitHub si absentes, puis enchaine les deux scripts d'analyse. Les figures sont enregistrees dans `output/figures/` et les tableaux dans `output/tables/`.

Pour compiler le rapport, ouvrir `rapport/rapport.Rmd` dans RStudio et cliquer sur **Knit**, ou executer :

```r
rmarkdown::render("rapport/rapport.Rmd")
```

Le rapport lit uniquement les fichiers `.rds` dans `data/processed/` et les `.csv` dans `output/tables/`. Il est donc necessaire que `main.R` ait ete execute completement au prealable.

---

## Packages R utilises

| Package     | Usage principal                                              |
|-------------|-------------------------------------------------------------|
| haven       | Lecture des fichiers Stata (.dta)                           |
| dplyr       | Manipulation et transformation des donnees                  |
| ggplot2     | Visualisations graphiques                                   |
| forcats     | Gestion des variables categorielles                         |
| scales      | Formatage des axes et etiquettes numeriques                 |
| rstatix     | Tests statistiques (Wilcoxon, Chi-deux)                     |
| gtsummary   | Tableaux de statistiques descriptives                       |
| patchwork   | Assemblage de plusieurs graphiques ggplot2                  |
| knitr       | Compilation du rapport R Markdown                           |

---

## Principaux resultats

- Le taux de morbidite global est de 6,9%, avec un profil en U selon l'age : plus eleve chez les enfants (0-14 ans) et les seniors (65+).
- Les affections infectieuses dominent (faiblesse/fatigue 25%, paludisme 25%, infections respiratoires), coherent avec le profil epidemiologique du Nigeria.
- La pharmacie est le premier point de recours aux soins (44% des malades) ; plus d'un tiers des individus malades n'ont recours a aucun soin formel.
- Les depenses de sante sont fortement asymetriques, avec une mediane autour de 700 Naira et des valeurs extremes atteignant plusieurs millions.
- Le recours aux soins est significativement associe au quintile de consommation (Chi-deux, p < 0,001 ; V de Cramer ~ 0,09).
- La difference des depenses de sante entre milieu urbain et rural n'est pas statistiquement significative (Wilcoxon-Mann-Whitney, p = 0,365 ; r = 0,010).

---

## Source des donnees

National Bureau of Statistics (NBS) Nigeria -- General Household Survey Panel, Wave 4, 2018/2019.  
Sections exploitees : sect1 (caracteristiques individuelles), sect4a (sante et depenses), totcons_final (consommation des menages).
