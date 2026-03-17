# Analyse 3 — Accès aux services de santé et chocs sanitaires des ménages nigérians

---

## Membres de l'équipe

- **Herman YAMAHA**
- **Bourama DIALLO**

Étudiants en ISE1 à l'ENSAE de Dakar

**Superviseur :** M. Aboubacar HEMA, Data Scientist

**Année académique : 2025 - 2026**

---

## 1. Description du projet

Ce projet s'inscrit dans le cadre du cours de **Projet Statistique sous R et Python** (ENSAE ISE 1, 2025-2026). Il porte sur l'**Analyse 3 : Accès aux services de santé et chocs sanitaires des ménages nigérians**, à partir des données du Nigeria General Household Survey (GHS) Panel, programme LSMS-ISA de la Banque Mondiale.

**Objectif principal** : Décrire les épisodes de maladie, les types de soins consultés et les coûts de santé supportés par les ménages nigérians, et analyser les disparités par sexe, âge, milieu de résidence et niveau de richesse.

**Questions analytiques traitées :**

1. Quel est le taux de morbidité déclarée et comment varie-t-il selon le sexe et l'âge ?
2. Quelles sont les pathologies les plus fréquemment déclarées ?
3. Vers quels prestataires les individus malades se tournent-ils ?
4. Comment se distribue la dépense de santé et quelles sont les valeurs aberrantes ?
5. Le recours aux soins est-il lié au niveau de richesse du ménage ?
6. Les dépenses de santé diffèrent-elles entre zones rurales et urbaines ?

---

## 2. Sources de données

| Fichier `.dta` | Contenu | Variables clés | Vague |
|:---|:---|:---|:---|
| `sect4a_harvestw4.dta` | Section santé individuelle | `s4aq3` (maladie), `s4aq3b_1` (type), `s4aq6a` (praticien), `s4aq9/14/17` (dépenses) | W4 |
| `sect1_harvestw4.dta` | Démographie du ménage | `s1q2` (sexe), `s1q4` (âge), `sector` (milieu) | W4 |
| `secta_harvestw4.dta` | Caractéristiques du ménage | `sector` (urbain/rural), `state` | W4 |
| `totcons_final.dta` | Agrégat de consommation | `totcons_adj` (dépense ajustée par tête) | W4 |

**Source officielle :** World Bank LSMS-ISA — Nigeria GHS Panel, Wave 4 (2018), Post-Harvest  
**Lien :** `https://microdata.worldbank.org/index.php/catalog/3557`

**Note :** Dans le GHS Wave 4, les variables de santé se trouvent dans `sect4a_harvestw4.dta` et non dans `sect3a` comme indiqué dans le document des projets. Les fichiers `sect3a_harvestw4.dta` et `sect3b_harvestw4.dta` contiennent respectivement les données d'emploi et d'assurance maladie (NHIS).

---

## 3. Structure du projet

```
Groupe7_YAMAHA_Herman_DIALLO_Bourama/
│
├── main.R                                  # Point d'entrée — orchestre l'exécution de tous les scripts
│
├── scripts/
│   ├── 01_morbidite_sexe_age.R             # Tâche 13 : taux de morbidité par sexe et groupe d'âge
│   ├── 02_types_maladies.R                 # Tâche 14 : top 10 des affections déclarées
│   ├── 03_recours_soins.R                  # Tâche 15 : recours aux soins par type de prestataire
│   ├── 04_depenses_sante.R                 # Tâche 16 : distribution des dépenses de santé
│   ├── 05_test_independance.R              # Tâche 17 : test chi-deux recours × quintile
│   └── 06_violin_rural_urbain.R            # Tâche 18 : violin plot dépenses rural/urbain
│
├── data/                                   # Les données sont importées depuis un compte GitHub et ce ne sont que les données épurées qui y seront après l'exécution
│
├── rapport/
│   ├── rapport avec code.Rmd               # Rapport long avec code (exploration + analyses complètes)
│   └── rapport_court.Rmd                   # Rapport court sans code (méthodologie + résultats)
│
├── outputs/
│   ├── figures/                            # Graphiques PNG produits par les scripts
│   │   ├── 01a_morbidite_sexe.png
│   │   ├── 01b_morbidite_age.png
│   │   ├── 01c_morbidite_sexe_age.png
│   │   ├── 02_types_maladies.png
│   │   ├── 03_recours_prestataires.png
│   │   ├── 04a_depenses_histogramme.png
│   │   ├── 04b_depenses_boxplot_prestataire.png
│   │   ├── 05_recours_quintile.png
│   │   ├── 06a_violin_rural_urbain.png
│   │   ├── 06b_violin_quintiles.png
│   │   └── 06c_violin_combined.png
│   │
│   └── tables/                             # Tableaux CSV produits par les scripts
│       ├── 02_top10_maladies.csv
│       ├── 03_recours_prestataires.csv
│       ├── 04_depenses_decile.csv
│       ├── 05_contingence_recours_quintile.csv
│       └── 06_wilcoxon_rural_urbain.csv
│
└── README.md
```

---

## 4. Méthodologie

L'analyse suit six tâches analytiques enchaînées, toutes implémentées en R.

### Tâche 13 — Taux de morbidité par sexe et âge

La morbidité est définie comme la proportion d'individus ayant déclaré une maladie ou blessure dans les quatre semaines précédant l'enquête (`s4aq3 = 1`). Les intervalles de confiance à 95 % sont calculés par l'approximation de Wilson. Les comparaisons sont stratifiées par sexe et par groupes d'âge de dix ans.

### Tâche 14 — Types de maladies déclarées

Les affections (`s4aq3b_1`) sont classées par fréquence et regroupées en trois catégories cliniques : *infectieuse*, *chronique* et *traumatique*, selon le codebook du questionnaire GHS. Les dix affections les plus fréquentes sont représentées par un barplot horizontal coloré.

### Tâche 15 — Recours aux soins par prestataire

Le type de praticien consulté (`s4aq6a`) est recodé en cinq groupes synthétiques : *hôpital/clinique*, *pharmacie*, *tradipraticien*, *agent de santé communautaire* et *aucun recours*. Les fréquences sont représentées par un barplot ordonné.

### Tâche 16 — Distribution des dépenses de santé

La dépense totale additionne trois composantes : consultation (`s4aq9`), médicaments (`s4aq14`) et hospitalisation (`s4aq17`). La distribution est analysée en échelle logarithmique. Les valeurs aberrantes sont détectées par la règle de Tukey étendue : seuil = Q3 + 3 × (Q3 − Q1).

### Tâche 17 — Test d'indépendance recours × quintile

L'association entre recours aux soins (consulté / non consulté) et quintile de consommation est testée par le **chi-deux de Pearson**. La force de l'association est mesurée par le **V de Cramér**. Le test exact de Fisher (simulation Monte-Carlo) est appliqué si l'effectif minimum est inférieur à 5.

### Tâche 18 — Comparaison rural/urbain des dépenses

La comparaison des dépenses médianes entre zones rurales et urbaines est réalisée par le **test de Wilcoxon-Mann-Whitney**. La taille d'effet est calculée par le *r* de Rosenthal. La distribution est visualisée par des violin plots superposés à des boxplots, en échelle logarithmique.

---

## 5. Packages R requis

```r
install.packages(c(
  "haven",       # lecture des fichiers .dta Stata
  "dplyr",       # manipulation des données
  "ggplot2",     # visualisation
  "forcats",     # réordonner les facteurs
  "scales",      # formatage des axes
  "naniar",      # diagnostic des valeurs manquantes
  "rstatix",     # tests statistiques (chi-deux, Wilcoxon)
  "ggpubr",      # graphiques statistiques annotés
  "patchwork",   # assemblage de graphiques
  "gtsummary",   # tableaux de synthèse statistique
  "tidyr"        # reshape des données
))
```

---

## 6. Reproduction des résultats

**Étape 1 — Exécuter l'analyse**

Ouvrir RStudio à partir du Rproj du projet, puis exécuter :

```r
main.R
```

Tous les scripts sont exécutés séquentiellement. Les figures PNG sont sauvegardées dans `output/figures/` et les tableaux CSV dans `output/tables/`.

**Étape 2 — Compiler le rapport**

Ouvrir `rapport/rapport_court.Rmd` dans RStudio et cliquer sur **Knit**. Les figures sont chargées depuis `output/figures/` via des chemins relatifs — l'étape 2 doit donc être réalisée en premier.

---

## 7. Principaux résultats

| Indicateur | Résultat |
|:---|:---|
| Taux de morbidité global | 9,3 % [IC 95 % : 8,9 – 9,7 %] |
| Taux de morbidité — Femmes | 10,3 % |
| Taux de morbidité — Hommes | 8,4 % |
| Première affection déclarée | Faiblesse/Fatigue (27,0 %) et Paludisme (26,8 %) |
| Taux de non-recours aux soins | 35,9 % des individus malades |
| Premier prestataire consulté | Pharmacie (44,4 %) |
| Médiane des dépenses de santé | 800 Naira (Q1 = 350, Q3 = 2 500) |
| Test chi-deux (recours × quintile) | χ² = 10,20 ; p = 0,037 ; V = 0,070 (association faible) |
| Test Wilcoxon (dépenses rural/urbain) | p = 0,365 ; r = 0,01 (non significatif) |

---

## 8. Références

- World Bank (2019). *Nigeria General Household Survey Panel 2018-2019, Wave 4*. LSMS-ISA. Washington DC : The World Bank.
- Deaton, A. (1997). *The Analysis of Household Surveys: A Microeconometric Approach to Development Policy*. Baltimore : Johns Hopkins University Press.
- Gwatkin, D.R. et al. (2007). *Socio-Economic Differences in Health, Nutrition, and Population within Developing Countries*. Washington DC : World Bank.
