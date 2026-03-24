# TP2 — Éducation et Alphabétisation des Ménages Nigérians

---

## Membres de l'équipe

- **Cheikh Thioub**
- **David Agnangma**

Étudiants en ISE1 à l'ENSAE de Dakar

**Superviseur :** M. Aboubacar HEMA, Data Scientist

**Année académique : 2025 - 2026**

---

## 1. Description du projet

Ce projet s'inscrit dans le cadre du cours de **Projet Statistique sous R et Python** (ENSAE ISE 1, 2025-2026). Il porte sur l'**Analyse 2 : Éducation et alphabétisation des membres des ménages nigérians**, à partir des données du Nigeria General Household Survey (GHS) Panel, programme LSMS-ISA de la Banque Mondiale.

**Objectif principal** : Analyser les niveaux d'instruction et les taux d'alphabétisation des membres des ménages nigérians, et comparer les profils éducatifs selon le sexe, le groupe d'âge, la zone géographique (urbain/rural) et l'État de résidence.

**Questions analytiques traitées :**

1. Comment construire une variable synthétique de niveau d'éducation à partir des variables brutes du GHS ?
2. Quelle est la distribution nationale du niveau d'éducation des adultes (18 ans et plus) ?
3. Le niveau d'éducation varie-t-il significativement selon le sexe ?
4. Existe-t-il un effet générationnel sur le niveau d'éducation selon le groupe d'âge ?
5. Les taux de scolarisation des enfants (6-17 ans) diffèrent-ils entre zones urbaines et rurales ?
6. Quelles sont les disparités éducatives entre les 37 États nigérians ?

---

## 2. Sources de données

| Fichier `.dta` | Contenu | Variables clés | Vague |
|:---|:---|:---|:---|
| `sect2_harvestw4.dta` | Section éducation individuelle | `s2aq13` (scolarisé ?), `s2aq9` (classe actuelle), `s2aq15` (niveau atteint), `s2aq10` (diplôme) | W4 |
| `sect1_harvestw4.dta` | Démographie du ménage | `s1q2` (sexe), `s1q4` (âge), `sector` (milieu) | W4 |
| `secta_harvestw4.dta` | Caractéristiques du ménage | `sector` (urbain/rural), `wt_wave4` (pondérations) | W4 |

**Source officielle :** World Bank LSMS-ISA — Nigeria GHS Panel, Wave 4 (2018/19), Post-Harvest  
**Lien :** `https://microdata.worldbank.org/index.php/catalog/3557`

> `sect2_harvestw4.dta` réunit les informations de **sect2a** (statut scolaire actuel) et **sect2b** (niveau atteint pour les individus ayant quitté l'école). La variable `sector` (milieu urbain/rural) est directement disponible dans ce fichier.

---

## 3. Structure du projet

```
TP2/
├── data/
│   ├── raw/                     # Fichiers .dta bruts
│   │   ├── sect1_harvestw4.dta
│   │   ├── sect2_harvestw4.dta
│   │   ├── secta_harvestw4.dta
│   │   └── gadm41_NGA_1.shp     # shapefile pour la carte
│   └── processed/               # Fichiers intermédiaires (optionnels)
│       └── base_complete_education.rds
├── rapport/
│   └── Rapport-.Rmd             # Document principal R Markdown
    ├── template.docx                # Modèle Word pour la mise en forme
├── resultats/
│   ├── figures/                 # Toutes les images générées (PNG)
│   │   ├── distribution_instruction.png
│   │   ├── instruction_par_sexe.png
│   │   ├── instruction_par_tranche_age.png
│   │   ├── scolarisation_par_milieu.png
│   │   ├── carte_sans_instruction_par_etat.png
│   │   └── na_section1.png, na_section2.png
│   └── tableaux/                # Tableaux de résultats (CSV, TXT)
│       ├── distribution_instruction_finale.csv
│       ├── props_instruction_par_sexe.csv
│       ├── dunn_age_instruction.csv
│       ├── scolarisation_par_milieu.csv
│       └── ...
├── scripts/                     # Scripts auxiliaires (optionnel)

└── README.md
```

---

## 4. Méthodologie

L'analyse suit six tâches structurées, toutes implémentées en R.

### Tâche 7 — Chargement et jointure des données

Chargement des trois fichiers `.dta` via le package `haven`. Jointure de `sect2_harvestw4` avec `sect1_harvestw4` sur les clés `hhid × indiv` pour ajouter le sexe et l'âge. Inspection des valeurs manquantes sur les variables d'éducation (structurellement conditionnelles selon le statut scolaire de l'individu).

### Tâche 8 — Construction de `niveau_educ` et distribution

Construction d'une variable ordonnée à **5 catégories** (Aucun, Primaire, Junior Secondary, Senior Secondary, Tertiaire) à partir de la combinaison de trois variables brutes (`s2aq13`, `s2aq15`, `s2aq9`, `s2aq10`), selon une règle de priorisation documentée dans le codebook GHS. Les analyses portent sur les **adultes de 18 ans et plus**. Barplot horizontal coloré par catégorie.

### Tâche 9 — Niveau d'éducation par sexe

Barplot 100 % empilé par sexe. Test du **chi-deux de Pearson** sur la table de contingence Sexe × Niveau. Force de l'association mesurée par le **V de Cramér** : V = √(χ² / (n × (min(r,c) − 1))).

### Tâche 10 — Niveau d'éducation par groupe d'âge

Boxplot du niveau d'éducation (converti en valeur numérique 1-5) par groupe d'âge (18-30, 31-45, 46-60, 60+). Test de **Kruskal-Wallis** pour la comparaison globale. Test **post-hoc de Dunn** avec correction de Bonferroni pour les comparaisons deux à deux.

### Tâche 11 — Scolarisation des 6-17 ans par zone

Sous-ensemble des enfants de 6 à 17 ans. Variable `s2aq13 = 1` identifie les individus scolarisés. Calcul des taux de scolarisation avec **intervalles de confiance à 95 %** (formule de Wilson). Test du **chi-deux** pour la comparaison Urbain/Rural. Barplot avec barres d'erreur.

### Tâche 12 — Heatmaps État × Niveau d'éducation

Calcul du taux d'adultes sans instruction (`niveau_educ = "Aucun"`) pour chacun des 37 États nigérians. Classement par quintile. Deux heatmaps avec le package `viridis` : (1) taux d'analphabétisme par État (palette `magma`) ; (2) distribution complète des niveaux par État (palette `cividis`), ordonnée par taux croissant d'analphabétisme.

---

## 5. Packages R requis

```r
install.packages(c(
  "haven",       # lecture des fichiers .dta Stata
  "dplyr",       # manipulation des données
  "ggplot2",     # visualisation
  "forcats",     # réordonner les facteurs
  "scales",      # formatage des axes
  "rstatix",     # tests statistiques (Kruskal-Wallis, Dunn, Chi-deux)
  "ggpubr",      # graphiques statistiques annotés
  "gtsummary",   # tableaux de synthèse statistique
  "viridis",     # palettes de couleurs pour les heatmaps
  "patchwork"    # assemblage de graphiques
))
```

---

## 6. Reproduction des résultats

**Étape 1 — Exécuter l'analyse**

Ouvrir RStudio, à partir de Rproj du projet, puis exécuter :

```r
main.R
```

Les deux scripts sont exécutés séquentiellement. Les objets intermédiaires sont sauvegardés dans `data/processed/` et les figures PNG dans `outputs/`.

**Étape 3 — Compiler le rapport**

Ouvrir `rapport/Rapport_TP2_education_alphabetisation.Rmd` dans RStudio et cliquer sur **Knit**. Les données sont lues depuis `../data/raw/` via des chemins relatifs — l'étape 1 doit donc être réalisée en premier.


## 8. Références

- World Bank (2019). *Nigeria General Household Survey Panel 2018-2019, Wave 4*. LSMS-ISA. Washington DC : The World Bank.
- Deaton, A. (1997). *The Analysis of Household Surveys: A Microeconometric Approach to Development Policy*. Baltimore : Johns Hopkins University Press.
- Dunn, O. J. (1964). Multiple comparisons using rank sums. *Technometrics*, 6(3), 241-252.
- Cramér, H. (1946). *Mathematical Methods of Statistics*. Princeton University Press.
