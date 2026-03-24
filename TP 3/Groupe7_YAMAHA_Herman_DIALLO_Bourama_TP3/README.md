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

**Objectif principal** : Décrire les épisodes de maladie, les types de soins consultés et les coûts de santé supportés par les ménages nigérians, en intégrant les **poids de sondage transversaux (wt_wave4)** pour généraliser les résultats à la population nigériane.

**Questions analytiques traitées :**

1. Quel est le taux de morbidité et comment varie-t-il selon le sexe et l'âge ?
2. Quelles sont les pathologies les plus fréquentes dans la population ?
3. Vers quels prestataires les individus malades se tournent-ils ?
4. Comment se distribue la dépense de santé et quelle est la médiane ?
5. Le recours aux soins est-il lié au quintile de richesse (chi-deux) ?
6. Les dépenses de santé diffèrent-elles entre zones rurales et urbaines ?

---

## 2. Sources de données

| Fichier `.dta` | Contenu | Variables clés | Vague |
|:---|:---|:---|:---|
| `sect4a_harvestw4.dta` | Section santé individuelle | `s4aq3` (maladie), `s4aq3b_1` (type), `s4aq6a` (praticien), `s4aq9/14/17` (dépenses) | W4 |
| `sect1_harvestw4.dta` | Démographie du ménage | `s1q2` (sexe), `s1q4` (âge), `sector` (milieu) | W4 |
| `secta_harvestw4.dta` | **Pondérations** | `wt_wave4` (poids transversal W4) | W4 |
| `totcons_final.dta` | Agrégat de consommation | `totcons_adj` (dépense ajustée par tête) | W4 |

**Source officielle :** World Bank LSMS-ISA — Nigeria GHS Panel, Wave 4 (2018), Post-Harvest
**Lien :** `https://microdata.worldbank.org/index.php/catalog/3557`

**Note :** Dans le GHS Wave 4, les variables de santé se trouvent dans `sect4a_harvestw4.dta` (et non `sect3a`). Les fichiers `sect4a` et `totcons_final` sont téléchargés automatiquement par `main.R` depuis GitHub.

---

## 3. Structure du projet

```
Groupe7_YAMAHA_Herman_DIALLO_Bourama_TP3/
│
├── main.R                                      # Point d'entrée — orchestre tous les scripts
│
├── Groupe7_YAMAHA_Herman_DIALLO_Bourama.Rproj  # Le fichier Rproj
│
├── scripts/
│   ├── 01_morbidite_sexe_age.R                 # Tâche 13 : taux morbidité pondéré
│   ├── 02_types_maladies.R                     # Tâche 14 : top 10 maladies (pondéré)
│   ├── 03_recours_soins.R                      # Tâche 15 : recours prestataires (pondéré)
│   ├── 04_depenses_sante.R                     # Tâche 16 : dépenses + médiane pondérée
│   ├── 05_test_independance.R                  # Tâche 17 : chi-deux Rao-Scott + Excel
│   └── 06_violin_rural_urbain.R                # Tâche 18 : violin plots + Excel
│
├── data/                                       # Ce dossier se crée automatiquement lors de l'exécution du main.R
│   ├── raw/                                    # Fichiers .dta bruts téléchargés par main.R lors de l'exécution
│   └── processed/                              # Objets .rds intermédiaires
│
├── rapport/
│   ├── rapport d'analyse.docs                  # Rapport Word 
│   └── rapport d'analyse.Rmd                   # Rapport rmd (officedown)
│
├── outputs/
│   ├── figures/                                # Graphiques PNG (11 figures)
│   └── tables/                                 # CSV + fichiers Excel
│       ├── 02_top10_maladies.csv
│       ├── 03_recours_prestataires.csv
│       ├── 04_depenses_decile.csv
│       ├── 05_contingence_recours_quintile.csv
│       ├── 06_wilcoxon_rural_urbain.csv
│       ├── tableau_recours_quintile.xlsx       # Tableau - tâche 17
│       └── tableau_depenses_milieu.xlsx        # Tableau - tâche 18
│
└── README.md
```

---

## 4. Pondérations — Points clés

| Élément | Détail |
|:---|:---|
| **Fichier source** | `secta_harvestw4.dta` |
| **Variable** | `wt_wave4` (poids transversal Wave 4) |
| **Niveau** | Ménage (un poids par `hhid`) |
| **Propagation** | Chaque individu hérite du poids de son ménage (`left_join` sur `hhid`) |
| **Package** | `survey` + `srvyr` |
| **Plan déclaré** | `svydesign(ids=~1, weights=~wt_wave4, data=...)` |
| **Tests pondérés** | `svychisq()` (Rao-Scott), `svymean()`, `svyquantile()` |
| **Wilcoxon** | Test classique (pas d'équivalent pondéré standard en R) |

---

## 5. Méthodologie

### Tâche 13 — Taux de morbidité par sexe et âge

Taux calculés avec `svymean(~malade, plan)`. IC à 95% pondérés via `srvyr::survey_mean(vartype="ci")`. Comparaisons stratifiées par sexe et groupes d'âge de dix ans.

### Tâche 14 — Types de maladies déclarées

Fréquences calculées comme `sum(wt_wave4)` par affection. Les dix affections les plus représentées dans la population sont représentées par un barplot horizontal coloré par catégorie clinique.

### Tâche 15 — Recours aux soins par prestataire

Parts pondérées calculées comme proportion de `sum(wt_wave4)` par groupe de prestataire. Cinq groupes : hôpital/clinique, pharmacie, tradipraticien, agent de santé communautaire, aucun recours.

### Tâche 16 — Distribution des dépenses de santé

Médiane pondérée via `svyquantile()`. Histogramme pondéré via `aes(weight=wt_wave4)`. Outliers détectés par la règle de Tukey étendue sur quantiles pondérés : seuil = Q3_pond + 3 × (Q3_pond − Q1_pond).

### Tâche 17 — Test d'indépendance recours × quintile

Test du **chi-deux de Rao-Scott** (`svychisq()`) qui corrige le test classique pour le plan de sondage complexe. Force de l'association : V de Cramér calculé sur les effectifs bruts. Tableau exporté en **Excel** (`openxlsx`).

### Tâche 18 — Comparaison rural/urbain des dépenses

Test de Wilcoxon-Mann-Whitney (données brutes). Taille d'effet *r* de Rosenthal. Violin plots sur échelle logarithmique. Tableau des médianes pondérées exporté en **Excel**.

---

## 6. Packages R requis

```r
install.packages(c(
  "haven",       # lecture .dta Stata
  "dplyr",       # manipulation des données
  "ggplot2",     # visualisation
  "forcats",     # réordonner les facteurs
  "scales",      # formatage des axes
  "naniar",      # valeurs manquantes
  "rstatix",     # tests statistiques
  "patchwork",   # assemblage graphiques
  "gtsummary",   # tableaux de synthèse pondérés (tbl_svysummary)
  "survey",      # plan de sondage complexe (svydesign, svychisq...)
  "srvyr",       # interface dplyr pour survey
  "openxlsx",    # export Excel
  "flextable",   # tableaux Word
  "officedown",  # rapport R Markdown → Word
  "officer"      # manipulation documents Word
))
```

---

## 7. Reproduction des résultats

**Étape 1 — Exécuter l'analyse**

```r
source("main.R")
```

Les scripts sont exécutés séquentiellement. Les figures PNG sont sauvegardées dans `outputs/figures/` et les tableaux dans `outputs/tables/`.

**Étape 2 — Compiler le rapport Word**

Ouvrir `rapport/rapport d'anlyse.Rmd` dans RStudio et cliquer sur **Knit**. Le rapport Word est généré dans `rapport/rapport_court.docx`. Les figures sont chargées depuis `../outputs/figures/` — l'étape 1 doit d'abord être réalisée.

---

## 8. Références

- World Bank (2019). *Nigeria General Household Survey Panel 2018-2019, Wave 4*. LSMS-ISA. Washington DC : The World Bank.
- Lumley, T. (2010). *Complex Surveys: A Guide to Analysis Using R*. Hoboken : John Wiley & Sons.
- Deaton, A. (1997). *The Analysis of Household Surveys: A Microeconometric Approach to Development Policy*. Baltimore : Johns Hopkins University Press.
- Gwatkin, D.R. et al. (2007). *Socio-Economic Differences in Health, Nutrition, and Population within Developing Countries*. Washington DC : World Bank.
