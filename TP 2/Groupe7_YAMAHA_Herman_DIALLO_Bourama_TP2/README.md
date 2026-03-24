# TP2 — Éducation et Alphabétisation des Ménages Nigérians (avec Pondérations)

---

## Membres de l'équipe

- **Herman YAMAHA**
- **Bourama DIALLO**

Étudiants en ISE1 à l'ENSAE de Dakar

**Superviseur :** M. Aboubacar HEMA, Data Scientist

**Année académique : 2025 - 2026**

---

## Mise à jour principale — Intégration des pondérations transversales

Cette version intègre les **pondérations transversales `wt_wave4`** issues du fichier `secta_harvestw4.dta`. Toutes les statistiques (proportions, taux, tests) sont calculées avec ces poids pour que les résultats soient **représentatifs de la population nigériane**.

**Autres changements :**
- Le tableau de synthèse est exporté en **Excel** (`.xlsx`) via `openxlsx`
- Le rapport génère uniquement un **document Word** (`.docx`) via `flextable` et `officer`
- Les données ne sont **pas incluses dans le projet** : elles sont téléchargées automatiquement depuis GitHub
- Les titres des outputs et sections ne contiennent pas le mot "pondéré" à répétition

---

## 1. Description du projet

Ce projet s'inscrit dans le cadre du cours de **Projet Statistique sous R et Python** (ENSAE ISE 1, 2025-2026). Il porte sur l'**Analyse 2 : Éducation et alphabétisation des membres des ménages nigérians**.

**Objectif principal** : Analyser les niveaux d'instruction et les taux d'alphabétisation des membres des ménages nigérians, et comparer les profils éducatifs selon le sexe, le groupe d'âge, la zone géographique et l'État de résidence — avec des estimations représentatives de la population grâce aux pondérations `wt_wave4`.

---

## 2. Sources de données

| Fichier `.dta` | Contenu | Variables clés | Vague |
|:---|:---|:---|:---|
| `sect2_harvestw4.dta` | Section éducation | `s2aq13`, `s2aq9`, `s2aq15`, `s2aq10` | W4 |
| `sect1_harvestw4.dta` | Démographie individuelle | `s1q2` (sexe), `s1q4` (âge) | W4 |
| `secta_harvestw4.dta` | Ménage + **pondérations** | `hhid`, **`wt_wave4`** | W4 |

> **Les fichiers `.dta` ne sont PAS inclus dans ce dépôt.** Ils sont hébergés sur GitHub et téléchargés automatiquement dans `data/raw/` à la première exécution de `main.R` :
> ```
> https://github.com/Herman-YAMAHA/NYHP/tree/bf1173ced39831e18d8e21c3b2880e597bbc6300/TP1_raw
> ```

---

## 3. Structure du projet

```
Groupe7_YAMAHA_Herman_DIALLO_Bourama_TP2/
│
├── main.R                                          # Point d'entrée — orchestre les scripts
├── Groupe7_YAMAHA_Herman_DIALLO_Bourama.Rproj      # Le ficher Rproj
│
├── scripts/
│   ├── 01_import_nettoyage.R                       # Import, jointure pondérations, construction niveau_educ
│   └── 02_statistiques_graphiques.R                # Statistiques + graphiques + Excel
│
├── data/                                           # Ce dossier se crée automatiquement lors de l'exécution du main.R
│   ├── raw/                                        # VIDE — fichiers téléchargés automatiquement depuis GitHub
│   └── processed/                                  # Objets .rds intermédiaires (wt_wave4 inclus)
│
├── rapport/
│   └── Rapport_educ_alphabetisation_menages.Rmd    # Rapport R Markdown → génère un .docx
│
├── outputs/
│   ├── fig01_barplot_niveau_instruction.png
│   ├── fig02_barres100_genre.png
│   ├── fig03_boxplot_age.png
│   ├── fig04_scolarisation_milieu.png
│   ├── fig05_heatmap_non_instruction_etat.png
│   └── tableau_gtsummary.xlsx                      # 2 feuilles : Profil éducatif + Niveaux par État
│
└── README.md
```

---

## 4. Méthodologie

### Pondérations — Note méthodologique

Les pondérations `wt_wave4` sont intégrées via :
- `aes(weight = wt_wave4)` dans les graphiques ggplot2
- `as_survey_design()` du package `srvyr` pour les statistiques descriptives
- `svydesign()` + `svychisq()` du package `survey` pour les tests
- `tbl_svysummary()` de `gtsummary` pour le tableau de synthèse

### Tâche 7 — Import et jointure

Chargement de `sect1`, `sect2` et `secta`. Extraction de `wt_wave4` depuis `secta`. Jointure triple sur `hhid` (pour les poids) et `hhid × indiv` (pour les variables démographiques).

### Tâche 8 — Construction de `niveau_educ` et distribution

Variable ordonnée à 5 catégories construite depuis `s2aq13`, `s2aq15`, `s2aq9`, `s2aq10`. Proportions estimées avec `survey_mean()` et IC 95%. Barplot horizontal avec barres d'erreur.

### Tâche 9 — Niveau d'éducation par genre

Barplot 100% empilé (effectifs pondérés). Test du **Chi-deux pondéré** via `svychisq()`. V de Cramér calculé sur les effectifs bruts.

### Tâche 10 — Niveau d'éducation par groupe d'âge

Boxplot pondéré via `aes(weight = wt_wave4)`. **Kruskal-Wallis** (non paramétrique). Test **post-hoc de Dunn** avec correction de Bonferroni.

### Tâche 11 — Scolarisation des 6-17 ans

Taux de scolarisation avec IC 95% via `survey_mean()`. Test **Chi-deux pondéré** `svychisq()`. Barplot avec barres d'erreur.

### Tâche 12 — Heatmaps par État

Proportions pondérées via `survey_mean()` par État. Deux heatmaps (palette `magma` et `cividis`).

### Tâche 13 — Tableau gtsummary → Excel

`tbl_svysummary()` stratifié par milieu. Export **Excel** via `openxlsx` avec mise en forme.

---

## 5. Packages R requis

```r
install.packages(c(
  "haven", "dplyr", "ggplot2", "forcats", "scales",
  "rstatix", "ggpubr", "gtsummary", "viridis", "patchwork",
  "tidyr",     # pivot pour mise en forme des tableaux
  "survey",    # plan de sondage complexe
  "srvyr",     # interface dplyr pour survey
  "openxlsx",  # export Excel (2 feuilles)
  "flextable", # tableaux Word
  "officer",   # documents Word
  "knitr"
))
```

---

## 6. Reproduction des résultats

**Étape 1 — Exécuter l'analyse**

```r
source("main.R")
```

> `data/raw/` est vide par défaut. Les fichiers `.dta` sont téléchargés automatiquement depuis GitHub à la première exécution.

**Étape 2 — Compiler le rapport Word**

Ouvrir `rapport/Rapport_educ_alphabetisation_menages.Rmd` dans RStudio et cliquer sur **Knit → Word Document**.

---

## 7. Références

- World Bank (2019). *Nigeria General Household Survey Panel 2018-2019, Wave 4*. LSMS-ISA.
- Lumley, T. (2010). *Complex Surveys: A Guide to Analysis Using R*. Hoboken : Wiley.
- Dunn, O. J. (1964). Multiple comparisons using rank sums. *Technometrics*, 6(3), 241-252.
- Cramér, H. (1946). *Mathematical Methods of Statistics*. Princeton University Press.
