# TP5 — Cultures pratiquées, intrants utilisés et rendements agricoles
## Nigeria General Household Survey — Wave 4 (2018/19)

---

## Membres de l'équipe

| Nom | Prénom | Établissement |
|:----|:-------|:-------------|
| YAMAHA | Herman | ENSAE Dakar — ISE 1 |
| DIALLO | Bourama | ENSAE Dakar — ISE 1 |

**Superviseur :** M. Aboubacar HEMA, Data Scientist  
**Année académique :** 2025 – 2026  
**Cours :** Projet Statistique sous R et Python

---

## 1. Description du projet

Ce projet analyse la **production agricole des ménages nigérians** à partir de la **Wave 4 (2018/19)** du Nigeria General Household Survey (GHS) Panel, conduit par la Banque mondiale et le Bureau national des statistiques du Nigeria (NBS).

Toutes les estimations intègrent les **pondérations transversales `wt_wave4`** afin de garantir la représentativité nationale des résultats.

### Questions de recherche traitées (Q25 à Q29)

| N° | Question | Objet d'analyse | Données mobilisées |
|:---|:---------|:----------------|:-------------------|
| **Q25** | Top 15 des cultures les plus fréquentes | Fréquence pondérée de pratique par culture | `secta3i_harvestw4` |
| **Q26** | Diversification culturale | Distribution du nombre de cultures par ménage ; comparaison urbain/rural | `secta3i_harvestw4` |
| **Q27** | Taux d'adoption des intrants | Proportions pondérées engrais (NPK, Urée), engrais organique, pesticide, herbicide | `secta11c2_harvestw4` |
| **Q28** | Rendements maïs et mil par État | Boxplots des rendements (kg/ha) par État nigérian ; test de Kruskal-Wallis | `secta3i` + `secta1_harvestw4` |
| **Q29** | Rendements selon usage d'engrais inorganique | Comparaison violin plot ; test de Wilcoxon + r de Rosenthal | `secta3i` + `secta11c2` + `secta1` |

> **Note :** La question Q30 est exclue du périmètre de ce TP conformément aux consignes données.

---

## 2. Sources de données

Les fichiers de données sont **téléchargés automatiquement** depuis le dépôt GitHub du projet au lancement de `main.R`.

**Dépôt source :**
```
https://github.com/Herman-YAMAHA/NYHP/tree/main/TP4_raw
```

### Fichiers bruts utilisés

| Fichier `.dta` | Contenu | Vague |
|:---------------|:--------|:------|
| `secta3i_harvestw4.dta` | Cultures par parcelle — `cropcode`, quantité récoltée, facteur de conversion kg | W4 |
| `secta11c2_harvestw4.dta` | Intrants par parcelle — engrais inorganique (NPK, Urée), engrais organique, pesticide, herbicide | W4 |
| `secta_harvestw4.dta` | Pondérations `wt_wave4`, zone géopolitique, État (`state`), milieu (`sector`) | W4 |
| `secta1_harvestw4.dta` | Superficies des parcelles — mesures GPS actuelles (`sa1q11`) et pré-remplies (`prefilled_gps_area`) | W4 |
| `secta3ii_harvestw4.dta` | Récapitulatif culture × ménage W4 | W4 |

---

## 3. Approche méthodologique

### 3.1 Pondération et plan de sondage

Toutes les analyses descriptives et les proportions sont **estimées sous le plan de sondage de l'enquête** à l'aide des packages `survey` et `srvyr`. La pondération utilisée est `wt_wave4`, pondération transversale de la Wave 4.

- Les taux d'adoption (Q27) et les moyennes de diversification (Q26) sont calculés avec `survey_mean()`, avec intervalles de confiance à 95%.
- L'effectif pondéré total des ménages agricoles sert de dénominateur pour les fréquences relatives (Q25).

### 3.2 Construction des superficies de parcelles (Q28, Q29)

La Wave 4 fournit deux sources de superficie GPS dans `secta1_harvestw4`, toutes deux exprimées en **mètres carrés** :

| Condition | Variable | Description |
|:----------|:---------|:------------|
| `sa1q9 == 1` | `sa1q11` | Superficie GPS mesurée lors de la visite actuelle |
| `sa1q9 == 2` | `prefilled_gps_area` | Superficie GPS issue de la visite précédente (fallback) |

**Logique de priorisation (du plus au moins précis) :**
1. GPS actuel (`sa1q11`) si `sa1q9 == 1` et valeur non nulle et > 0
2. GPS antérieur (`prefilled_gps_area`) en cas d'absence de mesure actuelle
3. `NA` si aucune superficie disponible → parcelle exclue du calcul de rendement

**Conversion :** `area_ha = area_m2 / 10 000`

Les parcelles sans superficie (~10 % des parcelles productives) sont **exclues uniquement des analyses de rendement** (Q28, Q29) ; ce taux de couverture est documenté dans le rapport et dans l'export Excel.

### 3.3 Calcul des rendements

```
rendement (kg/ha) = prod_kg / area_ha
```

avec `prod_kg = sa3iq6i × sa3iq6_conv`, le facteur `sa3iq6_conv` étant le facteur de conversion unité-locale → kilogramme fourni dans le fichier `secta3i`.

Seules les parcelles ayant effectivement récolté (`sa3iq3 == 1`), avec une production positive et une superficie GPS valide, entrent dans le calcul.

### 3.4 Traitement des valeurs extrêmes (outliers)

Pour les analyses de rendement (Q28 et Q29), les valeurs aberrantes sont supprimées selon la règle **IQR × 3** par culture :

```
outlier = rendement < Q1 − 3×IQR  ou  rendement > Q3 + 3×IQR
```

Ce critère conservateur (vs. le classique IQR×1,5) préserve la variabilité naturelle des rendements agricoles tout en éliminant les erreurs de saisie manifestes.

### 3.5 Tests statistiques

| Question | Test | Indicateur | Interprétation |
|:---------|:-----|:-----------|:---------------|
| **Q26** | Test de Wilcoxon-Mann-Whitney | Statistique W, p-valeur, r de Rosenthal | Différence de diversification urbain vs rural |
| **Q27** | Test du Chi-deux | χ², p-valeur | Association entre milieu et adoption de l'engrais inorganique |
| **Q28** | Test de Kruskal-Wallis | H, p-valeur | Différences de rendement maïs entre États |
| **Q29** | Test de Wilcoxon-Mann-Whitney | W, p-valeur, r de Rosenthal | Effet de l'engrais inorganique sur les rendements |

Le **r de Rosenthal** est calculé comme mesure de taille d'effet pour les tests de Wilcoxon :

```
r = |Z| / √N   avec Z = qnorm(p/2)
```

Grille d'interprétation : r < 0.1 → négligeable ; 0.1–0.3 → petite ; 0.3–0.5 → moyenne ; ≥ 0.5 → grande.

### 3.6 Dictionnaire des cultures

Les codes `cropcode` du GHS Wave 4 (World Bank) sont traduits en **français** via un dictionnaire interne (`cultures_dict`), couvrant 39 cultures classées en 6 types : Céréale, Légumineuse, Tubercule, Culture de rente, Légume, Autre.

### 3.7 Facteurs de conversion zone-spécifiques

Les facteurs de conversion d'unités locales (heaps, ridges, stands) vers l'hectare sont documentés dans le script, par zone géopolitique, conformément à la documentation GHS-Panel de la Banque mondiale. Ils ne sont pas appliqués dans cette analyse car toutes les superficies utilisées proviennent de mesures GPS.

| Zone | Heaps (ha) | Ridges (ha) | Stands (ha) |
|:-----|:----------:|:-----------:|:-----------:|
| North Central (1) | 0,00012 | 0,0027 | 0,00006 |
| North East (2) | 0,00016 | 0,0040 | 0,00016 |
| North West (3) | 0,00011 | 0,00494 | 0,00004 |
| South East (4) | 0,00019 | 0,0023 | 0,00004 |
| South South (5) | 0,00021 | 0,0023 | 0,00013 |
| South West (6) | 0,00012 | 0,00001 | 0,00041 |

---

## 4. Structure du projet

```
Groupe7_YAMAHA_Herman_DIALLO_Bourama_TP5/
│
├── main.R                                    ← Point d'entrée unique
├── Groupe7_YAMAHA_Herman_DIALLO_Bourama_TP5.Rproj
├── README.md
│
├── scripts/
│   ├── 01_import_nettoyage.R                ← Étape 1 : téléchargement, import,
│   │                                            recodages, construction des tables
│   └── 02_analyses_graphiques.R             ← Étape 2 : statistiques descriptives,
│                                                graphiques, export Excel
│
├── rapport/
│   ├── Rapport_TP5_cultures_rendements.Rmd  ← Rapport R Markdown → PDF
│   ├── Rapport_TP5_cultures_rendements.docx ← Rapport Word compilé
│   └── modele.docx                          ← Template Word de mise en forme
│
├── data/                                    ← Dossier des données brutes et objets intermédiaires initialement qui se rempli au cours de l'exécution des scripts
│   ├── raw/                                 ← Fichiers .dta bruts (auto-téléchargés)
│   │   ├── secta3i_harvestw4.dta
│   │   ├── secta3ii_harvestw4.dta
│   │   ├── secta11c2_harvestw4.dta
│   │   ├── secta_harvestw4.dta
│   │   └── secta1_harvestw4.dta
│   │
│   └── processed/                           ← Objets R intermédiaires (.rds)
│       ├── df_poids.rds                     ← Pondérations + géolocalisation
│       ├── cultures_dict.rds                ← Dictionnaire cropcode → nom français
│       ├── conv_zone.rds                    ← Facteurs de conversion zone-spécifiques
│       ├── df_cultures_w4.rds               ← Table cultures par ménage (Q25, Q26)
│       ├── df_diversif_w4.rds               ← Indice de diversification (Q26)
│       ├── df_intrants_w4.rds               ← Intrants par parcelle (Q27)
│       ├── df_intrants_menage.rds           ← Adoption agrégée au niveau ménage (Q27)
│       ├── df_crop_intrants.rds             ← Rendements maïs/mil + intrants (Q28, Q29)
│       ├── plot_areas.rds                   ← Superficies GPS des parcelles
│       ├── N_pond_total.rds                 ← Effectif pondéré total
│       ├── meta_superficie.rds              ← Taux de couverture superficie
│       ├── resultats_q26.rds                ← Test Wilcoxon + stats diversification
│       ├── resultats_q27.rds                ← Taux d'adoption + Chi-deux
│       ├── resultats_q28.rds                ← Kruskal-Wallis + stats rendements
│       └── resultats_q29.rds                ← Wilcoxon + r de Rosenthal rendements
│
└── outputs/                                 ← Figures PNG + tableau Excel
    ├── fig01_top15_cultures.png             ← Q25 : Top 15 cultures
    ├── fig02_diversification_culturale.png  ← Q26 : Histogramme + violin plot
    ├── fig03_adoption_intrants.png          ← Q27 : Taux d'adoption avec IC 95%
    ├── fig04_engrais_zone_milieu.png        ← Q27 : Adoption engrais par zone
    ├── fig05_rendement_etat.png             ← Q28 : Boxplots rendements par État
    ├── fig06_rendement_engrais.png          ← Q29 : Violin plots rendements × engrais
    └── tableau_intrants.xlsx               ← Export Excel 3 feuilles
```

---

## 5. Exécution

### Prérequis

- **R** ≥ 4.2.0
- **RStudio** (recommandé) ou tout environnement R standard
- Connexion internet (pour le téléchargement automatique des données)
- **LaTeX / TinyTeX** pour la compilation du rapport en PDF (optionnel)

### Lancement

```r
# 1. Ouvrir le projet dans RStudio
#    → Double-cliquer sur Groupe7_YAMAHA_Herman_DIALLO_Bourama_TP5.Rproj

# 2. Exécuter le point d'entrée
source("main.R")
```

### Ce que fait `main.R` automatiquement

| Étape | Action |
|:------|:-------|
| **0** | Installation des packages R manquants |
| **1** | Création des dossiers `data/raw/`, `data/processed/`, `outputs/` |
| **2** | Téléchargement des fichiers `.dta` depuis GitHub (si absents) |
| **3** | Import, recodage et construction des tables analytiques (`01_import_nettoyage.R`) |
| **4** | Analyses descriptives, tests statistiques, graphiques, export Excel (`02_analyses_graphiques.R`) |
| **5** | Compilation du rapport PDF via R Markdown (si `rmarkdown` et LaTeX sont disponibles) |

### Compilation du rapport PDF (si étape 5 échoue)

```r
# Installer TinyTeX si nécessaire
install.packages("tinytex")
tinytex::install_tinytex()

# Compiler manuellement le rapport
rmarkdown::render(
  "rapport/Rapport_TP5_cultures_rendements.Rmd",
  output_format = "pdf_document"
)

# Alternative : ouvrir le .Rmd dans RStudio → Knit → PDF Document
```

---

## 6. Résultats produits

### Figures

| Fichier | Description | Question |
|:--------|:------------|:---------|
| `fig01_top15_cultures.png` | Graphique en barres horizontales des 15 cultures les plus pratiquées, colorées par type, avec % pondérés | Q25 |
| `fig02_diversification_culturale.png` | Histogramme pondéré du nombre de cultures/ménage + violin plot urbain vs rural avec test Wilcoxon | Q26 |
| `fig03_adoption_intrants.png` | Barres horizontales des taux d'adoption de chaque intrant avec intervalles de confiance à 95% | Q27 |
| `fig04_engrais_zone_milieu.png` | Barres groupées (urbain/rural) du taux d'adoption de l'engrais inorganique par zone géopolitique | Q27 |
| `fig05_rendement_etat.png` | Boxplots maïs/mil par État (rendement en kg/ha), triés par médiane, avec résultat du test de Kruskal-Wallis | Q28 |
| `fig06_rendement_engrais.png` | Violin plots maïs et mil selon utilisation ou non d'engrais inorganique, avec test de Wilcoxon et r de Rosenthal | Q29 |

### Tableau Excel (`tableau_intrants.xlsx`)

Le fichier Excel contient **3 feuilles** :

| Feuille | Contenu |
|:--------|:--------|
| `Adoption_Intrants` | Taux d'adoption pondérés par intrant avec IC 95%, triés par taux décroissant |
| `Rendements_Mais_Mil` | Statistiques descriptives (n, moyenne, médiane, écart-type, Q1, Q3) des rendements en kg/ha |
| `Rendements_Engrais` | Comparaison des rendements avec/sans engrais inorganique + résultats des tests de Wilcoxon (W, p, r) |

---

## 7. Packages utilisés

| Package | Version minimale | Rôle |
|:--------|:-----------------|:-----|
| `haven` | ≥ 2.5 | Lecture des fichiers `.dta` Stata |
| `dplyr` | ≥ 1.1 | Manipulation et transformation des données |
| `tidyr` | ≥ 1.3 | Restructuration des données |
| `forcats` | ≥ 1.0 | Réordonner les niveaux de facteurs |
| `purrr` | ≥ 1.0 | Programmation fonctionnelle (itérations sur intrants) |
| `stringr` | ≥ 1.5 | Manipulation de chaînes de caractères (`str_wrap`) |
| `ggplot2` | ≥ 3.4 | Visualisation — couche de base de toutes les figures |
| `scales` | ≥ 1.2 | Formatage des axes (labels %, virgule) |
| `patchwork` | ≥ 1.1 | Composition multi-panneaux (fig02) |
| `ggpubr` | ≥ 0.6 | Annotations et combinaison de graphiques |
| `survey` | ≥ 4.1 | Plan de sondage complexe |
| `srvyr` | ≥ 1.2 | Interface `dplyr`-style pour `survey` |
| `rstatix` | ≥ 0.7 | Tests statistiques non paramétriques (Kruskal-Wallis) |
| `openxlsx` | ≥ 4.2 | Export Excel avec mise en forme avancée |
| `knitr` | ≥ 1.42 | Compilation du rapport R Markdown |
| `kableExtra` | ≥ 1.3 | Tableaux LaTeX dans le rapport PDF |
| `xfun` | ≥ 0.39 | Utilitaires pour knitr |

> Tous les packages manquants sont installés automatiquement au démarrage de `main.R`.

---

## 8. Notes techniques

### Agrégation des intrants au niveau ménage

L'adoption d'un intrant est définie au niveau **ménage** : un ménage est considéré comme adoptant si **au moins une de ses parcelles** a utilisé l'intrant. Cette agrégation est réalisée avec `any(., na.rm = TRUE)` dans le script 01.

### Gestion des valeurs manquantes

- Les ménages sans pondération (`wt_wave4 == NA`) sont systématiquement exclus des estimations pondérées.
- Les parcelles sans superficie GPS (~10 %) sont exclues uniquement des calculs de rendement (Q28, Q29) ; elles restent incluses dans les analyses de culture (Q25, Q26) et d'intrants (Q27).
- Les valeurs de production nulle ou négative sont filtrées en amont (`prod_kg > 0`).

### Reproductibilité

Le script `01_import_nettoyage.R` vérifie l'existence des fichiers avant de les télécharger (`if (!file.exists(.dest) || file.size(.dest) == 0)`). Une exécution ultérieure ne retélécharge pas les données déjà présentes.

Les objets intermédiaires sont persistés en `.rds` dans `data/processed/`. Le script `02_analyses_graphiques.R` recharge ces objets au démarrage et les régénère automatiquement s'ils sont absents.



---

*Projet réalisé dans le cadre du cours de Projet Statistique sous R et Python — ENSAE ISE 1, 2025-2026.*  
*Données : Nigeria General Household Survey Panel Wave 4 (2018/19) — World Bank / NBS Nigeria.*
