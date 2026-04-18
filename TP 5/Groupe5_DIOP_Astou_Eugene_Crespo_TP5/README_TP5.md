# TP5 — Cultures, Intrants et Rendements Agricoles

**Nigeria General Household Survey-Panel (GHS-Panel) — Wave 4 (2018/2019)**  
**Auteurs :** Diop Astou & Eugène Crespo  
**Professeur :** M. Aboubacar Hema  
**Classe :** ISE1 — ENSAE Dakar | Année académique 2025–2026

---

## Objectif

Ce TP analyse les pratiques agricoles des ménages nigérians à partir des données du GHS-Panel Wave 4 (2018/2019). Il couvre les **tâches 25 à 30** portant sur :

- Les cultures pratiquées et la diversification culturale
- L'utilisation des intrants agricoles (engrais, pesticides, herbicides)
- Les rendements agricoles du maïs et du mil
- L'adoption des semences améliorées

---

## Structure du projet

```
TP5/
├── data/                        ← Bases de données .dta (à placer ici)
│   ├── secta3i_harvestw4.dta
│   ├── secta11c2_harvestw4.dta
│   ├── secta11c3_harvestw4.dta
│   ├── sect11f_plantingw4.dta
│   ├── sect11e1_plantingw4.dta
│   ├── sect11a1_plantingw4.dta
│   └── secta_harvestw4.dta
├── scripts/
│   ├── 00_main.R                ← SCRIPT PRINCIPAL — lancer uniquement celui-ci
│   ├── 01_setup.R               ← Packages, chemins, thème graphique
│   ├── 02_donnees.R             ← Chargement et nettoyage des données
│   ├── 03_cultures.R            ← Tâches 25 & 26
│   ├── 04_intrants_rendements.R ← Tâches 27, 28 & 29
│   └── 05_semences.R            ← Tâche 30
├── outputs/
│   ├── figures/                 ← Figures produites (.png)
│   └── tables/                  ← Tableaux produits (.csv)
├── docs/
│   ├── Rapport_TP5.Rmd          ← Rapport R Markdown
│   ├── ANSD.png                 ← Logo ANSD
│   └── ENSAE.png                ← Logo ENSAE
└── README.md
```

---

## Données utilisées

| Fichier | Contenu | Vague |
|---|---|---|
| `secta3i_harvestw4` | Cultures récoltées — quantités, unités, conversion kg | W4 |
| `secta11c2_harvestw4` | Engrais (NPK, Urée, organique), pesticides, herbicides | W4 |
| `secta11c3_harvestw4` | Intrants achetés — quantité, coût, source | W4 |
| `sect11f_plantingw4` | Cultures plantées — type de semence (améliorée/traditionnelle) | W4 |
| `sect11e1_plantingw4` | Acquisition des semences | W4 |
| `sect11a1_plantingw4` | Superficie des parcelles (déclarée + GPS) | W4 |
| `secta_harvestw4` | Milieu de résidence, État, poids de sondage | W4 |

---

## Packages R requis

```r
install.packages(c(
  "haven",      # Lecture fichiers .dta
  "dplyr",      # Manipulation des données
  "tidyr",      # Restructuration
  "ggplot2",    # Visualisations
  "forcats",    # Gestion des facteurs
  "scales",     # Formatage des axes
  "patchwork",  # Combinaison de graphiques
  "rstatix",    # Tests statistiques
  "ggpubr",     # Graphiques publication
  "gtsummary",  # Tableaux descriptifs
  "knitr",      # Tableaux
  "labelled",   # Labels variables
  "stringr",    # Manipulation chaînes
  "viridis",    # Palette de couleurs
  "here"        # Chemins relatifs
))
```

---

## Exécution

**Placer les bases `.dta` dans le dossier `data/`**, puis ouvrir RStudio et exécuter uniquement :

```r
source("scripts/00_main.R")
```

Tous les scripts s'enchaînent automatiquement dans le bon ordre.

---

## Description des tâches

### Tâche 25 — Cultures les plus fréquentes
Identification des 15 cultures les plus pratiquées par les ménages. Calcul des fréquences et proportions. Barplot horizontal coloré par type de culture (céréales, tubercules, légumineuses).

**Résultat clé :** Le maïs est cultivé par 56,4% des ménages, suivi du sorgho (31,4%) et de l'igname blanche (22,2%).

### Tâche 26 — Diversification culturale
Calcul du nombre de cultures distinctes par ménage. Statistiques descriptives, histogramme de distribution, violin plot comparatif rural/urbain. Test de Wilcoxon pour tester la différence entre milieux.

**Résultat clé :** Médiane de 2 cultures par ménage. Les ménages ruraux diversifient davantage (moyenne 2,66) que les urbains (2,18) — différence hautement significative (p < 10⁻¹³).

### Tâche 27 — Utilisation des intrants agricoles
Calcul des taux d'adoption par type d'intrant (engrais inorganique, organique, pesticide, herbicide) et par milieu. Graphique avec intervalles de confiance à 95%. Test du chi-deux engrais × milieu.

**Résultat clé :** Herbicides (37,3% en rural) et engrais inorganiques (34,5% en rural) sont les intrants les plus utilisés. Association significative milieu × engrais inorganique (χ² = 18,3, p < 0,001).

### Tâche 28 — Distribution des rendements
Calcul des rendements en kg/ha pour le maïs et le mil (outliers exclus par IQR × 3). Statistiques descriptives par décile. Boxplots par État.

**Résultat clé :** Maïs médiane = 787 kg/ha, Mil médiane = 471 kg/ha. Forts disparités inter-États.

### Tâche 29 — Impact de l'engrais sur les rendements
Comparaison des rendements avec et sans engrais chimique. Test de Wilcoxon. Calcul de la taille d'effet (r de rang).

**Résultat clé :** L'engrais chimique quasi double le rendement du maïs (1 115 vs 582 kg/ha, p < 10⁻³⁷). Pas d'effet significatif sur le mil (p = 0,108).

### Tâche 30 — Adoption des semences améliorées
Taux d'adoption global et par culture. Comparaison rural/urbain. Barplot des cultures les plus adoptées.

**Résultat clé :** Seulement 15,9% des ménages utilisent des semences améliorées. Le manioc (16,4%) et le maïs (11,4%) ont les taux les plus élevés.

---

## Outputs produits

### Figures (dossier `outputs/figures/`)

| Fichier | Tâche | Description |
|---|---|---|
| `T25_cultures_frequentes.png` | 25 | Barplot des 15 cultures les plus fréquentes |
| `T26_diversification_culturale.png` | 26 | Distribution de l'indice de diversification |
| `T26a_histogramme_diversification.png` | 26 | Histogramme de diversification |
| `T26b_violin_diversification_milieu.png` | 26 | Violin plot rural vs urbain |
| `T27_taux_intrants_milieu.png` | 27 | Taux d'utilisation des intrants par milieu |
| `T28_rendements_par_etat.png` | 28 | Distribution des rendements par État |
| `T29_rendement_engrais.png` | 29 | Rendements selon utilisation d'engrais |
| `T30a_adoption_intrants_milieu.png` | 30 | Adoption des intrants par milieu |
| `T30b_semences_ameliorees_par_culture.png` | 30 | Adoption des semences améliorées par culture |

### Tableaux (dossier `outputs/tables/`)

| Fichier | Tâche | Description |
|---|---|---|
| `T25_cultures_frequentes.csv` | 25 | Fréquences et proportions des 15 cultures |
| `T26_stats_diversification.csv` | 26 | Statistiques descriptives diversification |
| `T26_stats_diversification_milieu.csv` | 26 | Diversification par milieu |
| `T26_test_wilcoxon_diversification.csv` | 26 | Résultat test de Wilcoxon |
| `T27_chi2_engrais_milieu.csv` | 27 | Résultat test chi-deux |
| `T27_taux_utilisation_intrants.csv` | 27 | Taux d'utilisation par intrant et milieu |
| `T28_stats_rendement_mais_mil.csv` | 28 | Statistiques rendements maïs et mil |
| `T29_stats_rendement_par_engrais.csv` | 29 | Rendements selon engrais |
| `T29_wilcoxon_engrais_rendement.csv` | 29 | Test de Wilcoxon engrais × rendement |
| `T29_taille_effet_engrais_rendement.csv` | 29 | Taille d'effet (r de rang) |
| `T30_adoption_semences_par_culture.csv` | 30 | Adoption semences par culture |
| `T30_tableau_recapitulatif_adoption.csv` | 30 | Récapitulatif global adoption |
| `T30_taux_adoption_global_milieu.csv` | 30 | Adoption par milieu |

---

## Compilation du rapport

Placer dans le dossier `docs/` :
- `Rapport_TP5.Rmd`
- `ANSD.png` et `ENSAE.png`
- Le dossier `figures/` et `tables/` (copies des outputs)

Puis compiler dans RStudio :

```r
install.packages(c("officedown", "officer", "flextable", "rmarkdown", "knitr", "dplyr"))
rmarkdown::render("docs/Rapport_TP5.Rmd")
```

---

## Note méthodologique

La comparaison des semences améliorées entre Wave 1 et Wave 4 n'a pas été réalisée car la variable distinguant semences améliorées et traditionnelles (`s11fq3b`) n'existe pas sous la même forme en Wave 1. Cette limite est inhérente à l'évolution du questionnaire entre vagues.

---

## Source des données

National Bureau of Statistics (NBS) & World Bank (2019).  
*Nigeria General Household Survey, Panel 2018/2019, Wave 4.*  
Abuja : NBS. Disponible sur : [microdata.worldbank.org](https://microdata.worldbank.org)
