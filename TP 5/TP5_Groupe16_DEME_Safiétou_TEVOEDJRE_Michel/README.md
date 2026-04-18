# TP5 -- Projet Statistique sous R et Python

Groupe 16 | ENSAE Pierre Ndiaye, ISE1, 2025-2026  
Auteurs : Safiétou DEME, Michel TEVOEDJRE  
Encadrant : Aboubacar HEMA

---

## Objet du travail

Ce dépôt contient les scripts, données traitées et rapport du cinquième travail pratique du cours de Projet Statistique sous R et Python. L'analyse porte sur les **cultures pratiquées, les intrants utilisés et les rendements agricoles au Nigéria**, à partir du Nigeria General Household Survey-Panel Wave 4 (GHS, 2018/19).

Les objectifs sont : identifier le top 15 des cultures pratiquées, analyser la diversification culturale par milieu (rural/urbain), étudier les taux d'adoption des intrants agricoles par zone, comparer les productions de maïs et mil par État, et mesurer l'effet de l'engrais inorganique sur les rendements. Toutes les estimations intègrent les **pondérations Wave 4**.

---

## Structure du dépôt

```
.
├── data/
│   ├── raw/
│   │   ├── secta_harvestw4.dta         # Pondérations et géographie (ménages)
│   │   ├── secta3i_harvestw4.dta       # Récoltes par parcelle-culture
│   │   ├── secta3ii_harvestw4.dta      # Synthèse culture par ménage
│   │   └── secta11c2_harvestw4.dta     # Intrants réels utilisés par parcelle
│   └── processed/                      # Objets RDS produits par les scripts
├── scripts/
│   ├── 01_preparation_donnees.R        # Importation, recodage, jointures, pondérations
│   └── 02_analyses_visualisations.R    # Analyses statistiques et figures
├── outputs/
│   ├── 25_top15_cultures.csv           # Top 15 des cultures (données)
│   ├── 25_top15_cultures.png           # Top 15 des cultures (figure)
│   ├── 26_diversification_culturale.png
│   ├── 26_wilcoxon_diversification.csv # Test de Wilcoxon (diversification)
│   ├── 27_chi2_zone_engrais.csv        # Test Chi-deux (zone x engrais)
│   ├── 27_intrants_par_zone.png
│   ├── 27_intrants_taux_global.png
│   ├── 28_production_par_etat.png
│   └── 29_effet_engrais.png
├── rapport/
│   ├── TP5_Groupe16_DEME_Safiétou_TEVOEDJRE_Michel.Rmd   # Rapport (R Markdown)
│   ├── TP5_Groupe16_DEME_Safiétou_TEVOEDJRE_Michel.docx  # Rapport (Word)
│   ├── style_reference.docx                              # Template de mise en forme
│   └── logos/
├── main.R
└── README.md
```

---

## Reproductibilité

Ouvrir le projet RStudio (`*.Rproj`) puis exécuter :

```r
source("main.R")
```

Pour compiler le rapport :

```r
rmarkdown::render("rapport/TP5_Groupe16_DEME_Safiétou_TEVOEDJRE_Michel.Rmd")
```

---

## Packages R utilisés

| Package       | Usage principal                                      |
|---------------|-----------------------------------------------------|
| haven         | Lecture des fichiers Stata (.dta)                   |
| dplyr / tidyr | Manipulation et transformation des données          |
| forcats       | Gestion des variables catégorielles                 |
| ggplot2       | Visualisations graphiques                           |
| scales        | Formatage des axes                                  |
| survey        | Prise en compte du plan de sondage                  |
| flextable     | Tableaux dans le rapport Word                       |
| officer       | Mise en forme du document Word                      |
| knitr         | Intégration dans le rapport R Markdown              |

---

## Principaux résultats

- Le maïs et le mil dominent le paysage cultural nigérian, représentant les deux premières cultures du top 15.
- La diversification culturale est significativement plus élevée en milieu rural qu'en milieu urbain (test de Wilcoxon, p < 0,05).
- Le taux d'adoption de l'engrais inorganique varie fortement selon la zone géographique (Chi-deux, p < 0,001).
- La production de maïs est significativement supérieure chez les ménages utilisant de l'engrais inorganique (test de Wilcoxon significatif).

---

## Source des données

World Bank / National Bureau of Statistics (NBS) Nigeria — General Household Survey Panel, Wave 4, 2018/2019.  
Sections exploitées : `secta`, `secta3i`, `secta3ii`, `secta11c2` (harvest wave, cultures et intrants).
