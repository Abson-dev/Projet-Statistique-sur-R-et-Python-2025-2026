---
output:
  pdf_document: default
  html_document: default
---
# TP1 – Profil démographique des ménages nigérians
## Analyse descriptive de la composition et des caractéristiques des ménages (GHS Panel W1-W4)

## Description du Projet
Ce TP analyse le profil démographique des ménages nigérians à partir du Nigeria General 
Household Survey (GHS) Panel, enquête longitudinale en quatre vagues (2010-2018) financée 
par la Banque Mondiale (programme LSMS-ISA). L'analyse porte principalement sur la vague 4 
(2018) et couvre la structure par âge, le sexe, le lien de parenté et la taille des ménages 
selon le milieu de résidence.

## Équipe
- **Cheikh Ahmadou Bamba FALL**
- **Cheikh Omar TRAORE**

**Classe :** ISE 1  
**École :** École Nationale de la Statistique et de l'Analyse Économique Pierre Ndiaye (ENSAE)  
**Année académique :** 2025-2026

## Structure du Projet
```
TP1_DEMOGRAPHIE/
├── main.R                        # Point d'entrée unique — exécuter ce fichier
├── TP1_DEMOGRAPHIE.Rproj         # Fichier projet RStudio
├── README.md                     # Ce fichier
├── data/
│   ├── raw/                      # Données brutes (JAMAIS modifiées)
│   │   ├── w1/                   # sect1_harvestw1, sect1_plantingw1, secta_harvestw1
│   │   ├── w2/                   # sect1_harvestw2, sect1_plantingw2, secta_harvestw2
│   │   ├── w3/                   # sect1_harvestw3, sect1_plantingw3, secta_harvestw3
│   │   └── w4/                   # sect1_harvestw4, sect1_plantingw4, secta_harvestw4
│   └── processed/                # Données nettoyées et transformées
├── R/
│   ├── fonctions.R               # Fonctions utilitaires réutilisables
│   ├── 01_import.R               # (Non utilisé — données locales)
│   ├── 02_nettoyage.R            # Chargement, exploration et nettoyage
│   └── 03_analyse.R              # Analyses, graphiques et tableaux
├── output/
│   ├── figures/                  # Graphiques exportés (.png)
│   └── tables/                   # Tableaux exportés (.csv)
└── rapport/
    └── rapport_final.Rmd         # Rapport R Markdown final
```

## Données Utilisées
| Fichier | Contenu | Vagues |
|---------|---------|--------|
| `sect1_harvestwX` | Membres du ménage : âge, sexe, lien de parenté | W1 W2 W3 W4 |
| `sect1_plantingwX` | Mêmes variables, visite Post-Planting | W1 W2 W3 W4 |
| `secta_harvestwX` | Zone rurale/urbaine, État, LGA | W1 W2 W3 W4 |

**Source :** World Bank LSMS-ISA — [https://microdata.worldbank.org](https://microdata.worldbank.org)

## Analyses Réalisées
| Question | Description | Output |
|----------|-------------|--------|
| Q1 | Chargement, exploration, doublons, valeurs manquantes | Console |
| Q2 | Analyse univariée de l'âge (histogramme, boxplot, stats, Shapiro-Wilk) | `T2_histogramme_age.png`, `T2_boxplot_age.png`, `T2_stats_age.csv` |
| Q3 | Pyramide des âges par sexe (groupes de 5 ans) – W4 | `T3_pyramide_ages_W4.png` |
| Q4 | Fréquences du lien de parenté avec IC à 95% | `T4_lien_parente.png`, `T4_frequences_parente.csv` |
| Q5 | Taille des ménages rural/urbain + test Wilcoxon | `T5_boxplot_taille_zone.png`, `T5_stats_taille_zone.csv` |
| Q6 | Tableau gtsummary stratifié par zone | `T6_tableau_gtsummary.html` |

## Comment Reproduire les Résultats
1. Ouvrir `TP1_DEMOGRAPHIE.Rproj` dans RStudio
2. Exécuter uniquement `main.R`
3. Tous les outputs sont générés automatiquement dans `output/`

## Packages R Utilisés
`here`, `haven`, `dplyr`, `ggplot2`, `apyramid`, `naniar`, `gtsummary`, 
`rstatix`, `scales`, `forcats`, `moments`, `patchwork`