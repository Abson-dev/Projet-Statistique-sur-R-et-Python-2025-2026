# TP3 – Accès aux services de santé et chocs sanitaires des ménages
## Analyse des épisodes de maladie, recours aux soins et dépenses de santé (GHS Panel W4)

## Description du Projet
Ce TP analyse les épisodes de maladie, les types de soins consultés et les coûts 
de santé supportés par les ménages nigérians à partir des sections santé du Nigeria 
General Household Survey (GHS) Panel. L'analyse décrit les disparités par sexe, âge, 
milieu de résidence et niveau de richesse, en s'appuyant principalement sur la vague 4 
(2018).

## Équipe
- **Cheikh Ahmadou Bamba FALL**
- **Cheikh Omar TRAORE**

**Classe :** ISE 1  
**École :** École Nationale de la Statistique et de l'Analyse Économique Pierre Ndiaye (ENSAE)  
**Année académique :** 2025-2026

## Structure du Projet
```
TP3_SANTE/
├── main.R                        # Point d'entrée unique — exécuter ce fichier
├── TP3_SANTE.Rproj               # Fichier projet RStudio
├── README.md                     # Ce fichier
├── data/
│   ├── raw/                      # Données brutes (JAMAIS modifiées)
│   │   ├── w1/                   # sect3a, sect3b, sect1, secta, cons_agg W1
│   │   ├── w2/                   # sect3a, sect3b, sect1, secta, cons_agg W2
│   │   ├── w3/                   # sect3, sect1, secta, cons_agg W3
│   │   └── w4/                   # sect3a, sect3b, sect1, secta W4
│   └── processed/                # Données nettoyées et transformées
├── R/
│   ├── fonctions.R               # Fonctions utilitaires réutilisables
│   ├── 01_import.R               # (Non utilisé — données locales)
│   ├── 02_nettoyage.R            # Chargement, jointure et nettoyage
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
| `sect3a_harvestwX` | Maladies/blessures déclarées, type de maladie | W1 W2 W3 W4 |
| `sect3b_harvestwX` | Consultations, dépenses totales de santé | W1 W2 W3 W4 |
| `sect1_harvestwX` | Sexe, âge (jointure individus) | W1 W2 W3 W4 |
| `secta_harvestwX` | Zone rurale/urbaine, État, LGA | W1 W2 W3 W4 |
| `cons_agg_wave3_visit2` | Consommation totale par ménage (quintile) | W3 |

**Source :** World Bank LSMS-ISA — [https://microdata.worldbank.org](https://microdata.worldbank.org)

## Analyses Réalisées
| Question | Description | Output |
|----------|-------------|--------|
| Q13 | Taux de morbidité par sexe et groupe d'âge avec IC 95% | `Q13_morbidite_sexe.png`, `Q13_morbidite_age.png`, `Q13_morbidite_sexe.csv` |
| Q14 | Top 10 types de maladies déclarées par catégorie | `Q14_types_maladies.png`, `Q14_top10_maladies.csv` |
| Q15 | Recours aux soins par type de prestataire | `Q15_prestataires.png`, `Q15_prestataires.csv` |
| Q16 | Distribution des dépenses de santé (log) + stats par décile | `Q16_histogramme_depenses.png`, `Q16_boxplot_depenses_presta.png`, `Q16_stats_decile_depenses.csv` |
| Q17 | Recours aux soins × quintile de consommation + chi-deux + V de Cramér | `Q17_recours_quintile.png`, `Q17_contingence_recours_quintile.csv` |
| Q18 | Dépenses médianes de santé rural/urbain + test Wilcoxon + violin plot | `Q18_violin_depenses_zone.png` |

## Comment Reproduire les Résultats
1. Ouvrir `TP3_SANTE.Rproj` dans RStudio
2. Exécuter uniquement `main.R`
3. Tous les outputs sont générés automatiquement dans `output/`

## Packages R Utilisés
`here`, `haven`, `dplyr`, `ggplot2`, `naniar`, `gtsummary`,
`rstatix`, `scales`, `forcats`, `patchwork`, `ggpubr`