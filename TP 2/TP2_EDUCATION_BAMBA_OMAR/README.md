# TP2 – Éducation et alphabétisation des membres des ménages
## Analyse des niveaux d'instruction et des taux de scolarisation (GHS Panel W4)

## Description du Projet
Ce TP analyse les niveaux d'instruction et les taux d'alphabétisation des membres 
des ménages nigérians à partir des sections éducation du Nigeria General Household 
Survey (GHS) Panel. L'analyse compare les profils entre sexes, tranches d'âge et 
zones géographiques, en s'appuyant principalement sur la vague 4 (2018).

## Équipe
- **Cheikh Ahmadou Bamba FALL**
- **Cheikh Omar TRAORE**

**Classe :** ISE 1  
**École :** École Nationale de la Statistique et de l'Analyse Économique Pierre Ndiaye (ENSAE)  
**Année académique :** 2025-2026

## Structure du Projet
```
TP2_EDUCATION/
├── main.R                        # Point d'entrée unique — exécuter ce fichier
├── TP2_EDUCATION.Rproj           # Fichier projet RStudio
├── README.md                     # Ce fichier
├── data/
│   ├── raw/                      # Données brutes (JAMAIS modifiées)
│   │   ├── w1/                   # (réservé)
│   │   ├── w2/                   # (réservé)
│   │   ├── w3/                   # (réservé)
│   │   └── w4/                   # sect2_harvestw4, sect1_harvestw4, secta_harvestw4
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
| `sect2_harvestw4` | Niveau d'éducation, scolarisation (W3 et W4 fusionnés) | W4 |
| `sect1_harvestwX` | Sexe, âge (jointure individus) | W4 |
| `secta_harvestwX` | Zone rurale/urbaine, État, LGA | W4 |

**Source :** World Bank LSMS-ISA — [https://microdata.worldbank.org](https://microdata.worldbank.org)

## Analyses Réalisées
| Question | Description | Output |
|----------|-------------|--------|
| Q7 | Chargement, jointure sect2 + sect1, valeurs manquantes | Console |
| Q8 | Variable niveau_educ (5 catégories) + fréquences + barplot | `Q8_barplot_niveau_educ.png`, `Q8_frequences_niveau_educ.csv` |
| Q9 | Niveau d'éducation hommes vs femmes (adultes 18+) + chi-deux + V de Cramér | `Q9_barplot_educ_sexe.png` |
| Q10 | Âge vs niveau d'éducation : boxplot + Kruskal-Wallis + Dunn | `Q10_boxplot_age_educ.png`, `Q10_dunn_test.csv` |
| Q11 | Taux de scolarisation 6-17 ans : rural vs urbain + chi-deux | `Q11_scolarisation_zone.png` |
| Q12 | Heatmap part d'adultes sans instruction par État | `Q12_heatmap_etat_educ.png`, `Q12_heatmap_data.csv` |

## Comment Reproduire les Résultats
1. Ouvrir `TP2_EDUCATION.Rproj` dans RStudio
2. Exécuter uniquement `main.R`
3. Tous les outputs sont générés automatiquement dans `output/`

## Packages R Utilisés
`here`, `haven`, `dplyr`, `ggplot2`, `naniar`, `gtsummary`,
`rstatix`, `scales`, `forcats`, `viridis`, `patchwork`