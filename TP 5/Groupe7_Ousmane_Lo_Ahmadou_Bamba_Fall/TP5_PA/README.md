# TP5 - Cultures pratiquées, intrants utilisés et rendements agricoles
## Analyse des choix de cultures, intrants agricoles et rendements (GHS Panel W1 & W4)

## Description du Projet

Ce TP analyse les choix de cultures, les types d'intrants utilisés (semences améliorées, engrais, pesticides) et les rendements obtenus par les exploitations nigérianes à partir des sections agriculture du Nigeria General Household Survey (GHS) Panel. L'analyse décrit les disparités par zone de résidence et par État, avec comparaison inter-vagues W1 vs W4, en s'appuyant principalement sur la vague 4 (2018).

## Équipe

- **Cheikh Ahmadou Bamba FALL**
- **Ousmane LO**

**Classe :** ISE 1
**École :** École Nationale de la Statistique et de l'Analyse Économique Pierre Ndiaye (ENSAE)
**Année académique :** 2025-2026

## Structure du Projet

```
TP5_PA/
├── main.R                        # Point d'entrée unique — exécuter ce fichier
├── TP5_PA.Rproj                  # Fichier projet RStudio
├── README.md                     # Ce fichier
├── data/
│   ├── raw/                      # Données brutes (JAMAIS modifiées)
│   │   ├── w1/                   # secta3, secta_harvest, sect11d_planting, sect11f_planting
│   │   ├── w2/                   # secta3, secta5a, secta_harvest
│   │   ├── w3/                   # secta3i/3ii, secta11d_harvest, secta_harvest
│   │   └── w4/                   # secta3i/3ii, secta11c2, sect11f_planting, secta_harvest
│   └── processed/                # Données nettoyées et transformées
├── R/
│   ├── fonctions.R               # Fonctions utilitaires réutilisables
│   ├── 01_import.R               # (Non utilisé — données locales)
│   ├── 02_nettoyage.R            # (Non utilisé — jointures dans 03_analyse.R)
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
| `secta3_harvestwX` | Cultures par parcelle (cropcode, superficie) | W1 W2 |
| `secta3i_harvestwX` | Cultures par parcelle — fichier scindé à partir de W3 | W3 W4 |
| `secta3ii_harvestwX` | Production récoltée par culture | W3 W4 |
| `sect11d_plantingw1` | Utilisation d'engrais par parcelle (visite planting) | W1 |
| `secta11d_harvestw3` | Utilisation d'engrais par parcelle | W3 |
| `secta11c2_harvestw4` | Utilisation d'engrais par type (NPK, urée, organique) | W4 |
| `sect11f_plantingwX` | Semences améliorées ou traditionnelles par culture-parcelle | W1 W4 |
| `secta_harvestwX` | Zone rurale/urbaine, État, LGA, poids de sondage `wt_wave4` | W1 W2 W3 W4 |

**Source :** World Bank LSMS-ISA — [https://microdata.worldbank.org](https://microdata.worldbank.org)

> **Note :** Le TP mentionne `secta5a` pour les engrais. Dans les fichiers réels du GHS Panel, les données d'utilisation des engrais se trouvent dans les sections `sect11d` (planting W1/W2) et `secta11d` / `secta11c2` (harvest W3/W4).

## Analyses Réalisées

| Question | Description | Output |
|----------|-------------|--------|
| Q25 | Top 15 cultures les plus fréquentes en W4, colorées par type | `Q25_top15_cultures.png`, `Q25_top15_cultures.csv` |
| Q26 | Indice de diversification culturale par ménage, rural vs urbain + Wilcoxon | `Q26_diversification_culturale.png` |
| Q27 | Taux d'utilisation des engrais par type et zone + chi-deux pondéré | `Q27_engrais_zone.png`, `Q27_taux_engrais_zone.csv` |
| Q28 | Rendement à l'hectare (maïs et millet) par État, outliers IQR×3 retirés | `Q28_rendements_etat.png`, `Q28_rendements.csv` |
| Q29 | Relation engrais chimique vs rendement + Wilcoxon + taille d'effet r | `Q29_engrais_rendement.png` |
| Q30 | Taux d'adoption des intrants W1 vs W4, évolution en points de pourcentage | `Q30_adoption_W1_W4.png`, `Q30_adoption_intrants.csv`, `Q30_evolution_adoption.csv` |

## Comment Reproduire les Résultats

1. Ouvrir `TP5_PA.Rproj` dans RStudio
2. Placer les fichiers `.dta` dans les sous-dossiers `data/raw/w1/`, `w3/`, `w4/` correspondants
3. Exécuter uniquement `main.R`
4. Tous les outputs sont générés automatiquement dans `output/`

## Packages R Utilisés

`here`, `haven`, `dplyr`, `ggplot2`, `forcats`, `scales`, `tidyr`,
`rstatix`, `survey`, `srvyr`, `patchwork`, `viridis`, `gtsummary`, `Hmisc`