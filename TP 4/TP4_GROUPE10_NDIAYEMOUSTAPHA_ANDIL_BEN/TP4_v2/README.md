# TP4 — Analyse des Parcelles Agricoles (Version Améliorée)
## Nigeria GHS Panel Wave 4 (2018-2019) | ENSAE ISE1 | 2025-2026

## Nouveautés v2
- 🗺️ **3 cartes choroplèthes réelles** du Nigeria (37 États) générées avec sf + Voronoï
- 📊 **Dashboard HTML interactif** avec navigation, KPI cards, barres de progression
- 📝 **Rapport Word enrichi** avec cartes géographiques intégrées

## Structure
```
TP4/
├── data/          ← sect11b1_plantingw4.dta | nga_plotgeovariables_y4.dta | secta_harvestw4.dta
├── scripts/       ← run_tp4.R (exécutable) | 01_analyse_parcelles.R (documenté)
├── outputs/       ← 14 graphiques PNG + dashboard HTML + 4 CSV
└── docs/          ← rapport_TP4_*.Rmd + rapport_TP4_*_v2.docx
```

## Exécution
```r
# En RStudio :
setwd("chemin/vers/TP4")
source("scripts/run_tp4.R")   # ou Ctrl+Shift+Enter
```
```bash
# En ligne de commande :
cd TP4 && Rscript scripts/run_tp4.R
```

## Résultats Clés
| Indicateur | Valeur |
|-----------|--------|
| Tenure dominante | Héritage familial (62,9%) |
| Médiane parcelles/ménage | 2 (CV = 59,3%) |
| % ménages ruraux | 84,3% |
| Chi² tenure × milieu | 498,6 — V Cramér = 0,212 |
| Kruskal-Wallis zones | H=51,98 — p<0,001 |
