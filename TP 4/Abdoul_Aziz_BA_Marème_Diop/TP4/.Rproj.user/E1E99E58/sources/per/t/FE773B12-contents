# Analyse des parcelles agricoles nigérianes
## GHS Panel Nigeria — Wave 4

## Description
Ce projet analyse la structure foncière des exploitations agricoles
nigérianes à partir du General Household Survey (GHS) Panel, vague 4.
Il couvre la distribution des superficies, les régimes de tenure
foncière et les disparités géographiques entre les 37 États nigérians.

## Structure du projet
## Données utilisées

| Fichier | Contenu |
|---------|---------|
| sect11a1_plantingw4.dta | Parcelles agricoles — superficies |
| sect11b1_plantingw4.dta | Tenure foncière des parcelles |
| secta_harvestw4.dta | Poids de sondage et strates |

## Ordre d'exécution

```r
source("R/01_import.R")      # 1. Charger les données
source("R/02_nettoyage.R")   # 2. Nettoyer et convertir
source("R/03_analyse.R")     # 3. Produire les analyses
# Puis Render rapport_final.qmd
```

## Packages R utilisés
`haven` `dplyr` `ggplot2` `survey` `scales` `patchwork` `viridis`

## Source des données
National Bureau of Statistics (NBS) & World Bank —
Nigeria General Household Survey Panel, Wave 4 (2018-2019)