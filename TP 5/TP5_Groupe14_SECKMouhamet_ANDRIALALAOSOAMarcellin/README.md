# TP5 : Cultures pratiquées, intrants utilisés et rendements agricoles (Nigeria GHS Panel - Vague 4)

Ce projet analyse les données du module agriculture de l'Enquête Générale des Ménages du Nigeria (GHS) Panel, vague 4 (2018-2019). Il décrit les cultures les plus fréquentes, la diversification culturale des ménages, l’utilisation des intrants (engrais chimiques, organiques, pesticides) et les rendements à l’hectare du maïs et du millet, avec une attention particulière aux disparités entre zones rurales/urbaines et entre États.

## Structure du Projet

```
TP5/
+-- data/
| +-- raw/ # Données brutes (.dta et .shp)
| +-- processed/
+-- outputs/
| +-- graphs/ # Graphiques générés (top 15 cultures, histogrammes, violin plots, cartes)
| +-- tables/ # Tableaux CSV et TXT (statistiques, tests, taux d'intrants)
+-- scripts/
| +-- script_TP5.R # Script principal d’analyse
+-- docs/
| +-- rapport_tp5_word.Rmd # Rapport R Markdown -> Word
| +-- assemble_tp5.R # Script d'assemblage (page de garde + TOC + rapport)
| +-- template.docx # Template Word de référence
| +-- page_garde.docx # Page de garde
+-- README.md # Ce fichier
+-- TP5.Rproj # Projet R
```


## Données

- **Sources** :
  - `sect11f_plantingw4.dta` : 14 411 observations (parcelle × culture) – cultures plantées.
  - `secta11c2_harvestw4.dta` : 9 953 parcelles – module intrants (engrais, pesticides).
  - `secta3i_harvestw4.dta` : 25 229 observations – production récoltée (maïs, millet).
  - `sect11a1_plantingw4.dta` : 10 406 parcelles – superficies (déclarée ou GPS).
  - `secta_harvestw4.dta` : 5 025 ménages – poids de sondage (`wt_wave4`).
  - `secta1_harvestw4.dta` : 10 961 parcelles – informations géographiques (état).
  - `gadm41_NGA_1.shp` : shapefile des 37 États nigérians (GADM).

- **Variables clés** : `cropcode` (code culture), `superficie_ha`, `engrais_inorg`, `engrais_npk`, `engrais_uree`, `engrais_org`, `pesticide`, `quantite_kg`, `rendement_ha`, `milieu_lib` (rural/urbain), `state_name` (État), `wt_wave4`.

## Analyses Réalisées

- **Qualité des données** : doublons, valeurs manquantes, aberrants, cohérence des identifiants.
- **Top 15 cultures** : fréquences des cultures (parcelle × culture), barplot coloré par type (céréale, légumineuse, tubercule, culture de rente).
- **Diversification culturale** : nombre de cultures distinctes par ménage, histogramme global, violin+boxplot rural/urbain, test de Wilcoxon (p = 9,6×10⁻⁷, r = 0,083).
- **Utilisation des intrants** :
  - Taux d’utilisation par milieu (rural/urbain) avec IC 95 % (engrais chimique, NPK, urée, engrais organique, pesticide).
  - Test du chi‑deux milieu × engrais chimique (χ² = 16,79, V de Cramér = 0,045).
  - Carte choroplèthe du taux d’engrais chimique par État.
- **Rendements à l’hectare (maïs et millet)** :
  - Nettoyage des outliers (IQR × 3), statistiques descriptives.
  - Violin + boxplot par État (États avec ≥ 10 observations).
  - Cartes séparées des rendements médians par État (maïs et millet).
- **Relation engrais chimique × rendement** :
  - Tests de Wilcoxon (maïs : p < 2,2×10⁻¹⁶, r = 0,27 ; millet : p = 0,489, n faible).
  - Boxplots comparatifs avec annotations des p‑valeurs et taille d’effet.

## Dépendances

- R version >= 4.0
- Packages : `haven`, `dplyr`, `ggplot2`, `tidyr`, `forcats`, `scales`, `sf`, `viridis`, `rstatix`, `patchwork`, `stringr`, `rmarkdown`, `knitr`, `officer`

## Exécution

1. Ouvrir `TP5.Rproj` dans RStudio.
2. Exécuter `scripts/script_TP5.R` pour reproduire toutes les analyses et générer les outputs.
3. Les graphiques et tableaux sont sauvegardés dans `outputs/`.

## Rapport

Consulter `docs/Rapport.Rmd` pour la présentation détaillée des résultats.
Exécuter `docs/Rapport.R` pour compiler le rapport final Word.

## Auteur

Mouhamet SECK,
Marcellin ANDRIALALAOSOA
