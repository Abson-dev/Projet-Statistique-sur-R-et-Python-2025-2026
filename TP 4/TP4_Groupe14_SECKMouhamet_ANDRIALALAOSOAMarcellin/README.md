# TP4 : Structure Foncière et Parcelles Agricoles (Nigeria GHS Panel - Vague 4)

Ce projet analyse les données du module parcelles agricoles de l'Enquête Générale des Ménages du Nigeria (GHS) Panel, vague 4 (2018-2019). Il décrit la structure foncière des exploitations nigérianes : superficie des parcelles, comparaison mesure déclarée vs GPS, régime de tenure foncière et disparités géographiques entre États.

## Structure du Projet

```
TP4/
+-- data/
|   +-- raw/                 # Données brutes (.dta et .shp)
|   +-- processed/           
+-- outputs/
|   +-- graphs/              # Graphiques générés (histogrammes, boxplot, scatter, carte)
|   +-- tables/              # Tableaux CSV et TXT (statistiques, tests, tenure)
+-- scripts/
|   +-- script_TP4.R         # Script principal d'analyse 
+-- docs/
|   +-- rapport_tp4_word.Rmd # Rapport R Markdown -> Word
|   +-- assemble_tp4.R       # Script d'assemblage (page de garde + TOC + rapport)
|   +-- template.docx        # Template Word de référence
|   +-- page_garde.docx      # Page de garde
+-- README.md                # Ce fichier
+-- TP4.Rproj                # Projet R
```

## Données

- **Sources** :
  - `secta1_harvestw4.dta` : 10 961 parcelles -- module agriculture post-récolte (superficie déclarée, accès, milieu, État)
  - `secta_harvestw4.dta` : 5 025 ménages -- poids de sondage (`wt_wave4`)
  - `nga_plotgeovariables_y4.dta`
  - `sect11b1_plantingw4.dta` 
  - `gadm41_NGA_1.shp` : shapefile des 37 États nigérians (GADM)

- **Variables clés** : superficie déclarée (`sa1q11`), superficie GPS (`prefilled_gps_area`), mode d'acquisition (`s11b1q4`), accès parcelle (`sa1q4`), raison perte d'accès (`sa1q7`), milieu (`sector`), État (`state`), poids (`wt_wave4`).

## Analyses Réalisées

- **Qualité des données** : doublons, valeurs manquantes, aberrants, cohérence des identifiants.
- **Analyse univariée de la superficie** : statistiques descriptives, déciles, histogrammes et boxplot (échelle log), par parcelle et par ménage.
- **Comparaison déclarée vs GPS** : corrélation de Spearman (r = 0,824), analyse du biais de sur-déclaration (62 %, ratio médian 1,19).
- **Tenure foncière** : distribution des 7 modalités d'acquisition (`s11b1q4`), test du Chi-deux tenure × milieu (χ² = 509,43, V de Cramér = 0,216).
- **Accès aux parcelles** : fréquences (`sa1q4`), raisons de perte (`sa1q7`), test du Chi-deux accès × milieu (χ² = 28,59, V de Cramér = 0,051).
- **Superficie × nombre de parcelles** : corrélation de Spearman (r = 0,443), courbe loess.
- **Carte choroplèthe** : superficie médiane par État (données brutes, indicatif uniquement).

## Dépendances

- R version >= 4.0
- Packages : `haven`, `dplyr`, `ggplot2`, `tidyr`, `sf`, `scales`, `rstatix`, `viridis`, `ggrepel`, `patchwork`, `rmarkdown`, `knitr`, `officer`

## Exécution

1. Ouvrir `TP4.Rproj` dans RStudio.
2. Exécuter `scripts/script_TP4.R` pour reproduire toutes les analyses et générer les outputs.
3. Les graphiques et tableaux sont sauvegardés dans `outputs/`.

## Rapport

Consulter `docs/rapport_tp4_word.Rmd` pour la présentation détaillée des résultats.
Exécuter `docs/assemble_tp4.R` pour compiler le rapport final Word 

## Auteur

Mouhamet SECK
Marcellin ANDRIALALAOSOA