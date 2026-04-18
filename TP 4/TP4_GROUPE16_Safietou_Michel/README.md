# TP4 -- Projet Statistique sous R et Python

Groupe 16 | ENSAE Pierre Ndiaye, ISE1, 2025-2026  
Auteurs : Safietou DEME, Michel TEVOEDJRE  
Encadrant : Aboubacar HEMA

---

## Objet du travail

Ce dépôt contient les scripts, données traitées et rapport du quatrième travail pratique du cours de Projet Statistique sous R et Python. L'analyse porte sur la **structure foncière des exploitations agricoles au Nigéria**, à partir du Nigeria General Household Survey-Panel Wave 4 (NBS, 2018/19), sections `sect11a1_plantingw4.dta` et `sect11b1_plantingw4.dta`.

Les objectifs sont : nettoyer et harmoniser les superficies déclarées et GPS, analyser la distribution des parcelles, étudier les régimes de tenure foncière, comparer superficies déclarées et mesurées, et mettre en évidence les disparités spatiales par zone et par État.

---

## Structure du dépôt

```
.
├── data/
│   └── raw/
│       ├── sect11a1_plantingw4.dta   # Caractéristiques des parcelles (superficie déclarée, tenure)
│       └── sect11b1_plantingw4.dta   # Informations complémentaires sur les parcelles
├── scripts/
│   └── 01_nettoyage.R                # Importation, recodage, harmonisation, nettoyage
├── output/
│   └── figures/
│       ├── boxplot_distribution.png  # Distribution des superficies par zone
│       ├── declaree_mesuree.png      # Comparaison superficies déclarées vs GPS
│       ├── heatmap_zone.png          # Heatmap des superficies par zone et État
│       ├── histogrammes.png          # Histogrammes des superficies
│       ├── p_scatter_loess.png       # Nuage de points (superficie vs nb parcelles)
│       └── regime_tenure.png         # Répartition des régimes de tenure foncière
├── rapport/
│   ├── Rapport_final.Rmd             # Rapport principal (R Markdown)
│   ├── Rapport_final.pdf             # Rapport principal (PDF)
│   ├── Rapport_final.docx            # Rapport principal (Word)
│   └── style.docx                    # Template de mise en forme
├── renv/                             # Environnement de reproductibilité
├── renv.lock
└── README.md
```

---

## Reproductibilité

Ouvrir le projet RStudio (`*.Rproj`) puis compiler le rapport :

```r
rmarkdown::render("rapport/Rapport_final.Rmd")
```

Le script `01_nettoyage.R` est sourcé automatiquement depuis le rapport. Les figures sont sauvegardées dans `output/figures/`.

---

## Packages R utilisés

| Package    | Usage principal                                        |
|------------|-------------------------------------------------------|
| haven      | Lecture des fichiers Stata (.dta)                     |
| dplyr      | Manipulation et transformation des données            |
| ggplot2    | Visualisations graphiques                             |
| ggrepel    | Étiquettes sur les graphiques                         |
| patchwork  | Assemblage de graphiques ggplot2                      |
| rstatix    | Tests statistiques                                    |
| viridis    | Palettes de couleurs pour heatmaps                    |
| scales     | Formatage des axes                                    |
| survey / srvyr | Prise en compte du plan de sondage              |
| gtsummary / gt | Tableaux de statistiques descriptives           |
| here       | Gestion des chemins relatifs                          |
| writexl    | Export des tables en Excel                            |

---

## Principaux résultats

- Les superficies déclarées et mesurées par GPS présentent des écarts significatifs, les déclarations tendant à sous-estimer la superficie réelle.
- La tenure foncière est dominée par l'héritage et la propriété directe ; la location reste marginale.
- De fortes disparités spatiales persistent entre zones et États, les exploitations du Nord étant en moyenne plus grandes.
- La relation entre superficie totale et nombre de parcelles est positive mais hétérogène selon la zone géographique.

---

## Source des données

National Bureau of Statistics (NBS) Nigeria — General Household Survey Panel, Wave 4, 2018/2019.  
Sections exploitées : `sect11a1` et `sect11b1` (planting wave, parcelles agricoles).
