# TP4 — Analyse des parcelles agricoles
## ENSAE ISE1 2025–2026 | Nigeria GHS Panel Wave 4

---

## Structure du projet

```
TP4/
│
├── mon_projet.Rproj           ← Fichier RStudio (ouvrir en premier)
│
├── data/
│   ├── raw/                   ← Fichiers .dta bruts (NE PAS MODIFIER)
│   │   ├── sect11a1_plantingw4.dta
│   │   ├── sect11b1_plantingw4.dta
│   │   └── secta_harvestw4.dta
│   └── processed/             ← Bases nettoyées (générées par 03_nettoyage.R)
│       ├── sect11a1_clean.rds
│       ├── sect11b1_clean.rds
│       └── menage_parcelle.rds
│
├── R/
│   ├── 01_import.R            ← Chargement et exploration des données
│   ├── 02_recodage.R          ← Création variables (superficie_ha, tenure, secteur…)
│   ├── 03_nettoyage.R         ← Doublons, aberrants, NA, agrégation ménage
│   └── 04_analyse.R           ← Analyses Q19, Q20, Q21, Q23, Q24
│
├── output/
│   ├── figures/               ← Graphiques .png et .RDATA
│   └── tables/                ← Tableaux .csv et .txt
│
└── rapport/
    └── rapport_final.Rmd      ← Rapport final (knit → PDF)
```

---

## Ordre d'exécution

1. Ouvrir `mon_projet.Rproj` dans RStudio
2. Exécuter `R/01_import.R`
3. Exécuter `R/02_recodage.R`
4. Exécuter `R/03_nettoyage.R`
5. Exécuter `R/04_analyse.R`
6. Knitter `rapport/rapport_final.Rmd` → PDF

> Les scripts 01 à 04 doivent être exécutés dans la même session R
> (les objets se transmettent de script en script).

---

## Données mobilisées

| Fichier                    | Contenu principal                        |
|----------------------------|------------------------------------------|
| sect11a1_plantingw4.dta    | Roster parcelles, superficie, zone, GPS  |
| sect11b1_plantingw4.dta    | Tenure foncière (s11b1q4)                |
| secta_harvestw4.dta        | Poids de sondage (wt_wave4)              |

---

## Questions traitées

| Question | Contenu                                             |
|----------|-----------------------------------------------------|
| Q19      | Import, conversion ha, qualité des données          |
| Q20      | Analyse univariée superficie (hist, boxplot, GPS)   |
| Q21      | Tenure foncière + test Chi-deux × milieu            |
| Q23      | Superficie × nb parcelles (Spearman + loess)        |
| Q24      | Heatmap superficie médiane par État (pondérée)      |

*Q22 (comparaison inter-vagues) non traitée conformément à la consigne.*
