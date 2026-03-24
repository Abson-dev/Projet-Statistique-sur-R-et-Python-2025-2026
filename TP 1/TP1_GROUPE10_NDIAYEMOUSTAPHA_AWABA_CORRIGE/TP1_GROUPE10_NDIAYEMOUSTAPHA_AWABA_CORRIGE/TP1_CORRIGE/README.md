# TP1 — Profil Démographique des Ménages Nigérians
## Nigeria GHS Panel Wave 4 (2018) | ENSAE ISE 1 | 2025-2026

**Auteurs :** Cheikh Mouhamdou Moustapha Ndiaye & Awa Ba  
**Encadrant :** Aboubacar HEMA

---

## Structure du dossier

```
TP1_GROUPE10_NDIAYEMOUSTAPHA_AWABA/
├── data/
│   └── NGA_2018_GHSP-W4_v03_M_Stata12/
│       ├── sect1_harvestw4.dta     ← individus (sexe, âge, parenté)
│       ├── secta_harvestw4.dta     ← ménages + poids wt_wave4
│       └── totcons_final.dta
├── scripts/
│   ├── TP1_Demographie.R           ← script principal (analyses pondérées)
│   ├── TP1_Rapport_Demographie.Rmd ← rapport Word (knitr → .docx)
│   ├── ansd_logo.png
│   └── ensae_logo.png
├── outputs/
│   ├── Dashboard_TP1.html          ← dashboard interactif (pondéré)
│   ├── T1_missing.png
│   ├── T2_age_univarie.png
│   ├── T3_pyramide_ages.png
│   ├── T4_lien_parente.png
│   ├── T5_taille_menage.png
│   ├── T5b_zones.png
│   ├── TP1_Figure_Synthese.png
│   └── TP1_Resultats.xlsx
└── README.md
```

---

## Lancement

### Script R (analyses + graphiques)
1. Ouvrir `scripts/TP1_Demographie.R` dans RStudio
2. Cliquer **Source**
3. Les graphiques s'enregistrent automatiquement dans `outputs/`

### Rapport Word
1. Ouvrir `scripts/TP1_Rapport_Demographie.Rmd` dans RStudio
2. Cliquer **Knit → Word Document**
3. Le fichier `.docx` est généré dans `scripts/`

### Dashboard
Ouvrir `outputs/Dashboard_TP1.html` dans un navigateur.

---

## Note méthodologique — Pondération

Le GHS W4 est une **enquête en grappes stratifiées** (milieu rural/urbain × états).  
La variable de pondération **`wt_wave4`** (niveau ménage) est disponible dans `secta_harvestw4.dta`.

**Principe :** On multiplie **directement par le poids** (jamais par 1-poids).  
Le poids représente le nombre d'individus dans la population réelle que chaque observation représente.

**Où sont appliqués les poids :**
- Histogramme des âges (`weight = wt_wave4` dans ggplot2)
- Pyramide des âges (effectifs = somme des poids par groupe)
- Proportions du lien de parenté (`svymean` via `{survey}`)
- Tableau de synthèse (`tbl_svysummary` via `{gtsummary}`)
- Moyennes de taille de ménage (`weighted.mean`)
- Proportions par zone géopolitique (somme des poids)

**Package R utilisé :** `{survey}` (Lumley, 2023)

---

## Filtre appliqué

Le fichier `sect1_harvestw4.dta` contient des membres qui ont **quitté** le ménage entre vagues (`s1q4a = code 2`, N = 3 780). Ces individus n'ont pas de données démographiques renseignées et sont **exclus** de toutes les analyses.

- **Avant filtre :** 30 337 observations
- **Après filtre :** 26 557 membres présents dans 4 980 ménages

---

## Packages R requis

```r
pacman::p_load(haven, dplyr, tidyr, ggplot2, patchwork, naniar,
               gtsummary, rstatix, scales, forcats, stringr,
               survey, purrr, flextable, officer, knitr)
```

---

## Données

**Source :** National Bureau of Statistics (NBS) / World Bank  
**Enquête :** Nigeria General Household Survey Panel Wave 4 (2018)  
**Accès :** https://microdata.worldbank.org/
