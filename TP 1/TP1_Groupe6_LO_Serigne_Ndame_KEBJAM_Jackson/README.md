# TP1 — Profil démographique des ménages nigérians

**Cours :** Projet Statistique sous R et Python | ENSAE ISE 1 — 2025-2026  
**Données :** Nigeria General Household Survey (GHS) Panel — Wave 4 (2018)  
**Source :** [World Bank LSMS-ISA](https://microdata.worldbank.org/index.php/catalog/lsms)

---

## Structure du projet

```
TP1_profil_demographique/
├── TP1_profil_demographique.Rproj   ← Ouvrir dans RStudio
├── data/
│   ├── raw/                         ← Fichiers .dta originaux (NE PAS MODIFIER)
│   │   ├── sect1_harvestw1.dta
│   │   ├── sect1_harvestw2.dta
│   │   ├── sect1_harvestw3.dta
│   │   ├── sect1_harvestw4.dta
│   │   ├── sect1_plantingw1.dta
│   │   ├── sect1_plantingw2.dta
│   │   ├── sect1_plantingw3.dta
│   │   ├── sect1_plantingw4.dta
│   │   ├── secta_harvestw1.dta
│   │   ├── secta_harvestw2.dta
│   │   ├── secta_harvestw3.dta
│   │   └── secta_harvestw4.dta
│   └── processed/                   ← Fichiers .rds générés automatiquement
│       ├── data_tp1_clean.rds
│       └── taille_menage.rds
├── R/
│   ├── 02_nettoyage.R               ← Nettoyage et transformation
│   ├── 03_analyse.R                 ← Analyses, tests, visualisations
│   └── fonctions.R                  ← Fonctions réutilisables
├── output/
│   ├── figures/                     ← Graphiques .png exportés
│   │   ├── fig0_synthese_tp1.png
│   │   ├── fig1_age_distribution.png
│   │   ├── fig2_pyramide_ages.png
│   │   ├── fig3_lien_parente.png
│   │   └── fig4_taille_zone.png
│   └── tables/                      ← Tableaux .xlsx et .html exportés
│       ├── stats_age.xlsx
│       ├── lien_parente_ic95.xlsx
│       └── tableau1_demo_zone.html
├── rapport/
│   └── rapport_final.Rmd            ← Rapport R Markdown (compiler pour obtenir le HTML)
└── README.md
```

## Structure conceptuelle de la basE

sect1_harvestw4.dta                    secta_harvestw4.dta
    │                                         │
    │ (composition du ménage)                 │ (localisation)
    │                                         │
    └──────────────┬──────────────────────────┘
                   │
                   ▼
            data_tp1_clean.rds
    (individus avec leur zone géographique)

---



## Ordre d'exécution

> Ouvrir `TP1_profil_demographique.Rproj` dans RStudio, puis exécuter dans l'ordre :

```r
# Étape 1 — Nettoyage et préparation des données
source("R/02_nettoyage.R")

# Étape 2 — Analyses, tests et visualisations
source("R/03_analyse.R")

# Étape 3 — Compiler le rapport R Markdown
rmarkdown::render("rapport/rapport_final.Rmd")
```

> `fonctions.R` est chargé automatiquement via `source("R/fonctions.R")` dans `03_analyse.R`.

---

## Description des scripts

| Fichier | Rôle |
|---|---|
| `R/02_nettoyage.R` | Charge `sect1_harvestw4.dta` et `secta_harvestw4.dta` depuis `data/raw/`, recode les variables (sexe, lien de parenté, groupes d'âge), calcule la taille des ménages, joint les deux fichiers et sauvegarde dans `data/processed/` |
| `R/fonctions.R` | Définit 4 fonctions : `stats_descriptives()`, `effet_wilcoxon_r()`, `sauvegarder_figure()`, `sauvegarder_tableau()` |
| `R/03_analyse.R` | Réalise les 6 tâches du TP : exploration, analyse de l'âge, pyramide des âges, lien de parenté avec IC 95%, comparaison taille ménage rural/urbain (Wilcoxon), tableau gtsummary |
| `rapport/rapport_final.Rmd` | Rapport R Markdown complet (~8 pages) avec commentaires, interprétations et tous les graphiques |

---

## Tâches couvertes

| # | Tâche | Méthode | Livrable |
|---|---|---|---|
| 1 | Exploration & valeurs manquantes | `glimpse`, `vis_miss`, doublons | Console |
| 2 | Analyse univariée de l'âge | Histogramme, boxplot, Shapiro-Wilk | `fig1_age_distribution.png` |
| 3 | Pyramide des âges par sexe | `apyramid::age_pyramid` | `fig2_pyramide_ages.png` |
| 4 | Lien de parenté + IC 95% | `binom.test`, barplot horizontal | `fig3_lien_parente.png` |
| 5 | Taille ménage rural/urbain | Boxplot, Wilcoxon, taille d'effet r | `fig4_taille_zone.png` |
| 6 | Tableau récapitulatif | `gtsummary::tbl_summary` | `tableau1_demo_zone.html` |

---

## Packages R requis

```r
install.packages(c(
  "haven",       # lecture fichiers .dta
  "dplyr",       # manipulation de données
  "ggplot2",     # visualisation
  "apyramid",    # pyramide des âges
  "naniar",      # valeurs manquantes
  "gtsummary",   # tableaux récapitulatifs
  "rstatix",     # tests statistiques
  "patchwork",   # assemblage de graphiques
  "moments",     # asymétrie (skewness)
  "writexl",     # export Excel
  "gt",          # export tableaux HTML
  "rmarkdown"    # compilation du rapport
))
```

---

## Résultats clés (Wave 4, 2018)

- **Âge médian :** 15 ans — population très jeune, pyramide à base large
- **Composition :** enfants 62% | autre famille 21% | chefs de ménage 16% | conjoints 14%
- **Normalité :** rejetée (Shapiro-Wilk p < 0,001) → tests non paramétriques justifiés
- **Taille ménage :** différence significative rural/urbain (Wilcoxon p < 0,001, r grand)
- **Genre :** répartition 50/50 hommes/femmes, sans différence entre zones (p = 0,4)

---

*ENSAE ISE 1 | 2025-2026 | Données : LSMS-ISA Banque Mondiale*