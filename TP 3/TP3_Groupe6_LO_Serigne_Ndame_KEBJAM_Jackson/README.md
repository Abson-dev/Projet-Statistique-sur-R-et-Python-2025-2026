# TP3 — Accès aux services de santé et chocs sanitaires des ménages nigérians

**Cours :** Projet Statistique sous R et Python | ENSAE ISE 1 — 2025-2026  
**Données :** Nigeria General Household Survey (GHS) Panel — Wave 4 (2018)  
**Source :** [World Bank LSMS-ISA](https://microdata.worldbank.org/index.php/catalog/lsms)

---

## Structure du projet

```
TP3_acces_sante/
├── TP3_acces_sante.Rproj              ← Ouvrir dans RStudio
├── data/
│   ├── raw/                           ← Fichiers .dta originaux (NE PAS MODIFIER)
│   │   ├── sect1_harvestw1.dta
│   │   ├── sect1_harvestw2.dta
│   │   ├── sect1_harvestw3.dta
│   │   ├── sect1_harvestw4.dta
│   │   ├── sect3a_harvestw1.dta
│   │   ├── sect3a_harvestw2.dta
│   │   ├── sect3a_harvestw4.dta
│   │   ├── sect3b_harvestw1.dta
│   │   ├── sect3b_harvestw2.dta
│   │   ├── sect3b_harvestw4.dta
│   │   └── totcons_final.dta
│   └── processed/                     ← Fichiers .rds générés automatiquement
│       ├── sect3a_clean.rds
│       ├── sect3b_clean.rds
│       ├── data_morbid.rds
│       ├── data_depenses.rds
│       ├── data_recours.rds
│       └── totcons_clean.rds
├── R/
│   ├── 02_nettoyage.R                 ← Nettoyage et transformation
│   ├── 03_analyse.R                   ← Analyses, tests, visualisations
│   └── fonctions.R                    ← Fonctions réutilisables
├── output/
│   ├── figures/                       ← Graphiques .png exportés
│   │   ├── fig00_synthese_tp3.png
│   │   ├── fig13_morbidite_sexe_age_zone.png
│   │   ├── fig14_types_maladies.png
│   │   ├── fig15_prestataires.png
│   │   ├── fig16_depenses_sante.png
│   │   ├── fig17_recours_quintile.png
│   │   └── fig18_depenses_zone.png
│   └── tables/                        ← Tableaux exportés
│       ├── tab15_prestataires.xlsx
│       ├── tab16_stats_depenses.xlsx
│       ├── tab16_deciles_depenses.xlsx
│       ├── tab17_contingence.xlsx
│       ├── tab18_depenses_zone1.xlsx
│       └── tableau_sante_zone1.html
├── rapport/
│   └── rapport_final.Rmd              ← Rapport R Markdown (~8-10 pages)
└── README.md
```
## Structure conceptuelle de la base
sect1_harvestw4.dta              sect3a_harvestw4.dta
    │ (caractéristiques               │ (morbidité)
    │  démographiques)                │
    │                                 │
    └──────────┬──────────────────────┘
               │
               │ jointure par hhid + indiv
               ▼
        données individuelles
        (âge, sexe, maladie)
               │
               │ jointure par hhid
               ▼
┌──────────────────────────────────────────────────┐
│                                                   │
│  sect3b_harvestw4.dta      cons_agg_wave4_visit2 │
│  (dépenses, recours)        (quintiles richesse) │
│                                                   │
└──────────────────────────────────────────────────┘

---

## Ordre d'exécution

> Ouvrir `TP3_acces_sante.Rproj` dans RStudio, puis exécuter dans l'ordre :

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
| `R/02_nettoyage.R` | Charge les 4 fichiers `.dta` depuis `data/raw/`, recode les variables (morbidité, prestataires, zone géopolitique, zone1 Nord/Sud), calcule les quintiles de consommation, construit 3 fichiers analytiques joints |
| `R/fonctions.R` | Définit 6 fonctions : `stats_descriptives()`, `stats_par_decile()`, `calc_ic_proportion()`, `v_cramer()`, `effet_wilcoxon_r()`, `sauvegarder_figure()`, `sauvegarder_tableau()` |
| `R/03_analyse.R` | Réalise les tâches 13 à 18 + tableau gtsummary + figure de synthèse patchwork |
| `rapport/rapport_final.Rmd` | Rapport R Markdown complet (8-10 pages) avec texte dynamique, interprétations et tous les graphiques |

---

## Variables clés

| Fichier source | Variable | Description |
|---|---|---|
| `sect3a_harvestw4.dta` | `s3q1` | Maladie/blessure déclarée (1=oui, 2=non) |
| `sect3a_harvestw4.dta` | `s3q3` | Type de maladie/affection |
| `sect3a_harvestw4.dta` | `zone` | Zone géopolitique (1=Centre-Nord … 6=Sud-Ouest) |
| `sect3b_harvestw4.dta` | `s3q50` | Dépense totale de santé (Naira) |
| `sect3b_harvestw4.dta` | `s3q51_1` à `s3q51_7` | Types de prestataires consultés (binaires) |
| `sect1_harvestw4.dta` | `s1q2` | Sexe du membre |
| `sect1_harvestw4.dta` | `s1q4` | Âge du membre |
| `totcons_final.dta` | variable auto-détectée | Consommation totale (quintiles) |

---

## Recodage de la zone géopolitique

```r
# 6 zones géopolitiques nigérianes
zone = factor(zone, levels = c(1,2,3,4,5,6),
              labels = c("Centre-Nord","Nord-Est","Nord-Ouest",
                         "Sud-Est","Sud-Sud","Sud-Ouest"))

# Regroupement Nord / Sud pour les tests comparatifs
zone1 = case_when(
  zone %in% c("Centre-Nord","Nord-Est","Nord-Ouest") ~ "Nord",
  zone %in% c("Sud-Est","Sud-Sud","Sud-Ouest")       ~ "Sud"
)
```

---

## Tâches couvertes

| # | Tâche | Méthode statistique | Livrable |
|---|---|---|---|
| 13 | Taux de morbidité par sexe, âge, zone | IC 95% (`binom.test`), barplots | `fig13_morbidite_sexe_age_zone.png` |
| 14 | Top 10 types de maladies | Fréquences, barplot coloré par catégorie | `fig14_types_maladies.png` |
| 15 | Recours aux soins par prestataire | Fréquences, barplot ordonné | `fig15_prestataires.png` |
| 16 | Distribution dépenses de santé | Histogramme log, déciles, boxplot, IQR×3 | `fig16_depenses_sante.png` |
| 17 | Recours × quintile de richesse | Chi-deux / Fisher, V de Cramér, barplot 100% empilé | `fig17_recours_quintile.png` |
| 18 | Dépenses médianes Nord vs Sud | Wilcoxon, taille d'effet r, violin + boxplot | `fig18_depenses_zone.png` |

---

## Packages R requis

```r
install.packages(c(
  "haven",      # lecture fichiers .dta
  "dplyr",      # manipulation de données
  "ggplot2",    # visualisation
  "scales",     # formatage des axes (comma, percent)
  "patchwork",  # assemblage de graphiques
  "gtsummary",  # tableaux récapitulatifs
  "rstatix",    # tests statistiques
  "naniar",     # valeurs manquantes
  "writexl",    # export Excel
  "gt",         # export tableaux HTML
  "moments",    # asymétrie (skewness)
  "rmarkdown"   # compilation du rapport
))
```

---

## Résultats clés (Wave 4, 2018)

- **Taux de morbidité global :** ~X% — disparités significatives par âge, sexe et zone
- **Profil épidémiologique :** dominé par les maladies infectieuses (paludisme, fièvres)
- **Premier recours :** hôpital public ou pharmacie selon la zone
- **Dépenses très dispersées :** CV > 100%, présence de dépenses catastrophiques
- **Inégalités riches/pauvres :** association significative recours × quintile (p < 0,001)
- **Disparités Nord/Sud :** dépenses médianes significativement différentes (Wilcoxon)

---

*ENSAE ISE 1 | 2025-2026 | Données : LSMS-ISA Banque Mondiale*