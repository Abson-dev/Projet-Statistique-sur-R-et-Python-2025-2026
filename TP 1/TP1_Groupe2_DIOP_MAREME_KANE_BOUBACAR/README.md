# Profil démographique des ménages nigérians
### Nigeria General Household Survey (GHS) Panel — Vague 4 (2018-2019)

> Analyse du profil démographique des ménages enquêtés lors de la quatrième vague du GHS Panel Nigeria, produite dans le cadre du cours de Projet statistique sous R à l'ENSAE Dakar.

---

## Auteurs et encadrement

| Rôle | Nom | Filière | Année |
|------|-----|---------|-------|
| Étudiant | **DIOP Marème** | ISE 1 Cycle Long | 2025-2026 |
| Étudiant | **KANE Boubacar** | ISE 1 Éco | 2025-2026 |
| Encadrant | **HEMA Aboubacar** | Research Scientist, IFPRI | — |

---

## Description du projet

Ce projet produit un rapport d'analyse démographique à partir de la **quatrième vague du Nigeria General Household Survey (GHS) Panel**, enquête longitudinale conduite par le Bureau National des Statistiques du Nigeria (NBS) en partenariat avec la Banque Mondiale.

L'analyse porte sur **25 767 individus** répartis dans **4 978 ménages**, et couvre les thématiques suivantes :

- Distribution de l'âge et test de normalité
- Pyramide des âges par sexe
- Composition des ménages selon le lien de parenté
- Comparaison de la taille des ménages entre zones rurales et urbaines

---

## Structure du dossier

```
TP1_Groupe3_DIOP_MAREME_KANE_BOUBACAR/
│
├── 📄 README.md                        # Ce fichier
├── 📄 mon_projet.Rproj                 # Fichier de projet RStudio (ancre les chemins)
│
├── 📁 data/
│   ├── 📁 raw/                         # Données brutes — ne jamais modifier
│   │   ├── sect1_harvestw4.dta         # Section démographique (30 337 obs., 59 vars.)
│   │   └── sectaa_harvestw4.dta        # Section complémentaire (zone de résidence)
│   └── 📁 processed/                   # Données nettoyées produites par les scripts
│       ├── sect1_w4.rds                # Import brut de sect1 (avant nettoyage)
│       ├── sect1_w4_clean.rds          # Échantillon final nettoyé — fichier analytique
│       └── secta_w4.rds               # Import brut de sectaa
│
├── 📁 R/                               # Scripts numérotés dans l'ordre d'exécution
│   ├── 01_import.R                     # Import des fichiers .dta → .rds
│   ├── 02_nettoyage.R                  # Nettoyage, contrôle qualité, export .rds
│   └── 03_analyse.R                    # Analyses statistiques et figures
│
├── 📁 output/
│   ├── 📁 figures/                     # Graphiques générés par 03_analyse.R (.png)
│   └── 📁 tables/                      # Tableaux exportés (.html)
│
└── 📁 rapport/
    ├── rapport_final.Rmd               # Source R Markdown du rapport
    └── rapport_final.pdf               # Rapport PDF compilé
```

---

## Pipeline d'analyse

```
Données brutes (.dta)
        │
        ▼
R/01_import.R           ← Chargement via haven::read_dta()
        │                  Sauvegarde en .rds dans data/processed/
        ▼
R/02_nettoyage.R        ← Exclusion membres partis (s1q4a = 2)
        │                  Vérification doublons sur hhid + indiv
        │                  Visualisation des NA (variables clés)
        │                  Export → data/processed/sect1_w4_clean.rds
        ▼
R/03_analyse.R          ← Statistiques descriptives de l'âge
        │                  Tests (Shapiro-Wilk, Wilcoxon-Mann-Whitney)
        │                  Production des figures → output/figures/
        │                  Export tableau gtsummary → output/tables/
        ▼
rapport/rapport_final.Rmd   ← Rédaction et mise en forme narrative
        ▼
rapport/rapport_final.pdf
```

---

## Contenu du rapport

| Section | Contenu | Méthodes |
|---------|---------|----------|
| **Qualité des données** | Doublons, valeurs manquantes, cohérence | `vis_miss()`, `miss_var_summary()` |
| **Âge — univarié** | Moyenne, médiane, CV, asymétrie | Statistiques descriptives, Shapiro-Wilk |
| **Distribution** | Histogramme, boîte à moustaches | `ggplot2`, `patchwork` |
| **Pyramide des âges** | Groupes quinquennaux par sexe | `ggplot2` — barres miroir |
| **Lien de parenté** | Fréquences avec IC à 95 % | Test binomial exact |
| **Taille des ménages** | Rural vs Urbain | Wilcoxon-Mann-Whitney, r de rang |
| **Tableau récapitulatif** | Comparaison stratifiée par zone | `gtsummary::tbl_summary()` |

---

## Packages R utilisés

| Package | Rôle |
|---------|------|
| `haven` | Import des fichiers Stata (`.dta`) |
| `dplyr` | Manipulation et transformation des données |
| `ggplot2` | Visualisations graphiques |
| `patchwork` | Combinaison de plusieurs graphiques |
| `gtsummary` | Tableaux récapitulatifs statistiques |
| `rstatix` | Tests statistiques (Wilcoxon, taille d'effet) |
| `naniar` | Visualisation des valeurs manquantes |
| `knitr` / `kableExtra` | Tableaux dans le rapport PDF |
| `rprojroot` | Gestion robuste des chemins de fichiers |

---

## Reproduire le rapport

### Prérequis

- R ≥ 4.2.0
- RStudio (recommandé) avec le fichier `mon_projet.Rproj` ouvert
- Les packages listés ci-dessus

### Installation des packages

```r
install.packages(c(
  "dplyr", "ggplot2", "gtsummary", "rstatix",
  "patchwork", "haven", "knitr", "kableExtra",
  "naniar", "rprojroot", "gt"
))
```

### Exécution dans l'ordre

```r
# Étape 1 — Import
source("R/01_import.R")

# Étape 2 — Nettoyage
source("R/02_nettoyage.R")

# Étape 3 — Analyses et figures
source("R/03_analyse.R")

# Étape 4 — Compilation du rapport PDF
rmarkdown::render(
  input         = "rapport/rapport_final.Rmd",
  output_format = "pdf_document",
  output_dir    = "rapport"
)
```

> **Note :** Le fichier `mon_projet.Rproj` doit être ouvert dans RStudio avant toute exécution. Il ancre la racine du projet et garantit que tous les chemins fonctionnent correctement.

---

## Source des données

| Champ | Détail |
|-------|--------|
| **Enquête** | Nigeria General Household Survey (GHS) Panel |
| **Vague** | Vague 4 — 2018-2019 |
| **Producteurs** | Bureau National des Statistiques du Nigeria (NBS) & Banque Mondiale |
| **Accès** | [World Bank Microdata Library](https://microdata.worldbank.org) |
| **Observations initiales** | 30 337 individus, 59 variables |
| **Échantillon analytique** | 25 767 individus (membres présents uniquement), 4 978 ménages |

---

## Principaux résultats

- **Âge médian : 18 ans** — population très jeune à structure expansive
- **CV = 81 %** — forte hétérogénéité des âges
- **55.9 %** des membres sont des enfants du chef de ménage
- Les ménages ruraux comptent en moyenne **1 membre de plus** que les ménages urbains (7.5 vs 6.4)
- La répartition par sexe est **équilibrée** dans les deux zones (p = 0.4)

---


*ENSAE Dakar — Projet statistique sous R — Année académique 2025-2026*