# TP5 — Analyse des cultures, intrants et rendements agricoles
## Vague 4 (2018/19) uniquement

---

## Équipe

- **David Landry AGNANGMA SANAM**
- **Hamadou DICKO**

Étudiants en ISE1 à l'ENSAE de Dakar

**Encadrant :** M. Aboubacar HEMA, Data Scientist

**Année académique : 2025 - 2026**

---

## 1. Présentation du projet

Ce travail porte sur l'analyse de la production agricole des ménages nigérians à partir de la **Vague 4 (2018/19)** du GHS-Panel. Les estimations reposent sur les **pondérations transversales `wt_wave4`**.

**Questions traitées (Q25 à Q29 — Q30 exclue) :**

| N° | Question | Source | Remarque |
|:---|:---|:---|:---|
| Q25 | 15 cultures les plus répandues | secta3i W4 | ✓ |
| Q26 | Diversification culturale — histogramme + violin + Wilcoxon | secta3i W4 | ✓ |
| Q27 | Taux d'utilisation des intrants par catégorie et zone | secta11c2 W4 | ✓ |
| Q28 | Rendement maïs/millet par État — boxplots + Kruskal-Wallis | secta3i + secta1 W4 | **kg/ha** |
| Q29 | Effet de l'engrais inorganique — violin + Wilcoxon + r | secta3i + secta1 + secta11c2 | **kg/ha** |

---

## 2. Données utilisées

| Fichier `.dta` | Contenu | Provenance | Vague |
|:---|:---|:---|:---|
| `secta3i_harvestw4.dta` | Récoltes par parcelle — code culture, quantités, facteur de conversion | GitHub | W4 |
| `secta1_harvestw4.dta` | **Superficies des parcelles** — mesure GPS (m²) et auto-déclarée (m²) | GitHub | W4 |
| `secta11c2_harvestw4.dta` | Intrants par parcelle — engrais (NPK, Urée), organique, pesticide, herbicide | GitHub | W4 |
| `secta_harvestw4.dta` | Pondérations wt_wave4, zone géopolitique, État, milieu | GitHub | W4 |

> **Téléchargement automatique depuis :**
> ```
> https://github.com/Herman-YAMAHA/NYHP/tree/main/TP4_raw
> ```

---

## 3. Calcul du rendement en kg/ha (Q28 et Q29)

Le rendement est exprimé en **kg/ha** grâce aux superficies du fichier `secta1_harvestw4.dta` :

- **Priorité GPS** : `prefilled_gps_area` (m², mesure GPS de la visite précédente)
- **Sinon auto-déclaré** : `sa1q11` (m², déclaration de l'enquêté)
- **Conversion** : m² ÷ 10 000 = hectares

Les variables clés de `secta1_harvestw4.dta` :

| Variable | Description |
|:---|:---|
| `prefilled_gps_area` | Superficie GPS en m² (visite précédente) |
| `sa1q9` | La parcelle a-t-elle été mesurée par GPS ? (1=Oui, 2=Non) |
| `sa1q11` | Superficie auto-déclarée en m² |

---

## 4. Arborescence du projet

```
Groupe2_AGNANGMA_David_DICKO_Hamadou_TP5/
│
├── main.R                              <- Script principal
├── Groupe2_AGNANGMA_David_DICKO_Hamadou_TP5.Rproj
│
├── scripts/
│   ├── 01_chargement_preparation.R     <- Acquisition, lecture, recodages, superficie
│   └── 02_statistiques_figures.R       <- Calculs, tests, graphiques (kg/ha)
│
├── rapport/
│   └── Rapport_TP5_agriculture.Rmd     <- Rapport → PDF
│
├── data/
│   ├── raw/                            <- Fichiers .dta (téléchargés auto.)
│   └── processed/                      <- Objets .rds intermédiaires
│
├── outputs/                            <- Graphiques PNG + tableau Excel
│   ├── graph01_classement_cultures.png
│   ├── graph02_diversification.png
│   ├── graph03_intrants.png
│   ├── graph04_engrais_zone.png
│   ├── graph05_rendement_etats.png     <- kg/ha !
│   ├── graph06_rendement_engrais.png   <- kg/ha !
│   └── synthese_intrants.xlsx
│
└── README.md
```

---

## 5. Lancement

```r
# Ouvrir le projet .Rproj dans RStudio, puis :
source("main.R")
```

Le script `main.R` enchaîne automatiquement :
1. Installation des packages manquants
2. Téléchargement des données depuis GitHub
3. Chargement, préparation et calcul des superficies (script 01)
4. Analyses statistiques et graphiques en kg/ha (script 02)
5. Compilation du rapport PDF

---

## 6. Packages mobilisés

| Package | Rôle |
|:---|:---|
| `haven` | Importation des fichiers .dta |
| `dplyr`, `tidyr` | Transformation des données |
| `ggplot2`, `patchwork` | Visualisation |
| `survey`, `srvyr` | Estimations pondérées |
| `rstatix` | Tests non paramétriques |
| `openxlsx` | Écriture de fichiers Excel |
| `knitr`, `rmarkdown`, `kableExtra` | Génération du rapport PDF |

---

*Travail réalisé dans le cadre du cours de Projet Statistique sous R et Python — ENSAE ISE 1, 2025-2026.*
