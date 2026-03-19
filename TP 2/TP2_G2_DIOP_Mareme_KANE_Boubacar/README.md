# TP2 — Éducation et alphabétisation des membres des ménages nigérians

**ENSAE ISE 1 | Projet Statistique sous R | 2025-2026**  
**Données : Nigeria General Household Survey (GHS) Panel — Wave 4 (2018)**  
**Auteurs : DIOP Marème, KANE Boubacar**

---

## Objectif

Analyser les niveaux d'instruction et les taux d'alphabétisation des membres des ménages nigérians, et comparer les profils entre sexes, tranches d'âge et zones géographiques.

---

## Structure du projet

```
TP2/
│
├── TP2.Rproj                    # Projet RStudio
├── TP2_rapport_final.Rmd        # Script R Markdown (source)
├── TP2_rapport.pdf              # Rapport final compilé
│
├── data/
│   ├── sect2_harvestw4.dta      # Section éducation individuelle
│   └── sect1_harvestw4.dta      # Section démographique
│
└── outputs/
    ├── fig1_barplot_niveau_educ.png
    ├── fig2_barplot_sexe_educ.png
    ├── fig3_boxplot_age_educ.png
    ├── fig4_barplot_scolarisation.png
    ├── fig5_heatmap_analphabetisme.png
    └── tableaux_TP2.xlsx
```

---

## Données mobilisées

| Fichier               | Contenu                                          | Tâches           |
| --------------------- | ------------------------------------------------ | ---------------- |
| `sect2_harvestw4.dta` | Niveau éducation, scolarisation, alphabétisation | 8, 9, 10, 11, 12 |
| `sect1_harvestw4.dta` | Sexe, âge des membres                            | 9, 10, 11, 12    |

> **Note sur la Wave 4** : contrairement aux Waves 1 et 2 où les modules éducation étaient scindés en `sect2a` et `sect2b`, la Wave 4 fusionne tout en un seul fichier `sect2_harvestw4`.

> Les fichiers `.dta` sont au format Stata et sont lus avec le package `haven`. Données accessibles via le programme **LSMS-ISA de la Banque Mondiale**.

---

## Construction de la variable clé

La variable `niveau_educ` (5 catégories ordonnées) a été construite en **3 passes** :

1. Individus jamais scolarisés (`s2aq6 = 2`) → **"Aucun"**
2. Niveau reconstruit par `coalesce(s2aq9, s2aq13b, s2aq15)` pour maximiser la couverture
3. Filet via `s2aq10` (diplôme obtenu) pour les cas résiduels

| Niveau           | Codes sources                   |
| ---------------- | ------------------------------- |
| Aucun            | `s2aq6 = 2` ou `niveau_raw = 0` |
| Primaire         | codes 11–16                     |
| Junior Secondary | codes 21–23                     |
| Senior Secondary | codes 24–28, 321, 322           |
| Tertiaire        | codes 31–43, 51–61, 411–424     |

---

## Tâches réalisées

| #   | Tâche                                     | Méthode                                               |
| --- | ----------------------------------------- | ----------------------------------------------------- |
| 8   | Distribution du niveau d'éducation        | Barplot horizontal + tableau gt                       |
| 9   | Niveau d'éducation par sexe (adultes 18+) | Barplot 100% empilé + Chi-deux + V de Cramér          |
| 10  | Relation âge et niveau d'éducation        | Boxplot + Kruskal-Wallis + post-hoc Dunn (Bonferroni) |
| 11  | Scolarisation 6-17 ans rural vs urbain    | Barplot + IC 95% + Chi-deux + V de Cramér             |
| 12  | Analphabétisme par État nigérian          | Heatmap colorée par quintile                          |

---

## Livrables générés

- **`TP2_rapport.pdf`** — Rapport complet de 8-10 pages incluant :
  - Barplot du niveau d'éducation par sexe (barres 100% empilées)
  - Heatmap des taux d'analphabétisme par État
  - Tableau de contingence commenté avec chi-deux et V de Cramér

- **`outputs/tableaux_TP2.xlsx`** — 4 feuilles :
  - `Niveau_education` : distribution globale
  - `Education_par_sexe` : répartition par sexe
  - `Scolarisation_milieu` : taux par milieu rural/urbain
  - `Analphabetisme_etat` : taux par État classés par ordre décroissant

- **`outputs/fig*.png`** — 5 graphiques exportés en haute résolution (300 dpi)

---

## Packages R utilisés

```r
haven, dplyr, ggplot2, rstatix, ggpubr, scales,
patchwork, gtsummary, gt, naniar, labelled,
viridis, forcats, openxlsx
```

---

## Reproduire le rapport

1. Ouvrir `TP2.Rproj` dans RStudio
2. Placer les fichiers `.dta` dans le dossier `data/`
3. Ouvrir `TP2_rapport_final.Rmd`
4. Cliquer sur **Knit** → le PDF et les outputs sont générés automatiquement

> Prérequis : avoir `tinytex` installé (`tinytex::install_tinytex()`)

---

## Source des données

World Bank LSMS-ISA — Nigeria General Household Survey Panel  
[https://microdata.worldbank.org/index.php/catalog/lsms](https://microdata.worldbank.org/index.php/catalog/lsms)
