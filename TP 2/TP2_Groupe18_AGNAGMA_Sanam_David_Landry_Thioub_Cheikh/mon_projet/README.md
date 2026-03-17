# Analyse 2 — Éducation et Alphabétisation des Membres des Ménages

## Nigeria GHS Panel W4 (2018) | ENSAE Pierre Ndiaye — ISE 1 — 2025-2026

---

## Contenu du projet

```
Analyse2_Education_Nigeria/
├── main.R                          ← Point d'entrée — exécuter ce fichier
├── README.md                       ← Ce fichier
├── data/
│   ├── raw/                        ← ⚠ Déposer ici les .dta bruts
│   │   ├── sect1_harvestw4.dta
│   │   ├── sect2_harvestw4.dta
│   │   └── secta_harvestw4.dta
│   └── processed/                  ← Générés automatiquement
├── scripts/
│   ├── 00_packages.R               ← Installation + thème ggplot
│   ├── 01_import.R                 ← Tâche 7 : Chargement & jointure
│   ├── 02_construction.R           ← Construction de niveau_educ
│   ├── 03_tache8.R                 ← Distribution nationale
│   ├── 04_tache9.R                 ← Éducation par sexe
│   ├── 05_tache10.R                ← Éducation par âge
│   ├── 06_tache11.R                ← Scolarisation rurale/urbaine
│   └── 07_tache12.R                ← Heatmap État × Niveau
├── rapport/
│   └── rapport_education.Rmd       ← Rapport complet (8-10 pages)
└── outputs/                        ← Graphiques exportés (générés)
```

---

## Instructions de reproduction

### 1. Préparer les données

Placer les fichiers `.dta` dans `data/raw/` :
- `sect1_harvestw4.dta` (caractéristiques individuelles : sexe, âge)
- `sect2_harvestw4.dta` (éducation : statut scolaire + niveau atteint)
- `secta_harvestw4.dta` (niveau ménage : zone géographique)

Source : [World Bank Microdata Library – Nigeria GHS](https://microdata.worldbank.org/index.php/catalog/1002)

### 2. Exécuter l'analyse complète

```r
# Dans RStudio, ouvrir le projet puis :
source("main.R")
```

Ou exécuter les scripts individuellement dans l'ordre numérique.

### 3. Compiler le rapport

```r
rmarkdown::render(
  "rapport/rapport_education.Rmd",
  output_format = "pdf_document",
  output_file   = "outputs/Rapport_Education_Nigeria_GHS_W4.pdf"
)
```

Pour le format HTML :
```r
rmarkdown::render("rapport/rapport_education.Rmd", output_format = "html_document")
```

---

## Tâches couvertes

| # | Tâche | Script | Description |
|---|-------|--------|-------------|
| 7 | Chargement & jointure | `01_import.R` | sect2 × sect1, inspection valeurs manquantes |
| 8 | Distribution nationale | `03_tache8.R` | Barplot horizontal + courbe cumulative |
| 9 | Éducation par sexe | `04_tache9.R` | Barres 100% empilées + chi-deux + V de Cramér |
| 10 | Éducation par âge | `05_tache10.R` | Violin plot + Kruskal-Wallis + Dunn |
| 11 | Scolarisation 6-17 ans | `06_tache11.R` | Barres groupées + IC 95% (Clopper-Pearson) + RR |
| 12 | Heatmap État × Niveau | `07_tache12.R` | Heatmap viridis + strip chart quintiles |

---

## Apports méthodologiques par rapport au Groupe 4

| Aspect | Groupe 4 | Ce projet |
|--------|----------|-----------|
| Algorithme `niveau_educ` | 1 passe (case_when direct) | 3 passes avec priorité explicite |
| Valeurs manquantes | Tableau simple | Tableau + % + interprétation structurelle |
| IC distributions | Non calculés | IC Wilson (T8), Clopper-Pearson (T11) |
| Taille d'effet | V de Cramér | V de Cramér + η² (Kruskal-Wallis) |
| Violin/boxplot | Boxplot seul | Violin + boxplot superposés |
| Tâche 11 | Zone seule | Zone + Zone×Sexe + Risque Relatif |
| Tâche 12 | Heatmap colonne unique | Heatmap croisée + strip chart quintiles |
| Rapport | ~6-8 sections | 8-10 pages avec limites & recommandations |

---

## Packages requis

```r
c("haven","dplyr","tidyr","forcats","ggplot2","scales",
  "rstatix","ggpubr","gtsummary","viridis","patchwork",
  "knitr","kableExtra")
```

> Tous installés automatiquement par `scripts/00_packages.R`

---

## Référence

**World Bank / NBS Nigeria** (2018). *General Household Survey, Panel (GHS-Panel), Wave 4, 2018-2019.*  
Données disponibles sur : https://microdata.worldbank.org/index.php/catalog/3557
