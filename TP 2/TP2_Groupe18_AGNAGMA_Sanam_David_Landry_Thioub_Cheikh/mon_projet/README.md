# Analyse 2 — Éducation et Alphabétisation des Membres des Ménages

## Nigeria GHS Panel W4 (2018) | ENSAE Pierre Ndiaye — ISE 1 — 2025-2026

---

## Contenu du projet

```
mon_projet/

├── README.md                       ← Ce fichier
├── data/
│   ├── raw/                        ← ⚠ Déposer ici les .dta bruts
│   │   ├── sect1_harvestw4.dta
│   │   ├── sect2_harvestw4.dta
│   │   └── secta_harvestw4.dta
│   └── processed/                  ← Générés automatiquement
├── scripts/
│   ├── script_TP2              ← Installation + thème ggplot
**
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
source("TP2.R")
```


### 3. Compiler le rapport

```r
rmarkdown::render(
  "rapport/rapport_education.Rmd",
  output_format = "pdf_document",
  output_file   = "rapport/rapport_education.pdf"
)
```

Pour le format HTML :
```r
rmarkdown::render("rapport/rapport_education.Rmd", output_format = "html_document")
```

---

## Tâches couvertes

| # | Tâche | Description |
|---|-------|-----------|
| 7 | Chargement & jointure |   sect2 × sect1, inspection valeurs manquantes |
| 8 | Distribution nationale |  Barplot horizontal + courbe cumulative |
| 9 | Éducation par sexe |  Barres 100% empilées + chi-deux + V de Cramér |
| 10 | Éducation par âge  | Violin plot + Kruskal-Wallis + Dunn |
| 11 | Scolarisation 6-17 ans |  Barres groupées + IC 95% (Clopper-Pearson) + RR |
| 12 | Heatmap État × Niveau |  Heatmap viridis + strip chart quintiles |

---

 
Un  Rapport a ete produit avec r markdown

---

## Packages requis

```r
c("haven","dplyr","tidyr","forcats","ggplot2","scales",
  "rstatix","ggpubr","gtsummary","viridis","patchwork",
  "knitr","kableExtra")
```



---

## Référence

**World Bank / NBS Nigeria** (2018). *General Household Survey, Panel (GHS-Panel), Wave 4, 2018-2019.*  
Données disponibles sur : https://microdata.worldbank.org/index.php/catalog/3557
