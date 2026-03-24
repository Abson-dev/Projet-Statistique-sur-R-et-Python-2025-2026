# TP3 -- Accès aux soins et dépenses de santé des ménages nigérians

## Informations générales

| | |
|---|---|
| **Cours** | Projet Statistique sous R et Python 2025-2026 |
| **Établissement** | ENSAE Pierre Ndiaye, Dakar |
| **Programme** | ISE1-CL |
| **Enseignant** | Aboubacar HEMA |
| **Auteurs** | David Landry AGNANGMA SANAM & Cheikh THIOUB |

## Structure du projet

```
TP3/
├── data/
│   ├── sect4a_harvestw4.dta    <- Section santé (morbidité, soins, dépenses)
│   ├── sect1_harvestw4.dta     <- Composition ménage (sexe, âge)
│   ├── secta_harvestw4.dta     <- Pondération wt_wave4
│   └── totcons_final.dta       <- Agrégats de consommation (quintiles)
├── scripts/
│   ├── 00_fonctions.R
│   ├── 01_import.R
│   ├── 02_nettoyage.R
│   └── 03_analyse.R
├── outputs/                    <- Livrables générés par les scripts
├── docs/
│   ├── TP3_Rapport.Rmd
│   ├── reference_style.docx    <- Modèle de mise en forme Word
│   └── page_de_garde.pdf       <- Page de garde (à fournir)
├── main.R
└── README.md
```

## Exécution

```r
# 1. Générer les outputs (graphiques, tableaux)
setwd("chemin/vers/TP3")
source("main.R")

# 2. Compiler le rapport Word
rmarkdown::render("docs/TP3_Rapport.Rmd", output_dir = "docs")
```
