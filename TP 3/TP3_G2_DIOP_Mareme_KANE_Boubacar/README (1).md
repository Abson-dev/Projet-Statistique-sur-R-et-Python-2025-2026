# TP3 — Accès aux services de santé des ménages nigérians

**ENSAE ISE 1 | Projet Statistique sous R | 2025-2026**  
**Données : Nigeria General Household Survey (GHS) Panel — Wave 4 (2018)**  
**Groupe 2: DIOP MAREME, KANE BOUBACAR**

---

## Objectif

Décrire les épisodes de maladie, les types de soins consultés et les coûts de santé supportés par les ménages nigérians, en analysant les disparités selon le sexe, l'âge, le milieu de résidence et le niveau de richesse.

---

## Structure du projet

```
TP3/
│
├── TP3.Rproj                  # Projet RStudio
├── TP3_script_rapport.Rmd     # Script R Markdown (source)
├── TP3_rapport.pdf            # Rapport final compilé  LIVRABLE
└── data/
    ├── sect4a_harvestw4.dta   # Section santé individuelle
    ├── sect1_harvestw4.dta    # Section démographique
    └── totcons_final.dta      # Agrégats de consommation
```

---

## Données mobilisées

| Fichier                | Contenu                              | Tâches                 |
| ---------------------- | ------------------------------------ | ---------------------- |
| `sect4a_harvestw4.dta` | Maladie, recours aux soins, dépenses | 13, 14, 15, 16, 17, 18 |
| `sect1_harvestw4.dta`  | Sexe, âge des membres                | 13                     |
| `totcons_final.dta`    | Consommation par équivalent adulte   | 17                     |

> **Note** : Les fichiers `.dta` sont au format Stata et sont lus avec le package `haven`.  
> Les données sont accessibles via le programme **LSMS-ISA de la Banque Mondiale**.

---

## Tâches réalisées

| #   | Tâche                                      | Méthode                                      |
| --- | ------------------------------------------ | -------------------------------------------- |
| 13  | Taux de morbidité par sexe et groupe d'âge | Barplot + IC 95% (binom.test)                |
| 14  | Types de maladies les plus fréquentes      | Barplot horizontal coloré par catégorie      |
| 15  | Recours aux soins par type de prestataire  | Barplot ordonné                              |
| 16  | Distribution des dépenses de santé         | Histogramme log + statistiques par décile    |
| 17  | Recours aux soins × quintile de richesse   | Chi-deux + V de Cramér + barplot 100% empilé |
| 18  | Dépenses médianes rural vs urbain          | Wilcoxon + violin plot                       |

---

## Livrables générés

- **`TP3_rapport.pdf`** — Rapport complet de 8-10 pages incluant :
  - Barplot des types de maladies et des prestataires consultés
  - Violin plot des dépenses de santé par milieu de résidence
  - Tableau de contingence recours aux soins × quintile de richesse
  - Tableaux statistiques descriptifs (`gt`)

---

## Packages R utilisés

```r
haven, dplyr, ggplot2, rstatix, ggpubr, scales,
patchwork, gtsummary, gt, naniar, labelled
```

---

## Reproduire le rapport

1. Ouvrir `TP3.Rproj` dans RStudio
2. Placer les fichiers `.dta` dans le dossier `data/`
3. Ouvrir `TP3_script_rapport.Rmd`
4. Cliquer sur **Knit** → le PDF est généré automatiquement

> Prérequis : avoir `tinytex` installé (`tinytex::install_tinytex()`)

---

## Source des données

World Bank LSMS-ISA — Nigeria General Household Survey Panel  
[https://microdata.worldbank.org/index.php/catalog/lsms](https://microdata.worldbank.org/index.php/catalog/lsms)
