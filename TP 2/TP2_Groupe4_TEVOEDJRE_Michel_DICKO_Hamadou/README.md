# TP2 -- Projet Statistique sous R et Python

Groupe 4 | ENSAE Pierre Ndiaye, ISE1, 2025-2026  
Auteurs : TEVOEDJRE Michel, DICKO Hamadou  
Encadrant : Aboubacar HEMA

---

## Objet du travail

Ce depot contient les scripts, donnees traitees et rapport du second travail pratique du cours de Projet Statistique sous R et Python. L'analyse porte sur les dimensions educatives de l'enquete Nigeria General Household Survey-Panel Wave 4 (NBS, 2018/19), en exploitant les sections 1 (caracteristiques individuelles) et 2 (education) ainsi que la section A (menages).

Les objectifs sont : construire une variable de niveau d'education en cinq categories, analyser la distribution nationale, etudier les disparites selon le sexe (Chi-deux, V de Cramer), selon le groupe d'age (Kruskal-Wallis, post-hoc de Dunn), estimer les taux de scolarisation des enfants de 6-17 ans par zone, et produire des heatmaps des profils educatifs par Etat nigerien.

---

## Structure du depot

```
.
├── data/
│   ├── raw/
│   │   ├── sect1_harvestw4.dta       # Caracteristiques individuelles (sexe, age)
│   │   ├── sect2_harvestw4.dta       # Education (statut scolaire, niveau atteint)
│   │   └── secta_harvestw4.dta       # Menages (zone, ponderations)
│   └── processed/
│       ├── df_educ.rds               # Base complete apres jointure et recodage
│       ├── df_adultes.rds            # Sous-ensemble adultes 18+
│       ├── df_scol.rds               # Sous-ensemble enfants 6-17 ans (scolarisation)
│       ├── df_heatmap.rds            # Agregats par Etat (% sans instruction)
│       ├── resultats_tache9.rds      # Chi-deux et V de Cramer (sexe x niveau)
│       ├── resultats_tache10.rds     # Kruskal-Wallis et post-hoc de Dunn (age x niveau)
│       └── resultats_tache11.rds     # Chi-deux et taux de scolarisation (zone x scol)
├── scripts/
│   ├── 01_chargement_donnees.R       # Chargement, jointure, recodage, sauvegarde
│   └── 02_analyses_visualisations.R  # Analyses statistiques et visualisations
├── outputs/
│   ├── fig01_barplot_niveau_educ_global.png   # Distribution nationale du niveau d'education
│   ├── fig02_barplot100_niveau_sexe.png        # Barres 100% empilees par sexe
│   ├── fig03_boxplot_niveau_age.png            # Boxplot niveau x groupe d'age
│   ├── fig04_scolarisation_zone.png            # Taux de scolarisation 6-17 ans par zone
│   ├── fig05_heatmap_analphabetisme_etat.png   # Heatmap % sans instruction par Etat
│   └── fig06_heatmap_etat_niveau.png           # Heatmap croisee Etat x niveau d'education
├── rapport/
│   ├── TP2_Groupe4_TEVOEDJRE_Michel_DICKO_Hamadou.Rmd                 # Rapport principal (R Markdown)
│   ├── TP2_Groupe4_TEVOEDJRE_Michel_DICKO_Hamadou.pdf                 # Rapport principal (pdf)
│   └── logos/
│       ├── ENSAE.PNG
│       ├── ANSD.png
│       └── SN.PNG
├── main.R                            # Script principal (orchestre tout le pipeline)
└── README.md
```

---

## Reproductibilite

Pour reproduire l'integralite du travail, ouvrir le projet RStudio (`*.Rproj`) puis executer :

```r
source("main.R")
```

Le script `main.R` installe les packages manquants, telecharge les trois fichiers de donnees depuis GitHub si absents, puis enchaine les deux scripts d'analyse. Les figures et resultats sont enregistres dans `outputs/` et `data/processed/`.

Pour compiler le rapport, ouvrir `rapport/rapport.Rmd` dans RStudio et cliquer sur **Knit**, ou executer :

```r
rmarkdown::render("rapport/rapport.Rmd")
```

---

## Packages R utilises

| Package     | Usage principal                                           |
|-------------|----------------------------------------------------------|
| haven       | Lecture des fichiers Stata (.dta)                        |
| dplyr       | Manipulation et transformation des donnees               |
| forcats     | Gestion des variables categorielles                      |
| ggplot2     | Visualisations graphiques                                |
| rstatix     | Tests statistiques (Kruskal-Wallis, post-hoc de Dunn)    |
| ggpubr      | Ajout d'annotations statistiques sur les graphiques      |
| gtsummary   | Tableaux de statistiques descriptives                    |
| viridis     | Palettes de couleurs pour heatmaps                       |
| patchwork   | Assemblage de plusieurs graphiques ggplot2               |
| scales      | Formatage des axes et etiquettes                         |
| knitr / kableExtra | Tableaux dans le rapport R Markdown               |

---

## Principaux resultats

- La majorite des adultes nigerians (18+) n'ont atteint aucun niveau d'instruction formel complete ; les niveaux secondaire et tertiaire restent tres minoritaires.
- Les disparites de genre sont statistiquement significatives (Chi-deux, p < 0,001 ; V de Cramer ~ 0,08) : les femmes sont surrepresentees parmi les adultes sans instruction.
- L'effet generationnel est net (Kruskal-Wallis, p < 0,001) : les jeunes adultes de 18-30 ans sont nettement mieux instruits que les generations anterieures.
- Le taux de scolarisation des enfants de 6-17 ans est significativement plus eleve en zone urbaine (~90%) qu'en zone rurale (~73%), ecart confirme par un test du Chi-deux (p < 0,001).
- Les heatmaps revelent une fracture Nord-Sud tres marquee : les Etats du Nord-Ouest (Zamfara, Kebbi, Katsina, Sokoto, Yobe) concentrent les taux d'analphabetisme les plus eleves.

---

## Source des donnees

National Bureau of Statistics (NBS) Nigeria -- General Household Survey Panel, Wave 4, 2018/2019.  
Sections exploitees : sect1 (caracteristiques individuelles), sect2 (education), secta (menages).
