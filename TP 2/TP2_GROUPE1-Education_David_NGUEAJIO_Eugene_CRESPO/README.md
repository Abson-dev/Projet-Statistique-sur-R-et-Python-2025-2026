# TP2 — Éducation et Alphabétisation des Membres des Ménages

**Module GHS** : sect2a / sect2b (W1-W2) · sect2 (W3-W4)  
**Thème** : Capital humain  
**Binôme** : David NGUEAJIO · Yemeli Crespo

---

## Objectif

Analyser les niveaux d'instruction et les taux d'alphabétisation des membres des ménages du Nigeria,
et comparer les profils entre sexes, tranches d'âge et zones géographiques sur les 4 vagues (2010-2018).

---

## Fichiers de ce TP

| Fichier | Description |
|---------|-------------|
| `00_build_data.R` | Charge, nettoie et harmonise les fichiers .dta des 4 vagues. Exporte les bases de travail dans `../data/processed/` |
| `01_analyse_education.Rmd` | Rapport d'analyse complet : tâches 7 à 12 |
| `outputs/` | Graphiques (PNG) et tableaux (HTML/CSV) exportés automatiquement |

---

## Données mobilisées

| Fichier .dta | Contenu | Vagues |
|---|---|---|
| `sect2a_harvestw1/w2` | Individus **ayant quitté l'école** — niveau atteint (`s2aq9`), alphabétisation (`s2aq5`) | W1 W2 |
| `sect2b_harvestw1/w2` | Individus **actuellement scolarisés** — niveau en cours (`s2bq3`) | W1 W2 |
| `sect2_harvestw3` | Module unifié — `s2aq9`, `s2aq13` (scolarisé 2015-2016) | W3 |
| `sect2_harvestw4` | Module unifié — `s2aq9`, `s2aq13`/`s2aq13a` | W4 |
| `sect1_harvestwX` | Sexe (`s1q2`), âge (`s1q4`), lien parenté (`s1q1`) | W1 W2 W3 W4 |
| `secta_harvestwX` | Zone rural/urbain, état, LGA | W1 W2 W3 W4 |

---

## Tâches couvertes

| # | Tâche |
|---|-------|
| 7 | Chargement et jointure sect2b_w4 + sect1_w4 ; valeurs manquantes |
| 8 | Variable ordonnée `niveau_educ` (5 catégories) ; barplot horizontal |
| 9 | Distribution éducation hommes/femmes (18+) ; chi-deux ; V de Cramér |
| 10 | Âge × niveau éducation ; boxplots par groupe d'âge ; Kruskal-Wallis + Dunn |
| 11 | Taux scolarisation 6-17 ans rural/urbain ; chi-deux ; barres groupées + IC |
| 12 | Heatmap État × niveau éducation : part d'adultes sans instruction |

---

## Instructions d'exécution

```r
# Étape 1 : construire les bases de travail
source("00_build_data.R")

# Étape 2 : knit le rapport
rmarkdown::render("01_analyse_education.Rmd", output_dir = "outputs/")
```

**Important** : adapter le chemin `DATA_RAW` dans `00_build_data.R` selon votre arborescence locale.

---

## Outputs attendus

- `outputs/fig_barplot_niveau_educ.png`
- `outputs/fig_barplot_sexe_educ.png`
- `outputs/fig_boxplot_age_educ.png`
- `outputs/fig_barplot_scolarisation_zone.png`
- `outputs/fig_heatmap_etat_educ.png`
- `outputs/tableau_contingence_sexe_educ.html`
- `outputs/rapport_TP2_education.html`
