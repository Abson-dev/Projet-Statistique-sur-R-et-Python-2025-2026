# TP3 — Accès aux Services de Santé et Chocs Sanitaires

**Module GHS** : sect4a / cons_agg (W1–W4)  
**Thème** : Santé  
**Binôme** : David NGUEAJIO · Yemeli Crespo

---

## Objectif

Décrire les épisodes de maladie, les types de soins consultés et les coûts de santé
supportés par les ménages nigérians, et analyser les disparités par sexe, âge, milieu
de résidence et niveau de richesse.

---

## Note importante sur les fichiers

> Le module **santé** dans le GHS est `sect4a_harvestwX` (et **non** sect3 qui contient
> l'emploi/travail). Les variables clés sont :
> - `s4aq3` — maladie/blessure dans les 4 dernières semaines  
> - `s4aq3b` / `s4aq3b_1` — type d'affection  
> - `s4aq6a` — type de prestataire consulté  
> - `s4aq9` — montant payé pour la première consultation  
> - `s4aq1` — consultation d'un praticien de santé

---

## Fichiers de ce TP

| Fichier | Description |
|---------|-------------|
| `00_build_data.R` | Construction des bases santé harmonisées (W1–W4) |
| `01_analyse_sante.Rmd` | Rapport d'analyse complet : tâches 13 à 18 |
| `outputs/` | Graphiques et tableaux exportés |

---

## Données mobilisées

| Fichier .dta | Contenu | Vagues |
|---|---|---|
| `sect4a_harvestw1` | Morbidité, recours aux soins, dépenses (W1) | W1 |
| `sect4a_harvestw2` | Idem W2 | W2 |
| `sect4a_harvestw3` | Idem W3 | W3 |
| `sect4a_harvestw4` | Idem W4 | W4 |
| `sect1_harvestwX` | Sexe, âge, lien parenté | W1 W2 W3 W4 |
| `secta_harvestwX` | Zone rural/urbain, état | W1 W2 W3 W4 |
| `cons_agg_wave4_visit2` | Quintile de consommation (W4) | W4 |

---

## Tâches couvertes

| # | Tâche |
|---|-------|
| 13 | Taux de morbidité par sexe et groupe d'âge ; barplots + IC à 95% |
| 14 | Types de maladies déclarées ; 10 affections les plus fréquentes |
| 15 | Recours aux soins par type de prestataire ; barplot ordonné |
| 16 | Distribution des dépenses de santé ; log, déciles, outliers |
| 17 | Recours aux soins × quintile de consommation ; chi-deux, Fisher, V de Cramér |
| 18 | Dépenses médianes rural/urbain ; Wilcoxon ; violin + boxplot |

---

## Instructions d'exécution

```r
source("00_build_data.R")
rmarkdown::render("01_analyse_sante.Rmd", output_dir = "outputs/")
```

---

## Outputs attendus

- `outputs/fig_morbidite_sexe_age.png`
- `outputs/fig_types_maladies.png`
- `outputs/fig_prestataires.png`
- `outputs/fig_depenses_distribution.png`
- `outputs/fig_recours_quintile.png`
- `outputs/fig_violin_depenses_zone.png`
- `outputs/rapport_TP3_sante.html`
