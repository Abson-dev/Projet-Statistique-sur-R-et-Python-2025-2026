# TP3 sur la santé : Accès aux soins et dépenses de santé des ménages nigérians

Analyse descriptive des caractéristiques sanitaires des ménages à partir du **Nigeria General Household Survey Panel Wave 4 (2018-2019)**, collecté par le National Bureau of Statistics (NBS) en collaboration avec la Banque Mondiale (programme LSMS-ISA).

**Auteurs** : Keita L. & Nkwa Tsamo L.

**Cours** : Projet Statistique avec R et Python par M. HEMA, research scientist at IFPRI
---

## Objectifs de l'analyse

1. Décrire le **taux de morbidité déclarée** par sexe et par groupe d'âge (IC 95% Clopper-Pearson)
2. Identifier les **10 affections les plus fréquentes** parmi les membres malades
3. Analyser le **recours aux soins selon le type de prestataire** (public, privé, autre)
4. Étudier la **distribution des dépenses en médicaments** et leur variation par prestataire
5. Examiner le **lien entre quintile de consommation et recours aux soins** (Chi-deux, V de Cramér)
6. Comparer les **dépenses de santé totales** entre milieux rural et urbain (test de Wilcoxon)

---

## Structure du projet

```
TP3/
|-- main.R                        # Script principal (lance tout le pipeline)
|-- scripts/
|   |-- 01_nettoyage.R            # Import, exploration, nettoyage, variables dérivées
|   |-- 02_analyses.R             # Analyses statistiques et visualisations
|-- data/
|   |-- raw/                      # Données brutes .dta (NON VERSIONNÉES, voir ci-dessous)
|   |-- processed/                # Données nettoyées .rds (générées par 01_nettoyage.R)
|-- outputs/
|   |-- figures/                  # Graphiques PNG (300 dpi)
|   |   |-- fig01a_morbidite_sexe.png
|   |   |-- fig01b_morbidite_age.png
|   |   |-- fig02_types_maladies.png
|   |   |-- fig03_recours_soins_prestataire.png
|   |   |-- fig04a_depenses_distribution.png
|   |   |-- fig04b_depenses_prestataire.png
|   |   |-- fig05_recours_quintile.png
|   |   |-- fig06_depenses_milieu.png
|   |-- tables/                   # Tableaux exportés
|       |-- tab01_gtsummary_sante.html
|       |-- tab01_gtsummary_sante.csv
|-- docs/
|   |-- reference.docx            # Fichier de styles Word pour Quarto 
|-- rapport_tp3.qmd               # Rapport académique Quarto (source)
|-- rapport_tp3.pdf               # Rapport compilé (PDF)
|-- rapport_tp3.docx              # Rapport compilé (Word)
|-- references.bib                # Bibliographie BibTeX (APA 7th)
|-- renv.lock                     # Versions exactes des packages R
|-- .Rprofile                     # Activation automatique de renv
|-- TP3.Rproj                     # Fichier projet RStudio
|-- README.md
```

---

## Données sources

Les données brutes ne sont **pas incluses dans ce dépôt** (taille ~50 Mo, `.gitignore`).

### Téléchargement

1. Accéder au catalogue : <https://microdata.worldbank.org/index.php/catalog/3557>
2. Télécharger les fichiers **Stata format** (version 03, octobre 2021)
3. Extraire et placer dans `data/raw/NGA_2018_GHSP-W4_v03_M_Stata12/` les fichiers suivants :

| Fichier                  | Description                                              | Taille  |
|--------------------------|----------------------------------------------------------|---------|
| `sect4a_harvestw4.dta`   | Module santé (morbidité, recours aux soins, dépenses)    | ~14 Mo  |
| `sect3b_harvestw4.dta`   | Module NHIS (couverture assurance maladie)               | ~1 Mo   |
| `sect1_harvestw4.dta`    | Roster individuel (âge, sexe, statut de résidence)       | ~15 Mo  |
| `secta_harvestw4.dta`    | Fichier Root ménage (géographie, poids)                  | ~273 Ko |
| `totcons_final.dta`      | Agrégat de consommation (quintiles de bien-être)         | ~20 Mo  |

Ces **5 fichiers** sont nécessaires pour le TP3.

---

## Prérequis

- **R** >= 4.4
- **RStudio** >= 2023.12 (recommandé)
- **Quarto** >= 1.3 (pour compiler le rapport)

---

## Installation et exécution

### Étape 1 : Cloner le dépôt

```bash
git clone <url-du-depot>
cd TP3
```

### Étape 2 : Restaurer l'environnement R

Ouvrir `TP3.Rproj` dans RStudio, puis dans la console R :

```r
renv::restore()
```

Cette commande installe automatiquement **toutes les dépendances** avec les versions exactes spécifiées dans `renv.lock`. Aucune installation manuelle de packages n'est nécessaire.

### Étape 3 : Placer les données brutes

Télécharger les 5 fichiers `.dta` (voir section Données sources) et les placer dans :

```
data/raw/NGA_2018_GHSP-W4_v03_M_Stata12/
```

### Étape 4 : Exécuter le pipeline complet

```r
source("main.R")
```

Ce script :
- Vérifie que tous les packages sont installés (sinon, demande `renv::restore()`)
- Charge les 14 packages requis
- Fixe la graine aléatoire (`set.seed(2070)`) pour la reproductibilité
- Exécute `01_nettoyage.R` : import, exploration, nettoyage, construction des variables sanitaires
- Exécute `02_analyses.R` : analyses statistiques, tests, visualisations
- Affiche un résumé final avec les livrables produits et les durées d'exécution

**Durée estimée** : 30-40 secondes.

### Étape 5 : Compiler le rapport (optionnel)

Le rapport compilé (`rapport_tp3.pdf` et `rapport_tp3.docx`) est déjà inclus dans le dépôt. Pour le régénérer :

```r
quarto::quarto_render("rapport_tp3.qmd", output_format = "all")
```

---

## Packages R utilisés

| Package      | Usage                                                        |
|--------------|--------------------------------------------------------------|
| `tidyverse`  | Manipulation et visualisation des données                    |
| `haven`      | Lecture des fichiers Stata (.dta)                            |
| `labelled`   | Gestion des labels Stata                                     |
| `here`       | Chemins relatifs portables                                   |
| `scales`     | Formatage des axes graphiques                                |
| `naniar`     | Diagnostic des valeurs manquantes (vis_miss)                 |
| `moments`    | Asymétrie (skewness) et aplatissement (kurtosis)             |
| `rstatix`    | Tests statistiques et tailles d'effet (Wilcoxon, Chi-deux)   |
| `coin`       | Tests non paramétriques                                      |
| `PropCIs`    | Intervalles de confiance exacts (Clopper-Pearson)            |
| `patchwork`  | Composition de graphiques                                    |
| `gtsummary`  | Tableaux descriptifs stratifiés                              |
| `gt`         | Formatage de tableaux                                        |
| `cards`      | Dépendance de gtsummary                                      |

Toutes les versions sont verrouillées dans `renv.lock`.

> **Note** : `coin` masque certaines fonctions de `rstatix` (`kruskal_test`, `wilcox_test`, etc.).
> Toujours utiliser le préfixe `rstatix::` devant ces fonctions dans les scripts.

---

## Principaux résultats

- **Échantillon** : 25 767 membres résidents actifs ; 21 496 avec données de morbidité renseignées
- **Taux de morbidité global** : 9,3% sur les 4 semaines de référence
- **Morbidité par sexe** : les femmes sont plus touchées (10,2% [9,7% ; 10,8%]) que les hommes (8,4% [7,9% ; 8,9%])
- **Morbidité par âge** : gradient croissant -- 7,4% chez les 0-14 ans vs 23,5% [21,2% ; 25,9%] chez les 60 ans et plus
- **Affections dominantes** : douleurs corporelles (25,5%) et paludisme (25,2%) représentent la moitié des cas déclarés
- **Recours aux soins** : parmi les consultants avec prestataire identifié (n = 3 851), 67,1% se dirigent vers le secteur privé et 30,5% vers le secteur public
- **Inégalités de recours** : taux de consultation croissant avec le niveau de vie -- 12,1% (quintile 1, plus pauvre) vs 26,1% (quintile 5, plus riche) ; Chi-deux p < 0,001, V de Cramér = 0,122
- **Dépenses en médicaments** : médiane de 700 nairas ; les consultations en établissement public coûtent plus cher (médiane ~2 200 nairas) que le privé (médiane ~900 nairas)
- **Dépenses rurales/urbaines** : aucune différence significative (Wilcoxon p = 0,937, r = 0,004) -- médiane de 15 425 nairas (urbain) vs 14 830 nairas (rural)
- **Couverture NHIS** : seulement 2,8% des ménages couverts par l'assurance maladie nationale

---

## Reproductibilité

- **Graine aléatoire** : `set.seed(2070)` (fixée dans `main.R`)
- **Environnement** : `renv.lock` verrouille les versions exactes de tous les packages
- **Pipeline** : un seul appel `source("main.R")` reproduit l'intégralité des résultats en ~30 secondes
- **Rapport** : `rapport_tp3.qmd` avec valeurs dynamiques (inline R, aucun chiffre codé en dur)

---

## Arborescence des sorties

### Figures (8)

| Fichier                               | Contenu                                                       |
|---------------------------------------|---------------------------------------------------------------|
| `fig01a_morbidite_sexe.png`           | Taux de morbidité par sexe (IC 95%)                           |
| `fig01b_morbidite_age.png`            | Taux de morbidité par groupe d'âge (IC 95%)                   |
| `fig02_types_maladies.png`            | Top 10 affections déclarées par catégorie                     |
| `fig03_recours_soins_prestataire.png` | Recours aux soins selon le type de gestionnaire               |
| `fig04a_depenses_distribution.png`    | Distribution des dépenses en médicaments (échelle log)        |
| `fig04b_depenses_prestataire.png`     | Dépenses médicaments par type de prestataire (boxplot log)    |
| `fig05_recours_quintile.png`          | Recours aux soins selon le quintile de consommation           |
| `fig06_depenses_milieu.png`           | Dépenses de santé par milieu de résidence (violin + boxplot)  |

### Tableaux (2)

| Fichier                          | Contenu                                          |
|----------------------------------|--------------------------------------------------|
| `tab01_gtsummary_sante.html`     | Tableau gtsummary interactif (indicateurs clés)  |
| `tab01_gtsummary_sante.csv`      | Tableau gtsummary export plat                    |

---

## Palette de couleurs

Les couleurs suivent les conventions des organisations internationales :

| Couleur         | Code HEX  | Convention    | Usage                              |
|-----------------|-----------|---------------|------------------------------------|
| Turquoise OMS   | `#009688` | OMS           | Couleur principale TP3             |
| Bleu hommes     | `#0058AB` | WHO/HMD       | Barres hommes, prestataire public  |
| Rose femmes     | `#E8416F` | WHO/HMD       | Barres femmes                      |
| Vert rural      | `#80BD41` | FAO/IFPRI     | Violin milieu rural                |
| Gris urbain     | `#6C757D` | ONU-HABITAT   | Violin milieu urbain               |

---

## Références principales

- National Bureau of Statistics Nigeria & World Bank (2019). *Nigeria General Household Survey Panel Wave 4 (2018-2019)*. LSMS-ISA. DOI: 10.48529/1hgw-dq47
- World Health Organization (2023). *World Health Statistics 2023*. Genève.
- Cohen, J. (1988). *Statistical Power Analysis for the Behavioral Sciences* (2nd ed.). Lawrence Erlbaum.

La bibliographie complète est disponible dans `references.bib` (format BibTeX, style APA 7th).

---

## Licence

Projet académique -- ENSAE Pierre Ndiaye, ISE1 2025-2026.
Les données GHS Panel sont la propriété du NBS Nigeria / Banque Mondiale (licence d'utilisation académique).
