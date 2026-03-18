# TP2 sur le  Capital humain : Éducation et alphabétisation des ménages nigérians

Analyse descriptive des caractéristiques éducatives des ménages à partir du **Nigeria General Household Survey Panel Wave 4 (2018-2019)**, collecté par le National Bureau of Statistics (NBS) en collaboration avec la Banque Mondiale (programme LSMS-ISA).

**Auteurs** : Keita L. & Nkwa Tsamo L.

**Cours** : Projet Statistique avec R et Python par M. HEMA, research scientist at IFPRI

---

## Objectifs de l'analyse

1. Décrire la **distribution du niveau d'éducation** parmi les membres éligibles (3 ans et plus)
2. Analyser les **inégalités éducatives par sexe** chez les adultes (test du Chi-deux, V de Cramér)
3. Étudier la **relation entre l'âge et le niveau d'éducation** atteint (Kruskal-Wallis, post-hoc Dunn-Bonferroni)
4. Comparer les **taux de scolarisation (6-17 ans)** entre milieux rural et urbain (IC 95% Clopper-Pearson)
5. Identifier les **états les plus touchés par l'analphabétisme adulte** (carte de chaleur par état)
6. Produire un **tableau synthétique** des indicateurs éducatifs (gtsummary)

---

## Structure du projet

```
TP2/
|-- main.R                        # Script principal (lance tout le pipeline)
|-- scripts/
|   |-- 01_nettoyage.R            # Import, exploration, nettoyage, variables dérivées
|   |-- 02_analyses.R             # Analyses statistiques et visualisations
|-- data/
|   |-- raw/                      # Données brutes .dta (NON VERSIONNÉES, voir ci-dessous)
|   |-- processed/                # Données nettoyées .rds (générées par 01_nettoyage.R)
|-- outputs/
|   |-- figures/                  # Graphiques PNG (300 dpi)
|   |   |-- fig00_diagnostic_NA_educ.png
|   |   |-- fig01_niveau_educ_distribution.png
|   |   |-- fig02_niveau_educ_sexe.png
|   |   |-- fig03_age_niveau_educ.png
|   |   |-- fig04_scolarisation_milieu.png
|   |   |-- fig05_heatmap_analphabetisme_etats.png
|   |-- tables/                   # Tableaux exportés
|       |-- tab01_gtsummary_education.html
|       |-- tab01_gtsummary_education.csv
|       |-- tab02_top10_analphabetisme.csv
|-- docs/
|   |-- reference.docx            # Fichier de styles Word pour Quarto
|   |-- documentation_tp2.html    # Documentation complémentaire
|-- rapport_tp2.qmd               # Rapport académique Quarto (source)
|-- rapport_tp2.pdf               # Rapport compilé (PDF)
|-- rapport_tp2.docx              # Rapport compilé (Word)
|-- references.bib                # Bibliographie BibTeX (APA 7th)
|-- renv.lock                     # Versions exactes des packages R
|-- .Rprofile                     # Activation automatique de renv
|-- TP2_GHS.Rproj                 # Fichier projet RStudio
|-- README.md
```

---

## Données sources

Les données brutes ne sont **pas incluses dans ce dépôt** (taille ~31 Mo, `.gitignore`).

### Téléchargement

1. Accéder au catalogue : <https://microdata.worldbank.org/index.php/catalog/3557>
2. Télécharger les fichiers **Stata format** (version 03, octobre 2021)
3. Extraire et placer dans `data/raw/NGA_2018_GHSP-W4_v03_M_Stata12/` les fichiers suivants :

| Fichier                  | Description                                              | Taille  |
|--------------------------|----------------------------------------------------------|---------|
| `sect2_harvestw4.dta`    | Module éducation (alphabétisation, niveau, scolarisation)| ~16 Mo  |
| `sect1_harvestw4.dta`    | Roster individuel (âge, sexe, statut de résidence)       | ~15 Mo  |
| `secta_harvestw4.dta`    | Fichier Root ménage (géographie, poids)                  | ~273 Ko |

Ces **3 fichiers** sont nécessaires pour le TP2.

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
cd TP2
```

### Étape 2 : Restaurer l'environnement R

Ouvrir `TP2_GHS.Rproj` dans RStudio, puis dans la console R :

```r
renv::restore()
```

Cette commande installe automatiquement **toutes les dépendances** avec les versions exactes spécifiées dans `renv.lock`. Aucune installation manuelle de packages n'est nécessaire.

### Étape 3 : Placer les données brutes

Télécharger les 3 fichiers `.dta` (voir section Données sources) et les placer dans :

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
- Exécute `01_nettoyage.R` : import, exploration, nettoyage, construction des variables éducatives
- Exécute `02_analyses.R` : analyses statistiques, tests, visualisations
- Affiche un résumé final avec les livrables produits et les durées d'exécution

**Durée estimée** : 20-30 secondes.

### Étape 5 : Compiler le rapport (optionnel)

Le rapport compilé (`rapport_tp2.pdf` et `rapport_tp2.docx`) est déjà inclus dans le dépôt. Pour le régénérer :

```r
quarto::quarto_render("rapport_tp2.qmd", output_format = "all")
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
| `rstatix`    | Tests statistiques et tailles d'effet (Kruskal-Wallis, Dunn) |
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

- **Niveau dominant** : Primaire (33,4% des membres éligibles), suivi du Secondaire (27%)
- **Analphabétisme** : 24,9% des membres éligibles n'ont aucun niveau d'instruction
- **Inégalités de genre** : les femmes adultes sont surreprésentées dans la catégorie "Aucun" (31,3% vs 14,7% chez les hommes) -- Chi-deux p < 0,001, V de Cramér = 0,214 (effet modéré)
- **Âge et niveau** : association significative (Kruskal-Wallis H(5) = 2 173,67, p < 0,001) -- les individus au niveau Secondaire sont les plus jeunes, ceux sans instruction les plus âgés
- **Scolarisation 6-17 ans** : taux de 92,4% en milieu urbain vs 88,5% en milieu rural -- différence significative (Chi-deux p < 0,001)
- **États les plus touchés** : Kebbi (66,7%), Niger (55,6%) et Bauchi (46,2%) affichent les taux d'analphabétisme adulte les plus élevés

---

## Reproductibilité

- **Graine aléatoire** : `set.seed(2070)` (fixée dans `main.R`)
- **Environnement** : `renv.lock` verrouille les versions exactes de tous les packages
- **Pipeline** : un seul appel `source("main.R")` reproduit l'intégralité des résultats en ~22 secondes
- **Rapport** : `rapport_tp2.qmd` avec valeurs dynamiques (inline R, aucun chiffre codé en dur)

---

## Arborescence des sorties

### Figures (6)

| Fichier                                  | Contenu                                                        |
|------------------------------------------|----------------------------------------------------------------|
| `fig00_diagnostic_NA_educ.png`           | Carte des valeurs manquantes (vis_miss)                        |
| `fig01_niveau_educ_distribution.png`     | Distribution du niveau d'éducation (barplot horizontal)        |
| `fig02_niveau_educ_sexe.png`             | Niveau d'éducation par sexe chez les adultes (barplot empilé)  |
| `fig03_age_niveau_educ.png`              | Distribution de l'âge par niveau d'éducation (boxplot)         |
| `fig04_scolarisation_milieu.png`         | Taux de scolarisation 6-17 ans par milieu (IC 95%)             |
| `fig05_heatmap_analphabetisme_etats.png` | Taux d'analphabétisme adulte par état nigérian (carte de chaleur)|

### Tableaux (3)

| Fichier                             | Contenu                                          |
|-------------------------------------|--------------------------------------------------|
| `tab01_gtsummary_education.html`    | Tableau gtsummary interactif (indicateurs clés)  |
| `tab01_gtsummary_education.csv`     | Tableau gtsummary export plat                    |
| `tab02_top10_analphabetisme.csv`    | Top 10 états par taux d'analphabétisme adulte    |

---

## Palette de couleurs

Les couleurs suivent les conventions des organisations internationales :

| Couleur         | Code HEX  | Convention        | Usage                            |
|-----------------|-----------|-------------------|----------------------------------|
| Violet UNESCO   | `#5C2D91` | UNESCO Brand Book | Couleur principale TP2           |
| Bleu hommes     | `#0058AB` | WHO/HMD           | Labels et annotations            |
| Rose femmes     | `#E8416F` | WHO/HMD           | Différenciation par sexe         |
| Vert rural      | `#80BD41` | FAO/IFPRI         | Barres milieu rural              |
| Gris urbain     | `#6C757D` | ONU-HABITAT       | Barres milieu urbain             |

---

## Références principales

- National Bureau of Statistics Nigeria & World Bank (2019). *Nigeria General Household Survey Panel Wave 4 (2018-2019)*. LSMS-ISA. DOI: 10.48529/1hgw-dq47
- UNESCO Institute for Statistics (2023). *Education and Literacy Data*. Montréal.
- Cohen, J. (1988). *Statistical Power Analysis for the Behavioral Sciences* (2nd ed.). Lawrence Erlbaum.

La bibliographie complète est disponible dans `references.bib` (format BibTeX, style APA 7th).

---

## Licence

Projet académique, ENSAE, ISE1 2025-2026.
Les données GHS Panel sont la propriété du NBS Nigeria / Banque Mondiale (licence d'utilisation académique).
