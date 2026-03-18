# TP1 :  Profil démographique des ménages nigérians

Analyse descriptive des caractéristiques démographiques des ménages à partir du **Nigeria General Household Survey Panel Wave 4 (2018-2019)**, collecté par le National Bureau of Statistics (NBS) en collaboration avec la Banque Mondiale (programme LSMS-ISA).

**Auteurs** : Keita L. & Nkwa Tsamo L.

**Cours** : Projet Statistique avec R et Python par M. HEMA, research scientist at IFPRI

---

## Objectifs de l'analyse

1. Décrire la **distribution de l'âge** et tester sa normalité (test de Shapiro-Wilk)
2. Construire la **pyramide des âges** par sexe
3. Analyser la **composition des ménages** selon le lien de parenté avec le chef
4. Comparer la **taille des ménages** entre milieux rural et urbain (test de Wilcoxon-Mann-Whitney, taille d'effet)
5. Produire un **tableau synthétique** stratifié par milieu de résidence (gtsummary)

---

## Structure du projet

```
TP1/
|-- main.R                        # Script principal (lance tout le pipeline)
|-- scripts/
|   |-- 01_nettoyage.R            # Import, exploration, nettoyage, variables dérivées
|   |-- 02_analyses.R             # Analyses statistiques et visualisations
|-- data/
|   |-- raw/                      # Données brutes .dta (NON VERSIONNÉES, voir ci-dessous)
|   |-- processed/                # Données nettoyées .rds (générées par 01_nettoyage.R)
|-- outputs/
|   |-- figures/                  # Graphiques PNG (300 dpi)
|   |   |-- fig00_diagnostic_NA.png
|   |   |-- fig01_histogramme_age.png
|   |   |-- fig02_boxplot_age.png
|   |   |-- fig03_pyramide_ages.png
|   |   |-- fig04_parente_barplot.png
|   |   |-- fig05_boxplot_taille_menage.png
|   |-- tables/                   # Tableaux exportés
|       |-- tableau_gtsummary_rural_urbain.html
|       |-- tableau_gtsummary_rural_urbain.csv
|-- docs/
|   |-- reference.docx            # Fichier de styles Word pour Quarto
|-- rapport_tp1.qmd               # Rapport académique Quarto (source)
|-- rapport_tp1.pdf               # Rapport compilé (PDF)
|-- rapport_tp1.docx              # Rapport compilé (Word)
|-- cover_canva.pdf               # Page de garde du rapport
|-- references.bib                # Bibliographie BibTeX (APA 7th)
|-- renv.lock                     # Versions exactes des packages R
|-- .Rprofile                     # Activation automatique de renv
|-- TP1_GHS.Rproj                 # Fichier projet RStudio
|-- README.md
```

---

## Données sources

Les données brutes ne sont **pas incluses dans ce dépôt** (taille ~15 Mo, `.gitignore`).

### Téléchargement

1. Accéder au catalogue : <https://microdata.worldbank.org/index.php/catalog/3557>
2. Télécharger les fichiers **Stata format** (version 03, octobre 2021)
3. Extraire et placer dans `data/raw/NGA_2018_GHSP-W4_v03_M_Stata12/` les fichiers suivants :

| Fichier                  | Description                                    | Taille  |
|--------------------------|------------------------------------------------|---------|
| `sect1_harvestw4.dta`    | Roster individuel (âge, sexe, parenté, statut) | ~15 Mo  |
| `secta_harvestw4.dta`    | Fichier Root ménage (géographie, poids)        | ~273 Ko |

Seuls ces **2 fichiers** sont nécessaires pour le TP1.

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
cd TP1
```

### Étape 2 : Restaurer l'environnement R

Ouvrir `TP1_GHS.Rproj` dans RStudio, puis dans la console R :

```r
renv::restore()
```

Cette commande installe automatiquement **toutes les dépendances** avec les versions exactes spécifiées dans `renv.lock`. Aucune installation manuelle de packages n'est nécessaire.

### Étape 3 : Placer les données brutes

Télécharger les 2 fichiers `.dta` (voir section Données sources) et les placer dans :

```
data/raw/NGA_2018_GHSP-W4_v03_M_Stata12/
```

### Étape 4 : Exécuter le pipeline complet

```r
source("main.R")
```

Ce script :
- Vérifie que tous les packages sont installés (sinon, demande `renv::restore()`)
- Charge les 15 packages requis
- Fixe la graine aléatoire (`set.seed(2070)`) pour la reproductibilité
- Exécute `01_nettoyage.R` : import, exploration, nettoyage, construction des variables
- Exécute `02_analyses.R` : analyses statistiques, tests, visualisations
- Affiche un résumé final avec les livrables produits et les durées d'exécution

**Durée estimée** : 20-30 secondes.

### Étape 5 : Compiler le rapport (optionnel)

Le rapport compilé (`rapport_tp1.pdf` et `rapport_tp1.docx`) est déjà inclus dans le dépôt. Pour le régénérer :

```r
quarto::quarto_render("rapport_tp1.qmd", output_format = "all")
```

---

## Packages R utilisés

| Package      | Usage                                            |
|--------------|--------------------------------------------------|
| `tidyverse`  | Manipulation et visualisation des données        |
| `haven`      | Lecture des fichiers Stata (.dta)                |
| `labelled`   | Gestion des labels Stata                         |
| `here`       | Chemins relatifs portables                       |
| `scales`     | Formatage des axes graphiques                    |
| `naniar`     | Diagnostic des valeurs manquantes (vis_miss)     |
| `moments`    | Asymétrie (skewness) et aplatissement (kurtosis) |
| `rstatix`    | Tests statistiques et tailles d'effet            |
| `coin`       | Tests non paramétriques                          |
| `PropCIs`    | Intervalles de confiance exacts (Clopper-Pearson)|
| `apyramid`   | Pyramide des âges                                |
| `gtsummary`  | Tableaux descriptifs stratifiés                  |
| `gt`         | Formatage de tableaux                            |
| `patchwork`  | Composition de graphiques                        |
| `cards`      | Dépendance de gtsummary                          |

Toutes les versions sont verrouillées dans `renv.lock`.

> **Note** : `coin` masque certaines fonctions de `rstatix` (`kruskal_test`, `wilcox_test`, etc.).
> Toujours utiliser le préfixe `rstatix::` devant ces fonctions dans les scripts.

---

## Principaux résultats

- **Échantillon** : 25 767 membres résidents actifs issus de 4 978 ménages (4 570 membres absents retirés)
- **Âge médian** : 18 ans -- population très jeune, asymétrie positive (skewness = 0,98)
- **Normalité rejetée** : test de Shapiro-Wilk W = 0,905, p < 0,001 -- tests non paramétriques utilisés
- **Équilibre des sexes** : 49,8% hommes / 50,2% femmes, pas de différence significative par milieu (p = 0,4)
- **Composition des ménages** : les enfants représentent 55,6% [55,0% ; 56,2%] des membres, suivis des chefs de ménage (19,3%) et des conjoints (16,4%)
- **Taille médiane des ménages** : 5 membres (rural) vs 4 membres (urbain)
- **Test de Wilcoxon** : différence significative (W = 3 098 048, p < 0,001) mais effet petit (r = 0,121, r² = 1,45%)
- **Pyramide des âges** : structure expansive typique des pays à forte fécondité (ISF = 5,3, DHS Nigeria 2018)

---

## Reproductibilité

- **Graine aléatoire** : `set.seed(2070)` (fixée dans `main.R`)
- **Environnement** : `renv.lock` verrouille les versions exactes de tous les packages
- **Pipeline** : un seul appel `source("main.R")` reproduit l'intégralité des résultats en ~24 secondes
- **Rapport** : `rapport_tp1.qmd` avec valeurs dynamiques (inline R, aucun chiffre codé en dur)

---

## Arborescence des sorties

### Figures (6)

| Fichier                           | Contenu                                          |
|-----------------------------------|--------------------------------------------------|
| `fig00_diagnostic_NA.png`         | Carte des valeurs manquantes (vis_miss)          |
| `fig01_histogramme_age.png`       | Distribution de l'âge (classes quinquennales)    |
| `fig02_boxplot_age.png`           | Boîte à moustaches de l'âge (valeurs aberrantes) |
| `fig03_pyramide_ages.png`         | Pyramide des âges par sexe                       |
| `fig04_parente_barplot.png`       | Répartition par lien de parenté (IC 95%)         |
| `fig05_boxplot_taille_menage.png` | Taille des ménages rural vs urbain               |

### Tableaux (2)

| Fichier                               | Contenu                          |
|---------------------------------------|----------------------------------|
| `tableau_gtsummary_rural_urbain.html` | Tableau gtsummary interactif     |
| `tableau_gtsummary_rural_urbain.csv`  | Tableau gtsummary export plat    |

---

## Palette de couleurs

Les couleurs suivent les conventions des organisations internationales :

| Couleur          | Code HEX  | Convention        | Usage                      |
|------------------|-----------|-------------------|----------------------------|
| Bleu UNICEF      | `#1CABE2` | UNICEF Brand Book | Titres, histogrammes       |
| Bleu nuit UNICEF | `#374EA2` | UNICEF Brand Book | Sous-titres, médianes      |
| Bleu hommes      | `#0058AB` | WHO/HMD           | Pyramide des âges (gauche) |
| Rose femmes      | `#E8416F` | WHO/HMD           | Pyramide des âges (droite) |
| Vert rural       | `#80BD41` | FAO/IFPRI         | Boxplots rural             |
| Gris urbain      | `#6C757D` | ONU-HABITAT       | Boxplots urbain            |

---

## Références principales

- National Bureau of Statistics Nigeria & World Bank (2019). *Nigeria General Household Survey Panel Wave 4 (2018-2019)*. LSMS-ISA. DOI: 10.48529/1hgw-dq47
- National Population Commission & ICF (2019). *Nigeria Demographic and Health Survey 2018*. Abuja/Rockville.
- Cohen, J. (1988). *Statistical Power Analysis for the Behavioral Sciences* (2nd ed.). Lawrence Erlbaum.

La bibliographie complète est disponible dans `references.bib` (format BibTeX, style APA 7th).

---

## Licence

Projet académique -- ENSAE Pierre Ndiaye, ISE1 2025-2026.
Les données GHS Panel sont la propriété du NBS Nigeria / Banque Mondiale (licence d'utilisation académique).
