# TP1 -- Profil demographique des menages nigerians

Analyse descriptive des caracteristiques demographiques des menages a partir du **Nigeria General Household Survey Panel Wave 4 (2018-2019)**, collecte par le National Bureau of Statistics (NBS) en collaboration avec la Banque Mondiale (programme LSMS-ISA).

**Auteurs** : Keita L. & Nkwa Tsamo L.

**Cours** : Statistiques Descriptives -- ENSAE Pierre Ndiaye, ISE1 2025-2026

---

## Objectifs de l'analyse

1. Decrire la **distribution de l'age** et tester sa normalite (test de Shapiro-Wilk)
2. Construire la **pyramide des ages** par sexe
3. Analyser la **composition des menages** selon le lien de parente avec le chef
4. Comparer la **taille des menages** entre milieux rural et urbain (test de Wilcoxon-Mann-Whitney, taille d'effet)
5. Produire un **tableau synthetique** stratifie par milieu de residence (gtsummary)

---

## Structure du projet

```
TP1/
|-- main.R                        # Script principal (lance tout le pipeline)
|-- scripts/
|   |-- 01_nettoyage.R            # Import, exploration, nettoyage, variables derivees
|   |-- 02_analyses.R             # Analyses statistiques et visualisations
|-- data/
|   |-- raw/                      # Donnees brutes .dta (NON VERSIONNEES, voir ci-dessous)
|   |-- processed/                # Donnees nettoyees .rds (generees par 01_nettoyage.R)
|-- outputs/
|   |-- figures/                  # Graphiques PNG (300 dpi)
|   |   |-- fig00_diagnostic_NA.png
|   |   |-- fig01_histogramme_age.png
|   |   |-- fig02_boxplot_age.png
|   |   |-- fig03_pyramide_ages.png
|   |   |-- fig04_parente_barplot.png
|   |   |-- fig05_boxplot_taille_menage.png
|   |-- tables/                   # Tableaux exportes
|       |-- tableau_gtsummary_rural_urbain.html
|       |-- tableau_gtsummary_rural_urbain.csv
|-- docs/
|   |-- reference.docx            # Fichier de styles Word pour Quarto
|   |-- (documentation annexe)
|-- rapport_tp1.qmd               # Rapport academique Quarto (source)
|-- rapport_tp1.pdf               # Rapport compile (PDF)
|-- rapport_tp1.docx              # Rapport compile (Word)
|-- cover_canva.pdf               # Page de garde du rapport
|-- references.bib                # Bibliographie BibTeX (APA 7th)
|-- renv.lock                     # Versions exactes des packages R
|-- .Rprofile                     # Activation automatique de renv
|-- TP1_GHS.Rproj                 # Fichier projet RStudio
|-- README.md
```

---

## Donnees sources

Les donnees brutes ne sont **pas incluses dans ce depot** (taille ~15 Mo, `.gitignore`).

### Telechargement

1. Acceder au catalogue : <https://microdata.worldbank.org/index.php/catalog/3557>
2. Telecharger les fichiers **Stata format** (version 03, octobre 2021)
3. Extraire et placer dans `data/raw/NGA_2018_GHSP-W4_v03_M_Stata12/` les fichiers suivants :

| Fichier                  | Description                                   | Taille |
|--------------------------|-----------------------------------------------|--------|
| `sect1_harvestw4.dta`    | Roster individuel (age, sexe, parente, statut)| ~15 Mo |
| `secta_harvestw4.dta`    | Fichier Root menage (geographie, poids)       | ~273 Ko|

Seuls ces **2 fichiers** sont necessaires pour le TP1.

---

## Prerequis

- **R** >= 4.4
- **RStudio** >= 2023.12 (recommande)
- **Quarto** >= 1.3 (pour compiler le rapport)

---

## Installation et execution

### Etape 1 : Cloner le depot

```bash
git clone <url-du-depot>
cd TP1
```

### Etape 2 : Restaurer l'environnement R

Ouvrir `TP1_GHS.Rproj` dans RStudio, puis dans la console R :

```r
renv::restore()
```

Cette commande installe automatiquement **toutes les dependances** avec les versions exactes specifiees dans `renv.lock`. Aucune installation manuelle de packages n'est necessaire.

### Etape 3 : Placer les donnees brutes

Telecharger les 2 fichiers `.dta` (voir section Donnees sources) et les placer dans :

```
data/raw/NGA_2018_GHSP-W4_v03_M_Stata12/
```

### Etape 4 : Executer le pipeline complet

```r
source("main.R")
```

Ce script :
- Verifie que tous les packages sont installes (sinon, demande `renv::restore()`)
- Charge les 14 packages requis
- Fixe la graine aleatoire (`set.seed(2070)`) pour la reproductibilite
- Execute `01_nettoyage.R` : import, exploration, nettoyage, construction des variables
- Execute `02_analyses.R` : analyses statistiques, tests, visualisations
- Affiche un resume final avec les livrables produits et les durees d'execution

**Duree estimee** : 30-60 secondes.

### Etape 5 : Compiler le rapport (optionnel)

Le rapport compile (`rapport_tp1.pdf` et `rapport_tp1.docx`) est deja inclus dans le depot. Pour le regenerer :

```r
quarto::quarto_render("rapport_tp1.qmd", output_format = "docx")
```

---

## Packages R utilises

| Package      | Usage                                          |
|--------------|------------------------------------------------|
| `tidyverse`  | Manipulation et visualisation des donnees      |
| `haven`      | Lecture des fichiers Stata (.dta)              |
| `labelled`   | Gestion des labels Stata                       |
| `here`       | Chemins relatifs portables                     |
| `scales`     | Formatage des axes graphiques                  |
| `naniar`     | Diagnostic des valeurs manquantes (vis_miss)   |
| `moments`    | Asymetrie (skewness) et aplatissement (kurtosis)|
| `rstatix`    | Tests statistiques et tailles d'effet          |
| `coin`       | Tests non parametriques                        |
| `PropCIs`    | Intervalles de confiance exacts (Clopper-Pearson)|
| `apyramid`   | Pyramide des ages                              |
| `gtsummary`  | Tableaux descriptifs stratifies                |
| `gt`         | Formatage de tableaux                          |
| `patchwork`  | Composition de graphiques                      |
| `cards`      | Dependance de gtsummary                        |

Toutes les versions sont verrouillees dans `renv.lock`.

---

## Principaux resultats

- **Age median** : 18 ans (population tres jeune, ISF = 5,3)
- **Equilibre des sexes** : 49,8% hommes / 50,2% femmes
- **Enfants** : 55,6% des membres de menages (categorie dominante)
- **Taille mediane des menages** : 5 membres (rural) vs 4 membres (urbain)
- **Test de Wilcoxon** : difference significative (p < 0,001) mais effet petit (r = 0,12)
- **Normalite rejetee** : test de Shapiro-Wilk, W = 0,94, p < 0,001

---

## Reproductibilite

- **Graine aleatoire** : `set.seed(2070)` (fixee dans `main.R`)
- **Environnement** : `renv.lock` verrouille les versions exactes de tous les packages
- **Pipeline** : un seul appel `source("main.R")` reproduit l'integralite des resultats
- **Rapport** : `rapport_tp1.qmd` avec valeurs dynamiques (inline R, aucun chiffre code en dur)

---

## Arborescence des sorties

### Figures (6)

| Fichier                          | Contenu                                        |
|----------------------------------|------------------------------------------------|
| `fig00_diagnostic_NA.png`        | Carte des valeurs manquantes (vis_miss)        |
| `fig01_histogramme_age.png`      | Distribution de l'age (classes quinquennales)  |
| `fig02_boxplot_age.png`          | Boite a moustaches de l'age                    |
| `fig03_pyramide_ages.png`        | Pyramide des ages par sexe                     |
| `fig04_parente_barplot.png`      | Repartition par lien de parente                |
| `fig05_boxplot_taille_menage.png`| Taille des menages rural vs urbain             |

### Tableaux (2)

| Fichier                                    | Contenu                                  |
|--------------------------------------------|------------------------------------------|
| `tableau_gtsummary_rural_urbain.html`      | Tableau gtsummary interactif             |
| `tableau_gtsummary_rural_urbain.csv`       | Tableau gtsummary export plat            |

---

## Palette de couleurs

Les couleurs suivent les conventions des organisations internationales :

| Couleur              | Code HEX  | Convention        | Usage                      |
|----------------------|-----------|-------------------|----------------------------|
| Bleu UNICEF          | `#1CABE2` | UNICEF Brand Book | Titres, histogrammes       |
| Bleu nuit UNICEF     | `#374EA2` | UNICEF Brand Book | Sous-titres, medianes      |
| Bleu hommes          | `#0058AB` | WHO/HMD           | Pyramide des ages (gauche) |
| Rose femmes          | `#E8416F` | WHO/HMD           | Pyramide des ages (droite) |
| Vert rural           | `#80BD41` | FAO/IFPRI         | Boxplots rural             |
| Gris urbain          | `#6C757D` | ONU-HABITAT       | Boxplots urbain            |

---

## References principales

- National Bureau of Statistics Nigeria & World Bank (2019). *Nigeria General Household Survey Panel Wave 4 (2018-2019)*. LSMS-ISA. DOI: 10.48529/1hgw-dq47
- National Population Commission & ICF (2019). *Nigeria Demographic and Health Survey 2018*. Abuja/Rockville.
- Cohen, J. (1988). *Statistical Power Analysis for the Behavioral Sciences* (2nd ed.). Lawrence Erlbaum.

La bibliographie complete est disponible dans `references.bib` (format BibTeX, style APA 7th).

---

## Licence

Projet academique -- ENSAE Pierre Ndiaye, ISE1 2025-2026.
Les donnees GHS Panel sont la propriete du NBS Nigeria / Banque Mondiale (licence d'utilisation academique).
