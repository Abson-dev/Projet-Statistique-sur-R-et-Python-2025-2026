# TP1 — Profil Démographique des Ménages Nigérians

---

## Membres de l'équipe

- **Herman YAMAHA**
- **Bourama DIALLO**

Étudiants en ISE1 à l'ENSAE de Dakar

**Superviseur :** M. Aboubacar HEMA, Data Scientist

**Année académique : 2025 - 2026**

---

## 1. Description du projet

Ce projet s'inscrit dans le cadre du cours de **Projet Statistique sous R et Python** (ENSAE ISE 1, 2025-2026). Il porte sur l'**Analyse 1 : Profil démographique des ménages nigérians**, à partir des données du Nigeria General Household Survey (GHS) Panel, programme LSMS-ISA de la Banque Mondiale.

**Objectif principal** : Explorer la structure sociodémographique des ménages nigérians à partir de la section 1 du GHS-Panel Wave 4 (2018/19), en analysant la distribution de l'âge, du sexe, du lien de parenté et de la taille des ménages selon le milieu de résidence (urbain/rural).

**Questions analytiques traitées :**

1. Quelle est la structure du jeu de données et quelles variables clés en sont issues ?
2. Le fichier présente-t-il des doublons ou des valeurs manquantes sur les variables clés ?
3. Comment se distribue l'âge des membres des ménages et suit-il une loi normale ?
4. Quelle est la fréquence des différents liens de parenté au sein des ménages ?
5. La taille des ménages diffère-t-elle significativement entre zones urbaines et rurales ?
6. Quelle synthèse peut-on dresser des caractéristiques démographiques selon le milieu de résidence ?

---

## 2. Sources de données

| Fichier `.dta` | Contenu | Variables clés | Vague |
|:---|:---|:---|:---|
| `sect1_harvestw4.dta` | Section démographie individuelle | `s1q2` (sexe), `s1q4` (âge), `s1q3` (lien parenté), `sector` (milieu) | W4 |

**Source officielle :** World Bank LSMS-ISA — Nigeria GHS Panel, Wave 4 (2018/19), Post-Harvest  
**Lien :** `https://microdata.worldbank.org/index.php/catalog/3557`

---

## 3. Structure du projet

```
Groupe7_YAMAHA_Herman_DIALLO_Bourama_TP1/
│
├── main.R                                       # Point d'entrée — orchestre l'exécution des scripts
│
├── scripts/
│   ├── 01_import_nettoyage.R                    # Tâche 1-2 : import, recodages, contrôle qualité
│   └── 02_exploration_visualisation.R           # Tâches 3-6 : statistiques, graphiques, tableau
│
├── data/                                        # Les données sont importées depuis un compte GitHub et ce ne sont que les données épurées qui y seront après l'exécution 
│
├── rapport/
│   ├── Rapport_TP1_profil_demographique.Rmd     # Rapport R Markdown complet (avec code)
│   └── Rapport_TP1_profil_demographique.pdf     # Version PDF compilée
│
├── outputs/
│   ├── fig01_histogramme_age.png                # Distribution de l'âge (histogramme)
│   ├── fig02_boxplot_age.png                    # Boîte à moustaches de l'âge
│   ├── fig03_pyramide_ages.png                  # Pyramide des âges par sexe
│   ├── fig04_lien_parente.png                   # Fréquence du lien de parenté
│   ├── fig05_boxplot_menages.png                # Taille des ménages Urbain vs Rural
│   └── tableau_gtsummary.html                   # Tableau récapitulatif gtsummary (HTML)
│
└── README.md
```

---

## 4. Méthodologie

L'analyse suit six tâches structurées, toutes implémentées en R.

### Tâche 1 — Exploration de la structure du dataset

Chargement du fichier `sect1_harvestw4.dta` via le package `haven`. Construction des variables dérivées : âge (continu), sexe, milieu de résidence, zone géopolitique, lien de parenté (4 catégories), tranches d'âge quinquennales. Résumé des dimensions et des indicateurs globaux.

### Tâche 2 — Doublons et valeurs manquantes

Vérification de l'unicité de la clé `hhid × indiv`. Comptage des valeurs manquantes sur les variables clés (âge, sexe, milieu, lien de parenté, zone géographique). Visualisation avec `vis_miss()` du package `naniar`.

### Tâche 3 — Analyse univariée de l'âge

Statistiques descriptives complètes (moyenne, médiane, écart-type, Q1, Q3, IQR). Histogramme (binwidth = 5 ans) avec annotation de la moyenne. Boîte à moustaches. Test de normalité de **Shapiro-Wilk** sur un sous-échantillon de 5 000 individus (graine = 2025). Pyramide des âges par sexe avec le package `apyramid`.

### Tâche 4 — Lien de parenté et IC à 95 %

Diagramme en barres horizontales ordonné par fréquence. Calcul des proportions avec **intervalles de confiance exacts de Clopper-Pearson** (package `PropCIs`), plus robustes que l'approximation normale pour les catégories minoritaires. Visualisation des IC par un graphique pointrange.

### Tâche 5 — Taille des ménages et test de Wilcoxon-Mann-Whitney

Construction de la base ménages (une ligne par ménage). Statistiques descriptives par milieu (urbain/rural). Test de **Wilcoxon-Mann-Whitney** bilatéral (test non paramétrique, justifié par la non-normalité de la distribution). Taille d'effet *r* de Rosenthal : r = |Z| / √n. Boxplot groupé avec annotation du résultat du test.

### Tâche 6 — Tableau gtsummary stratifié

Tableau de synthèse `tbl_summary()` stratifié par milieu de résidence, incluant l'âge, le sexe et la taille du ménage. Tests statistiques associés : Wilcoxon (variables continues) et Chi-deux (variables catégorielles). Export en HTML.

---

## 5. Packages R requis

```r
install.packages(c(
  "haven",       # lecture des fichiers .dta Stata
  "dplyr",       # manipulation des données
  "ggplot2",     # visualisation
  "naniar",      # diagnostic des valeurs manquantes
  "PropCIs",     # intervalles de confiance exacts (Clopper-Pearson)
  "gtsummary",   # tableaux de synthèse statistique
  "gt",          # export HTML des tableaux gtsummary
  "apyramid",    # pyramide des âges
  "forcats",     # réordonner les facteurs
  "scales"       # formatage des axes
))
```

---

## 6. Reproduction des résultats

**Étape 1 — Exécuter l'analyse**

Ouvrir RStudio à partir du Rproj du projet, puis exécuter :

```r
main.R
```

Les deux scripts sont exécutés séquentiellement. Les figures PNG sont sauvegardées dans `outputs/` et le tableau HTML dans `outputs/tableau_gtsummary.html`.

**Étape 2 — Compiler le rapport**

Ouvrir `rapport/Rapport_TP1_profil_demographique.Rmd` dans RStudio et cliquer sur **Knit**. Les données sont lues depuis `../data/raw/` via des chemins relatifs — l'étape 1 doit donc être réalisée en premier.

---

## 7. Principaux résultats

| Indicateur | Résultat |
|:---|:---|
| Nombre d'individus | 30 337 |
| Nombre de ménages | 5 025 |
| Âge moyen | 24,0 ans |
| Âge médian | 18 ans |
| Part des hommes | 49,7 % |
| Part des ménages urbains | 32,3 % |
| Doublons sur hhid × indiv | 0 |
| Normalité de l'âge (Shapiro-Wilk) | Non normale (W ≈ 0,97 ; p < 0,001) |
| Catégorie de parenté majoritaire | Enfant (~47 %) |
| Taille médiane — Urbain | 5 membres |
| Taille médiane — Rural | 6 membres |
| Test Wilcoxon (taille Urbain vs Rural) | p < 0,001 ; r ≈ 0,11 (petit effet) |
| Différences Urbain/Rural (gtsummary) | Toutes significatives (p < 0,001) |

---

## 8. Références

- World Bank (2019). *Nigeria General Household Survey Panel 2018-2019, Wave 4*. LSMS-ISA. Washington DC : The World Bank.
- Deaton, A. (1997). *The Analysis of Household Surveys: A Microeconometric Approach to Development Policy*. Baltimore : Johns Hopkins University Press.
- Clopper, C. J. & Pearson, E. S. (1934). The use of confidence or fiducial limits illustrated in the case of the binomial. *Biometrika*, 26(4), 404-413.
