# TP 4 : Analyse des parcelles agricoles: superficie, tenure foncière et utilisation des terres

Ce TP est pour le compte du cours intitulé **Projet Statistique avec R & Python**, dispensé par M. Aboubacar HEMA qui est research scientist à IFPRI.

**Auteurs**: Astou DIOP ET YEMELI SAAH Eugène Crespo, ISE1

## 1.Source de données

Les données mobilisées sont les bases `sect11a1_planttingtw4.dta` et `secta_planting.dta`. Toutes provienent du **Nigeria General Household Survey Panel Wave 4 (2018-2019)**, collectées par le National Bureau of Statistics (NBS) en collaboration avec la Banque Mondiale (programme LSMS-ISA).

Pour des soucis de légèreté, les données ont été placées dans le `gitignore`. Vous pouvez accéder aux données via le catalogue <https://microdata.worldbank.org/index.php/catalog/3557> de la banque mondiale.

## 2.Objectifs de ce TP

Décrire la structure foncière des exploitations agricoles nigérianes à partir des modules parcelles du GHS: superficie, régime de tenure, mode d'acquisition et localisation géographique, avec comparaison entre vagues. \## 3.Reproductibilité \### Étape 1 : Cloner le dépôt

``` bash
git clone <url-du-depot>
cd ANALYSE_4_ASTOU_DIOP_&_YEMELI_SAAH_EUGENE_CRESPO
```

### Étape 2 : Restaurer l'environnement R

Ouvrir `TP1_REPRISE.Rproj` dans RStudio, puis dans la console R, tapez:

``` r
renv::restore()
```

Cette commande installe automatiquement **toutes les dépendances** avec les versions exactes utilisées au cours du travail et spécifiées dans `renv.lock`.Cela fait qu'aucune installation manuelle de packages n'est nécessaire.

### Étape 3 : Placer les données brutes

Télécharger les 2 fichiers `.dta` (voir section Données sources) et les placer dans :

```         
data/raw/
```

### Étape 4 : Exécuter le pipeline complet

``` r
source("main.R")
```

Ce script : - Vérifie que tous les packages sont installés (sinon, demande de taper `renv::restore()`) - Charge les 14 packages requis - Fixe la graine aléatoire (`set.seed(2070)`) pour la reproductibilité - Exécute `nettoyage.R` : import, exploration, nettoyage, construction des variables éducatives - Exécute `analyses.R` : analyses statistiques, tests, visualisations - Termine par Knit le `rapport.Rmd` en word complet et stylisé selon les conventions des instituts nationaux.

## 4.Structure locale du projet

```         
ANALYSE4_TP4_ASTOU_DIOP_YEMELI_SAAH_EUGENE_CRESPO/
├── data/
│   ├── processed
│   ├── raw
├── docs/
│   ├── ghs_panel_wave4_survey_report.pdf   # Rapport officiel 
│   ├── rapport.docx
│   ├── rapport.Rmd
|   ├── reference.docx  # Styles prédéfinis
│   ├── reference.bib  # Styles prédéfinis pour la bibliographie   
├── outputs/
│   ├── figures
│   ├── tables
├── renv/
├── scripts/
│   ├── analyses.R
│   ├── donnee.R
│   ├── nettoyage.R
│   ├── setup.R
├── .gitignore
├── .RData
├── .Rhistory
├── .Rprofile
├── main.R
├── renv.lock
├── ANALYSE_4.Rproj
├── README.md
```

## 5.Key Messages

Les résultats princpaux sont dans la section *Messages clés* du rapport.docx

## 6.Crédit

ENSAE de Dakar.
