# TP 1 : Etude du profil démographique des ménages nigérians 

Ce TP est pour le compte du cours intitulé **Projet Statistique avec R & Python**, dispensé par M. Aboubacar HEMA qui est research scientist à IFPRI.

**Auteurs**: Nkwa Tsamo Lelsye et KEITA Lancina, ISE1

## 1.Source de données

Les données mobilisées sont les bases  `sect1_harvestw4.dta` et `secta_harvestw4.dta`. Toutes provienent du **Nigeria General Household Survey Panel Wave 4 (2018-2019)**, collectées par le National Bureau of Statistics (NBS) en collaboration avec la Banque Mondiale (programme LSMS-ISA).

Pour des soucis de légèreté, les données ont été placées dans le `gitignore`.
Vous pouvez accéder aux données via le catalogue  <https://microdata.worldbank.org/index.php/catalog/3557> de la banque mondiale.

## 2.Objectifs de ce TP

Décrire la composition et les caractéristiques démographiques des ménages enquêtés dans la vague 4 du GHS Panel à partir des sections sect1_harvest et sect1_planting en intégrant les pondérations pour généraliser. 

## 3.Reproductibilité
### Étape 1 : Cloner le dépôt

```bash
git clone <url-du-depot>
cd GROUPE16_TP1_LESLYE_NKWA_KEITA_LANCINA
```

### Étape 2 : Restaurer l'environnement R

Ouvrir `TP1_REPRISE.Rproj` dans RStudio, puis dans la console R, tapez:

```r
renv::restore()
```
Cette commande installe automatiquement **toutes les dépendances** avec les versions exactes utilisées au cours du travail et spécifiées dans `renv.lock`.Cela fait qu'aucune installation manuelle de packages n'est nécessaire.

### Étape 3 : Placer les données brutes
Télécharger les 2 fichiers `.dta` (voir section Données sources) et les placer dans :

```
data/raw/
```

### Étape 4 : Exécuter le pipeline complet

```r
source("main.R")
```
Ce script :
- Vérifie que tous les packages sont installés (sinon, demande de taper `renv::restore()`)
- Charge les 14 packages requis
- Fixe la graine aléatoire (`set.seed(2070)`) pour la reproductibilité
- Exécute `nettoyage.R` : import, exploration, nettoyage, construction des variables éducatives
- Exécute `analyses.R` : analyses statistiques, tests, visualisations
- Termine par Knit le `rapport.Rmd` en word complet et stylisé selon les conventions des instituts nationaux.

## 4.Structure locale du projet 
```
GROUPE16_TP1_LESLYE_NKWA_KEITA_LANCINA/
├── data/
│   ├── processed
│   ├── raw
├── docs/
│   ├── LSMS_Integrated_Panel_Survey_Report-14-27.pdf   # Rapport officiel page 14 à 21
│   ├── rapport.docx
│   ├── rapport.Rmd
│   ├── reference.docx    # Styles prédéfinis
├── outputs/
│   ├── figures
│   ├── tables
├── renv/
├── scripts/
│   ├── analyses.R
│   ├── nettoyage.R
├── .gitignore
├── .RData
├── .Rhistory
├── .Rprofile
├── main.R
├── renv.lock
├── TP1_REPRISE.Rproj
├── README.md
```
## 5.Key Messages

Les résultats princpaux sont dans la section *Messages clés* du rapport.docx

## 6.Crédit

ENSAE de Dakar.
