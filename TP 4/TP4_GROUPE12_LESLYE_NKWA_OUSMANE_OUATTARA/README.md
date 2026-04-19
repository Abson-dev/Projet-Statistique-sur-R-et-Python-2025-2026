# TP 4 : Parcelles agricoles, superficie et tenure foncière

Ce TP est pour le compte du cours intitulé **Projet Statistique avec R & Python**, dispensé par M. Aboubacar HEMA qui est research scientist à IFPRI.

**Auteurs**: NKWA Lelsye et OUATTARA Ousmane, étudiants en ISE1.

## 0.Résumé

Ce TP analyse la superficie et la tenure foncière de 11 076 parcelles agricoles issues du GHS Panel W4. La superficie retenue suit une hiérarchie à trois niveaux qui sont le GPS Post-Harvest, le GPS Post-Planting, puis la superficie déclarée convertie en hectares via les facteurs de conversion du BID Appendix 2 (stands, heaps, ridges, plots, acres, hectares). L'analyse concerne la distribution des superficies, la comparaison des superficies GPS/déclarée, les modes d'acquisition foncière désagrégés par sexe et milieu, la cartographie par État et zone géopolitique, etc.

## 1.Source de données

Les données mobilisées sont les bases `secta_harvestw4.dta` , `secta1_harvestw4.dta` , `sect11b1_plantingw4.dta`, `sect11a1_plantingw4.dta` et `secta_plantingw4.dta`. Toutes provienent du **Nigeria General Household Survey Panel Wave 4 (2018-2019)**, collectées par le National Bureau of Statistics (NBS) en collaboration avec la Banque Mondiale (programme LSMS-ISA).

Pour des soucis de légèreté, les données ont été placées dans le `gitignore`.
Vous pouvez accéder aux données via le catalogue  <https://microdata.worldbank.org/index.php/catalog/3557> de la banque mondiale.

## 2.Reproduction

# A. Cloner le dépôt
```bash
git clone <url-du-depot>
cd GROUPE16_TP2_LESLYE_NKWA_KEITA_LANCINA
```
# B. Restaurer l'environnement avec
```r
renv::restore()
```
# C. Télécharger les fichiers `.dta` (voir section Données sources) et les placer dans le dossier `raw` puis lancer le pipeline complet
```r
source("main.R")
```
## 3.Structure du projet

```
TP4_GROUPE16_LESLYE_NKWA_OUSMANE_OUATTARA/
|-- main.R                    
|-- scripts/
|   |-- preparation.R         
|   |-- analyses.R            # Analyses statistiques et visualisations
|-- data/
|   |-- raw/                  # Fichiers .dta bruts 
|   |-- processed/            # Objets R intermédiaires (.rds)
|-- outputs/
|   |-- figures/             
|   |-- tables/               
|-- docs/
|   |-- rapport.Rmd           # Rapport reproductible
|   |-- rapport.docx          # Rapport généré
|   |-- reference.docx        # Template Word
|-- renv/                     # Environnement reproductible
|-- renv.lock                 # Verrouillage des versions
|-- .gitignore                # Fichiers ignorés lors du push
├── .RData
├── .Rhistory
├── .Rprofile
├── TP4_GROUPE12_LESLYE_NKWA_OUSMANE_OUATTARA.Rproj
├── README.md
```
## 4.Key Messages

Les résultats princpaux sont dans la section *Messages clés* du rapport.docx

## 5.Crédit

ENSAE de Dakar.