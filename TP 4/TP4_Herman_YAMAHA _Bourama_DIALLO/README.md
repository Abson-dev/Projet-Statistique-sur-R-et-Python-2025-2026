# README — TP4 : Analyse des parcelles agricoles

**Auteurs :** Herman YAMAHA \| Bourama DIALLO (Groupe 7)\
**Cours :** Statistique — ENSAE Pierre Ndiaye \| 2025–2026\
**Données :** Nigeria General Household Survey (GHS) Panel Wave 4 (2018/19)\
**Source :** World Bank Living Standards Measurement Study (LSMS)

------------------------------------------------------------------------

## Description

Ce travail analyse la distribution des superficies agricoles et des régimes de tenure foncière au Nigeria à partir de l'enquête GHS Panel Wave 4. 
Les statistiques sont pondérées par `wt_wave4` afin d'être représentatives de la population nigériane.
Il est essentiellement question dans ce projet d'utiliser les bases post planting du GHS du Nigéria afin de dégager des caractéristiques agricoles des ménages nigérians.

## Méthodologie  

Nous avons constaté que 49 ménages n'ont pas de poids donc partout où le poids a été nécessaire pour les calculs,
ces 49 ménages ont été écartés. 
Il y a deux types ou mesures de superficies qui sont effectués sur la plupart
des parcelles nigérians durant la vague 4, notamment la superficie de la parcelle 
mésurée par GPS et la superficie déclarée par le ménage.
Il a été constaté que 4202 parcelles n'ont pas de superficies GPS (34% des parcelles). Donc, afin de pouvoir construire la superficie en hectare nous avons utilisé 
la superficie déclarée pour laquelle seulement 4 ménages n'ont pas de superficies.
Les superficies déclarées que nous avons utilisé pour construire la variable 
superficie en hectare, étaient exprimées dans des unités locales dont la correspondance
en hectare varient selon le secteur. Nous avons utilisé le document ghs_panel_binfo_2018_19_rev_oct1_2021.pdf
qui contient les informations nécéssaires aux conversions en hectare par secteur et par unité locale.
Pour le heatmap nous avons utilisé la vague 4 seulement.
Pour ce qui est de l'utilisation du poids dans les calculs, nous avons utilisé deux approches:
pour certaines fonctions, nous avons utilisé manuellement le poids en le passant directement 
en arguments dans la fonction sans faire de plan de sondage (mean,quantile) et pour des cas spécifiques 
comme les tests et le regime de tenure, nous avons fait le plan de sondage en tenant
compte du degré de l'échantillonnage.

## Résultats  

L'exploration des données a révélé 5 valeurs de superficies aberrantes (superficie_ha > 500) dont
trois parcelles dans le secteur North East ( 2000 ha, 1000 ha, 1800 ha) et deux 
parcelles dans le secteur North Central(1200 ha, 11495 ha).
le regime de tenure "family inheritance" est le plus dominant avec 63,1% des cas.
20,9% des parcelles ont une superficie comprise entre 025 ha et 05 ha et c'est ce groupe
de parcelles qui domine.
41,5% des ménages ont une superficie totale de parcelle comprise entre 1 ha et 5 ha
et c'est ce groupe qui regroupe le plus grand nombre de ménages.
Seulement 0.1% des ménages ont une superficie totale supérieure à 50 ha.
Il existe une corrélation linéaire forte entre les superficies GPS et les superficies déclarées.

------------------------------------------------------------------------

## Structure du projet

```         
TP4/
├── TP4_Herman_YAMAHA _Bourama_DIALLO.Rproj                      
├── main.R                                                   # Point d'entrée : exécute tous les scripts
├── data/                                                    # Dossier initialement inexistant, qui se crée et se remplit lors de l'exécution de main.R
│   ├── raw/                                                 # Fichiers .dta bruts (téléchargés depuis github)
│   │   ├── secta_harvestw4.dta
│   │   ├── sect11a1_plantingw4.dta
│   │   └── sect11b1_plantingw4.dta
│   └── processed/                                           # Bases intermédiaires produites par les scripts
│       ├── base_parcelle.rds
│       ├── bp_clean.rds
│       ├── prop_data.rds
│       └── superficie_menage.rds
├── outputs/
│   ├── figures/                                             # Graphiques produits (.png)
│   └── tables/                                              # Tableaux produits
├── scripts/
│   ├── 01_nettoyage.R                                       # Script d'import et de nettoyage
|   ├── 02_analyse.R                                         # Script d'analyse 
├── docs/
│   ├──  Rapport_TP4_Analyse_parcelles_agricoles_Nigeria.Rmd              
|   ├──  Rapport_TP4_Analyse_parcelles_agricoles_Nigeria.docs                
  
```

------------------------------------------------------------------------

## Comment reproduire les résultats

1.  Ouvrir le projet dans RStudio
2.  Ouvrir `main.R` et cliquer sur **Source**, ou ouvrir `Rapport_TP4_Analyse_parcelles_agricoles_Nigeria.Rmd` et cliquer sur **Knit**

Les données sont téléchargées automatiquement depuis GitHub au premier lancement.\
Les packages manquants sont installés automatiquement.
