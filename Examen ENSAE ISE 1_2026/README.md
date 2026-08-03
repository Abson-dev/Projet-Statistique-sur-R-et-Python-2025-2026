# Données de l'examen : ENSAN 2026, Tchad

Ces données sont simulées à des fins pédagogiques pour l'examen pratique
« Projet Statistique sous R », ENSAE ISE1, session 2025 2026. Les 23
provinces, leurs pcodes et leurs superficies officielles proviennent du
jeu de données HDX / OCHA COD-AB Tchad (tcd_admin_boundaries.xlsx, valide
au 12 février 2025), fourni par l'utilisateur. **Toutes les valeurs
statistiques (revenus, chocs, prix, consommation) restent entièrement
simulées et ne représentent aucune donnée officielle**.

## Contenu de data/raw/

- **ensan_menages.dta** : module ménage (3540 ménages), avec hhid, region
  (les 23 provinces du Tchad, libellés officiels HDX/OCHA), milieu (labels
  Stata Urbain/Rural), grappe, strate, poids_final, revenu, taille_menage.
  N'Djamena est traitée comme une province exclusivement urbaine.
- **ensan_individus.csv** : module individu
- **ensan_consommation.xlsx** : 12 groupes alimentaires FAO et dépenses.
- **chocs_menages.csv** : chocs et stratégies d'adaptation.
- **prix_marches.csv** : prix mensuels de 4 céréales sur 24 mois.
- **tcd_admin1.shp** (+ .dbf, .shx, .prj, .cpg) : le shapefile officiel du Tchad (HDX).

