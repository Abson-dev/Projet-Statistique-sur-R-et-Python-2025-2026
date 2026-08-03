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



*Barème appliqué : 70 points sur la Partie 1, hors exigence Git et hors renv.lock. La Partie 2 n'est pas exigible et n'intervient qu'en bonus, plafonné à 5 points.*



---

## 1. Méthode et limites

### Ce qui a été vérifié

Chaque dépôt a été téléchargé, décompressé et analysé : arborescence, présence de `.Rproj`, packages mobilisés, déclaration du plan de sondage, construction des indices, livrables produits, et traitement des pièges intégrés au jeu de données.



Le barème attribue 2 points pour une exécution intégrale, et une fraction proportionnelle à l'avancement pour un pipeline qui s'interrompt.



### Barème appliqué

| Bloc | Points |
|---|---:|
| Organisation et reproductibilité | 5 |
| Section 1 - Import, structuration, nettoyage | 17 |
| Section 2 - Pondération et indices composites | 20 |
| Section 3 - Visualisation | 9 |
| Section 5 - Communication | 7 |
| **Diagnostic technique - exécution et livrables** | **12** |
| **Total** | **70** |
| *Bonus* - Partie 2, API SDG | *+5* |

**Chaque malus vaut −1 point, chaque bonus +1 point.**

| Malus (−1 chacun) | Bonus (+1 chacun) |
|---|---|
| Quintiles non pondérés présentés comme représentatifs | Au moins deux pièges non signalés traités |
| Absence totale de plan de sondage | Quintiles pondérés |
| Vocabulaire causal sans cadrage | Piège des chefs dupliqués détecté et documenté |
| Chemin absolu ou `setwd()` dans le code | Au moins deux indicateurs publiés conformes |
| Chaîne de fichiers rompue | |
| Figures exportées à la main, non regénérables | |
| `ggsave()` présent sans figure produite | |
| Effectif du fichier source mal décrit | |
| Erreur de résultat constatée à la lecture | |

### Le bloc « Diagnostic technique » (12 points)

Il mesure ce que le rendu **produit effectivement**, là où les sections 3 et 5 mesurent ce que le code *demande*. Un rendu peut appeler `ggsave()` sans livrer la moindre figure, ou déclarer `word_document` sans que le `.docx` existe. Pour éviter tout double comptage, l'export des figures a été retiré de la section 3, et le contrôle du rapport compilé de la section 5.

| Élément | Points |
|---|---:|
| **Exécution du code sur banc isolé** | **6** |
| Rapport compilé en HTML | 1 |
| Rapport compilé en Word | 1 |
| Figures exportées en PNG | 1 |
| Figures exportées en PDF | 1 |
| Tableaux exportés | 1 |
| Notes régionales effectivement produites | 1 |

L'exécution est notée proportionnellement à l'avancement du pipeline : 6 points pour un rendu qui s'exécute intégralement, une fraction sinon. Les échecs imputables à la machine et non au code ne sont pas pénalisés.

---



---

## 3. Constats transversaux

### Justesse des résultats publiés

Les valeurs publiées par chaque rendu ont été confrontées à celles du corrigé : **Gini 0,442**, **HDDS moyen 6,48**, **revenu par tête moyen 17 336 FCFA**, pour 3 540 ménages et 23 provinces.

| Étudiant | Gini | HDDS | Revenu/tête |
|---|---|---|---|
| KEITA Lancina | **0.443335** | **6** | **17313** |
| NGUEAJIO David | **0.454** | **6.48** | **17824** |
| AGNANGMA SANAM David Landry | **0.434** | **6.5** | **17355** |
| NGAKE YAMAHA Herman Parfait | **0.442** | **6** | **17009** |
| NGOYI Parfait Jemmy Prodige | **0.449187** | **6.8** | - |
| FALL Cheikh Ahmadou Bamba | **0.446999** | **6** | - |
| DIOP Papa Boubacar | **0.445866** | **6** | - |
| TEVOEDJRE Michel | **0.453889** | **6.48** | - |



### Exécution réelle du code

| Résultat | Effectif |
|---|---:|
| S'exécute intégralement | 8 |
| S'interrompt en cours de pipeline | 8 |
| Échoue dès le premier script | 20 |

**Rendus s'exécutant intégralement :** ADDJITA Gérald Guerngué, ANDIL BEN, ANDRIALALAOSOA Marcellin, DICKO Hamadou, KANE Boubacar, KEBJAM Jackson, NDIAYE Cheikh Mouhamadou Moustapha, OUATTARA Ousmane.



### Cohérence entre les résultats et le code

**Résultats saisis à la main.** Cinq rapports contiennent des chiffres qui ne sont pas recalculés : ils ne se mettront pas à jour si les données changent. Il s'agit de: SECK Mouhamet, YEMELI SAAH Eugène Crespo ,FINARITRINIAINA Manampisoa Clarrat et OUATTARA Ousmane.



Le cas de **SECK Mouhamet** est le plus net : une centaine de valeurs chiffrées rédigées dans le rapport, aucune produite par `inline code`.

**Chaîne de fichiers rompue.** Trois rendus lisent des fichiers qu'aucun script ne produit et qui ne sont pas livrés :

- **NGUEAJIO David** - 1 fichier : `03_menage_enrichi.rds`
- **NGAKE YAMAHA Herman Parfait** - 5 fichier : `<fichier_module>.dta`, `<fichier_secta>.dta`, `<fichier_section>.dta`, `fichier.dta`, `secta_harvestw4.dta`
- **DIABANG Mamadou Lamine** - 1 fichier : `dictionnaire_variables_ensan2026_tchad.xlsx`
- **SECK Mouhamet** - 5 fichier : `nga_plotgeovariables_y4.dta`, `regions.shp`, `sect11b1_plantingw4.dta`, `secta1_harvestw4.dta`, `secta_harvestw4.dta`



**Figures et code.** **DEME Safiétou** livre des fichiers nommés `Rplot.png`, `Rplot01.png` - les noms par défaut de l'export manuel depuis RStudio - sans aucun code graphique dans le rendu : ces figures ne peuvent pas être regénérées. À l'inverse, **OUMAROU SOULEYE** appelle `ggsave()` sans qu'aucune figure ne soit présente : le code n'a pas été exécuté.


### Les pièges du jeu de données

| Piège | Traité par |
|---|---:|
| Typage de `hhid` (chaîne à zéros vs entier) | 15 / 36 |
| Harmonisation des libellés de régions | 13 / 36 |
| Contrôle du nombre de lignes après jointure | 15 / 36 |
| `nest = TRUE` dans le plan de sondage | 26 / 36 |
| NA tagués Stata (`is_tagged_na`) | 14 / 36 |
| **Quintiles pondérés** | **9 / 36** |

Le piège des quintiles est le plus discriminant : la majorité utilise `ntile()` sur le revenu brut, ce qui partitionne l'échantillon et non la population. Aucune sortie ne signale l'erreur. Deux candidats aggravent le cas en calculant les quintiles sur le revenu du ménage au lieu du revenu par tête (GUEBEDIANG, NDIAYE).

### Erreurs de plan de sondage


- **DIALLO** : `svydesign(ids = ~1)` - les grappes sont ignorées, les erreurs-types sont sous-estimées, alors même que l'organisation du code est l'une des meilleures.
- **DIOP Marème** : `strata = milieu` au lieu de `strata = strate` - la stratification déclarée n'est pas celle de l'enquête.
- **NDIAYE** : `id =` sans `nest = TRUE` - les grappes homonymes de strates différentes seront confondues.
- **OUATTARA** : `svyquantile` appliqué à un plan déclaré avec `ids = ~1`.

### Absence de rapport rédigé

7 dépôts ne contiennent **aucun** fichier R Markdown : ADDJITA Gérald Guerngué, DIABANG Mamadou Lamine, DICKO Hamadou, DIOP Astou, DIOP Joo Young, LO Serigne Ndame, RIRADJIM NGARMOUNDOU Trésor. Toute la Section 5 est perdue, et la justification de l'indice composite n'a aucun support.



### Conformité du rendu


- **Dépôts en dossier** au lieu d'une archive unique : DIOP Marème, DIOP Papa Boubacar.
- **Fichier hors archive** : un `.Rproj` de NGOYI Parfait traîne à la racine du dossier de rendu.
- **Nommage** : une majorité de dépôts ne respecte pas le format `Nom_Prenom_Examen_Partiex_ISE1_2026.zip`.

---
