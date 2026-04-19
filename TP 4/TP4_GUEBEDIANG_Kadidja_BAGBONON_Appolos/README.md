# README — Analyse 4 : Parcelles agricoles (GHS-Panel Nigeria Wave 4)

## 1. Contexte de l’étude

Ce projet s’inscrit dans le cadre des travaux pratiques d’analyse de données (ENSAE, ISE 1, année 2025–2026). Il exploite les données du **GHS-Panel Wave 4 (2018/2019)**, issues de la collaboration entre le National Bureau of Statistics (Nigeria) et la Banque mondiale (programme LSMS-ISA).

L’objectif principal est d’analyser les caractéristiques des parcelles agricoles, notamment :

* la superficie des parcelles,
* le régime de tenure foncière,
* l’utilisation des terres,
* les disparités spatiales à l’échelle nationale.

L’analyse couvre les questions 19, 20, 21, 23 et 24 (la question 22 étant exclue sur instruction).

---

## 2. Sources de données

Les bases utilisées proviennent des modules suivants :

* Post-Planting :

  * `sect11a1_plantingw4`
  * `sect11a_plantingw4`
  * `sect11b1_plantingw4`
  * `secta_plantingw4`
* Post-Harvest :

  * `secta1_harvestw4`
  * `secta_harvestw4`

Ces bases contiennent respectivement des informations sur :

* les parcelles agricoles (identifiants, superficies, unités),
* les pratiques agricoles,
* la tenure foncière,
* les caractéristiques des ménages,
* les poids d’enquête (`wt_wave4`).

---

## 3. Étapes de nettoyage et de préparation des données

### 3.1. Gestion des interviews incomplètes

Une première étape a consisté à identifier les ménages dont l’interview n’était pas complète (`interview_result != 1`).

* Ces ménages ont été croisés avec les données de parcelles.
* Les identifiants concernés (`hhid`) ont été exclus de toutes les bases utilisées.

Objectif :
Garantir la cohérence et la qualité des informations exploitées.

---

### 3.2. Harmonisation et construction des superficies

Les données de superficie sont exprimées sous différentes formes :

* mesures GPS (en m²),
* unités standards (hectares, acres),
* unités locales (heaps, ridges, stands).

Un travail de conversion a été réalisé :

* Conversion des surfaces GPS en hectares.
* Conversion des acres en hectares (facteur 0.4).
* Conversion des unités locales à l’aide de facteurs spécifiques par zone géopolitique.

Ces facteurs dépendent de :

* l’unité déclarée (`s11aq4b`),
* la zone géographique (`zone`).

Une variable homogène `superficie` (en hectares) a ainsi été construite.

---

### 3.3. Construction de la base principale

Plusieurs jointures ont été effectuées afin de regrouper l’information :

* Superficie des parcelles (`sect11a1_plantingw4`)
* Informations de tenure (`sect11b1_plantingw4`)
* Poids d’enquête (`secta_harvestw4`)
* Données de production (`secta1_harvestw4`)

Les jointures ont été réalisées sur les clés :

* `hhid` (ménage)
* `plotid` (parcelle)

---

### 3.4. Construction des variables analytiques

Les variables suivantes ont été construites :

* **Superficie par parcelle (`superficie`)**
* **Superficie estimée (`superficie_estimee`)**
* **Superficie totale par ménage (`superficie_par_menage`)**

  * calculée comme la somme des superficies des parcelles,
  * mise à `NA` si au moins une parcelle du ménage a une valeur manquante.
* **Nombre de parcelles (`nb_parcelles`)**

---

### 3.5. Gestion des valeurs manquantes

Une analyse exploratoire des valeurs manquantes a été réalisée :

* Visualisation via `vis_miss()` sur les variables clés.
* Calcul des proportions de valeurs manquantes par variable.

Aucune imputation n’a été effectuée. Les observations avec valeurs manquantes ont été exclues selon le contexte d’analyse.

---

### 3.6. Détection des valeurs aberrantes

Les anomalies suivantes ont été identifiées :

* Superficies nulles ou négatives.
* Superficies extrêmement élevées (> 500 hectares).

Ces observations ont été conservées pour l’analyse descriptive mais signalées comme valeurs potentiellement aberrantes.

---

### 3.7. Création des variables de stratification

Une variable de strate a été construite :

* `strata = interaction(zone, sector)`

Elle permet de capturer les différences entre :

* zones géopolitiques,
* milieux de résidence (urbain/rural).

---

## 4. Pondération et design d’enquête

Les données étant issues d’un plan de sondage complexe, les estimations doivent être pondérées.

* Poids utilisé : `wt_wave4`
* Deux designs ont été construits :

  * un design au niveau parcelle,
  * un design au niveau ménage.

Les objets `svydesign` et `srvyr` ont été utilisés pour :

* calculer des statistiques pondérées,
* produire des tableaux représentatifs à l’échelle nationale.

Remarque :
Les effectifs pondérés correspondent à la population nationale (en millions de ménages), contrairement aux effectifs bruts qui reflètent uniquement l’échantillon.

---

## 5. Choix méthodologiques

### 5.1. Analyse univariée

* Utilisation de quantiles pondérés (`svyquantile`).
* Transformation logarithmique pour traiter la forte asymétrie des superficies.

### 5.2. Comparaisons

* Tests du chi-deux pondérés (`svychisq`) pour les variables qualitatives.
* Corrélation de Spearman pour les variables quantitatives.

### 5.3. Visualisation spatiale

La heatmap initialement envisagée a été remplacée par une carte choroplèthe :

* Justification : meilleure représentation des disparités géographiques.
* Utilisation de données spatiales (`sf`, `rnaturalearth`).
* Harmonisation des noms d’États entre les données statistiques et géographiques.

---

## 6. Organisation des outputs

Tous les résultats sont enregistrés dans le dossier `output/` :

### Tableaux

* Statistiques descriptives
* Répartition des régimes fonciers
* Relation superficie–nombre de parcelles
* Résultats par État

### Figures

* Visualisation des valeurs manquantes
* Histogrammes
* Boxplots pondérés
* Scatter plots
* Graphiques de tenure
* Carte choroplèthe
* Dot plot des États

---

## 7. Limites

* Les conversions d’unités locales reposent sur des facteurs moyens pouvant introduire des approximations.
* Certaines valeurs extrêmes peuvent influencer les distributions malgré l’utilisation de transformations logarithmiques.
* L’absence d’imputation peut réduire la taille effective de l’échantillon pour certaines analyses.

---

## 8. Conclusion

Le processus de nettoyage et de préparation a permis :

* d’harmoniser des données hétérogènes,
* d’assurer la cohérence des unités de mesure,
* de garantir la représentativité nationale via les pondérations,
* de produire des indicateurs fiables sur les exploitations agricoles nigérianes.

Ce travail constitue une base solide pour des analyses économiques et spatiales approfondies.
