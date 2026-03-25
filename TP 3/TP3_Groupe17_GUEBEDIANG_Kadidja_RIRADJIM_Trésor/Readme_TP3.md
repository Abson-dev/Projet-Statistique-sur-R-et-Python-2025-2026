# Accès aux services de santé et chocs sanitaires des ménages

## Description du projet

Ce projet propose une **analyse statistique pondérée** de l’accès aux services de santé et des dépenses de santé des ménages à partir des données du **Nigeria GHS Panel (Wave 4)**.

L’objectif est de produire une analyse **représentative et rigoureuse** des comportements sanitaires des ménages en intégrant les poids d’enquête.

---

## Objectifs

* Décrire la **morbidité déclarée** selon les caractéristiques individuelles
* Identifier les **maladies les plus fréquentes**
* Analyser les **prestataires de soins consultés**
* Étudier la **distribution des dépenses de santé**
* Examiner les **inégalités d’accès aux soins** selon :

  * le niveau de vie
  * le milieu de résidence (urbain/rural)

---

## Données utilisées

* **Source** : Nigeria GHS Panel (Wave 4)
* Fichiers mobilisés :

  * Données individuelles
  * Données de santé
  * Données de consommation des ménages

 Pondération utilisée : `wt_wave4` (niveau ménage, projetée au niveau individuel)

---

## Méthodologie

* Construction d’une base analytique par **jointures de données multi-sources**
* Analyse réalisée en **logique pondérée (survey design)**
* Outils statistiques adaptés aux **enquêtes complexes**
* Production automatisée de :

  * tableaux
  * graphiques
  * tests statistiques

---

## Principaux résultats

### Morbidité

* La morbidité augmente avec l’âge
* Les personnes âgées (65+) sont les plus touchées
* Différences modérées entre hommes et femmes

### Maladies dominantes

* **Paludisme** largement dominant
* Forte présence de maladies infectieuses
* Symptômes généraux (douleurs, maux de tête) fréquents

### Recours aux soins

* Les **pharmacies/dépôts** sont les plus utilisés
* Une part non négligeable des individus **ne consulte pas**
* Présence de recours alternatifs (tradipraticiens)

### Dépenses de santé

* Distribution très **inégalitaire et asymétrique**
* Présence de **valeurs extrêmes élevées**
* Forte dispersion des dépenses

### Inégalités sociales

* Le recours aux soins **augmente avec le niveau de vie**
* Association statistiquement significative (p < 0.001)
* Pas de différence significative entre **urbain et rural** sur les dépenses

---

## Message clé

 **L’accès aux soins n’est pas uniquement une question de besoin médical, mais fortement lié au niveau de vie.**

 Malgré une morbidité importante, une grande partie des ménages ne consulte pas, ce qui suggère des **barrières économiques ou structurelles**.

 L’analyse montre que :

* Les ménages pauvres consultent moins
* Les dépenses de santé sont très inégalement réparties
* Les systèmes informels (pharmacies, tradipraticiens) jouent un rôle majeur

 **Conclusion centrale :**

> L’intégration des pondérations est essentielle pour révéler les inégalités réelles d’accès aux soins et produire des résultats fiables pour l’aide à la décision publique.

---

## Tests statistiques

* Test de **Rao-Scott (Chi² ajusté)** → association significative consultation × quintile
* Test de **Wilcoxon pondéré** → pas de différence significative urbain/rural

---

## Structure du projet

```
├── data/
├── scripts/
├── output/
│   ├── tables/
│   ├── figures/
├── rapport/
│   └── rapport_final.docx
└── README.md
```

---

## Outils utilisés

* R (version 4.5+)
* Packages principaux :

  * `survey`
  * `dplyr`, `tidyr`
  * `ggplot2`
  * `haven`
  * `knitr`, `rmarkdown`
  * `flextable`, `officer`

---

## Reproductibilité

Le projet est entièrement reproductible :

1. Charger les données
2. Exécuter les scripts
3. Générer automatiquement :

   * tableaux
   * figures
   * rapport final

---

## Auteurs

* RIRADJIM NGARMOUNDOU Trésor
* GUEBEDIANG A NKEN Kadidja

---

## Contexte académique

* ENSAE
* Année académique : 2025–2026

---

## Remarque

Ce projet met en évidence l’importance :

* des **pondérations d’enquête**
* de la **qualité des données**
* et de la **reproductibilité analytique**

---
