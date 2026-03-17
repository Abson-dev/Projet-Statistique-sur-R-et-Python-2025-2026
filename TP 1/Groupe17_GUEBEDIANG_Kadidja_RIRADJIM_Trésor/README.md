# Analyse 1 — Profil démographique des ménages nigérians (GHS-Panel, vague 4)

## Description du projet

Ce projet présente une analyse descriptive du **profil démographique des ménages nigérians** à partir des données du **Nigeria General Household Survey Panel (GHS-Panel), vague 4 (2018)**.

L’objectif est d’examiner certaines caractéristiques démographiques des individus et des ménages, notamment :

- la distribution de l’âge
- la répartition par sexe
- le lien de parenté avec le chef de ménage
- le secteur de résidence (urbain / rural)
- la taille des ménages

Cette analyse constitue une première étape descriptive avant des analyses économiques ou sociales plus approfondies.

---

# Structure du projet

Le projet est organisé selon une structure standard de projet R reproductible.

```
ghs_nigeria_panel
│
├── data
│   ├── raw/            # données brutes (jamais modifiées)
│   └── processed/      # données nettoyées et préparées
│
├── R
│   ├── 01_import.R     # importation des données
│   ├── 02_nettoyage.R  # nettoyage et transformation des données
│   ├── 03_analyse.R    # analyses statistiques et descriptives
│   └── fonctions.R     # fonctions réutilisables
│
├── output
│   ├── figures/        # graphiques générés
│   └── tables/         # tableaux statistiques exportés
│
├── rapport
│   └── rapport_final.qmd   # rapport automatisé Quarto
│
├── README.md
└── ghs_nigeria_panel.Rproj
```

---

# Données

Les données proviennent du :

**Nigeria General Household Survey Panel (GHS-Panel), vague 4 (2018)**.

Les variables utilisées incluent notamment :

- âge
- sexe
- lien de parenté avec le chef de ménage
- secteur de résidence
- taille des ménages

---

# Organisation des scripts

Le dossier **R/** contient les scripts principaux du projet :

- **01_import.R** : importation des données brutes
- **02_nettoyage.R** : nettoyage et préparation des données
- **03_analyse.R** : production des statistiques descriptives, tableaux et graphiques
- **fonctions.R** : fonctions personnalisées réutilisables dans le projet

---

# Production du rapport

Le rapport final est généré automatiquement avec **Quarto**.

Le fichier principal du rapport est :

```
rapport/rapport_final.qmd
```

Pour produire le rapport :

1. ouvrir le projet dans **RStudio**
2. ouvrir le fichier `rapport_final.qmd`
3. cliquer sur **Render**

Le fichier généré est :

```
rapport_final.pdf
```

---

# Packages utilisés

Le projet utilise principalement les packages R suivants :

- `tidyverse`
- `knitr`
- `readr`
- `dplyr`
- `ggplot2`

---

# Auteurs

- **RIRADJIM NGARMOUNDOU TRESOR**
- **GUEBEDIAN A KEN KADIDJA**

---

# Licence

Projet académique réalisé dans le cadre d’un exercice d’analyse de données avec R.