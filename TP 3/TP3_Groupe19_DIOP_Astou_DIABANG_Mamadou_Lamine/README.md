# Analyse 3 -- Acces aux services de sante et chocs sanitaires des menages nigerians

> **Projet Statistique sous R** | ENSAE Pierre Ndiaye | ISE1 2025-2026
> **Donnees :** Nigeria General Household Survey (GHS) Panel -- Wave 4 (2018-2019)
> **Superviseur :** Aboubacar HEMA, Analyste de recherche, IFPRI

---

## Auteurs

| Nom | Statut |
|-----|--------|
| Astou DIOP | Eleve Ingenieure Statisticienne Economiste, ENSAE Pierre Ndiaye |
| Mamadou Lamine DIABANG | Eleve Ingenieur Statisticien Economiste, ENSAE Pierre Ndiaye |

---

## Structure du projet

```
projet/
├── README.md
├── scripts/
│   ├── TP3_Sante_GHS_Nigeria.R       # Script R principal (analyses + exports)
│   ├── TP3_Sante_GHS_Nigeria.Rmd     # Rapport R Markdown (PDF professionnel)
│   └── preamble.tex                  # En-tetes LaTeX (requis pour le Rmd)
├── output/                           # Graphiques et tableaux generes automatiquement
│   ├── 00_valeurs_manquantes.png
│   ├── fig1_morbidite_sexe_age.png
│   ├── fig2_top10_affections.png
│   ├── fig3_recours_prestataire.png
│   ├── fig4_depenses_sante.png
│   ├── fig5_depenses_rural_urbain.png
│   └── tableau1_recapitulatif.html
└── data/
    └── raw/                          # Fichiers .dta (non versiones)
        ├── sect4a_harvestw4.dta
        ├── sect1_harvestw4.dta
        ├── secta_harvestw4.dta
        ├── sect3b_harvestw4.dta
        └── cons_agg_wave3_visit2.dta
```

---

## Donnees

Les fichiers proviennent du programme **LSMS-ISA** de la Banque Mondiale :

> https://microdata.worldbank.org/index.php/catalog/3557
> Nigeria General Household Survey Panel 2018-2019 (Wave 4) -- Post-Harvest -- Household

| Fichier `.dta` | Contenu | Variables cles |
|----------------|---------|----------------|
| `sect4a_harvestw4` | Module sante | `s4aq1` (consultation), `s4aq3` (maladie), `s4aq6a/b` (prestataire), `s4aq9/14/17` (depenses) |
| `sect1_harvestw4` | Demographie individuelle | `s1q2` (sexe), `s1q4` (age) |
| `secta_harvestw4` | Variables geographiques | `sector` (1=Urban, 2=Rural) |
| `sect3b_harvestw4` | Assurance sante NHIS | `s3q50` (couverture menage) |
| `cons_agg_wave3_visit2` | Agregat consommation Wave 3 | `totcons` (consommation par tete) |

> **Note :** La Wave 4 ne dispose pas de fichier `cons_agg` publie. Les quintiles de
> niveau de vie sont construits a partir de la Wave 3 (2015-2016), les menages etant
> apparries via leur `hhid`. Approche standard dans la litterature LSMS-ISA.

---

## Codage des variables cles

Les variables sont stockees en **numerique pur** dans les fichiers Wave 4.
Ne pas appliquer `as_factor()`.

| Variable | Code | Signification |
|----------|------|---------------|
| `s4aq3` | 1 / 2 | Malade = 1, Non malade = 2 |
| `s4aq1` | 1 / 2 | A consulte = 1, Non = 2 |
| `s1q2` | 1 / 2 | Homme = 1, Femme = 2 |
| `sector` | 1 / 2 | Urban = 1, Rural = 2 |
| `s4aq6a` | 0 a 15 | Codes numeriques des prestataires |
| `s4aq3b_1` | 1 a 27 | Codes numeriques des maladies |

---

## Installation et execution

### Packages R requis

```r
install.packages(c(
  "haven", "dplyr", "tidyr", "ggplot2", "forcats", "scales",
  "rstatix", "gtsummary", "naniar", "patchwork", "gt",
  "knitr", "kableExtra"
))
```

### LaTeX requis pour le PDF

```r
install.packages("tinytex")
tinytex::install_tinytex()
tinytex::reinstall_tinytex(repository = "illinois")  # si erreur de version
tinytex::tlmgr_install(c("caption", "fancyhdr", "booktabs", "longtable", "float"))
```

### Lancer le script R

```r
setwd("chemin/vers/projet/scripts")
source("TP3_Sante_GHS_Nigeria.R")
```

### Compiler le rapport PDF

```r
rmarkdown::render("scripts/TP3_Sante_GHS_Nigeria.Rmd")
```

> **Important :** `preamble.tex` doit etre dans le **meme dossier** que le `.Rmd`.

---

## Analyses realisees

| Tache | Description | Methode |
|-------|-------------|---------|
| **13** | Taux de morbidite par sexe et groupe d'age | Proportions + IC 95% (prop.test) |
| **14** | Distribution des types d'affections declarees | Frequences + classification epidemiologique |
| **15** | Recours aux soins par type de prestataire | Frequences relatives |
| **16** | Distribution des depenses de sante | Statistiques descriptives + log-transformation |
| **18** | Comparaison depenses rural vs urbain | Test de Wilcoxon-Mann-Whitney + taille d'effet r |

> **Tache 17** exclue : absence de fichier `cons_agg_wave4`.

---

## Principaux resultats

- **Taux de morbidite global : 9,4%** sur 4 semaines
- Les femmes (10,3%) sont plus touchees que les hommes (8,4%)
- Les 60 ans+ atteignent 23,3% -- trois fois la moyenne nationale
- **Paludisme en tete** (666 cas), suivi des douleurs corporelles (572 cas)
- **40,6% des consultations** aupres du chimiste de quartier
- **13,2% des malades** ne consultent aucun prestataire
- **Mediane des depenses : 800 Naira** (~0,5 USD). CV = 3,8
- Pas de difference significative rural/urbain (Wilcoxon p = 0,365)

---

## Limites

- Poids de sondage non mobilises
- Quintiles construits sur Wave 3 (proxy du niveau de vie 2018)
- Biais de selection sur les depenses (non-consultants exclus)
- Fenetre de rappel de 4 semaines (biais de telescopage possible)

---

*Donnees : Nigeria General Household Survey Panel, Banque Mondiale / NBS Nigeria*
