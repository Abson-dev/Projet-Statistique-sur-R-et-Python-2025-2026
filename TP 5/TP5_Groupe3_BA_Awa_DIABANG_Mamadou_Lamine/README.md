# TP5 : Cultures, intrants et rendements agricoles

**Nigeria GHS Panel Wave 4 (2018-2019)**  
**ENSAE Pierre Ndiaye — ISE1 | Projet Statistique sous R | 2025-2026**

---

## Auteurs

| Rôle | Nom |
|------|-----|
| Auteurs | Mamadou Lamine DIABANG & Awa Ba |
| Encadrement | Aboubacar HEMA — Analyste de recherche, IFPRI |

---

## Description

Ce script analyse les systèmes de culture, l'utilisation des intrants agricoles et les rendements des exploitations nigérianes à partir du **Nigeria General Household Survey (GHS) Panel Wave 4 (2018-2019)**. Toutes les estimations intègrent le plan de sondage complexe via le package `{survey}`.

**Questions traitées :** 25, 26, 27, 28, 29

---

## Structure du projet

```
projet/
├── data/
│                     
├── output/
│   └── TP5/                      
└── TP5_Cultures_GHS_W4.R 
```

---

## Données requises

Télécharger depuis [microdata.worldbank.org/catalog/3557](https://microdata.worldbank.org/index.php/catalog/3557) (Nigeria GHS Panel Wave 4) :

| Fichier `.dta` | Visite | Contenu | Questions |
|----------------|--------|---------|-----------|
| `secta3i_harvestw4.dta` | Post-Harvest | Cultures récoltées par parcelle, production | Q25, Q26, Q28 |
| `secta3ii_harvestw4.dta` | Post-Harvest | Commercialisation et transformation des cultures | Q25, Q26 |
| `secta1_harvestw4.dta` | Post-Harvest | Superficie des parcelles (GPS) | Q28 |
| `secta_harvestw4.dta` | Post-Harvest | Poids de sondage (`wt_wave4`), strates, PSU | Toutes |
| `secta11c2_harvestw4.dta` | Post-Harvest | Engrais inorg./org., herbicides, pesticides | Q27, Q29 |
| `sect11f_plantingw4.dta` | Post-Planting | Type de semences (améliorées vs locales) | Q29 |


---

## Packages R requis

```r
install.packages(c(
  "haven", "dplyr", "tidyr", "ggplot2", "forcats",
  "scales", "patchwork", "rstatix", "gtsummary",
  "viridis", "officer", "flextable",
  "survey", "srvyr"
))
```

**Version R recommandée :** R ≥ 4.2

---

## Variables clés construites

| Variable | Source | Description |
|----------|--------|-------------|
| `crop_nom` | `cropcode` | Label français de la culture |
| `crop_type` | `cropcode` | Type agronomique (Céréale, Tubercule, Légumineuse…) |
| `nb_cultures` | Agrégation `hhid` | Nombre de cultures distinctes par ménage |
| `pct_men_pond` | `wt_wave4` | % pondéré de ménages cultivant chaque culture |
| `utilise_inorg` | `s11dq1a` | Utilisation engrais inorganique (oui/non) |
| `utilise_org` | `s11dq36` | Utilisation engrais organique (oui/non) |
| `utilise_herb` | `s11c2q10` | Utilisation herbicide (oui/non) |
| `utilise_pest` | `s11c2q1` | Utilisation pesticide (oui/non) |
| `npk_kg` | `s11c2q37a × conv` | Quantité NPK en kg |
| `urea_kg` | `s11c2q38a × conv` | Quantité Urée en kg |
| `prod_kg` | `sa3iq6i × sa3iq6_conv` | Production en kg |
| `rdt_kg_ha` | `prod_kg / superf_ha` | Rendement en kg/ha |

---

## Plan de sondage

```r
options(survey.lonely.psu = "adjust")

plan <- svydesign(
  ids     = ~ea,       # PSU (Enumeration Areas)
  strata  = ~strata,   # 6 strates géographiques
  weights = ~wt_wave4, # Poids transversal (somme ≈ 27M ménages)
  nest    = TRUE,
  data    = données_filtrées
)
```

---

## Questions et méthodes

### Q25 — Top 15 cultures pratiquées
- Combinaison `secta3i` + `secta3ii` (cultures en production + commercialisation)
- **% pondéré** = Σ poids ménages cultivants / Σ poids total (`wt_wave4`)
- Graphe : barplot horizontal coloré par type agronomique

### Q26 — Diversification culturale
- Indice = nombre de cultures distinctes par ménage
- **Moyenne pondérée** via `svymean()`, **médiane pondérée** via `svyquantile()`
- **Test de Wilcoxon pondéré** Rural vs Urbain : `svyranktest()` sur plan de sondage
- Graphes : histogramme (ligne = moyenne pondérée) + violin plot (p-value pondérée)

### Q27 — Utilisation des engrais et intrants
- Source : `secta11c2_harvestw4` (vraie variable, pas de proxy)
- **Taux pondérés** globaux et par zone via `svymean()` + `confint()`
- **Chi-deux pondéré** engrais × zone : `svychisq()`
- Graphes : barplot groupé par zone + heatmap taux inorganique par État

### Q28 — Rendement à l'hectare
- Rendement = `(sa3iq6i × sa3iq6_conv)` / `superf_ha` (jointure `secta3i × secta1`)
- Détection outliers : IQR × 3 par culture (Maïs, Mil, Sorgho)
- **Statistiques pondérées** (médiane, moyenne, Q1, Q3) via `svydesign` + `svyquantile()`
- Graphes : violin plot + boxplots rendement maïs par État

### Q29 — Engrais inorganique vs rendement
- Jointure `secta11c2 × secta3i × secta1` sur `hhid + plotid`
- **Test de Wilcoxon pondéré** par culture : `svyranktest()` sur plan de sondage
- Graphe : boxplots facettés avec p-value pondérée dans chaque panneau

---

## Outputs produits

| Fichier | Description |
|---------|-------------|
| `q25_top15_cultures.png` | Top 15 cultures — % pondérés |
| `q26_diversification_culturale.png` | Histogramme + violin par zone |
| `q27a_intrants_zone.png` | Taux pondérés d'utilisation des intrants par zone |
| `q27b_engrais_etat.png` | Taux engrais inorganique par État |
| `q28_rendements_etat.png` | Violin rendements + boxplots maïs par État |
| `q29_rendement_engrais.png` | Rendement selon utilisation engrais (facettes) |
| `TP5_Cultures_GHS_W4.docx` | Rapport Word complet (4 parties) |

---

## Rapport Word — Structure

| Partie | Contenu |
|--------|---------|
| **I. Introduction** | Contexte agricole nigérian, objectifs des 5 questions |
| **II. Données et méthodologie** | 6 fichiers, plan de sondage, méthodes statistiques pondérées |
| **III. Résultats** | Q25 à Q29 avec tableaux, figures et interprétations pondérées |
| **IV. Conclusion** | Enseignements clés + limites |
| **V. Références** | 7 références bibliographiques |

---

## Exécution

```r
# Adapter les chemins si nécessaire
path_raw <- "../data/"   # dossier contenant les .dta
path_out <- "../output/TP5/" # dossier de sortie (créé automatiquement)

# Lancer le script complet
source("TP5_Cultures_GHS_W4.R")
```

Le script génère automatiquement les figures PNG et le rapport Word à la fin de l'exécution.

---

---

## Correspondance cropcode → culture (principales)

| Code | Culture | Type |
|------|---------|------|
| 1080 | Maïs | Céréale |
| 1020 | Manioc | Tubercule |
| 1070 | Sorgho | Céréale |
| 1010 | Haricots/Niébé | Légumineuse |
| 1121 | Igname blanche | Tubercule |
| 1100 | Mil | Céréale |
| 1060 | Arachide | Légumineuse |
| 1110 | Riz | Céréale |

---

*GHS Panel Wave 4 — National Bureau of Statistics Nigeria / Banque Mondiale (LSMS-ISA)*
