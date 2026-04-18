# TP4 : Analyse des parcelles agricoles

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

Ce script analyse la structure foncière des exploitations agricoles nigérianes à partir du **Nigeria General Household Survey (GHS) Panel Wave 4 (2018-2019)**. Il couvre la superficie des parcelles, les régimes de tenure foncière et leur distribution géographique, avec intégration complète du plan de sondage.

**Questions traitées :** 19, 20, 21, 23, 24  

---

## Structure du projet

```
projet/
├── data/
│                      
├── output/
│   └── TP4/
└── TP4_Parcelles_GHS_W4.R
```

---

## Données requises

Télécharger depuis [microdata.worldbank.org/catalog/3557](https://microdata.worldbank.org/index.php/catalog/3557) (Nigeria GHS Panel Wave 4) :

| Fichier `.dta` | Visite | Contenu | Questions |
|----------------|--------|---------|-----------|
| `secta1_harvestw4.dta` | Post-Harvest | Roster parcelles — superficie GPS | Q19, Q20, Q23, Q24 |
| `secta_harvestw4.dta` | Post-Harvest | Poids de sondage (`wt_wave4`), strates, PSU | Toutes |
| `sect11b1_plantingw4.dta` | Post-Planting | Tenure foncière — mode d'acquisition (`s11b1q4`) | Q21 |

---

## Packages R requis

```r
install.packages(c(
  "haven", "dplyr", "tidyr", "ggplot2", "ggrepel",
  "scales", "patchwork", "rstatix", "gtsummary",
  "naniar", "viridis", "forcats",
  "officer", "flextable",
  "survey", "srvyr"
))
```

**Version R recommandée :** R ≥ 4.2

---

## Variables clés construites

| Variable | Source | Description |
|----------|--------|-------------|
| `superf_ha` | `sa1q11` ou `prefilled_gps_area` | Superficie en hectares (priorité re-mesure W4) |
| `superf_m2` | Idem | Superficie en m² |
| `seuil_h` | Calculé | Seuil outlier = Q3 + 3 × IQR |
| `mode_acq` | `s11b1q4` | Mode d'acquisition de la parcelle (7 modalités) |
| `titre_legal` | `s11b1q7` | Possession d'un titre légal (oui/non) |
| `wt_wave4` | `secta_harvestw4` | Poids de sondage transversal |

---

## Plan de sondage

```r
options(survey.lonely.psu = "adjust")

plan <- svydesign(
  ids     = ~ea,       # 402 PSU (Enumeration Areas)
  strata  = ~strata,   # 6 strates géographiques (zones)
  weights = ~wt_wave4, # Poids transversal (somme ≈ 27M ménages)
  nest    = TRUE,
  data    = données_filtrées
)
```

Toutes les estimations (médianes, moyennes, proportions) sont pondérées. Les intervalles de confiance sont produits par linéarisation de Taylor via `svymean()` et `svyquantile()`.

---

## Questions et méthodes

### Q19 — Superficie en ha, valeurs manquantes et aberrantes
- Construction de `superf_ha` depuis deux sources GPS
- Taux de disponibilité : 66,8% des parcelles
- Détection des outliers par règle **IQR × 3** (seuil ≈ 2,17 ha, 267 aberrants)
- Graphe : `gg_miss_var()` + boxplot avec jitter outliers

### Q20 — Analyse univariée de la superficie
- Histogramme log-normal, déciles, boxplot par zone (Urban/Rural)
- Scatter superficie déclarée W4 vs GPS W3 avec **corrélation de Spearman** et courbe LOESS

### Q21 — Régime de tenure foncière
- Variable `s11b1q4` (*sect11b1_plantingw4*, Post-Planting W4 uniquement)
- Proportions **pondérées** par mode d'acquisition
- Test du **chi-deux pondéré** (`svychisq`) tenure × zone + V de Cramér
- Graphes : barplot proportions + barplot 100% empilé par zone

### Q23 — Superficie totale vs nombre de parcelles
- Agrégation au niveau ménage
- **Corrélation de Spearman** + courbe LOESS

### Q24 — Heatmap superficie médiane par État
- **Médiane pondérée** par État via fonction `wtd_median_state()`
- Barplot ordonné coloré par quintile (palette viridis)

---

## Outputs produits

| Fichier | Description |
|---------|-------------|
| `q19_superficie_manquants_aberrants.png` | Valeurs manquantes + outliers |
| `q20_superficie_univarie.png` | Histogramme log, boxplot zone, scatter GPS |
| `q21_tenure_fonciere.png` | Régimes tenure + distribution par zone |
| `q23_scatter_superficie_parcelles.png` | Superficie totale vs nb parcelles |
| `q24_superficie_par_etat.png` | Superficie médiane par État (37 États) |
| `TP4_Parcelles_GHS_W4.docx` | Rapport Word complet (4 parties) |

---

## Rapport Word — Structure

| Partie | Contenu |
|--------|---------|
| **I. Introduction** | Contexte foncier nigérian, objectifs des 5 questions |
| **II. Données et méthodologie** | 3 fichiers, plan de sondage, méthodes statistiques |
| **III. Résultats** | Q19 à Q24 avec tableaux, figures et interprétations pondérées |
| **IV. Conclusion** | 4 enseignements clés + limites |
| **V. Références** | 6 références bibliographiques |

---

## Exécution

```r
# Adapter les chemins si nécessaire
path_raw <- "../data/"   # dossier contenant les .dta
path_out <- "../output/TP4/" # dossier de sortie (créé automatiquement)

# Lancer le script complet
source("TP4_Parcelles_GHS_W4.R")
```

Le script génère automatiquement les figures PNG et le rapport Word à la fin de l'exécution.

---

---

*GHS Panel Wave 4 — National Bureau of Statistics Nigeria / Banque Mondiale (LSMS-ISA)*
