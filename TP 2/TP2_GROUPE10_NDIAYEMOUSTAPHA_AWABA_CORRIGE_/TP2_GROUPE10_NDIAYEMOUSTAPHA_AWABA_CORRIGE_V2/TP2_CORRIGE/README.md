# TP2 — Éducation et Alphabétisation
## ENSAE ISE 1 | 2025-2026
**Auteurs :** Cheikh Mouhamdou Moustapha Ndiaye & Awa Ba

---

## Structure du dossier

```
TP2/
├── README.md
├── scripts/
│   └── TP2_Education.R
├── outputs/
│   ├── T8_educ_distribution.png
│   ├── T9_educ_par_sexe.png
│   ├── T10_age_education.png
│   ├── T11_scolarisation.png
│   ├── T12_heatmap_etats.png
│   ├── TP2_Figure_Synthese.png
│   └── Dashboard_TP2.html
└── data/
    └── NGA_2018_GHSP-W4_v03_M_Stata12/
        ├── sect2_harvestw4.dta
        └── sect1_harvestw4.dta
```

---

## Notes méthodologiques critiques

### 1. Structure des fichiers selon les vagues
- **W1/W2** : deux fichiers séparés — `sect2a` (ex-scolarisés, niveau complété `s2aq9`) et `sect2b` (encore scolarisés, niveau en cours `s2bq3`)
- **W3/W4** : **un seul fichier** `sect2_harvestw4` — toute la population ≥3 ans

### 2. Classification "Aucun" — correction critique
`s2aq9` (niveau le plus élevé) est `NA` pour les **3 054 adultes** qui ont répondu `s2aq6 = "No"` (jamais allé à l'école). Sans traitement, ils seraient **exclus silencieusement**.  
**Règle appliquée :** `s2aq6 = "No"` → classé directement **"Aucun"**

| | Sans correction | Avec correction |
|---|---|---|
| **Aucun** | 1 060 (10.2%) | **4 114 (30.7%)** |
| **N adultes analysés** | 10 345 | **13 399** |

### 3. Variable de scolarisation — W4 scinde la question
- `s2aq13` = fréquenté en **2017/2018** (année passée) → NE PAS utiliser
- `s2aq13a` = fréquente en **2018/2019** (actuelle) → **Variable utilisée** ✓

---

## Résultats clés (adultes 18+, N=13 399)

| Niveau | N | % |
|---|---|---|
| Aucun | 4 114 | 30.7% |
| Primaire | 2 630 | 19.6% |
| Junior Sec. | 738 | 5.5% |
| Senior Sec. | 4 387 | 32.7% |
| Tertiaire | 1 530 | 11.4% |

- Chi² éducation × sexe : **541.23**, V Cramér = **0.201**
- Scolarisation Urbain : **88.5%** vs Rural : **72.5%**

---

## Exécution

```r
# Extraire NGA_2018_GHSP-W4_v03_M_Stata12.zip dans TP2/data/
# Ouvrir TP2/scripts/TP2_Education.R dans RStudio → Source
```

*Nigeria GHS Panel W4 (2018) · World Bank LSMS-ISA · ENSAE ISE 1 · 2025-2026*
