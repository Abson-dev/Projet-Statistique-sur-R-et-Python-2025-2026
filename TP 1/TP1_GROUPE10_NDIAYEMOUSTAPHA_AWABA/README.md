# TP1 — Profil Démographique des Ménages Nigérians
## ENSAE ISE 1 | 2025-2026
**Auteurs :** Cheikh Mouhamdou Moustapha Ndiaye & Awa Ba

---

## Structure du dossier

```
TP1/
├── README.md
├── scripts/
│   └── TP1_Demographie.R
├── outputs/
│   ├── T1_missing.png
│   ├── T2_age_univarie.png
│   ├── T3_pyramide_ages.png
│   ├── T4_lien_parente.png
│   ├── T5_taille_menage.png
│   ├── T5b_zones.png
│   ├── TP1_Figure_Synthese.png
│   └── Dashboard_TP1.html
└── data/
    └── NGA_2018_GHSP-W4_v03_M_Stata12/   ← extraire le zip ici
        ├── sect1_harvestw4.dta
        └── secta_harvestw4.dta
```

---

## Bases utilisées

| Fichier | Variables clés |
|---|---|
| `sect1_harvestw4.dta` | `s1q4a` (membre présent), `s1q2` (sexe), `s1q4` (âge), `s1q3` (lien parenté), `sector` |
| `secta_harvestw4.dta` | `sector`, `zone`, `state`, `lga` |

---

## Note méthodologique critique — Filtre `s1q4a`

GHS W4 est une **enquête panel longitudinale**. Le fichier `sect1_harvestw4` contient **30 337 lignes** mais inclut 3 780 membres qui ont **quitté le ménage** entre la vague précédente et W4 (`s1q4a = "NO"`). Ces individus n'ont aucune donnée démographique (âge, sexe = NA).

**Sans filtre** : 30 337 obs · taille ménage Rural méd=6, Urbain méd=5  
**Avec filtre** : 26 557 obs · taille ménage Rural méd=5, Urbain méd=4 ✓

Le script applique ce filtre dès le chargement.

---

## Résultats clés

| Indicateur | Valeur |
|---|---|
| Membres présents W4 | 26 557 |
| Ménages W4 | 4 980 |
| Âge médian | 18 ans |
| Âge moyen | 24.0 ans |
| CV âge | 82.2% |
| Asymétrie Pearson | 0.910 |
| Shapiro-Wilk | W=0.9009, p<0.001 → NON-NORMALE |
| Taille méd. ménage Rural | 5 personnes |
| Taille méd. ménage Urbain | 4 personnes |
| Wilcoxon Rural vs Urbain | W=3 095 898, p<0.001, r=0.573 (fort) |

---

## Exécution

```r
# Extraire NGA_2018_GHSP-W4_v03_M_Stata12.zip dans TP1/data/
# Ouvrir TP1/scripts/TP1_Demographie.R dans RStudio → Source
```

*Nigeria GHS Panel W4 (2018) · World Bank LSMS-ISA · ENSAE ISE 1 · 2025-2026*
