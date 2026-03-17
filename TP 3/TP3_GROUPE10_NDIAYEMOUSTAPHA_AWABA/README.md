# TP3 — Accès aux Soins et Dépenses de Santé
## ENSAE ISE 1 | 2025-2026
**Auteurs :** Cheikh Mouhamdou Moustapha Ndiaye & Awa Ba

---

## Structure du dossier

```
TP3/
├── README.md
├── scripts/
│   └── TP3_Sante.R
├── outputs/
│   ├── T13_morbidite.png
│   ├── T14_types_maladies.png
│   ├── T15_recours_soins.png
│   ├── T16_depenses_sante.png
│   ├── T17_recours_quintile.png
│   ├── T18_depenses_rural_urbain.png
│   ├── TP3_Figure_Synthese.png
│   └── Dashboard_TP3.html
└── data/
    └── NGA_2018_GHSP-W4_v03_M_Stata12/
        ├── sect4a_harvestw4.dta
        ├── sect1_harvestw4.dta
        └── totcons_final.dta
```

---

## Notes méthodologiques critiques

### 1. Section santé = `sect4a` (PAS `sect3a`)
| Fichier | Contenu réel |
|---|---|
| `sect3a_harvestw4` | Emploi (heures travaillées, type activité) |
| `sect3b_harvestw4` | Assurance NHIS (enrollment ménage) |
| **`sect4a_harvestw4`** | ✅ **Vraie section santé** |

### 2. Taux de morbidité — construction correcte
`s4aq3` ("souffert d'une maladie en 4 sem.") a **4 385 NA** correspondant exactement aux individus qui ont **consulté** (`s4aq1=YES`). La raison de consultation (`s4aq2a`) confirme : 4 207 = maladie, 178 = blessure.

**Formule correcte :**  
`Malades = (s4aq3=YES) + (s4aq1=YES et s4aq2a=maladie/blessure)`  
`Taux = 6 450 / 26 556 = 24.3%`  
(Un taux de 7.8% calculé sur s4aq3 seul serait erroné)

### 3. Dépenses totales de santé — 4 composantes
| Variable | Contenu | N>0 | Médiane |
|---|---|---|---|
| `s4aq9` | Frais consultation | 1 631 | 500₦ |
| `s4aq10` | Transport aller-retour | 2 318 | 200₦ |
| `s4aq14` | Médicaments pharmacie/kiosque | 7 924 | 700₦ |
| `s4aq17` | Hospitalisation | 782 | 10 000₦ |
| **Total** | **Somme des 4** | **8 624** | **900₦** |

---

## Résultats clés

| Indicateur | Valeur |
|---|---|
| Taux de morbidité global | **24.3%** |
| Taux Hommes | 22.6% |
| Taux Femmes | 25.9% |
| Taux 60+ ans | 48.9% |
| 1ère cause | Paludisme (13.1%) |
| 1er recours | Chemist/Pharmacien (46%) |
| Médiane dépenses totales | 900₦ |
| Chi² consult × quintile | 430.96, V Cramér=0.127 |

---

## Exécution

```r
# Extraire NGA_2018_GHSP-W4_v03_M_Stata12.zip dans TP3/data/
# Ouvrir TP3/scripts/TP3_Sante.R dans RStudio → Source
```

*Nigeria GHS Panel W4 (2018) · World Bank LSMS-ISA · ENSAE ISE 1 · 2025-2026*
