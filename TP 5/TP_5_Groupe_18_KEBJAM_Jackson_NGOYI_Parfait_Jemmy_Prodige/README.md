# Analyse des Pratiques Agricoles au Nigeria

## LSMS-ISA Nigeria — Vague 4 (2018-2019)

### Auteurs
- **KEBJAM Jackson**
- **NGOYI Parfait Jemmy Prodige**

### Groupe
Groupe 18 — ISE 1, ENSAE (Année académique 2025/2026)

---

## Présentation du projet

Ce projet réalise une analyse approfondie des pratiques agricoles des ménages nigérians à partir de la **quatrième vague (2018-2019)** de l'enquête *Living Standards Measurement Study — Integrated Surveys on Agriculture* (LSMS-ISA), conduite par la Banque mondiale en partenariat avec le Bureau national de statistiques du Nigeria (NBS).

---

## Objectifs

L'étude examine **cinq aspects essentiels** de l'agriculture nigériane :

1. La structure culturale nationale (Top 15 des cultures)
2. La diversification culturale selon le milieu de résidence (rural/urbain)
3. L'utilisation des engrais par type et par zone
4. La distribution des rendements par État pour le maïs et le millet
5. La relation entre l'usage d'engrais chimique et le niveau de rendement

---

## Structure du rapport

| Tâche | Titre | Description |
|-------|-------|-------------|
| 25 | Cultures les plus pratiquées | Classement pondéré des 15 cultures principales |
| 26 | Diversification culturale | Comparaison rural/urbain de l'indice de diversification |
| 27 | Utilisation des engrais | Taux d'utilisation par type (NPK, Urée, organique) et par zone |
| 28 | Rendements par État | Distribution des rendements du maïs et du millet |
| 29 | Engrais et rendement | Association entre engrais chimique et niveau de rendement |

---

## Données utilisées

| Base | Contenu | Usage |
|------|---------|-------|
| `secta3ii` | Cultures déclarées par ménage-parcelle | Top 15 cultures |
| `secta3` | Cultures par ménage | Diversification culturale |
| `secta11c2` | Utilisation des intrants agricoles | Engrais par zone |
| `secta_rendement` | Rendements calculés (kg/ha) | Rendements par État |

**Pondération** : Toutes les estimations sont pondérées par `wt_wave4` (poids ménage) pour garantir la représentativité nationale.

---

## Méthodes statistiques mobilisées

| Méthode | Utilisation |
|---------|-------------|
| Pondération par poids de sondage | Toutes les analyses |
| Test de Wilcoxon-Mann-Whitney | Comparaison rural/urbain (diversification, rendements) |
| Coefficient *r* de Rosenthal | Taille d'effet des différences |
| Méthode de Wilson | Intervalles de confiance des proportions |
| Test du chi-deux | Association zone × usage d'engrais |
| Boxplots triés par médiane | Visualisation des rendements par État |

---

## Principaux résultats

### 1. Structure culturale
- **Le maïs domine** très nettement le paysage cultural nigérian
- Les céréales (maïs, sorgho, millet) concentrent plus de 43 % des fréquences pondérées
- Légumineuses (niébé, arachide) et tubercules (manioc, igname) constituent le second plan

### 2. Diversification culturale
- Diversification **plus élevée en milieu rural** (test de Wilcoxon : p < 0,001)
- Taille d'effet faible (r = 0,131) : différence réelle mais modeste
- La polyculture rurale est une stratégie de résilience face aux aléas

### 3. Utilisation des engrais
- Association **significative** entre zone et usage d'engrais chimique (p < 0,001)
- Engrais organiques plus fréquents en milieu rural
- Forte **hétérogénéité géographique** : certains États du Nord (taux élevés) contrastent avec le Sud-Est (taux faibles)

### 4. Rendements par État
- **Maïs** : États de la ceinture centrale (Benue, Plateau, Nasarawa) aux rendements les plus élevés ; Nord-Est pénalisé par le déficit hydrique
- **Millet** : dispersion inter-États plus modérée (culture adaptée aux zones sahéliennes)
- Forte hétérogénéité individuelle au sein de chaque État

### 5. Engrais chimique et rendement
- Gain médian associé à l'usage d'engrais : **+233 kg/ha** (p < 0,001)
- Taille d'effet faible (r = 0,189) : l'engrais n'explique qu'une partie de la variabilité
- L'amélioration durable des rendements nécessite une **approche intégrée** (semences, eau, conseil, crédit)

---

## Structure des fichiers
Groupe_18_KEBJAM_Jackson_NGOYI_Parfait_Jemmy_Prodige/
│
├── rapport_final.Rmd # Script principal R Markdown
├── rapport_final.pdf # Rapport final compilé
├── README.md # Ce fichier
│
├── R/
│ └── fonctions.R # Fonctions personnalisées
│
├── data/
│ └── processed/ # Données nettoyées
│ ├── secta3ii.rds
│ ├── secta3.rds
│ ├── secta11c2.rds
│ └── secta_rendement.rds
│
├── output/
│ ├── figures/ # Graphiques générés
│ └── tables/ # Tableaux générés
│
└── preamble.tex # Préambule LaTeX personnalisé