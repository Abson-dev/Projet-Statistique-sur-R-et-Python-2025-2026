# TP2 : Explications techniques et méthodologiques
**Tests statistiques, graphiques et interprétations — Démystification complète**

---

## 1. Statistiques descriptives : comment les calculer et les interpréter

### 1.1 Fréquences et proportions (variables catégorielles)

**Cas d'usage** : variable `niveau_educ` (catégorielle ordinale à 5 modalités).

#### Calcul en R

```r
# Fréquences absolues
freq <- table(educ$niveau_educ)
#> Aucun Primaire Junior Secondary Senior Secondary Tertiaire
#>  8500     7000              3500              2500      1500

# Proportions (0 à 1)
prop <- prop.table(freq)
#> Aucun Primaire Junior Secondary Senior Secondary Tertiaire
#> 0.370   0.304            0.152            0.109      0.065

# Pourcentages (0 à 100)
pct <- round(prop * 100, 1)
#> Aucun Primaire Junior Secondary Senior Secondary Tertiaire
#>  37.0    30.4            15.2            10.9       6.5
```

#### Interprétation

- **Fréquence** : nombre d'observations dans chaque catégorie.
  - Exemple : 8 500 personnes n'ont aucune instruction.
- **Proportion** : fréquence divisée par le total (somme = 1).
  - Exemple : 0,370 = 37% de l'échantillon n'a aucune instruction.
- **Pourcentage** : proportion × 100 (somme = 100%).
  - Plus lisible que la proportion dans un rapport.

### 1.2 Médiane, quartiles, IQR (variables ordinales ou continues)

**Cas d'usage** : niveau d'éducation converti en numérique (1 = Aucun, 2 = Primaire, ..., 5 = Tertiaire).

#### Calcul en R

```r
# Médiane (Q2, 50e percentile)
median(niveau_educ_num)
#> 2  (= Primaire)

# Quartiles (Q1 = 25e, Q3 = 75e percentile)
quantile(niveau_educ_num, c(0.25, 0.75))
#> Q1: 1 (Aucun), Q3: 3 (Junior Secondary)

# Intervalle interquartile (IQR = Q3 - Q1)
IQR(niveau_educ_num)
#> 2
```

#### Interprétation

- **Médiane = 2 (Primaire)** :
  - 50% des individus ont un niveau ≤ Primaire.
  - 50% des individus ont un niveau ≥ Primaire.
  - La médiane est **robuste** aux valeurs extrêmes (contrairement à la moyenne).

- **Q1 = 1 (Aucun), Q3 = 3 (Junior Secondary)** :
  - 25% des individus n'ont aucune instruction.
  - 75% des individus ont atteint au moins le Junior Secondary.
  - L'**IQR = 2** mesure la dispersion de la moitié centrale de la distribution.

- **Pourquoi médiane et non moyenne ?** :
  - La variable est **ordinale** (catégories ordonnées mais distances inégales).
  - La différence Aucun → Primaire n'est pas égale à Primaire → Junior Secondary en années d'étude.
  - La médiane décrit le centre sans supposer des intervalles égaux.

---

## 2. Tests statistiques pour variables catégorielles

### 2.1 Test du chi-deux de Pearson (χ²)

#### Quand l'utiliser ?

- **Objectif** : tester si deux variables catégorielles sont **indépendantes** ou **associées**.
- **Exemple** : sexe (Homme/Femme) et niveau d'éducation (5 catégories) sont-ils indépendants ?

#### Hypothèses

- **H0** (hypothèse nulle) : les deux variables sont **indépendantes** (pas d'association).
- **H1** (hypothèse alternative) : les deux variables sont **associées** (dépendantes).

#### Conditions d'application

1. **Échantillon aléatoire** : les observations doivent être tirées au hasard (GHS Panel : ok).
2. **Effectifs théoriques ≥ 5** : chaque cellule du tableau de contingence doit avoir un effectif attendu ≥ 5.
   - Si violée : utiliser le test exact de Fisher (pour tableaux 2×2) ou regrouper les catégories.

#### Calcul en R

```r
# Tableau de contingence
tab <- table(educ$sexe, educ$niveau_educ)
#>         Aucun Primaire Junior Sec. Senior Sec. Tertiaire
#> Homme    3000     3800        1900        1400       800
#> Femme    5500     3200        1600        1100       700

# Test du chi-deux
chi2_test <- chisq.test(tab)
print(chi2_test)
#> Chi-squared = 842.5, df = 4, p-value < 2.2e-16
```

#### Interprétation

1. **Statistique χ² = 842,5** :
   - Mesure l'écart entre les **fréquences observées** (tableau réel) et les **fréquences théoriques** (si H0 vraie).
   - Plus χ² est grand, plus l'écart est important → plus l'association est forte.

2. **Degrés de liberté (df) = 4** :
   - Formule : df = (nb lignes − 1) × (nb colonnes − 1) = (2 − 1) × (5 − 1) = 4.

3. **p-value < 2,2e-16** (quasi nulle) :
   - Probabilité d'observer un χ² aussi élevé si H0 était vraie.
   - Règle de décision :
     - Si p < 0,05 : **rejet de H0** → association significative.
     - Si p ≥ 0,05 : **non-rejet de H0** → pas d'évidence d'association.
   - Ici : **p < 0,001** → rejet de H0 → **le sexe et l'éducation sont associés**.

#### Limites du chi-deux

- Le chi-deux indique **si** une association existe, mais **pas son intensité**.
- Avec un grand échantillon (n > 10 000), même une petite différence devient significative (p < 0,05).
- Solution : calculer le **V de Cramér** pour mesurer la **taille d'effet**.

---

### 2.2 V de Cramér (mesure de la taille d'effet)

#### Objectif

Mesurer l'**intensité de l'association** entre deux variables catégorielles, indépendamment de la taille d'échantillon.

#### Formule

$$
V = \sqrt{\frac{\chi^2}{n \times (k - 1)}}
$$

Où :
- χ² = statistique du chi-deux
- n = effectif total
- k = min(nb lignes, nb colonnes)

#### Calcul en R

```r
n <- sum(tab)                # effectif total
k <- min(nrow(tab), ncol(tab))  # min(2, 5) = 2
V <- sqrt(chi2_test$statistic / (n * (k - 1)))
#> V = 0.151
```

#### Interprétation (seuils de Cohen 1988)

Pour df* = k − 1 = 1 (tableaux 2×C) :

| V          | Interprétation       |
|------------|----------------------|
| < 0,10     | Effet négligeable    |
| 0,10–0,30  | Effet petit          |
| 0,30–0,50  | Effet moyen          |
| ≥ 0,50     | Effet grand          |

**Exemple** : V = 0,151 → **effet petit**.

- Le sexe et l'éducation sont **statistiquement associés** (p < 0,001).
- Mais l'association est **faible en intensité** : le sexe explique seulement **V² = 0,151² = 2,3%** de la variabilité du niveau d'éducation.
- **97,7% de la variabilité** s'explique par d'autres facteurs (zone géographique, richesse, religion).

#### Pourquoi V plutôt que χ² ?

- Le χ² dépend de la taille d'échantillon : χ² × 2 si n × 2.
- Le V est **standardisé** (varie de 0 à 1) et **indépendant** de n.
- Le V permet de **comparer** la force de l'association entre plusieurs analyses.

---

### 2.3 Test exact de Fisher (alternative au chi-deux)

#### Quand l'utiliser ?

- Tableaux 2×2 avec **effectifs théoriques < 5** dans au moins une cellule.
- Petits échantillons (n < 50).

#### Exemple

```r
tab_scol <- table(educ$milieu, educ$scolarise)
#>         Scolarisé Non scolarisé
#> Urbain       1200            300
#> Rural        2100            900

fisher_test <- fisher.test(tab_scol)
#> p-value = 1.8e-10
```

#### Interprétation

- **p < 0,001** → rejet de H0 → **association significative** entre milieu et scolarisation.
- Le test exact de Fisher calcule la p-value **exacte** (non approximative comme le chi-deux).

---

## 3. Tests statistiques pour variables ordinales ou continues

### 3.1 Test de Kruskal-Wallis (comparaison de K groupes indépendants)

#### Quand l'utiliser ?

- **Objectif** : comparer les **distributions** d'une variable ordinale ou continue entre K groupes (K ≥ 2).
- **Exemple** : le niveau d'éducation (variable ordinale) diffère-t-il entre 4 tranches d'âge (18–30, 31–45, 46–60, 60+) ?
- **Alternative non paramétrique à l'ANOVA** : ne suppose **pas** la normalité des distributions.

#### Hypothèses

- **H0** : les distributions sont identiques dans tous les groupes.
- **H1** : au moins un groupe diffère des autres.

#### Conditions d'application

1. **Variable ordinale ou continue** : niveau d'éducation (1 = Aucun, ..., 5 = Tertiaire).
2. **Groupes indépendants** : un individu appartient à une seule tranche d'âge.
3. **Aucune hypothèse de normalité** : le test compare les **rangs** (classement des valeurs).

#### Calcul en R

```r
kw_test <- kruskal.test(niveau_educ_num ~ groupe_age_educ, data = educ)
#> Kruskal-Wallis chi-squared = 387.2, df = 3, p-value < 2.2e-16
```

#### Interprétation

1. **Statistique H = 387,2** :
   - Variante du χ² qui compare la somme des rangs entre groupes.
   - Plus H est grand, plus les distributions diffèrent.

2. **df = 3** (K − 1 = 4 − 1 = 3).

3. **p-value < 0,001** :
   - Rejet de H0 → **au moins une tranche d'âge diffère des autres** en termes de niveau d'éducation.

#### Que faire après un Kruskal-Wallis significatif ?

Le test indique qu'il existe **au moins** une différence, mais ne dit pas **quels groupes** diffèrent.

→ **Post-hoc de Dunn** : compare toutes les paires de groupes.

---

### 3.2 Test post-hoc de Dunn (comparaisons multiples)

#### Objectif

Identifier **quels groupes** diffèrent significativement après un Kruskal-Wallis significatif.

#### Problème des comparaisons multiples

Avec 4 groupes, il y a **6 paires** possibles :
- 18–30 vs 31–45
- 18–30 vs 46–60
- 18–30 vs 60+
- 31–45 vs 46–60
- 31–45 vs 60+
- 46–60 vs 60+

Si on teste chaque paire au seuil α = 0,05, la probabilité de commettre **au moins une erreur de type I** (faux positif) est :

$$
1 - (1 - 0.05)^6 = 0.265 = 26.5\%
$$

→ **Risque inacceptable** : 1 chance sur 4 de conclure à tort à une différence.

#### Correction de Bonferroni

Pour contrôler le risque global à 5%, on ajuste le seuil de chaque test :

$$
\alpha_{\text{ajusté}} = \frac{\alpha}{m} = \frac{0.05}{6} = 0.0083
$$

- Une paire est significative si **p.adj < 0,05** (et non p < 0,05).

#### Calcul en R

```r
dunn_result <- educ %>%
  rstatix::dunn_test(niveau_educ_num ~ groupe_age_educ,
                     p.adjust.method = "bonferroni")

print(dunn_result)
#>   group1  group2  statistic p.value p.adj
#> 1 18-30   31-45      2.1    0.036   0.216  (NS après correction)
#> 2 18-30   46-60      8.5    <0.001  <0.001 (significatif)
#> 3 18-30   60+       15.2    <0.001  <0.001 (significatif)
#> 4 31-45   46-60      6.3    <0.001  <0.001 (significatif)
#> 5 31-45   60+       12.8    <0.001  <0.001 (significatif)
#> 6 46-60   60+        5.9    <0.001   0.001 (significatif)
```

#### Interprétation

- **Ligne 1** : 18–30 vs 31–45, p.adj = 0,216 > 0,05 → **non significatif**.
  - Les deux tranches ont un niveau d'éducation similaire (toutes deux bénéficient de l'UBE Act 2004).

- **Lignes 2–6** : toutes les autres paires sont significatives (p.adj < 0,05).
  - Conclusion : amélioration générationnelle nette entre générations éloignées.

---

## 4. Graphiques : construction et interprétation

### 4.1 Barplot horizontal (fréquences d'une variable catégorielle)

#### Code R (ggplot2)

```r
ggplot(freq_educ, aes(x = niveau_educ, y = effectif, fill = niveau_educ)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label = paste0(pourcentage, "%")), hjust = -0.1) +
  coord_flip() +
  scale_fill_manual(values = pal_educ) +
  labs(title = "Répartition par niveau d'éducation",
       x = "", y = "Effectif")
```

#### Éléments clés

1. **coord_flip()** : inverse les axes pour obtenir des barres horizontales.
   - Utile quand les labels de catégories sont longs (« Junior Secondary »).

2. **geom_text()** : affiche le pourcentage à droite de chaque barre.
   - hjust = -0.1 : décale le texte légèrement à droite pour éviter le chevauchement.

3. **scale_fill_manual()** : couleurs personnalisées (rouge → bleu selon le niveau).
   - Rouge (Aucun) = absence d'éducation.
   - Bleu foncé (Tertiaire) = éducation supérieure.

#### Interprétation

- **Barre la plus longue** = catégorie modale (« Aucun », 37%).
- **Ordre des barres** : respecte l'ordre logique (Aucun → Tertiaire).
- **Comparaison visuelle** : la barre « Aucun » est 2,5× plus longue que « Tertiaire ».

---

### 4.2 Barplot empilé à 100% (deux variables catégorielles)

#### Objectif

Comparer la **répartition relative** d'une variable (niveau d'éducation) entre les modalités d'une autre variable (sexe).

#### Code R

```r
prop_sexe_educ <- educ %>%
  count(sexe, niveau_educ) %>%
  group_by(sexe) %>%
  mutate(pct = n / sum(n) * 100) %>%
  ungroup()

ggplot(prop_sexe_educ, aes(x = sexe, y = pct, fill = niveau_educ)) +
  geom_col(position = "stack") +
  geom_text(aes(label = ifelse(pct >= 3, paste0(round(pct, 1), "%"), "")),
            position = position_stack(vjust = 0.5)) +
  scale_fill_manual(values = pal_educ) +
  labs(title = "Niveau d'éducation par sexe (adultes 18+)",
       y = "Pourcentage (%)")
```

#### Éléments clés

1. **position = "stack"** : empile les barres (somme = 100% par sexe).

2. **geom_text() + position_stack(vjust = 0.5)** : place le texte au centre de chaque segment.

3. **ifelse(pct >= 3, ...)** : affiche le % seulement si ≥ 3% (évite les segments trop petits).

#### Interprétation

- **Hommes** : barre rouge (« Aucun ») occupe ~25% de la hauteur.
- **Femmes** : barre rouge occupe ~42% de la hauteur.
- → **Écart visuel immédiat** : les femmes ont une proportion plus élevée sans instruction.

#### Piège à éviter

- **Ne pas confondre** :
  - **Hauteur de la barre** = 100% (normalisé par sexe).
  - **Largeur de chaque segment coloré** = pourcentage de ce niveau d'éducation dans ce sexe.

---

### 4.3 Barplot groupé avec intervalles de confiance

#### Objectif

Comparer des **proportions** (taux de scolarisation) entre groupes (urbain/rural) avec leur **incertitude** (IC 95%).

#### Code R

```r
# Calcul des IC 95% (Clopper-Pearson)
taux_par_milieu <- educ %>%
  group_by(milieu) %>%
  summarise(
    n_total = n(),
    n_scol = sum(scolarise == "Scolarisé"),
    taux = n_scol / n_total * 100
  ) %>%
  rowwise() %>%
  mutate(
    test = list(binom.test(n_scol, n_total, conf.level = 0.95)),
    ic_bas = test$conf.int[1] * 100,
    ic_haut = test$conf.int[2] * 100
  )

ggplot(taux_par_milieu, aes(x = milieu, y = taux, fill = milieu)) +
  geom_col() +
  geom_errorbar(aes(ymin = ic_bas, ymax = ic_haut), width = 0.15) +
  geom_text(aes(label = sprintf("%.1f%%", taux)), vjust = -1.5) +
  labs(title = "Taux de scolarisation des 6-17 ans par milieu",
       y = "Taux (%)")
```

#### Éléments clés

1. **geom_errorbar()** : barres d'erreur (IC 95%).
   - Largeur (width = 0,15) contrôle l'épaisseur horizontale.

2. **binom.test()** : calcule l'IC exact de Clopper-Pearson.
   - Préféré à l'approximation normale (p ± 1,96 × SE) pour éviter des bornes < 0 ou > 100%.

3. **vjust = -1,5** : place le texte au-dessus de la barre d'erreur.

#### Interprétation

- **Urbain** : 78,5% [75,2% ; 81,5%]
- **Rural** : 61,3% [58,9% ; 63,6%]

**Conclusion** :
- Les IC **ne se chevauchent pas** → différence significative (p < 0,05).
- Écart = 78,5% − 61,3% = **17,2 points de pourcentage**.

---

### 4.4 Boxplot (distribution d'une variable ordinale par groupe)

#### Objectif

Visualiser la **médiane**, les **quartiles** et les **valeurs extrêmes** d'une variable ordinale/continue par groupe.

#### Code R

```r
ggplot(educ, aes(x = groupe_age_educ, y = niveau_educ_num, fill = groupe_age_educ)) +
  geom_boxplot() +
  scale_y_continuous(
    breaks = 1:5,
    labels = c("Aucun", "Primaire", "Junior Sec.", "Senior Sec.", "Tertiaire")
  ) +
  labs(title = "Niveau d'éducation par tranche d'âge",
       x = "Groupe d'âge", y = "Niveau d'éducation")
```

#### Éléments du boxplot

```
      |            <- Valeur max (ou 1.5 × IQR au-dessus de Q3)
      |
   ---+---   Q3   <- Troisième quartile (75e percentile)
   |     |
   |     |
   ---+---   Médiane (Q2, 50e percentile)
   |     |
   |     |
   ---+---   Q1   <- Premier quartile (25e percentile)
      |
      |            <- Valeur min (ou 1.5 × IQR en dessous de Q1)
      •            <- Outlier (valeur au-delà de 1.5 × IQR)
```

#### Interprétation

- **Médiane** (ligne épaisse au centre de la boîte) :
  - 18–30 : médiane = 2 (Primaire)
  - 60+ : médiane = 1 (Aucun)
  - → Les jeunes ont un niveau médian supérieur.

- **Boîte** (Q1 à Q3) :
  - Contient 50% des observations centrales.
  - Plus la boîte est courte, plus la distribution est concentrée.

- **Moustaches** (whiskers) :
  - S'étendent jusqu'à la valeur min/max ou 1,5 × IQR.

- **Points isolés** (outliers) :
  - Valeurs au-delà de 1,5 × IQR.
  - Exemple : un individu de 60+ ayant un niveau Tertiaire (rare).

---

### 4.5 Heatmap (geom_tile)

#### Objectif

Visualiser une **matrice de proportions** (État × Niveau d'éducation) sous forme de carte de chaleur.

#### Code R

```r
prop_state_educ <- educ %>%
  count(state_name, niveau_educ) %>%
  group_by(state_name) %>%
  mutate(pct = n / sum(n) * 100) %>%
  ungroup()

ggplot(prop_state_educ, aes(x = niveau_educ, y = state_name, fill = pct)) +
  geom_tile(color = "white", linewidth = 0.3) +
  geom_text(aes(label = ifelse(pct >= 5, sprintf("%.0f%%", pct), "")),
            size = 2.2, color = "white") +
  scale_fill_viridis_c(option = "B", name = "Proportion (%)") +
  labs(title = "Niveau d'éducation par état", x = "Niveau", y = "")
```

#### Éléments clés

1. **geom_tile()** : crée une tuile (rectangle) pour chaque cellule (État × Niveau).

2. **color = "white", linewidth = 0.3** : bordures blanches entre les tuiles.

3. **scale_fill_viridis_c()** : palette de couleurs continue (magma, inferno, viridis, plasma).
   - Option "B" (Magma) : noir (valeurs basses) → jaune (valeurs hautes).
   - Accessible aux daltoniens.

4. **geom_text()** : affiche le % dans chaque tuile (si ≥ 5%).

#### Interprétation

- **Lecture horizontale** (par État) :
  - Sokoto : tuile rouge très grande dans "Aucun" (70%), tuiles bleues absentes dans "Tertiaire".
  - Lagos : tuile rouge petite dans "Aucun" (10%), tuiles bleues présentes dans "Tertiaire".

- **Lecture verticale** (par niveau) :
  - Colonne "Aucun" : dégradé Sud (bleu foncé, faible %) → Nord (jaune/blanc, fort %).
  - Colonne "Tertiaire" : inverse.

- **Conclusion visuelle** : fracture Nord-Sud immédiatement identifiable.

---

## 5. Intervalles de confiance : pourquoi et comment

### 5.1 Pourquoi calculer un intervalle de confiance ?

**Problème** : on estime une proportion (ex : taux de scolarisation urbain = 78,5%) sur un **échantillon** (1 500 enfants urbains). Mais quel est le taux **réel** dans la **population** complète (tous les enfants urbains du Nigeria) ?

**Réponse** : on ne peut jamais le savoir exactement. Mais on peut construire un **intervalle de confiance à 95%** :

> « Avec 95% de confiance, le taux réel est compris entre 75,2% et 81,5%. »

**Interprétation** :
- Si on répétait l'enquête 100 fois sur 100 échantillons différents, **95 fois sur 100**, l'intervalle contiendrait la vraie valeur.
- **5 fois sur 100**, l'intervalle ne contiendrait pas la vraie valeur (erreur d'échantillonnage).

### 5.2 Méthode de Clopper-Pearson (IC exact pour proportions)

#### Quand l'utiliser ?

- Variable binaire (Scolarisé/Non scolarisé).
- Petits échantillons ou proportions extrêmes (p proche de 0 ou 1).

#### Calcul en R

```r
binom.test(n_scol, n_total, conf.level = 0.95)$conf.int
#> [1] 0.7523 0.8147  (urbain : 75,2% - 81,5%)
```

#### Avantages sur l'approximation normale

**Approximation normale** (formule de Wald) :

$$
\text{IC}_{95\%} = p \pm 1.96 \times \sqrt{\frac{p(1-p)}{n}}
$$

**Problèmes** :
1. Peut produire des bornes < 0 ou > 1 (absurde pour une proportion).
2. Couverture insuffisante (< 95%) si p proche de 0 ou 1.

**Clopper-Pearson** :
- Garantit une couverture ≥ 95% dans tous les cas.
- Méthode recommandée par l'OMS et l'UNFPA pour les enquêtes ménages.

---

## 6. P-value : ce qu'elle signifie (et ce qu'elle ne signifie PAS)

### 6.1 Définition correcte

La **p-value** est la probabilité d'observer un résultat **aussi extrême ou plus extrême** que celui observé, **si H0 était vraie**.

**Exemple** : test du chi-deux pour sexe × éducation, p = 1,2e-16.

**Interprétation** :
- Si le sexe et l'éducation étaient **vraiment indépendants** (H0), la probabilité d'obtenir un χ² ≥ 842,5 serait quasi nulle (< 0,000 000 000 000 000 1).
- → Conclusion : H0 est **incompatible** avec les données → on **rejette H0**.

### 6.2 Ce que la p-value NE signifie PAS

❌ **« p < 0,05 → l'effet est important »** :
- Faux. La p-value mesure la **significativité statistique**, pas la **taille d'effet**.
- Avec n = 100 000, même un effet minuscule (V = 0,05) donne p < 0,001.

❌ **« p = 0,04 → H0 est fausse avec 96% de certitude »** :
- Faux. La p-value n'est **pas** la probabilité que H0 soit vraie.
- Elle mesure la compatibilité des données avec H0.

❌ **« p > 0,05 → H0 est vraie »** :
- Faux. « Absence de preuve ≠ preuve d'absence ».
- p = 0,08 signifie qu'on n'a pas assez d'évidence pour rejeter H0, mais H0 pourrait quand même être fausse (manque de puissance).

### 6.3 Seuils de décision

| p-value   | Décision        | Formulation |
|-----------|-----------------|-------------|
| < 0,001   | Très significatif | p < 0,001 |
| < 0,01    | Significatif      | p < 0,01  |
| < 0,05    | Significatif      | p < 0,05  |
| 0,05–0,10 | Marginalement significatif (interpréter avec prudence) | p = 0,08 |
| ≥ 0,10    | Non significatif  | p = 0,15 (NS) |

---

## 7. Résumé : quel test pour quelle situation ?

| Objectif | Variables | Test | Mesure d'effet | Graphique |
|----------|-----------|------|----------------|-----------|
| Comparer des proportions (2 groupes) | 1 catégorielle binaire, 1 catégorielle binaire | Chi-deux ou Fisher | V de Cramér | Barplot groupé + IC |
| Comparer des proportions (2+ groupes) | 1 catégorielle, 1 catégorielle | Chi-deux | V de Cramér | Barplot empilé 100% |
| Comparer des distributions (2 groupes) | 1 ordinale/continue, 1 catégorielle binaire | Wilcoxon-Mann-Whitney | r de rang | Boxplot |
| Comparer des distributions (K groupes) | 1 ordinale/continue, 1 catégorielle (K niveaux) | Kruskal-Wallis + Dunn | η² (effet taille ANOVA) | Boxplot groupé |

---

## 8. Checklist : comment rapporter un test statistique

Pour **chaque test**, rapportez :

1. **Nom du test** : « Test du chi-deux de Pearson »
2. **Hypothèses** : H0 et H1 en langage clair
3. **Statistique de test** : χ² = 842,5
4. **Degrés de liberté** : df = 4
5. **P-value** : p < 0,001 (ou p = 1,2e-16 si très petite)
6. **Décision** : rejet ou non-rejet de H0 au seuil α = 0,05
7. **Mesure d'effet** : V = 0,151 (effet petit, 2,3% de variance expliquée)
8. **Interprétation contextualisée** : « Le sexe et l'éducation sont significativement associés, mais l'association est faible. La zone géographique joue un rôle bien plus important. »

**Exemple complet** :

> Un test du chi-deux de Pearson a été réalisé pour tester l'indépendance entre le sexe et le niveau d'éducation chez les adultes de 18 ans et plus (n = 23 000). Les résultats montrent une association statistiquement significative (χ² = 842,5, df = 4, p < 0,001). Cependant, la taille d'effet est faible (V de Cramér = 0,151), indiquant que le sexe explique seulement 2,3% de la variabilité du niveau d'éducation. Les femmes ont une proportion plus élevée sans instruction (42%) que les hommes (27%), reflétant les inégalités historiques de genre dans l'accès à l'éducation au Nigeria.

---

**Auteure** : Leslye Patricia Nkwa Tsamo
**Formation** : ENSAE Pierre Ndiaye — ISE3
**Date** : Mars 2026
