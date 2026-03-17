# TP2 : Interprétations attendues des résultats
**Éducation et alphabétisation — Nigeria GHS Panel W4 (2018-2019)**

---

## Tâche 8 : Répartition par niveau d'éducation

### Résultats attendus

**Distribution attendue** (adultes et enfants avec données éducation) :

| Niveau d'éducation     | Pourcentage attendu | Interprétation |
|------------------------|---------------------|----------------|
| Aucun                  | 35–42%              | Catégorie modale |
| Primaire               | 28–33%              | Deuxième catégorie |
| Junior Secondary       | 12–17%              | Système 6-3-3-4 |
| Senior Secondary       | 8–12%               | Abandon avant SSS |
| Tertiaire              | 4–8%                | Élite urbaine |

### Interprétation contextuelle

**Pourquoi "Aucun" est si élevé ?**

1. **10,5 millions d'enfants non scolarisés en 2018** (UNICEF) — le chiffre le plus élevé au monde.

2. **Génération âgée (60+)** : scolarisés avant 1976 (Universal Primary Education), époque où le taux de scolarisation primaire était < 40%.

3. **Fracture Nord-Sud** : dans le Nord-Ouest et le Nord-Est, 60–70% des adultes n'ont jamais fréquenté l'école formelle (préférence pour l'éducation coranique).

4. **Insécurité** : Boko Haram (dont le nom signifie « l'éducation occidentale est interdite ») a détruit 900+ écoles entre 2009 et 2018, privant des centaines de milliers d'enfants d'accès.

**Pourquoi le tertiaire est si faible ?**

- Accès réservé à une élite urbaine et économiquement favorisée.
- Coût prohibitif (universités fédérales : 50 000–150 000 ₦/an, universités privées : 500 000–2 000 000 ₦/an).
- Sous-financement des universités publiques → grèves chroniques, dégradation de la qualité.
- Brain drain : les diplômés du tertiaire migrent vers l'étranger (USA, UK, Canada).

**Comparaison avec d'autres pays subsahariens** :
- Ghana (2017) : 25% "Aucun", 15% Tertiaire — meilleure performance.
- Niger (2012) : 65% "Aucun", 2% Tertiaire — pire performance.
- Nigeria : position intermédiaire reflétant les grandes disparités internes.

---

## Tâche 9 : Éducation x Sexe (adultes 18+)

### Résultats attendus

**Proportions par sexe dans "Aucun"** :
- **Hommes** : 25–30% sans instruction
- **Femmes** : 40–50% sans instruction

**Écart de 10–20 points de pourcentage** entre hommes et femmes.

**Test du chi-deux** :
- Chi² attendu : 500–1500 (dépend de l'effectif)
- p-value : < 0,001 → rejet de H0
- **Conclusion** : association significative entre sexe et niveau d'éducation.

**V de Cramér** :
- Valeur attendue : **0,10–0,25**
- **Interprétation** : effet **petit à moyen** (Cohen 1988).
- Le sexe explique **1–6% de la variabilité** du niveau d'éducation (r² = V²).
- **98–99% de la variabilité** s'explique par d'autres facteurs : zone géographique (Nord vs Sud), richesse, religion, État de résidence.

### Interprétation contextuelle

**Pourquoi cet écart de genre ?**

1. **Mariage précoce** : 43% des filles nigérianes sont mariées avant 18 ans (DHS 2018), principalement dans le Nord. Le mariage met fin à la scolarisation.

2. **Travail domestique** : les filles sont retirées de l'école pour aider aux tâches ménagères, garder les jeunes frères et sœurs, vendre au marché.

3. **Normes culturelles** : dans certaines communautés du Nord, l'éducation des filles est perçue comme inutile (« une fille éduquée ne se marie pas ») voire dangereuse (risque de « corruption morale »).

4. **Insécurité** : enlèvements de filles scolarisées par Boko Haram :
   - Cas de Chibok (2014) : 276 lycéennes enlevées.
   - Cas de Dapchi (2018) : 110 lycéennes enlevées (5 mortes, 1 retenue).

5. **Coût d'opportunité** : dans les familles pauvres, si un seul enfant peut être scolarisé, c'est le garçon (futur « breadwinner »).

**Évolution générationnelle** :
- L'écart de genre **se réduit** chez les jeunes adultes urbains (18–30 ans) grâce aux programmes de bourses ciblant les filles (Girl's Education Project, UNICEF Girls' Education Programme Phase III).
- Le Gender Parity Index (GPI) est passé de 0,75 (2003) à 0,86 (2018) au primaire.
- Mais dans le Nord-Ouest, le GPI reste < 0,70 (UNICEF Nigeria 2018).

**Pourquoi le V de Cramér est faible malgré un chi-deux très significatif ?**
- Le chi-deux mesure la **significativité statistique** (dépend de la taille d'échantillon).
- Le V de Cramér mesure l'**ampleur de l'association** (indépendant de la taille d'échantillon).
- Avec n > 13 000 adultes, même une petite différence devient significative (p < 0,001).
- Mais le V ~ 0,15 indique que le sexe n'explique que 2–3% de la variabilité.
- **Conclusion** : le sexe **compte**, mais la **zone géographique** et la **richesse** comptent bien **plus** (V attendu > 0,40 pour zone géopolitique x éducation).

**Référence** : Okonkwo & Obidike (2016), *Gender disparity in education in Nigeria*, Journal of Education and Practice, 7(12), 50–58.

---

## Tâche 10 : Relation âge–éducation (adultes 18+)

### Résultats attendus

**Niveau d'éducation médian par tranche d'âge** :

| Groupe d'âge | Niveau médian attendu         | Code numérique |
|--------------|-------------------------------|----------------|
| 18–30        | Primaire / Junior Secondary   | 2–3            |
| 31–45        | Primaire                       | 2              |
| 46–60        | Aucun / Primaire               | 1–2            |
| 60+          | Aucun                          | 1              |

**Test de Kruskal-Wallis** :
- Chi² attendu : 200–600
- p-value : < 0,001 → rejet de H0
- **Conclusion** : le niveau d'éducation diffère significativement entre les groupes d'âge.

**Post-hoc de Dunn (correction de Bonferroni)** :

Comparaisons significatives attendues (p.adj < 0,05) :
- **18–30 vs 60+** : différence très significative (p < 0,001)
- **18–30 vs 46–60** : différence significative (p < 0,01)
- **31–45 vs 60+** : différence significative (p < 0,01)

Comparaisons **non significatives** après correction :
- **18–30 vs 31–45** : p.adj > 0,05 (générations proches, bénéficient toutes deux de l'UBE Act de 2004)
- **46–60 vs 60+** : p.adj > 0,05 (générations pré-UPE 1976, faible accès)

### Interprétation contextuelle

**Amélioration générationnelle : pourquoi les jeunes sont plus éduqués ?**

1. **Universal Primary Education (UPE) de 1976** :
   - Générations nées après 1970 (aujourd'hui 46–55 ans) ont bénéficié de l'expansion massive de l'accès primaire.
   - Taux de scolarisation primaire passe de 32% (1970) à 65% (1980).

2. **Universal Basic Education (UBE) Act de 2004** :
   - Générations nées après 1998 (aujourd'hui < 28 ans) bénéficient de la gratuité théorique du primaire + JSS.
   - Taux de scolarisation primaire atteint 68% en 2018.

3. **Urbanisation** :
   - Les jeunes adultes (18–30) vivent davantage en ville que leurs parents (migration rurale-urbaine).
   - Taux d'urbanisation : 27% (1980) → 52% (2018).
   - Accès aux écoles urbaines > rural.

4. **Valorisation sociale de l'éducation** :
   - Les parents ayant peu ou pas d'éducation (génération 46–60) ont souvent fait de gros sacrifices pour scolariser leurs enfants.
   - Perception croissante de l'éducation comme voie de mobilité sociale.

**Pourquoi la différence 18–30 vs 31–45 n'est pas significative ?**
- L'UBE Act de 2004 a bénéficié aux 18–30 ans (nés 1988–2000).
- Mais les 31–45 ans (nés 1973–1987) avaient déjà bénéficié de l'UPE de 1976.
- Les deux générations partagent un accès élevé au primaire et une partie du secondaire.
- L'écart réel se situe entre ces générations et les 60+ (pré-UPE).

**Référence** : Humphreys & Crawford (2014), *Improving equity and access for marginalised children in Nigeria*, Comparative Education, 50(4), 483–497.

---

## Tâche 11 : Scolarisation des 6–17 ans (rural vs urbain)

### Résultats attendus

**Taux de scolarisation** :
- **Urbain** : 75–85%
- **Rural** : 55–65%

**Écart de 15–25 points de pourcentage**.

**Test du chi-deux** :
- Chi² attendu : 50–200
- p-value : < 0,001 → rejet de H0
- **Conclusion** : association significative entre milieu et scolarisation.

**V de Cramér** :
- Valeur attendue : **0,15–0,30** (effet petit à moyen)

**Intervalle de confiance (Clopper-Pearson) 95%** :
- Urbain : [73% ; 83%] (précis car n urbain ~ 1 500)
- Rural : [53% ; 63%] (précis car n rural ~ 3 500)

### Interprétation contextuelle

**Pourquoi le taux de scolarisation rural est si bas ?**

1. **Distance aux écoles** :
   - En milieu rural, 35% des enfants vivent à > 5 km de l'école la plus proche (Federal Ministry of Education, 2018).
   - Pas de transport scolaire → marche quotidienne de 2–3 heures → abandon.
   - Insécurité sur les trajets (enlèvements, violence).

2. **Coût d'opportunité** :
   - Travail des enfants dans l'agriculture familiale (semis, récolte, garde du bétail).
   - Pic d'absentéisme pendant la saison des pluies (mai–juillet) et la récolte (octobre–décembre).
   - Revenus tirés du travail des enfants (vente au marché, collecte de bois) nécessaires à la survie du ménage.

3. **Pauvreté** :
   - Bien que l'UBE Act rende l'éducation gratuite, les frais indirects persistent :
     - Uniformes : 2 000–5 000 ₦/an
     - Fournitures : 1 000–3 000 ₦/an
     - Cotisations PTA (Parent-Teacher Association) : 500–2 000 ₦/an
   - Pour un ménage rural gagnant 50 000 ₦/mois (~ 100 USD), scolariser 3 enfants = 10–15% du revenu annuel.

4. **Qualité médiocre des écoles rurales** :
   - Sureffectifs : 70–100 élèves par classe (vs 35–50 en urbain).
   - Enseignants non qualifiés : 40% des enseignants ruraux n'ont pas de NCE (Nigeria Certificate in Education).
   - Infrastructures dégradées : 30% des écoles rurales n'ont pas de toilettes fonctionnelles, 50% n'ont pas d'eau potable (UNICEF Nigeria 2018).
   - Absentéisme des enseignants : 25–40% en zone rurale vs 10–15% en urbain.

5. **Mariage précoce (filles rurales)** :
   - Âge médian au premier mariage : 16 ans en rural vs 21 ans en urbain (DHS 2018).
   - Les filles rurales sont retirées de l'école dès 12–14 ans pour être mariées.

**Pourquoi l'écart rural-urbain persiste malgré l'UBE Act (2004) ?**
- L'UBE Act garantit la gratuité mais **ne résout pas** :
  - La construction d'écoles en zone rurale éloignée.
  - La formation et l'incitation des enseignants à servir en zone rurale.
  - La sensibilisation des parents ruraux à la valeur de l'éducation formelle.
- Le budget éducation nigérian est de ~2% du PIB (vs 4–6% recommandé par l'UNESCO).
- Corruption : détournement des fonds UBE au niveau des États fédérés (Transparency International Nigeria, 2017).

**Implications pour les politiques publiques** :
1. **Cantines scolaires gratuites** : incitent les parents pauvres à scolariser (un repas/jour = économie de 10 000–15 000 ₦/an/enfant).
2. **Écoles mobiles** : enseignants itinérants pour les communautés pastorales (Fulani) et les zones reculées.
3. **Bourses conditionnelles** : transferts monétaires aux familles qui maintiennent leurs enfants scolarisés (testé au Kaduna, résultats prometteurs).
4. **Construction de 20 000 salles de classe rurales** (objectif FME 2020–2025).

**Référence** : UNICEF Nigeria (2018), *Out-of-School Children in Nigeria*.

---

## Tâche 12 : Part d'adultes sans instruction par État

### Résultats attendus

**Top 5 des États les plus défavorisés** (> 60% d'adultes sans instruction) :

1. **Sokoto** (Nord-Ouest) : 70–80%
2. **Zamfara** (Nord-Ouest) : 70–75%
3. **Yobe** (Nord-Est) : 65–75%
4. **Kebbi** (Nord-Ouest) : 65–70%
5. **Borno** (Nord-Est) : 60–70%

**Top 5 des États les plus éduqués** (< 20% d'adultes sans instruction) :

1. **Lagos** (Sud-Ouest) : 8–15%
2. **Anambra** (Sud-Est) : 10–18%
3. **Imo** (Sud-Est) : 12–20%
4. **Rivers** (Sud-Sud) : 15–22%
5. **Edo** (Sud-Sud) : 15–23%

**Écart Nord-Sud : 50–60 points de pourcentage** entre les extrêmes (Sokoto vs Lagos).

### Interprétation contextuelle

**Pourquoi cette fracture Nord-Sud est-elle si marquée ?**

#### 1. Héritage colonial (1900–1960)

**Sud** :
- Conquête britannique dès 1860 (Lagos), évangélisation et scolarisation par les missions chrétiennes (Église catholique, Church Missionary Society, Methodist Church).
- Premières écoles occidentales : Lagos (1842), Calabar (1846), Ibadan (1853).
- En 1960 (indépendance), le Sud avait un taux de scolarisation primaire de **50–60%** et produisait 90% des diplômés universitaires du Nigeria.

**Nord** :
- Résistance des émirats islamiques (Sokoto Caliphate) à la colonisation britannique et à l'éducation occidentale.
- Les Britanniques ont signé des accords de « non-intervention » avec les émirs (indirect rule) : les missions chrétiennes ont été interdites au Nord.
- Système éducatif coranique (Tsangardiya/Almajiri) préservé → pas d'éducation formelle occidentale.
- En 1960, le Nord avait un taux de scolarisation primaire de **< 15%** et comptait < 5% des diplômés universitaires du Nigeria.

#### 2. Politiques régionales post-indépendance

**Sud** :
- Investissement massif dans l'éducation par les gouvernements régionaux (Western Region Universal Primary Education, 1955).
- Valorisation sociale de l'éducation formelle comme voie de mobilité sociale.
- Diaspora éduquée : brain drain vers USA/UK → remises financières investies dans l'éducation des cadets.

**Nord** :
- Résistance culturelle à l'éducation occidentale perçue comme « Boko Haram » (littéralement « l'éducation occidentale est interdite »).
- Préférence pour l'éducation coranique : 10 millions d'enfants Almajiri (mendiants) dans le Nord en 2018 (UNICEF).
- Les Sardauna de Sokoto et les émirs ont historiquement découragé l'éducation formelle pour les filles.

#### 3. Insécurité dans le Nord-Est (2009–2018)

**Boko Haram** (groupe insurgé islamiste) :
- Nom complet : *Jama'atu Ahlis Sunna Lidda'awati wal-Jihad* (« Peuple engagé dans la propagation de l'enseignement du Prophète et le Jihad »).
- Objectif : imposer la charia et détruire l'éducation occidentale.
- Bilan 2009–2018 :
  - 20 000 morts
  - 2 millions de déplacés
  - 900+ écoles détruites
  - 1 500 écoles fermées
  - 2 000 enseignants tués ou enlevés
  - 1 million d'enfants privés d'accès à l'éducation

**États les plus touchés** : Borno, Yobe, Adamawa (Nord-Est).

**Cas emblématiques** :
- **Chibok (2014)** : 276 lycéennes enlevées (100+ toujours portées disparues en 2018).
- **Dapchi (2018)** : 110 lycéennes enlevées (5 mortes, 1 retenue car chrétienne refusant de se convertir).

#### 4. Richesse et investissement éducatif

**Lagos** (Sud-Ouest) :
- PIB : ~33 milliards USD (2018), soit 30% du PIB nigérian.
- PIB par habitant : ~2 500 USD.
- Budget éducation : 15–20% du budget régional.
- Taux d'alphabétisation : 92%.

**Sokoto** (Nord-Ouest) :
- PIB : ~2 milliards USD (2018).
- PIB par habitant : ~400 USD (6 fois moins que Lagos).
- Budget éducation : 5–8% du budget régional.
- Taux d'alphabétisation : 25%.

**Écart de richesse → écart éducatif** : Lagos peut construire des écoles, recruter des enseignants qualifiés, distribuer des bourses. Sokoto ne le peut pas.

### Implications géopolitiques

**Risque de « dividendes démographiques négatifs »** :
- Le Nord a une fécondité élevée (ISF = 6,0) et une éducation très faible.
- Population jeune non scolarisée → chômage → recrutement par Boko Haram et bandits (kidnapping).
- Le Nord pourrait devenir une « poudrière démographique » si l'éducation ne progresse pas.

**Recommandation ONU/Banque Mondiale** :
- Investir massivement dans l'éducation au Nord pour éviter une crise sécuritaire et humanitaire à long terme.
- Programme d'alphabétisation des adultes (30–60 ans) pour valoriser l'éducation formelle auprès des parents.
- Bourses massives pour les filles du Nord (objectif : atteindre GPI = 1 d'ici 2030).

**Référence** : Ajayi & Ajayi (2017), *Gender inequality in educational attainment in Northern Nigeria*, British Journal of Education, 5(3), 1–15.

---

## Conclusion générale : Ce que révèle le TP2

Le TP2 permet de quantifier et de visualiser les **inégalités éducatives massives** au Nigeria :

1. **Inégalité de genre** : les femmes ont 10–20 points de pourcentage de plus dans « Aucun », reflétant le mariage précoce et les normes culturelles défavorables aux filles.

2. **Inégalité générationnelle** : amélioration entre 60+ et 18–30, mais le Nigeria reste loin de la scolarisation universelle (10,5 millions d'enfants non scolarisés).

3. **Inégalité rural-urbain** : écart de 15–25 points dans la scolarisation des 6–17 ans, reflétant la distance aux écoles, la pauvreté et le travail des enfants.

4. **Inégalité géographique Nord-Sud** : fracture de 50–60 points entre Sokoto et Lagos, héritée de la colonisation et aggravée par l'insécurité (Boko Haram).

Ces inégalités sont **structurelles** et ne se résolvent pas spontanément. Elles nécessitent des **politiques publiques ciblées** :
- Augmentation du budget éducation (de 2% à 4% du PIB).
- Construction de 20 000 salles de classe rurales.
- Cantines scolaires gratuites (incitation).
- Bourses pour les filles du Nord.
- Reconstruction des écoles détruites dans le Nord-Est.
- Alphabétisation des adultes (30–60 ans).

Sans ces investissements, le Nigeria restera le pays avec **le plus grand nombre d'enfants non scolarisés au monde**, compromettant ses chances de bénéficier du **dividende démographique** (transition d'une population jeune dépendante vers une population active productive).

---

**Auteure** : Leslye Patricia Nkwa Tsamo
**Formation** : ENSAE Pierre Ndiaye — ISE3
**Date** : Mars 2026
