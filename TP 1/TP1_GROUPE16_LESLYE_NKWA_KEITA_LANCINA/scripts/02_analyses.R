# =============================================================================
# TP1 Nigeria General Household Survey Panel Wave 4 (2018-2019)
# Script 02 : Analyses démographiques | Tâches 2 à 6
#
# Données  : NBS Nigeria / World Bank LSMS-ISA
# Prérequis: 01_nettoyage.R doit avoir été exécuté (produit sect1_clean.rds)
# =============================================================================

# Les packages et la graine aléatoire sont chargés par main.R.
# Ce script suppose que main.R a été exécuté au préalable.

# --- Palette institutionnelle ---
# Couleurs calquées sur les conventions des organisations internationales :
# bleu UNICEF pour les variables démographiques générales,
# convention WHO/HMD pour la distinction homme/femme dans la pyramide,
# convention FAO/HABITAT pour la distinction rural/urbain.
pal <- list(
  primaire   = "#1CABE2",  # Bleu UNICEF
  secondaire = "#374EA2",  # Bleu nuit UNICEF
  homme      = "#0058AB",  # Convention WHO/HMD : bleu = hommes
  femme      = "#E8416F",  # Convention WHO/HMD : rose = femmes
  rural      = "#80BD41",  # Vert FAO/IFPRI : rural, agriculture
  urbain     = "#6C757D"   # Gris HABITAT/ONU : urbain, infrastructures
)

# --- Chargement des données nettoyées ---
# readRDS() recharge l'objet R exact produit par 01_nettoyage.R :
# 25 767 membres actifs (filtre s1q4a == 1), variable aberrante corrigée
# (130 ans -> NA), variables construites : sexe, groupe_age, milieu,
# parente, taille_menage.
sect1_clean <- readRDS(here("data", "processed", "sect1_clean.rds"))

cat("Données chargées :", nrow(sect1_clean), "observations\n\n")
# Attendu : 25767 observations


# =============================================================================
# TACHE 2 — Analyse univariée de l'âge
# =============================================================================
# L'âge (s1q4, années complètes) est la variable démographique de référence.
# On décrit d'abord sa distribution graphiquement, puis par les statistiques
# robustes, et on teste la normalité pour orienter le choix des tests
# bivariés qui suivent (tâche 5).

# --- 2.1 Histogramme ---
# binwidth = 5 ans : convention internationale ONU/UNFPA/DHS pour les
# analyses démographiques. Les classes quinquennales correspondent aux
# cohortes de recensement et permettent la comparaison inter-pays.
# La règle de Sturges donnerait ~7 ans par classe, trop large pour détecter
# les pics démographiques (0-4 ans, 15-24 ans) caractéristiques des pays
# à forte fécondité comme le Nigeria (ISF = 5.3 en 2018, DHS Nigeria 2018).

p_hist <- ggplot(sect1_clean, aes(x = s1q4)) +
  geom_histogram(
    binwidth  = 5,
    fill      = pal$primaire,
    color     = "white",
    boundary  = 0       # classes ancrées à 0 (convention démographique)
  ) +
  geom_vline(
    aes(xintercept = median(s1q4, na.rm = TRUE)),
    linetype  = "dashed",
    color     = pal$secondaire,
    linewidth = 0.8
  ) +
  annotate(
    "text",
    x        = median(sect1_clean$s1q4, na.rm = TRUE) + 3,
    y        = Inf,
    vjust    = 2,
    label    = paste0("Médiane = ", median(sect1_clean$s1q4, na.rm = TRUE), " ans"),
    color    = pal$secondaire,
    fontface = "bold",
    size     = 3.5
  ) +
  labs(
    title    = "Distribution de l'âge des membres de ménages",
    subtitle = "Nigeria GHS Panel W4 (2018-2019) : classes quinquennales",
    x        = "Age (années complètes)",
    y        = "Effectif",
    caption  = "Source : NBS Nigeria / World Bank LSMS-ISA | binwidth = 5 ans (convention ONU)"
  ) +
  theme_minimal(base_size = 12) +
  theme(plot.caption = element_text(hjust = 1, size = 8, color = "grey50"))

ggsave(here("outputs", "figures", "fig01_histogramme_age.png"),
       plot = p_hist, width = 10, height = 7, dpi = 300)

# Résultat attendu : distribution très asymétrique à droite.
# La classe [0,5) est la plus peuplée mais [5,10) est presque aussi large,
# ce qui reflète à la fois la mortalité infantile élevée du Nigeria
# (69 décès pour 1 000 naissances vivantes, UNICEF 2018) et la sous-déclaration
# des nourrissons dans certaines zones rurales du Nord.
# Le pic décroît ensuite régulièrement vers les âges élevés, ce qui est
# cohérent avec une espérance de vie d'environ 54 ans (Banque Mondiale 2018).
# La médiane à 18 ans confirme que la moitié de la population des ménages
# est constituée d'enfants et d'adolescents — signature d'une fécondité soutenue.

cat("fig01_histogramme_age.png exporté\n")


# --- 2.2 Boîte à moustaches ---
# Complète l'histogramme : montre les quartiles, la médiane et les valeurs
# au-delà de 1.5 x IQR. Après correction de la valeur de 130 ans (script 01),
# les points roses restants sont des personnes âgées réelles (> 78 ans environ),
# pas des erreurs — le Nigeria compte environ 3% de personnes de 65 ans et plus
# selon les projections ONU (World Population Prospects 2019).

p_box <- ggplot(sect1_clean, aes(y = s1q4)) +
  geom_boxplot(
    fill          = pal$primaire,
    color         = pal$secondaire,
    alpha         = 0.7,
    outlier.color = pal$femme,
    outlier.shape = 16,
    outlier.size  = 1.5,
    width         = 0.4
  ) +
  coord_flip() +
  labs(
    title    = "Boite à moustaches de l'âge",
    subtitle = "Nigeria GHS Panel W4 (2018-2019) : après correction de la valeur aberrante (130 ans -> NA)",
    y        = "Age (années complètes)",
    x        = "",
    caption  = "Source : NBS Nigeria / World Bank LSMS-ISA | Points roses = valeurs au-delà de 1.5 x IQR"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.y  = element_blank(),
    axis.ticks.y = element_blank(),
    plot.caption = element_text(hjust = 1, size = 8, color = "grey50")
  )

ggsave(here("outputs", "figures", "fig02_boxplot_age.png"),
       plot = p_box, width = 10, height = 7, dpi = 300)

# Résultat attendu :
# Médiane = 18 ans, Q1 = 9, Q3 = 37, IQR = 28 ans.
# La boîte est asymétrique : la distance médiane-Q3 (19 ans) est bien plus
# grande que la distance Q1-médiane (9 ans), confirmant l'étalement à droite.
# Les points outliers roses (> ~78 ans) sont des personnes réelles, non des
# erreurs de saisie — le maximum corrigé est 100 ans, plausible.

cat("fig02_boxplot_age.png exporté\n")


# --- 2.3 Statistiques descriptives ---
# On extrait l'âge sans NA une seule fois pour éviter na.rm = TRUE répété.
age_vec <- sect1_clean$s1q4[!is.na(sect1_clean$s1q4)]

stats_age <- data.frame(
  Statistique = c(
    "N (valide)", "Moyenne", "Médiane",
    "Q1 (25e percentile)", "Q3 (75e percentile)",
    "Écart-type", "CV (%)", "Minimum", "Maximum",
    "Asymétrie (skewness)", "Kurtosis"
  ),
  Valeur = c(
    length(age_vec),
    round(mean(age_vec), 2),
    round(median(age_vec), 2),
    round(quantile(age_vec, 0.25), 2),
    round(quantile(age_vec, 0.75), 2),
    round(sd(age_vec), 2),
    round(sd(age_vec) / mean(age_vec) * 100, 2),   # CV = sd / mean x 100
    min(age_vec),
    max(age_vec),
    round(moments::skewness(age_vec), 4),
    round(moments::kurtosis(age_vec), 4)
  )
)

cat("=== Statistiques descriptives de l'âge ===\n")
print(stats_age, row.names = FALSE)

# Résultats obtenus :
# Moyenne = 24.37 ans, Médiane = 18 ans -> écart de 6 ans révélateur
# de l'asymétrie : les individus très âgés tirent la moyenne vers le haut
# sans déplacer la médiane. En démographie des pays à forte fécondité,
# la médiane est l'indicateur central (ONU recommande la médiane d'âge
# comme indicateur de structure par âge, World Population Prospects 2022).
# CV = 80.96% : population très hétérogène, des nourrissons aux centenaires.
# Skewness = 0.98 : asymétrie positive modérée, étalée vers les âges élevés.
# Kurtosis = 3.21 : proche d'une loi normale aplatie (kurtosis normale = 3),
# indiquant une distribution à queues légèrement plus épaisses que la normale.

cat("\nInterprétation : asymétrie =", round(moments::skewness(age_vec), 4), "\n")
cat("  -> Distribution asymétrique positive (étalée à droite)\n")
cat("  -> Moyenne (", round(mean(age_vec), 1), ") > Médiane (",
    round(median(age_vec), 1), ") : beaucoup de jeunes, peu d'âgés\n")


# --- 2.4 Test de normalité de Shapiro-Wilk ---
# shapiro.test() est plafonné à n = 5 000 dans R (contrainte algorithmique).
# On tire un échantillon aléatoire reproductible grâce au set.seed(2070)
# défini en début de script.
#
# H0 : la distribution de l'âge suit une loi normale
# H1 : la distribution de l'âge ne suit PAS une loi normale
#
# Si p < 0.05 -> rejet de H0 -> distribution non normale.
# Conséquence directe : les tests paramétriques (t de Student, ANOVA)
# supposent la normalité et sont donc inappropriés. On utilisera le test
# de Wilcoxon-Mann-Whitney pour la comparaison rural/urbain (tâche 5).
# Si p >= 0.05 -> non-rejet de H0 : pas de preuve contre la normalité,
# les tests paramétriques restent envisageables mais on vérifie aussi
# visuellement (QQ-plot, histogramme).

echantillon_shapiro <- sample(age_vec, size = min(5000, length(age_vec)))
shapiro_result      <- shapiro.test(echantillon_shapiro)

cat("\n=== Test de Shapiro-Wilk (n =", length(echantillon_shapiro), ") ===\n")
cat("W =", round(shapiro_result$statistic, 6), "\n")
cat("p-value =", format.pval(shapiro_result$p.value, digits = 4), "\n")

if (shapiro_result$p.value < 0.05) {
  cat("-> Rejet de H0 au seuil 5% : la distribution de l'âge n'est PAS normale.\n")
  cat(sprintf("   W = %.4f, p = %s : valeur de W éloignée de 1 (normalité parfaite = 1).\n",
              shapiro_result$statistic,
              format.pval(shapiro_result$p.value, digits = 4)))
  cat("   Conséquence : tests paramétriques (t de Student, ANOVA) inappropriés.\n")
  cat("   On utilisera le test de Wilcoxon-Mann-Whitney à la tâche 5.\n")
} else {
  cat("-> Non-rejet de H0 : pas de preuve contre la normalité.\n")
  cat("   Les tests paramétriques restent envisageables.\n")
  cat("   Vérification visuelle recommandée (QQ-plot, histogramme).\n")
}


# =============================================================================
# TACHE 3 — Pyramide des âges
# =============================================================================
# apyramid::age_pyramid() est privilégié à un ggplot2 manuel pour trois raisons :
# 1. Il gère automatiquement le miroir des barres (hommes à gauche, femmes
#    à droite) — construire cela manuellement requiert de passer les effectifs
#    masculins en négatif, source d'erreurs fréquentes.
# 2. Il ordonne les groupes d'âge du bas (jeunes) vers le haut (âgés) sans
#    recourir à un fct_reorder() complexe.
# 3. Il retourne un objet ggplot2 entièrement personnalisable.
# On exclut l'unique individu dont groupe_age = NA (la valeur 130 ans
# corrigée en NA au script 01 n'a pas pu être assignée à un groupe).

donnees_pyramide <- sect1_clean |>
  filter(!is.na(groupe_age), !is.na(sexe))

p_pyramide <- apyramid::age_pyramid(
  data           = donnees_pyramide,
  age_group      = "groupe_age",
  split_by       = "sexe",
  show_midpoint  = FALSE,
  pal            = c(Homme = pal$homme, Femme = pal$femme)
) +
  labs(
    title    = "Pyramide des âges des membres de ménages : Nigeria GHS Panel W4 (2018-2019)",
    subtitle = "Base large = fécondité élevée (ISF ≈ 5.3 en 2018, DHS Nigeria 2018)",
    x        = "Groupe d'âge",
    y        = "Effectif",
    fill     = "Sexe",
    caption  = "Source : NBS Nigeria / World Bank LSMS-ISA"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title    = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(size = 10, color = "grey30"),
    plot.caption  = element_text(hjust = 1, size = 8, color = "grey50"),
    legend.position = "bottom"
  )

ggsave(here("outputs", "figures", "fig03_pyramide_ages.png"),
       plot = p_pyramide, width = 10, height = 12, dpi = 300)

# Résultat attendu — lecture classe par classe :
#
# [0,5) vs [5,10) : la classe [5,10) est plus large que [0,5), ce qui est
# contre-intuitif. Deux explications simultanées : la mortalité infantile
# (69 décès pour 1 000 naissances vivantes au Nigeria, UNICEF 2018) réduit
# mécaniquement l'effectif des 0-4 ans par rapport aux naissances réelles ;
# et la sous-déclaration des nourrissons dans les zones rurales du Nord
# (parfois différée par crainte du mauvais oeil ou par habitude culturelle).
#
# [10,25) : larges barres symétriques entre hommes et femmes — ces cohortes
# représentent les enfants issus des naissances des années 1993-2008,
# période de fécondité soutenue (ISF > 6 selon DHS Nigeria 2003 et 2008).
#
# [20,35) : léger étranglement du côté masculin. Il reflète la migration
# sélective des hommes jeunes vers les centres urbains (Lagos, Abuja, Kano)
# pour le travail — ces individus sont présents dans les ménages urbains
# et absents des ménages ruraux enquêtés.
#
# [50 ans et plus) : domination féminine au sommet de la pyramide,
# cohérente avec une espérance de vie féminine supérieure d'environ 2 ans
# (57 ans pour les femmes vs 55 ans pour les hommes, Banque Mondiale 2018).
#
# Forme générale : pyramide expansive (base large, sommet étroit),
# caractéristique des pays en transition démographique précoce.
# Cohérente avec un âge médian de 17.9 ans (DHS Nigeria 2018).
# Notre estimation : 18 ans — résultat validé.

cat("\nfig03_pyramide_ages.png exporté\n")


# =============================================================================
# TACHE 4 — Lien de parenté
# =============================================================================
# Variable parente recodée en 4 catégories au script 01 :
# Enfant (14 328), Chef de ménage (4 968), Conjoint(e) (4 214), Autre (2 257).
# Aucun NA après le filtre des membres actifs.

# --- 4.1 Fréquences et proportions ---
freq_parente <- sect1_clean |>
  filter(!is.na(parente)) |>
  count(parente, name = "effectif") |>
  mutate(
    proportion  = effectif / sum(effectif),
    pourcentage = round(proportion * 100, 1)
  ) |>
  arrange(desc(effectif))

cat("\n=== Fréquences des liens de parenté ===\n")
print(freq_parente, row.names = FALSE)

# Résultats obtenus :
# Enfant : 55.6% — catégorie dominante, reflet direct de l'ISF élevé.
# Dans un ménage nigérian médian, plus d'un membre sur deux est un enfant
# du chef. Ce ratio est supérieur à la moyenne subsaharienne (~50%,
# Bongaarts 2001, Population and Development Review).
# Chef de ménage : 19.3% — chaque ménage ayant exactement un chef,
# ce pourcentage est l'inverse de la taille moyenne des ménages (1/5.18 ≈ 19%).
# Conjoint(e) : 16.4% — inférieur à celui du chef, ce qui s'explique
# partiellement par la polygamie : un chef peut avoir plusieurs conjointes,
# mais dans l'enquête, certaines peuvent ne pas être listées comme membres actifs.
# Autre : 8.8% — neveux, parents, beaux-parents, domestiques, etc.
# Reflète la structure élargie des ménages africains décrite par
# Goody (1976, Production and Reproduction, Cambridge University Press).


# --- 4.2 Intervalles de confiance exacts (Clopper-Pearson) ---
# binom.test() calcule l'IC exact de Clopper-Pearson, préféré à
# l'approximation normale p ± 1.96 x sqrt(p(1-p)/n) pour deux raisons :
# 1. L'approximation normale peut produire des bornes négatives ou > 1
#    quand p est proche de 0 ou de 1 — absurde pour une proportion.
# 2. L'IC exact de Clopper-Pearson a une couverture garantie >= 95%
#    quelle que soit la taille d'échantillon.
# C'est la méthode recommandée par l'OMS et l'UNFPA pour les indicateurs
# démographiques issus d'enquêtes ménages.

n_total   <- sum(freq_parente$effectif)

ic_parente <- freq_parente |>
  rowwise() |>
  mutate(
    test   = list(binom.test(effectif, n_total, conf.level = 0.95)),
    ic_bas = test$conf.int[1] * 100,
    ic_haut = test$conf.int[2] * 100
  ) |>
  select(-test) |>
  ungroup()

cat("\n=== Proportions avec IC 95% (Clopper-Pearson) ===\n")
for (i in seq_len(nrow(ic_parente))) {
  cat(sprintf("  %s : %.1f%% [%.1f%% ; %.1f%%]\n",
              ic_parente$parente[i],
              ic_parente$pourcentage[i],
              ic_parente$ic_bas[i],
              ic_parente$ic_haut[i]))
}

# Résultats obtenus :
# Enfant      : 55.6% [55.0% ; 56.2%]
# Chef        : 19.3% [18.8% ; 19.8%]
# Conjoint(e) : 16.4% [15.9% ; 16.8%]
# Autre       :  8.8% [ 8.4% ;  9.1%]
#
# Les IC sont très étroits (± 0.3 point de pourcentage) grâce au grand
# échantillon (n = 25 767). Cette précision est caractéristique des enquêtes
# LSMS-ISA conçues pour produire des estimations nationales fiables.
# Des IC larges signaleraient un sous-échantillonnage ou un problème de plan
# de sondage (Kish, 1965, Survey Sampling, Wiley).


# --- 4.3 Diagramme en barres horizontales ---
# Barres horizontales (coord_flip) car les labels de catégories sont longs.
# fct_reorder() ordonne les barres par effectif décroissant : la catégorie
# la plus fréquente apparaît en haut, facilitant la lecture immédiate.
# Palette séquentielle dérivée du bleu institutionnel #1CABE2.

n_cat       <- nrow(freq_parente)
palette_seq <- scales::seq_gradient_pal(
  low  = "#A8E0F3",
  high = "#0A6E8F"
)(seq(0, 1, length.out = n_cat))

p_parente <- ggplot(
  freq_parente,
  aes(
    x    = fct_reorder(parente, effectif),
    y    = effectif,
    fill = fct_reorder(parente, effectif)
  )
) +
  geom_col(show.legend = FALSE) +
  geom_text(
    aes(label = paste0(pourcentage, "%")),
    hjust    = -0.1,
    size     = 3.5,
    fontface = "bold"
  ) +
  scale_fill_manual(
    values = setNames(
      rev(palette_seq),
      levels(fct_reorder(freq_parente$parente, freq_parente$effectif))
    )
  ) +
  coord_flip() +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(
    title    = "Répartition des membres par lien de parenté avec le chef de ménage",
    subtitle = "Nigeria GHS Panel W4 (2018-2019) : membres actifs, 4 catégories",
    x        = "",
    y        = "Effectif",
    caption  = "Source : NBS Nigeria / World Bank LSMS-ISA | IC 95% Clopper-Pearson"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title    = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(size = 10, color = "grey30"),
    plot.caption  = element_text(hjust = 1, size = 8, color = "grey50")
  )

ggsave(here("outputs", "figures", "fig04_parente_barplot.png"),
       plot = p_parente, width = 10, height = 7, dpi = 300)

cat("\nfig04_parente_barplot.png exporté\n")


# =============================================================================
# TACHE 5 — Comparaison rural/urbain de la taille des ménages
# =============================================================================
# Les ménages ruraux plus grands impliquent des besoins différenciés en
# éducation, santé et alimentation — enjeu central pour le ciblage des
# politiques sociales nigérianes (Vision 2030, NASS 2021).

# --- 5.1 Données au niveau ménage ---
# On travaille au niveau MÉNAGE (une ligne = un ménage) et non au niveau
# individu pour éviter le biais de taille : un ménage de 10 membres serait
# sinon compté 10 fois, surpondérant les grands ménages dans la distribution
# (Kish, 1965, Survey Sampling, Wiley).
# distinct(hhid, .keep_all = TRUE) conserve la première ligne de chaque ménage
# avec toutes ses variables, dont milieu et taille_menage.

menages <- sect1_clean |>
  distinct(hhid, .keep_all = TRUE) |>
  filter(!is.na(milieu))

cat("\n=== Taille des ménages par milieu ===\n")
menages |>
  group_by(milieu) |>
  summarise(
    n          = n(),
    moyenne    = round(mean(taille_menage), 2),
    mediane    = median(taille_menage),
    Q1         = quantile(taille_menage, 0.25),
    Q3         = quantile(taille_menage, 0.75),
    ecart_type = round(sd(taille_menage), 2),
    .groups    = "drop"
  ) |>
  print()

# Résultats obtenus :
# Urbain : n = 1 595, médiane = 4, Q1 = 3, Q3 = 6, SD = 2.89
# Rural  : n = 3 383, médiane = 5, Q1 = 3, Q3 = 7, SD = 3.38
#
# Les deux groupes partagent Q1 = 3 : les petits ménages existent autant
# en ville qu'à la campagne (ménages monoparentaux, couples sans enfant).
# La différence se concentre sur la borne haute : Q3 passe de 6 à 7 en rural,
# signe que les grands ménages polygames sont plus fréquents en milieu rural,
# notamment dans le Nord (Banque Mondiale, Nigeria Poverty Assessment 2022).
# Ces résultats sont cohérents avec le DHS Nigeria 2018 :
# taille médiane 4.4 en urbain / 5.8 en rural (membres actifs uniquement
# dans notre échantillon -> légèrement inférieurs au DHS qui compte tous membres).


# --- 5.2 Boxplot groupé ---
# Couleurs selon convention FAO/HABITAT : vert = rural, gris = urbain.
# Les médianes sont annotées directement sur le graphique pour une lecture
# immédiate sans avoir à lire l'axe.

medianes <- menages |>
  group_by(milieu) |>
  summarise(med = median(taille_menage), .groups = "drop")

p_box_milieu <- ggplot(menages, aes(x = milieu, y = taille_menage, fill = milieu)) +
  geom_boxplot(
    alpha         = 0.8,
    outlier.color = "grey40",
    outlier.shape = 16,
    outlier.size  = 1,
    width         = 0.5
  ) +
  geom_text(
    data     = medianes,
    aes(x = milieu, y = med, label = paste0("Méd = ", med)),
    vjust    = -0.8,
    fontface = "bold",
    size     = 4,
    color    = "black"
  ) +
  scale_fill_manual(
    values = c("Urbain" = pal$urbain, "Rural" = pal$rural),
    guide  = "none"
  ) +
  labs(
    title    = "Taille des ménages selon le milieu de résidence",
    subtitle = "Nigeria GHS Panel W4 (2018-2019) : un point par ménage (n = 4 978)",
    x        = "Milieu de résidence",
    y        = "Nombre de membres du ménage",
    caption  = "Source : NBS Nigeria / World Bank LSMS-ISA | Comparaison au niveau ménage"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title    = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(size = 10, color = "grey30"),
    plot.caption  = element_text(hjust = 1, size = 8, color = "grey50")
  )

ggsave(here("outputs", "figures", "fig05_boxplot_taille_menage.png"),
       plot = p_box_milieu, width = 10, height = 7, dpi = 300)

cat("fig05_boxplot_taille_menage.png exporté\n")


# --- 5.3 Test de Wilcoxon-Mann-Whitney ---
# Le test de Shapiro-Wilk (tâche 2) a rejeté la normalité de l'âge.
# La taille des ménages, variable discrète bornée inférieurement par 1,
# est encore moins susceptible d'être normale (distribution asymétrique à
# droite, pics aux petites valeurs, maximum = 29).
#
# Le test t de Student requiert :
# (a) la normalité des distributions dans chaque groupe
# (b) l'homoscédasticité (variances égales) — SD urbain = 2.89 vs rural = 3.38
# Ces deux hypothèses sont violées ici.
#
# Le test de Wilcoxon-Mann-Whitney (U de Mann-Whitney) :
# - ne suppose PAS la normalité
# - compare les RANGS plutôt que les valeurs brutes
# - H0 : les distributions de taille de ménage sont identiques en rural et urbain
# - H1 : les distributions diffèrent (test bilatéral)

taille_urbain <- menages$taille_menage[menages$milieu == "Urbain"]
taille_rural  <- menages$taille_menage[menages$milieu == "Rural"]

wilcox_result <- wilcox.test(taille_rural, taille_urbain,
                             alternative = "two.sided",
                             conf.int    = TRUE)

cat("\n=== Test de Wilcoxon-Mann-Whitney ===\n")
cat("W =", wilcox_result$statistic, "\n")
cat("p-value =", format.pval(wilcox_result$p.value, digits = 4), "\n")

if (wilcox_result$p.value < 0.05) {
  cat("-> Rejet de H0 au seuil 5% : différence significative de taille\n")
  cat("   des ménages entre milieu rural et urbain.\n")
  cat(sprintf("   W = %.0f, p = %s : la différence n'est pas due au hasard.\n",
              wilcox_result$statistic,
              format.pval(wilcox_result$p.value, digits = 4)))
} else {
  cat("-> Non-rejet de H0 : pas de différence significative.\n")
  cat("   La différence observée pourrait être due à la variabilité d'échantillonnage.\n")
}


# --- 5.4 Taille d'effet r de rang ---
# La significativité statistique seule ne suffit pas : avec n = 4 978 ménages,
# même une différence minuscule atteint le seuil p < 0.05.
# La taille d'effet quantifie l'AMPLEUR de la différence, indépendamment
# de la taille d'échantillon.
#
# Formule : r = Z / sqrt(N), où Z est dérivé de la statistique W.
# Seuils de Cohen (1988, Statistical Power Analysis for the Behavioral Sciences) :
# - r < 0.10          : effet négligeable
# - 0.10 <= r < 0.30  : effet petit
# - 0.30 <= r < 0.50  : effet moyen
# - r >= 0.50         : effet grand
# Confirmés pour le contexte non paramétrique par Fritz, Morris & Richler
# (2012, British Journal of Mathematical and Statistical Psychology).
#
# wilcox_effsize() de rstatix calcule r directement depuis la statistique W.

effet <- menages |>
  rstatix::wilcox_effsize(taille_menage ~ milieu)

cat("\n=== Taille d'effet ===\n")
print(effet)
cat("\nInterprétation : r =", round(effet$effsize, 4), "\n")
cat("Variance expliquée : r² =", round(effet$effsize^2 * 100, 2), "%\n")

if (effet$effsize < 0.1) {
  cat("-> Effet négligeable : la différence, bien que statistiquement\n")
  cat("   significative, est trop faible pour avoir une portée pratique.\n")
  cat("   Le milieu de résidence n'explique quasi rien de la variabilité\n")
  cat("   de la taille des ménages.\n")
} else if (effet$effsize < 0.3) {
  cat(sprintf("-> Effet petit (Cohen 1988) : r = %.3f, r² = %.2f%%.\n",
              effet$effsize, effet$effsize^2 * 100))
  cat("   Le milieu n'explique qu'une faible part de la variabilité de la taille.\n")
  cat("   98.54% de la variabilité vient d'autres facteurs : zone géopolitique,\n")
  cat("   religion, polygamie, niveau d'éducation du chef de ménage.\n")
  cat("   La différence est réelle mais modeste en ampleur.\n")
  cat("   Elle reste socialement significative à grande échelle : un membre\n")
  cat("   supplémentaire en médiane rurale, appliqué aux ~38 millions de ménages\n")
  cat("   ruraux nigérians, représente des millions de personnes supplémentaires\n")
  cat("   à nourrir, scolariser et soigner (Sullivan & Feinn 2012,\n")
  cat("   Journal of Graduate Medical Education).\n")
} else if (effet$effsize < 0.5) {
  cat("-> Effet moyen : la différence rural/urbain est substantielle.\n")
  cat("   Elle justifie des politiques sociales différenciées\n")
  cat("   (éducation, santé, alimentation).\n")
} else {
  cat("-> Effet grand : la différence est majeure et socio-économiquement\n")
  cat("   très significative pour le ciblage des programmes sociaux.\n")
}


# =============================================================================
# TACHE 6 — Tableau gtsummary
# =============================================================================
# tbl_summary() produit un tableau de statistiques descriptives au format
# publication, directement exportable en HTML, Word ou LaTeX.
# On travaille ici au niveau INDIVIDU (contrairement à la tâche 5 au niveau
# ménage) car on décrit les caractéristiques des membres par milieu de résidence.
# Conséquence : la taille du ménage est répétée autant de fois que le ménage
# compte de membres actifs, ce qui induit un biais de taille (Kish 1965) —
# les grands ménages sont surreprésentés. C'est pourquoi les médianes de
# taille_menage sont plus élevées ici (6/7) qu'à la tâche 5 (4/5).

donnees_tableau <- sect1_clean |>
  filter(!is.na(milieu)) |>
  select(
    milieu,        # variable de stratification
    s1q4,          # âge en années complètes
    sexe,          # sexe recodé (Homme / Femme)
    taille_menage  # taille du ménage (niveau ménage répété par individu)
  )

tableau <- donnees_tableau |>
  tbl_summary(
    by = milieu,
    label = list(
      s1q4          ~ "Age (années)",
      sexe          ~ "Sexe",
      taille_menage ~ "Taille du ménage"
    ),
    statistic = list(
      # Médiane et IQR pour les variables continues car le test de
      # Shapiro-Wilk (tâche 2) a rejeté la normalité. La médiane est robuste
      # aux valeurs extrêmes ; l'IQR (Q1-Q3) décrit la dispersion centrale
      # sans être influencé par les ménages extrêmes (max = 29 membres).
      all_continuous()  ~ "{median} ({p25} - {p75})",
      all_categorical() ~ "{n} ({p}%)"
    ),
    # "ifany" : les valeurs manquantes ne s'affichent que si elles existent.
    # Évite d'encombrer le tableau avec des lignes "0 manquant" inutiles.
    missing      = "ifany",
    missing_text = "Valeurs manquantes"
  ) |>
  # Colonne Total : vue d'ensemble avant stratification rural/urbain.
  add_overall(last = FALSE) |>
  # add_p() choisit automatiquement le test selon le type de variable :
  # Wilcoxon pour les variables continues (cohérent avec notre rejet de normalité),
  # Chi-deux de Pearson pour les variables catégorielles (sexe).
  add_p() |>
  modify_header(
    label         = "**Variable**",
    all_stat_cols() ~ "**{level}**\nN = {n}"
  ) |>
  bold_labels()

cat("\n=== Tableau gtsummary : caractéristiques par milieu ===\n")
print(tableau)

# Résultats obtenus :
# Age       : 18 (9-37) global | 20 (10-38) urbain | 17 (8-36) rural | p < 0.001
# La population rurale est plus jeune en médiane d'un an (17 vs 20 ans),
# cohérent avec une fécondité rurale plus élevée (ISF rural = 6.0 vs 4.1 urbain,
# DHS Nigeria 2018). Les ruraux ont moins de 20 ans à 50% contre 38% en urbain.
#
# Sexe      :prsq 50% Homme / 50% Femme dans les deux milieux | p = 0.4
# Non-rejet de H0 : pas de différence significative de répartition par sexe
# selon le milieu. Ce résultat est attendu : le sex-ratio à la naissance
# est biologiquement stable (~1.05) dans tous les milieux.
# La légère surreprésentation féminine en urbain (51%) reflète la migration
# féminine vers les villes pour le travail domestique et le commerce,
# documentée par Tacoli (2012, Environment and Urbanization), mais la
# différence est trop faible pour être statistiquement significative.
# Attention : ne pas confondre absence de preuve et preuve d'absence,
# p = 0.4 ne prouve pas que les sex-ratios sont identiques, il dit seulement
# que les données ne fournissent pas assez d'évidence pour les distinguer.
#
# Taille    : 6 (5-9) global | 6 (4-8) urbain | 7 (5-9) rural | p < 0.001
# Valeurs plus élevées qu'à la tâche 5 (4/5) car ici niveau individu :
# biais de taille de Kish (1965) — les grands ménages sont surreprésentés.


# Export HTML
tableau_html <- tableau |>
  gtsummary::as_gt() |>
  gt::tab_header(
    title    = "Caractéristiques démographiques des membres de ménages",
    subtitle = "Nigeria GHS Panel W4 (2018-2019) : stratification rural/urbain"
  ) |>
  gt::tab_source_note(
    source_note = "Source : NBS Nigeria / World Bank LSMS-ISA"
  ) |>
  gt::tab_options(
    heading.background.color    = "#1CABE2",
    heading.title.font.size     = 16,
    heading.subtitle.font.size  = 12,
    column_labels.font.weight   = "bold"
  )

gt::gtsave(
  tableau_html,
  filename = here("outputs", "tables", "tableau_gtsummary_rural_urbain.html")
)

# Export CSV 
tableau_csv <- tableau |>
  as_tibble() |>
  mutate(across(everything(), as.character))

write_excel_csv(
  tableau_csv,
  file = here("outputs", "tables", "tableau_gtsummary_rural_urbain.csv")
)

cat("\n=== Exports finalisés ===\n")
cat("HTML :", here("outputs", "tables", "tableau_gtsummary_rural_urbain.html"), "\n")
cat("CSV  :", here("outputs", "tables", "tableau_gtsummary_rural_urbain.csv"), "\n")


# =============================================================================
cat("\n======================================================\n")
cat("  Script 02_analyses.R terminé avec succès\n")
cat("  Figures exportées dans outputs/figures/\n")
cat("  Tableaux exportés dans outputs/tables/\n")
cat("======================================================\n")