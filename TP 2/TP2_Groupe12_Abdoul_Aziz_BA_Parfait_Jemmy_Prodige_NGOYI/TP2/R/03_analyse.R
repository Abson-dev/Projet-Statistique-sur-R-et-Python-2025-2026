
# =====================================================
# SCRIPT : 03_analyse.R
# OBJECTIF : Analyse du niveau d'éducation
# =====================================================

# =========================
# 1. Chargement des packages
# =========================
library(tidyverse)

# =========================
# 2. Chargement des données propres
# =========================
data_clean <- readRDS("data/processed/data_clean.rds")
View(data_clean)

colnames(data_clean)
# =========================
# 3. Statistiques descriptives
# =========================

# Calcul des fréquences et pourcentages pondérés
profil_educ_pondere <- data_clean %>%
  group_by(niveau_educ) %>%
  summarise(
    Frequence_Ponderee = sum(wt_wave4, na.rm = TRUE)
  ) %>%
  mutate(
    Pourcentage = round((Frequence_Ponderee / sum(Frequence_Ponderee)) * 100, 2)
  ) %>%
  rename(Niveau = niveau_educ)

# Affichage du résultat
print(profil_educ_pondere)

# Calcul du profil éducatif avec pondération
df_visu <- data_clean %>%
  group_by(niveau_educ) %>%
  summarise(
    Effectif_Pondere = sum(wt_wave4, na.rm = TRUE)
  ) %>%
  mutate(
    Pourcentage = round((Effectif_Pondere / sum(Effectif_Pondere)) * 100, 2)
  ) %>%
  # On retire les NA pour le graphique si nécessaire
  filter(!is.na(niveau_educ))

library(ggplot2)

ggplot(df_visu, aes(x = niveau_educ, y = Pourcentage, fill = niveau_educ)) +
  # Barres horizontales
  geom_bar(stat = "identity", width = 0.7) +
  
  # Ajout des étiquettes (labels) au bout des barres
  geom_text(aes(label = paste0(Pourcentage, "%")), 
            hjust = -0.1, 
            size = 4, 
            fontface = "bold") +
  
  # Inversion pour le format horizontal
  coord_flip() +
  
  # Palette de couleurs "Set2" (très lisible)
  scale_fill_brewer(palette = "Set2") +
  
  # Habillage professionnel
  labs(
    title = "Répartition du niveau d'éducation (Données pondérées)",
    subtitle = "Source : Enquête Wave 4 - Pondération : wt_wave4",
    x = "Niveau d'instruction",
    y = "Proportion dans la population (%)",
    fill = "Légende"
  ) +
  
  # Thème épuré
  theme_minimal() +
  theme(
    legend.position = "none", # Pas besoin de légende si l'axe Y est clair
    axis.text.y = element_text(size = 11, face = "italic"),
    plot.title = element_text(size = 14, face = "bold")
  ) +
  
  # Ajuster l'échelle pour ne pas couper les textes au bout des barres
  scale_y_continuous(expand = expansion(mult = c(0, 0.2)))


## Comparer la distribution du niveau d'éducation entre hommes et femmes (adultes 18+): 

# On filtre d'abord pour ne garder que les adultes (18+)
data_adultes <- data_clean %>% filter(s1q4 >= 18)

# --- A. Tableau de contingence pondéré ---
tab_pond <- wtd.table(data_adultes$niveau_educ, 
                      data_adultes$sexe, 
                      weights = data_adultes$wt_wave4)

View(tab_pond)
# --- B. Test du Chi-deux (version Survey) ---
# On définit l'objet de design pour un test valide
design <- svydesign(ids = ~1, data = data_adultes, weights = ~wt_wave4)
test_chi2 <- svychisq(~niveau_educ + sexe, design)

# --- C. V de Cramér (Intensité de la relation) ---
v_cramer <- cramer.v(tab_pond)

print(test_chi2)
cat("V de Cramér :", round(v_cramer, 4))

# Barplot

# Calcul des pourcentages au sein de chaque sexe
df_plot <- data_adultes %>%
  group_by(sexe, niveau_educ) %>%
  summarise(poids_somme = sum(wt_wave4, na.rm = TRUE), .groups = "drop") %>%
  group_by(sexe) %>%
  mutate(Pourcentage = (poids_somme / sum(poids_somme))*100)

# Graphique
library(scales)

# 1. Préparation des données avec pondération
profil_educ_pond <- data_clean %>%
  # On filtre les adultes et les valeurs manquantes (poids inclus)
  filter(s1q4 >= 18, !is.na(niveau_educ), !is.na(s1q2), !is.na(wt_wave4)) %>%
  # Recodage du sexe
  mutate(sexe = factor(s1q2, levels = c(1,2), labels = c("Homme","Femme"))) %>%
  # Groupement pour calculer la structure interne de chaque niveau
  group_by(niveau_educ, sexe) %>%
  summarise(Effectif_Pondere = sum(wt_wave4), .groups = "drop") %>%
  # Calcul du pourcentage au sein de chaque niveau d'éducation
  group_by(niveau_educ) %>%
  mutate(Pourcentage = round(Effectif_Pondere / sum(Effectif_Pondere) * 100, 1))

# 2. Visualisation (Barplot horizontal 100% empilé)
ggplot(profil_educ_pond, aes(x = niveau_educ, y = Pourcentage, fill = sexe)) +
  # Utilisation de position = "stack" car les données sont déjà des pourcentages totalisant 100
  geom_bar(stat = "identity", width = 0.7) +
  
  # Ajout des étiquettes au centre des segments
  geom_text(aes(label = paste0(Pourcentage, "%")), 
            position = position_stack(vjust = 0.5), 
            size = 4, 
            color = "white", 
            fontface = "bold") +
  
  # Inversion des axes pour le format horizontal
  coord_flip() +
  
  # Couleurs et limites de l'axe Y à 100%
  scale_fill_manual(values = c("Homme" = "steelblue", "Femme" = "tomato")) +
  scale_y_continuous(limits = c(0, 100), expand = c(0, 0)) +
  
  labs(
    title = "Répartition Hommes/Femmes par niveau d'éducation (18+)",
    subtitle = "Source : EHCVM - Calculs pondérés (wt_wave4)",
    x = "Niveau d'éducation",
    y = "Pourcentage (%)",
    fill = "Sexe"
  ) +
  
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

#10. Analyser la relation entre âge et niveau d'éducation

library(ggplot2)

# 1. Nettoyage des données (on s'assure que le poids n'est pas NA)
data_plot <- data_clean %>%
  filter(!is.na(niveau_educ), !is.na(s1q4), !is.na(wt_wave4))

# 2. Création du Boxplot pondéré
ggplot(data_plot, aes(x = niveau_educ, y = s1q4, fill = niveau_educ, weight = wt_wave4)) +
  
  # L'argument weight à l'intérieur de aes() ajuste les boîtes à la réalité démographique
  geom_boxplot(outlier.shape = 21, 
               outlier.size = 1.5, 
               alpha = 0.7, 
               width = 0.6) +
  
  # Palette de couleurs adaptée aux catégories ordonnées
  scale_fill_brewer(palette = "Set2") +
  
  labs(
    title = "Distribution de l'âge par niveau d'éducation",
    subtitle = "Calculs pondérés (wt_wave4) - Population de l'enquête",
    x = "Niveau d'instruction atteint",
    y = "Âge (en années)"
  ) +
  
  # Amélioration visuelle
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.text.x = element_text(angle = 0, vjust = 0.5), # Garder horizontal pour la lisibilité
    panel.grid.minor = element_blank()
  )


library(survey)

# 1. Création du score numérique pour le test
data_test <- data_clean %>%
  filter(!is.na(niveau_educ), !is.na(tranche_age), !is.na(wt_wave4)) %>%
  mutate(educ_num = as.numeric(niveau_educ))

# 2. Définition du design (si pas déjà fait)
design_test <- svydesign(ids = ~1, data = data_test, weights = ~wt_wave4)

# 3. Test de Kruskal-Wallis pondéré
# H0 : La distribution de l'éducation est la même pour tous les groupes d'âge
kw_test <- svyranktest(educ_num ~ tranche_age, design_test, test = "KruskalWallis")

print(kw_test)


if(!require(FSA)) install.packages("dunn.test")
library(dunn.test)

# Test de Dunn avec correction de p-value (Bonferroni ou Holm)
resultat_dunn <- dunn.test(
  x = data_plot$s1q4, 
  g = data_plot$niveau_educ,
  method = "bonferroni", 
  kw = TRUE, 
  label = TRUE
)

# 1. On prépare les données proprement
data_scol <- readRDS("data/processed/data_scol")

print(table(data_scol$zone, data_scol$scolarise))

design_scol <- svydesign(ids = ~1, data = data_scol, weights = ~wt_wave4)
# Calcul des taux et IC 95%
res_scol <- svyby(~scolarise, ~zone, design_scol, svymean, vartype = "ci")
print(res_scol)

# Test du Chi-deux de Rao-Scott
print(svychisq(~zone + scolarise, design_scol))


# 4. Taux + IC à 95% par zone
data_ic <- data_scol %>%
  group_by(zone) %>%
  summarise(
    n        = n(),
    n_scol   = sum(scolarise),
    taux     = n_scol / n,
    se       = sqrt(taux * (1 - taux) / n),
    ic_lower = taux - 1.96 * se,
    ic_upper = taux + 1.96 * se
  )

print(data_ic)

# 5. Graphique en barres groupées avec IC 95%
ggplot(data_ic, aes(x = zone, y = taux * 100, fill = zone)) +
  geom_col(width = 0.5, alpha = 0.85) +
  geom_errorbar(aes(ymin = ic_lower * 100, ymax = ic_upper * 100),
                width = 0.15, linewidth = 0.8) +
  geom_text(aes(label = paste0(round(taux * 100, 1), "%")),
            vjust = -1.8, fontface = "bold", size = 3.5) +
  scale_fill_brewer(palette = "Set2") +
  scale_y_continuous(limits = c(0, 100),
                     labels = scales::percent_format(scale = 1)) +
  labs(
    title    = "Taux de scolarisation des 6-17 ans\npar zone géographique",
    x        = "Zone",
    y        = "Taux de scolarisation (%)",
    caption  = "IC à 95% — Test du Chi-deux"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position  = "none",
    plot.title       = element_text(face = "bold", hjust = 0.5),
    panel.grid.minor = element_blank()
  )

# 10.	Heatmap (geom_tile) croisant État nigérian (state) et niveau d’éducation: part d’adultes sans instruction par État, colorée par quintile.

library(dplyr)
library(ggplot2)
library(survey)

# 1. Préparation des noms d'États (on utilise data_clean comme convenu)
data_heat_prep <- data_clean %>%
  mutate(state_nom = factor(state.x,
                            levels = 1:37,
                            labels = c("Abia","Adamawa","Akwa Ibom","Anambra","Bauchi",
                                       "Bayelsa","Benue","Borno","Cross River","Delta",
                                       "Ebonyi","Edo","Ekiti","Enugu","Gombe",
                                       "Imo","Jigawa","Kaduna","Kano","Katsina",
                                       "Kebbi","Kogi","Kwara","Lagos","Nasarawa",
                                       "Niger","Ogun","Ondo","Osun","Oyo",
                                       "Plateau","Rivers","Sokoto","Taraba","Yobe",
                                       "Zamfara","FCT")
  )) %>%
  # Filtrage des adultes et gestion des valeurs manquantes
  filter(s1q4 >= 18, !is.na(state_nom), !is.na(niveau_educ), !is.na(wt_wave4))

# 2. Calcul des statistiques pondérées par État
data_heat <- data_heat_prep %>%
  group_by(state_nom) %>%
  summarise(
    # Somme des poids pour le dénominateur (population totale de l'état)
    pop_totale = sum(wt_wave4),
    # Somme des poids pour ceux qui n'ont "Aucun" niveau
    pop_aucun  = sum(wt_wave4[niveau_educ == "Aucun"]),
    # Part pondérée
    part_aucun = pop_aucun / pop_totale
  ) %>%
  # 3. Calcul des quintiles sur la base des parts pondérées
  mutate(
    quintile = cut(part_aucun,
                   breaks = quantile(part_aucun, probs = seq(0, 1, 0.2)),
                   labels = c("Q1 (plus bas)","Q2","Q3","Q4","Q5 (plus haut)"),
                   include.lowest = TRUE)
  )

# 4. Génération de la Heatmap (Tile Plot)
ggplot(data_heat, aes(x = 1, y = reorder(state_nom, part_aucun), fill = quintile)) +
  geom_tile(color = "white", linewidth = 0.5) +
  
  # Ajout du texte (pourcentage pondéré)
  geom_text(aes(label = paste0(round(part_aucun * 100, 1), "%")),
            size = 3.2, color = "black", fontface = "bold") +
  
  # Palette de couleurs (du jaune au rouge pour marquer l'intensité)
  scale_fill_brewer(palette = "YlOrRd", direction = 1) +
  
  labs(
    title = "Part d'adultes sans instruction par État",
    subtitle = "Calculs pondérés (wt_wave4) — Adultes 18 ans et plus",
    x = NULL,
    y = "État",
    fill = "Niveau d'analphabétisme\n(par quintile)",
  
  ) +
  
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x     = element_blank(),
    axis.ticks.x    = element_blank(),
    panel.grid      = element_blank(),
    plot.title      = element_text(face = "bold", hjust = 0.5),
    plot.subtitle   = element_text(hjust = 0.5, color = "grey40"),
    legend.position = "right",
    # On ajuste la taille du texte des ordonnées si la liste est longue
    axis.text.y     = element_text(size = 8)
  )
