# ==========================================
# SCRIPT 03 : ANALYSE ET VISUALISATION
# ==========================================

library(dplyr)
library(ggplot2)
library(viridis)
library(patchwork)
library(rstatix)

# 1. Charger les données propres
df <- readRDS("data/processed/data_ready.rds")

# --- TÂCHE 8 : Barplot horizontal des fréquences ---

plot_freq <- df %>%
  count(niveau_educ) %>%
  ggplot(aes(x = niveau_educ, y = n, fill = niveau_educ)) +
  geom_col() +
  coord_flip() + # Pour le rendre horizontal
  scale_fill_viridis_d(option = "mako") +
  labs(title = "Distribution du niveau d'éducation au Nigeria",
       x = "Niveau d'instruction",
       y = "Nombre d'individus",
       fill = "Catégorie") +
  theme_minimal()

# Afficher le graphique
print(plot_freq)

# Sauvegarder le graphique
ggsave("output/figures/barplot_frequences.png", plot_freq, width = 8, height = 5)


# --- TÂCHE 9 : Comparaison Hommes/Femmes (Adultes 18+) ---

# On utilise zap_labels() pour transformer la variable "spéciale Stata" en nombre pur
library(haven)

df_adultes <- df %>%
  filter(s1q4 >= 18) %>%
  mutate(sexe = ifelse(as.numeric(zap_labels(s1q2)) == 1, "Homme", "Femme"))

# Graphique barres 100% empilées
plot_sexe <- ggplot(df_adultes, aes(x = sexe, fill = niveau_educ)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_viridis_d() +
  labs(title = "Niveau d'éducation par sexe (18 ans et +)",
       x = "Sexe",
       y = "Proportion",
       fill = "Éducation") +
  theme_minimal()

print(plot_sexe)

# Sauvegarder
ggsave("output/figures/education_par_sexe.png", plot_sexe, width = 8, height = 5)

# --- TÂCHE 9 (Suite) : Statistiques ---

# 1. Créer le tableau de contingence (Fréquences observées)
table_contingence <- table(df_adultes$sexe, df_adultes$niveau_educ)
print("--- TABLEAU DE CONTINGENCE ---")
print(table_contingence)

# 2. Test du Chi-deux
test_chi2 <- chisq_test(table_contingence)
print("--- RÉSULTAT DU TEST DU CHI-DEUX ---")
print(test_chi2)

# 3. Calcul du V de Cramér (pour mesurer l'intensité de la relation)
# On utilise la fonction du package rstatix
v_cramer <- cramer_v(table_contingence)
print(paste("V de Cramér :", round(v_cramer, 4)))

# 4. Exportation du tableau propre pour le rapport
library(gtsummary)
table_propre <- df_adultes %>%
  select(sexe, niveau_educ) %>%
  tbl_summary(by = sexe, 
              label = list(niveau_educ ~ "Niveau d'éducation")) %>%
  add_p() # Ajoute la p-value du test automatiquement

# Sauvegarder le tableau en format image ou texte
library(dplyr)
library(gtsummary)
library(rstatix)
tab_sexe <- df_adultes %>%
  filter(!is.na(sexe), !is.na(niveau_educ)) %>%
  select(sexe, niveau_educ) %>%
  tbl_summary(
    by = sexe,
    label = list(niveau_educ ~ "Niveau d'instruction")
  ) %>%
  add_p()
# Calcul des statistiques
chi2_sexe <- rstatix::chisq_test(table(df_adultes$sexe, df_adultes$niveau_educ))
v_sexe    <- rstatix::cramer_v(table(df_adultes$sexe, df_adultes$niveau_educ))

# Mise à jour du tableau gtsummary et sauvegarde
tab_sexe %>%
  modify_footnote(
    everything() ~ paste0("Test du Khi-deux : Chi2(", chi2_sexe$df, ") = ", 
                          round(chi2_sexe$statistic, 2), 
                          ", p = ", format.pval(chi2_sexe$p, digits = 3), 
                          " | V de Cramér = ", round(v_sexe, 3))
  ) %>%
  saveRDS("output/tables/tab_sexe.rds")

# (On pourra l'insérer dans le rapport Qmd plus tard)

# --- TÂCHE 10 : ÉDUCATION VS ÂGE ---

# 1. Graphique : Boxplot de l'âge par niveau d'éducation
plot_age_educ <- ggplot(df_adultes, aes(x = niveau_educ, y = s1q4, fill = niveau_educ)) +
  geom_boxplot(alpha = 0.7) +
  scale_fill_viridis_d(option = "plasma") +
  labs(title = "Distribution de l'âge par niveau d'éducation",
       x = "Niveau d'éducation",
       y = "Âge (ans)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) # Incline les étiquettes pour la lisibilité

print(plot_age_educ)

# Sauvegarder le graphique
ggsave("output/figures/boxplot_age_education.png", plot_age_educ, width = 10, height = 6)

# 2. Test statistique de Kruskal-Wallis
test_kruskal <- kruskal.test(s1q4 ~ niveau_educ, data = df_adultes)

print("--- TEST DE KRUSKAL-WALLIS (Âge vs Éducation) ---")
print(test_kruskal)


# ==========================================================
# TÂCHE 11 : SCOLARISATION 6-17 ANS (URBAIN VS RURAL)
# ==========================================================

library(haven)
library(dplyr)
library(ggplot2)

# 1. Charger les bases
secta_w4 <- read_dta("data/raw/secta_harvestw4.dta")
sect2_w4 <- read_dta("data/raw/sect2_harvestw4.dta")
sect1_w4 <- read_dta("data/raw/sect1_harvestw4.dta")

# 2. Fusion et préparation robuste
df_scolar <- sect2_w4 %>%
  left_join(sect1_w4, by = c("hhid", "indiv")) %>%
  left_join(secta_w4 %>% select(hhid, sector), by = "hhid") %>%
  filter(s1q4 >= 6 & s1q4 <= 17) %>%
  mutate(
    # On transforme en facteur pour lire les étiquettes Stata (Urban/Rural, Yes/No)
    zone = as_factor(sector),
    scolarise = as_factor(s2aq6)
  )

# 3. Calcul des proportions avec Intervalle de Confiance (IC)
df_plot <- df_scolar %>%
  # On enlève les NA pour ne pas fausser les pourcentages
  filter(!is.na(scolarise), !is.na(zone)) %>%
  group_by(zone, scolarise) %>%
  summarise(n = n(), .groups = 'drop') %>%
  group_by(zone) %>%
  mutate(
    prop = n / sum(n),
    se = sqrt(prop * (1 - prop) / sum(n)),
    ic_inf = prop - 1.96 * se,
    ic_sup = prop + 1.96 * se
  ) %>%
  # FILTRE CRUCIAL : on garde uniquement ceux qui sont scolarisés
  # On cherche "Yes", "Oui" ou le chiffre "1" dans l'étiquette
  filter(grepl("Yes|Oui|1", scolarise, ignore.case = TRUE))

# 4. Génération du graphique
plot_urb_rur <- ggplot(df_plot, aes(x = zone, y = prop, fill = zone)) +
  geom_col(width = 0.5, show.legend = FALSE) +
  geom_errorbar(aes(ymin = ic_inf, ymax = ic_sup), width = 0.1) +
  scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
  scale_fill_viridis_d(option = "mako", begin = 0.3, end = 0.7) +
  labs(title = "Taux de scolarisation des 6-17 ans par zone",
       subtitle = "Source : GHS Nigeria Wave 4 | IC à 95%",
       x = "Zone de résidence", 
       y = "Taux de scolarisation (%)") +
  theme_minimal()

print(plot_urb_rur)

# 5. Sauvegarde
ggsave("output/figures/scolarisation_zone_final.png", plot_urb_rur, width = 8, height = 5)

# 6. Test statistique pour ton rapport
print("--- TEST DU CHI-DEUX ---")
print(chisq.test(table(df_scolar$zone, df_scolar$scolarise)))

library(dplyr)
library(gtsummary)
library(rstatix)

# ÉTAPE A : Créer l'objet
tab_zone <- df_scolar %>%
  filter(!is.na(zone), !is.na(scolarise)) %>%
  select(zone, scolarise) %>%
  tbl_summary(
    by = zone,
    label = list(scolarise ~ "Scolarisé")
  ) %>%
  add_p()

# ÉTAPE B : Calculer les stats
chi2_zone <- rstatix::chisq_test(table(df_scolar$zone, df_scolar$scolarise))
v_zone    <- rstatix::cramer_v(table(df_scolar$zone, df_scolar$scolarise))

# ÉTAPE C : Note et sauvegarde
tab_zone %>%
  modify_footnote(
    everything() ~ paste0("Test du Khi-deux : Chi2(", chi2_zone$df, ") = ", 
                          round(chi2_zone$statistic, 2), 
                          ", p = ", format.pval(chi2_zone$p, digits = 3), 
                          " | V de Cramér = ", round(v_zone, 3))
  ) %>%
  saveRDS("output/tables/tab_zone.rds")

# ==========================================================
# TÂCHE 12 : HEATMAP DE L'ILLETTRISME PAR ÉTAT 
# ==========================================================

library(haven)
library(dplyr)
library(ggplot2)
library(forcats)

# 1. Préparation des données
# On récupère les états dans secta et on les joint aux données adultes
secta_w4 <- read_dta("data/raw/secta_harvestw4.dta")
df_ready <- readRDS("data/processed/data_ready.rds")

df_heatmap <- df_ready %>%
  filter(s1q4 >= 18) %>% # Uniquement les adultes
  left_join(secta_w4 %>% select(hhid, state), by = "hhid") %>%
  mutate(
    etat_nom = as_factor(state),
    sans_instruction = ifelse(niveau_educ == "Aucun", 1, 0)
  ) %>%
  # Calculer la part d'adultes sans instruction par État
  group_by(etat_nom) %>%
  summarise(part_sans_inst = mean(sans_instruction, na.rm = TRUE) * 100) %>%
  # Créer les quintiles (5 groupes de couleurs)
  mutate(quintile = ntile(part_sans_inst, 5))

# 2. Création de la Heatmap (geom_tile)
plot_heatmap <- ggplot(df_heatmap, aes(x = 1, y = fct_reorder(etat_nom, part_sans_inst))) +
  geom_tile(aes(fill = as.factor(quintile)), color = "white") +
  geom_text(aes(label = paste0(round(part_sans_inst, 1), "%")), color = "black", size = 3) +
  scale_fill_brewer(palette = "YlOrRd", name = "Quintile\n(1=Bas, 5=Haut)") +
  labs(
    title = "Part d'adultes sans instruction par État",
    subtitle = "Coloré par quintile (Rouge foncé = Taux le plus élevé)",
    x = "",
    y = "États du Nigeria"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_blank(), # C'est element_blank() qu'il fallait utiliser
    axis.ticks.x = element_blank(),
    panel.grid = element_blank()
  )

print(plot_heatmap)

# 3. Sauvegarde
ggsave("output/figures/heatmap_education_etats.png", plot_heatmap, width = 7, height = 10)

