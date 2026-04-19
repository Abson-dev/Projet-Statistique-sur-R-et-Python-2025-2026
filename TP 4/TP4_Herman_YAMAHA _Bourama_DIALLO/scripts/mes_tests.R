library(haven)
library(labelled)
library(writexl)
library(here)

# Extraire les variables et leurs labels de la base sect11a1_plantingw4
meta <- data.frame(
  variable = names(sect11a1_plantingw4),
  label    = sapply(sect11a1_plantingw4, function(x) {
    lab <- var_label(x)
    if (is.null(lab)) NA else lab
  })
)

# Exporter vers Excel
dir.create(here("outputs", "tables"), recursive = TRUE, showWarnings = FALSE)
write_xlsx(meta, here("outputs", "tables", "labels_sect11a1_plantingw4.xlsx"))


#Extraire les variables et leurs labels de la base sect11b1_plantingw4

meta1 <- data.frame(
  variable = names(sect11b1_plantingw4),
  label = sapply(sect11b1_plantingw4 , function(x) {
    lab <- var_label(x)
    if(is.null(lab)) NA else lab
  }
)
)

#Exporter vers Excel
dir.create(here("outputs","tables"), recursive = TRUE, showWarnings = FALSE)
write_xlsx(meta1,here("outputs","tables","labels_sect11b1_planting.xlsx"))



# Vérifier que toutes les variables communes ont les mêmes valeurs
vars_communes <- intersect(names(sect11a1_plantingw4), names(sect11b1_plantingw4))
all_equal <- all(sapply(vars_communes, function(var) {
  all(sect11a1_plantingw4[[var]] == sect11b1_plantingw4[[var]], na.rm = TRUE)
}))

all_equal



#cat("Chargement de ", fichier_ponderation, "...\n")
library(dplyr)
library(haven)
library(here)
df_poids <- read_dta(here("data","raw","secta_harvestw4.dta")) %>%
  select(hhid,wt_wave4,zone,state,lga,sector)
cat(" --> ", nrow(df_poids), "ménages avec pondération", ncol(df_poids),"Variables\n")
cat(" wt_wave4 : min =", round(min(df_poids$wt_wave4, na.rm = TRUE),1),
    "| max =", round(max(df_poids$wt_wave4, na.rm = TRUE),1),
    "| NA =", sum(is.na(df_poids$wt_wave4))
    
)



library(survey)


# Design avec nest
design_nest <- svydesign(
  ids = ~ea,
  strata = ~zone,
  weights = ~wt_wave4,
  data = df_poids,
  nest = TRUE
)

# Moyenne de la superficie des parcelles

mean_nest    <- svymean(~, design_nest, na.rm = TRUE)

mean_nest


rm(list=ls())


library(dplyr)

# ================================
# 1. Tables de conversion
# ================================
conv_generale <- c("5" = 0.4, "6" = 1, "7" = 0.0001)

conv_zone <- matrix(
  c(0.00012, 0.0027,  0.00006,
    0.00016, 0.004,   0.00016,
    0.00011, 0.00494, 0.00004,
    0.00019, 0.0023,  0.00004,
    0.00021, 0.0023,  0.00013,
    0.00012, 0.00001, 0.00041),
  nrow = 6, byrow = TRUE,
  dimnames = list(1:6, c("1","2","3"))
)

unit_names <- c("1"="heap","2"="ridge","3"="stand",
                "5"="acres","6"="hectares","7"="sqm")

zone_names <- c("1"="North Central","2"="North East","3"="North West",
                "4"="South East",  "5"="South South","6"="South West")

# ================================
# 2. Fonction de conversion
# ================================
convert_ha_single <- function(valeur, unite, zone) {
  if (is.na(valeur) | is.na(unite)) return(NA_real_)
  u <- as.character(unite)
  if (u %in% names(conv_generale)) return(valeur * conv_generale[u])
  if (u %in% c("1","2","3") & !is.na(zone))
    return(valeur * conv_zone[as.character(zone), u])
  return(NA_real_)
}

# ================================
# 3. Appliquer sur base_parcelle
# ================================
base_parcelle <- base_parcelle %>%
  mutate(
    zone_11a1     = as.integer(zone_11a1),
    s11aq4b       = as.integer(s11aq4b),
    s11aq4aa      = as.numeric(s11aq4aa),
    superficie_ha = mapply(convert_ha_single, s11aq4aa, s11aq4b, zone_11a1),
    unit_name     = unit_names[as.character(s11aq4b)],
    zone_name     = zone_names[as.character(zone_11a1)]
  )

# ================================
# 4. Contrôles et warnings
# ================================
bad_units <- base_parcelle %>%
  filter(is.na(unit_name)) %>%
  pull(s11aq4b) %>% unique()
if (length(bad_units) > 0)
  message("Unités non reconnues (s11aq4b) : ", paste(bad_units, collapse = ", "))

bad_zones <- base_parcelle %>%
  filter(s11aq4b %in% c(1,2,3) & is.na(zone_name)) %>%
  pull(zone_11a1) %>% unique()
if (length(bad_zones) > 0)
  message("Zones manquantes pour unités locales (zone_11a1) : ", paste(bad_zones, collapse = ", "))

cat("NA dans superficie_ha :", sum(is.na(base_parcelle$superficie_ha)), "\n")



# ================================
# 1. Superficie totale par ménage
# ================================
superficie_menage <- base_parcelle %>%
  group_by(hhid) %>%
  summarise(
    superficie_totale_ha = sum(superficie_ha, na.rm = TRUE),
    n_parcelles          = n(),
    n_manquantes         = sum(is.na(superficie_ha))
  )

print(head(superficie_menage))

# ================================
# 2. Valeurs manquantes
# ================================
na_count <- sum(is.na(base_parcelle$superficie_ha))
na_pct   <- round(na_count / nrow(base_parcelle) * 100, 2)
cat("Valeurs manquantes :", na_count, "(", na_pct, "% )\n")

# Afficher les lignes concernées
base_parcelle %>% filter(is.na(superficie_ha))

# ================================
# 3. Valeurs aberrantes
# ================================
aberrantes <- base_parcelle %>%
  filter(superficie_ha < 0 | superficie_ha > 500)

cat("Valeurs aberrantes :", nrow(aberrantes), "\n")

# Afficher les lignes concernées
print(aberrantes %>% select(hhid, superficie_ha, unit_name, zone_name))

# Statistiques descriptives générales
summary(base_parcelle$superficie_ha)










attach(secta_harvestw4)
# Voir les hhid concernés
base_parcelle %>% 
  filter(is.na(wt_wave4)) %>% 
  select(hhid) %>% 
  distinct()

# Ces hhid existent-ils dans secta_harvestw4 ?
hhid_manquants <- base_parcelle %>% 
  filter(is.na(wt_wave4)) %>% 
  pull(hhid) %>% 
  unique()

hhid_manquants %in% secta_harvestw4$hhid




# Avant jointure : noter le nombre de lignes
n_avant <- nrow(base_parcelle)
cat("Lignes avant jointure :", n_avant, "\n")

# Jointure
base_parcelle <- base_parcelle %>%
  left_join(
    secta_harvestw4 %>% select(hhid, wt_wave4),
    by = "hhid"
  )

# Après jointure : vérifier que le nombre de lignes n'a pas changé
n_apres <- nrow(base_parcelle)
cat("Lignes après jointure :", n_apres, "\n")

# Conclusion
if (n_avant == n_apres) {
  cat("✅ Jointure correcte : l'unité statistique est restée la parcelle\n")
} else {
  cat("❌ Problème : des doublons ont été créés, vérifier secta_harvestw4\n")
}

# Vérifier les ménages sans poids
cat("Parcelles sans poids :", sum(is.na(base_parcelle$wt_wave4)), "\n")




# Nombre moyen de parcelles par ménage sans poids
base_parcelle %>%
  filter(is.na(wt_wave4)) %>%
  group_by(hhid) %>%
  summarise(n_parcelles = n()) %>%
  summarise(moyenne = mean(n_parcelles))







# 1. Définir des classes aux valeurs rondes et lisibles
breaks_ha <- c(0, 0.01, 0.05, 0.1, 0.25, 0.5, 1, 2, 5, 10, 50, Inf)
labels_ha <- c("0–0.01", "0.01–0.05", "0.05–0.1", "0.1–0.25",
               "0.25–0.5", "0.5–1", "1–2", "2–5", "5–10", "10–50", ">50")

# 2. Classer chaque parcelle
bp_clean2 <- bp_clean %>%
  filter(!is.na(superficie_ha), superficie_ha >= 0) %>%
  mutate(classe = cut(superficie_ha, breaks = breaks_ha,
                      labels = labels_ha,
                      include.lowest = TRUE, right = FALSE))

# 3. Calculer la proportion pondérée par classe
prop_data <- bp_clean2 %>%
  group_by(classe) %>%
  summarise(poids = sum(wt_wave4, na.rm = TRUE)) %>%
  mutate(proportion = poids / sum(poids))

# 4. Graphique
histog <- ggplot(prop_data, aes(x = classe, y = proportion)) +
  geom_col(fill = "#2E86AB", color = "black", alpha = 0.85) +
  geom_text(
    aes(label = percent(proportion, accuracy = 0.1)),
    vjust = -0.5,
    size = 3.5
  ) +
  scale_y_continuous(
    labels = percent_format(accuracy = 1),
    breaks = seq(0, 0.40, by = 0.02),
    limits = c(0, 0.40)
  ) +
  labs(
    title = "Distribution de la superficie par parcelle",
    subtitle = "Wave 4 — Nigeria GHS Panel | Pondéré par wt_wave4",
    x = "Classe de superficie (ha)",
    y = "Proportion pondérée"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.major.x = element_blank()
  )

ggsave(
  here("outputs", "figures", "Distribution_de_la_superficie_par_parcelle.png"),
  plot = histog, width = 10, height = 7, dpi = 300
)








library(dplyr)
library(survey)
library(haven)
library(ggplot2)
library(here)
# =============================================================================
# PARTIE 5 — Heatmap superficie médiane par État
# =============================================================================
# Agrégation par ménage avec variables géographiques
superficie_menage <- base_parcelle %>%
  group_by(hhid, state_11a1, ea_11a1, zone_11a1) %>%
  summarise(
    superficie_totale_ha = sum(superficie_ha, na.rm = TRUE),
    n_parcelles          = n(),
    wt_wave4             = first(wt_wave4),
    .groups = "drop"
  ) %>%
  filter(!is.na(wt_wave4), superficie_totale_ha > 0)

# Plan de sondage multi-degrés
design_menage <- svydesign(
  ids     = ~ea_11a1,
  strata  = ~zone_11a1,
  weights = ~wt_wave4,
  data    = superficie_menage,
  nest    = TRUE
)

# Médiane pondérée par État
median_state <- svyby(
  ~superficie_totale_ha,
  ~state_11a1,
  design_menage,
  svyquantile,
  quantiles = 0.5,
  ci        = TRUE,
  keep.var  = TRUE
)

# Préparation des données
median_state_df <- median_state %>%
  as_tibble() %>%
  rename(
    median_ha = superficie_totale_ha,
    se_median = se.superficie_totale_ha
  ) %>%
  mutate(
    wave = "Wave 4",
    # Tri du plus grand au plus petit
    state_11a1 = reorder(state_11a1, median_ha)
  )

# Heatmap
heatmap <- ggplot(median_state_df,
                  aes(x = wave, y = state_11a1, fill = median_ha)) +
  geom_tile(color = "white") +
  # Une seule couleur (bleu) avec intensité proportionnelle à la médiane
  scale_fill_gradient(
    low  = "#d0e8f5",
    high = "#08306b",
    name = "Médiane (ha)"
  ) +
  # Affichage de la valeur dans chaque barre
  geom_text(
    aes(label = round(median_ha, 2)),
    color = "white", size = 3, fontface = "bold"
  ) +
  labs(
    title = "Superficie médiane pondérée par État (Wave 4)",
    subtitle = "États classés du plus grand au plus petit",
    x     = "Vague",
    y     = "État",
    fill  = "Médiane (ha)"
  ) +
  theme_minimal(base_size = 13) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(
  here("outputs", "figures", "heatmap_etats.png"),
  plot = heatmap, width = 10, height = 10, dpi = 300
)










# =============================================================================
# QUESTION 20 — Analyse univariée de la superficie par parcelle ET par ménage
# =============================================================================

library(tidyverse)
library(scales)
library(Hmisc)
library(wCorr)
library(survey)
library(here)

# -----------------------------------------------------------------------------
# DONNÉES DE BASE
# -----------------------------------------------------------------------------

# Filtrer parcelles valides
bp_clean <- base_parcelle %>%
  filter(!is.na(superficie_ha), superficie_ha > 0)

cat("Parcelles avec superficie valide :", nrow(bp_clean), "\n")

# Agrégation par ménage (superficie totale = somme des parcelles)
menage_clean <- bp_clean %>%
  group_by(hhid) %>%
  summarise(
    superficie_totale_ha = sum(superficie_ha, na.rm = TRUE),
    n_parcelles          = n(),
    wt_wave4             = first(wt_wave4),
    .groups = "drop"
  ) %>%
  filter(!is.na(wt_wave4), superficie_totale_ha > 0)

cat("Ménages avec superficie valide :", nrow(menage_clean), "\n")

# =============================================================================
# PARTIE 1 — Histogrammes (échelle log) par parcelle ET par ménage
# =============================================================================

breaks_ha <- c(0, 0.01, 0.05, 0.1, 0.25, 0.5, 1, 2, 5, 10, 50, Inf)
labels_ha <- c("0–0.01", "0.01–0.05", "0.05–0.1", "0.1–0.25",
               "0.25–0.5", "0.5–1", "1–2", "2–5", "5–10", "10–50", ">50")

# -- Parcelle --
prop_parcelle <- bp_clean %>%
  mutate(classe = cut(superficie_ha, breaks = breaks_ha,
                      labels = labels_ha,
                      include.lowest = TRUE, right = FALSE)) %>%
  group_by(classe) %>%
  summarise(poids = sum(wt_wave4, na.rm = TRUE)) %>%
  mutate(proportion = poids / sum(poids))

hist_parcelle <- ggplot(prop_parcelle, aes(x = classe, y = proportion)) +
  geom_col(fill = "#2E86AB", color = "black", alpha = 0.85) +
  geom_text(aes(label = percent(proportion, accuracy = 0.1)),
            vjust = -0.5, size = 3.5) +
  scale_y_continuous(labels = percent_format(accuracy = 1),
                     breaks = seq(0, 0.40, by = 0.02),
                     limits = c(0, 0.42)) +
  labs(title    = "Distribution de la superficie par parcelle (échelle log)",
       subtitle = "Wave 4 — Nigeria GHS Panel | Pondéré par wt_wave4",
       x = "Classe de superficie (ha)", y = "Proportion pondérée") +
  theme_minimal(base_size = 13) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.major.x = element_blank())

ggsave(here("outputs", "figures", "hist_superficie_parcelle.png"),
       plot = hist_parcelle, width = 10, height = 7, dpi = 300)

# -- Ménage --
prop_menage <- menage_clean %>%
  mutate(classe = cut(superficie_totale_ha, breaks = breaks_ha,
                      labels = labels_ha,
                      include.lowest = TRUE, right = FALSE)) %>%
  group_by(classe) %>%
  summarise(poids = sum(wt_wave4, na.rm = TRUE)) %>%
  mutate(proportion = poids / sum(poids))

hist_menage <- ggplot(prop_menage, aes(x = classe, y = proportion)) +
  geom_col(fill = "#E07B39", color = "black", alpha = 0.85) +
  geom_text(aes(label = percent(proportion, accuracy = 0.1)),
            vjust = -0.5, size = 3.5) +
  scale_y_continuous(labels = percent_format(accuracy = 1),
                     breaks = seq(0, 0.40, by = 0.02),
                     limits = c(0, 0.42)) +
  labs(title    = "Distribution de la superficie totale par ménage (échelle log)",
       subtitle = "Wave 4 — Nigeria GHS Panel | Pondéré par wt_wave4",
       x = "Classe de superficie (ha)", y = "Proportion pondérée") +
  theme_minimal(base_size = 13) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.major.x = element_blank())

ggsave(here("outputs", "figures", "hist_superficie_menage.png"),
       plot = hist_menage, width = 10, height = 7, dpi = 300)

# =============================================================================
# PARTIE 2 — Statistiques descriptives par décile (parcelle ET ménage)
# =============================================================================

# -- Parcelle --
deciles_breaks_parcelle <- wtd.quantile(
  bp_clean$superficie_ha,
  weights = bp_clean$wt_wave4,
  probs   = seq(0, 1, by = 0.1)
)

bp_clean <- bp_clean %>%
  mutate(decile = cut(superficie_ha,
                      breaks         = deciles_breaks_parcelle,
                      labels         = paste0("D", 1:10),
                      include.lowest = TRUE))

stats_decile_parcelle <- bp_clean %>%
  group_by(decile) %>%
  summarise(
    n_parcelles  = n(),
    moy_ponderee = round(weighted.mean(superficie_ha, wt_wave4, na.rm = TRUE), 3),
    mediane_pond = round(wtd.quantile(superficie_ha, weights = wt_wave4, probs = 0.5), 3),
    min          = round(min(superficie_ha, na.rm = TRUE), 3),
    max          = round(max(superficie_ha, na.rm = TRUE), 3),
    .groups = "drop"
  )

cat("\n--- Statistiques par décile (PARCELLE) ---\n")
print(stats_decile_parcelle, n = 10)

# -- Ménage --
deciles_breaks_menage <- wtd.quantile(
  menage_clean$superficie_totale_ha,
  weights = menage_clean$wt_wave4,
  probs   = seq(0, 1, by = 0.1)
)

menage_clean <- menage_clean %>%
  mutate(decile = cut(superficie_totale_ha,
                      breaks         = deciles_breaks_menage,
                      labels         = paste0("D", 1:10),
                      include.lowest = TRUE))

stats_decile_menage <- menage_clean %>%
  group_by(decile) %>%
  summarise(
    n_menages    = n(),
    moy_ponderee = round(weighted.mean(superficie_totale_ha, wt_wave4, na.rm = TRUE), 3),
    mediane_pond = round(wtd.quantile(superficie_totale_ha, weights = wt_wave4, probs = 0.5), 3),
    min          = round(min(superficie_totale_ha, na.rm = TRUE), 3),
    max          = round(max(superficie_totale_ha, na.rm = TRUE), 3),
    .groups = "drop"
  )

cat("\n--- Statistiques par décile (MÉNAGE) ---\n")
print(stats_decile_menage, n = 10)

# =============================================================================
# PARTIE 3 — Boxplots pondérés (parcelle ET ménage)
# =============================================================================

# Fonction pour calculer les quantiles pondérés pour le boxplot
boxplot_stats_pond <- function(x, w) {
  q <- wtd.quantile(x, weights = w, probs = c(0.25, 0.5, 0.75))
  iqr <- q[3] - q[1]
  data.frame(
    ymin   = max(min(x), q[1] - 1.5 * iqr),
    lower  = q[1],
    middle = q[2],
    upper  = q[3],
    ymax   = min(max(x), q[3] + 1.5 * iqr)
  )
}

# -- Parcelle --
bp_stats <- boxplot_stats_pond(bp_clean$superficie_ha, bp_clean$wt_wave4)

box_parcelle <- ggplot(bp_stats, aes(x = "Parcelles W4")) +
  geom_boxplot(
    aes(ymin = ymin, lower = lower, middle = middle,
        upper = upper, ymax = ymax),
    stat = "identity", fill = "#2E86AB", alpha = 0.7, width = 0.4
  ) +
  scale_y_log10() +
  labs(title    = "Boxplot de la superficie par parcelle (pondéré)",
       subtitle = "Quantiles pondérés par wt_wave4 — échelle log",
       x = "", y = "Superficie (ha) — échelle log") +
  theme_minimal(base_size = 13)

ggsave(here("outputs", "figures", "boxplot_superficie_parcelle.png"),
       plot = box_parcelle, width = 6, height = 7, dpi = 300)

# -- Ménage --
men_stats <- boxplot_stats_pond(menage_clean$superficie_totale_ha, menage_clean$wt_wave4)

box_menage <- ggplot(men_stats, aes(x = "Ménages W4")) +
  geom_boxplot(
    aes(ymin = ymin, lower = lower, middle = middle,
        upper = upper, ymax = ymax),
    stat = "identity", fill = "#E07B39", alpha = 0.7, width = 0.4
  ) +
  scale_y_log10() +
  labs(title    = "Boxplot de la superficie totale par ménage (pondéré)",
       subtitle = "Quantiles pondérés par wt_wave4 — échelle log",
       x = "", y = "Superficie totale (ha) — échelle log") +
  theme_minimal(base_size = 13)

ggsave(here("outputs", "figures", "boxplot_superficie_menage.png"),
       plot = box_menage, width = 6, height = 7, dpi = 300)

# =============================================================================
# PARTIE 4 — Scatter déclaré vs GPS + ligne 45° + Spearman pondéré
# =============================================================================

bp_gps <- base_parcelle %>%
  filter(!is.na(superficie_ha), !is.na(s11aq4c),
         superficie_ha > 0, s11aq4c > 0,
         !is.na(wt_wave4)) %>%
  mutate(superficie_gps_ha = s11aq4c / 10000)  # Conversion m² → ha

cat("\nObservations avec GPS non manquant :", nrow(bp_gps), "\n")
cat("Valeurs manquantes s11aq4c :", sum(is.na(base_parcelle$s11aq4c)), "\n")

# Corrélation de Spearman pondérée
rho_spearman <- weightedCorr(
  bp_gps$superficie_ha,
  bp_gps$superficie_gps_ha,
  method  = "Spearman",
  weights = bp_gps$wt_wave4
)
cat("Corrélation de Spearman pondérée :", round(rho_spearman, 3), "\n")

scatter_gps <- ggplot(bp_gps, aes(x = superficie_ha, y = superficie_gps_ha)) +
  geom_point(aes(size = wt_wave4), alpha = 0.3, color = "#2E86AB") +
  geom_abline(slope = 1, intercept = 0,
              color = "red", linewidth = 1, linetype = "dashed") +
  scale_x_log10() +
  scale_y_log10() +
  scale_size_continuous(guide = "none") +
  labs(
    title    = "Superficie déclarée vs Superficie GPS",
    subtitle = paste0("Wave 4 | Spearman pondéré ρ = ", round(rho_spearman, 3),
                      " | N = ", nrow(bp_gps), " parcelles"),
    x        = "Superficie déclarée (ha) — échelle log",
    y        = "Superficie GPS (ha) — échelle log",
    caption  = paste0("Note : ", sum(is.na(base_parcelle$s11aq4c)),
                      " valeurs manquantes exclues pour s11aq4c")
  ) +
  theme_minimal(base_size = 13)

ggsave(here("outputs", "figures", "scatter_declare_vs_gps.png"),
       plot = scatter_gps, width = 8, height = 7, dpi = 300)





#===============================================================================
#Pour chercher un mot dans mon code
#===============================================================================

library(readr)
library(dplyr)
library(stringr)
library(here)

# 1. Charger le fichier contenant le code (ex. script R)
code <- read_lines(here("scripts","02_analyse.R"))

# 2. Transformer en tibble pour manipuler facilement
df <- tibble(ligne = seq_along(code), contenu = code)

# 3. Chercher un mot clé (ex. "mutate")
mot_cle <- "prop_data"

resultats <- df %>%
  filter(str_detect(contenu, fixed(mot_cle, ignore_case = TRUE)))

print(resultats)








