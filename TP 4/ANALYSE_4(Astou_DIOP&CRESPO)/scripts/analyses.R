# scripts des analyses.
# Analyses statistiques pondérées et visualisations

##====construction de la variable  superficie en hectares; Calcule de la superficie totale par ménage (sum par hhid).
#Description les valeurs  manquantes et les valeurs aberrantes (superficie < 0 ou > 500 ha). ======
# Labels des États nigérians
state_labels <- c(
  `1`="Abia",`2`="Adamawa",`3`="Akwa Ibom",`4`="Anambra",`5`="Bauchi",
  `6`="Bayelsa",`7`="Benue",`8`="Borno",`9`="Cross River",`10`="Delta",
  `11`="Ebonyi",`12`="Edo",`13`="Ekiti",`14`="Enugu",`15`="FCT",
  `16`="Gombe",`17`="Imo",`18`="Jigawa",`19`="Kaduna",`20`="Kano",
  `21`="Katsina",`22`="Kebbi",`23`="Kogi",`24`="Kwara",`25`="Lagos",
  `26`="Nasarawa",`27`="Niger",`28`="Ogun",`29`="Ondo",`30`="Osun",
  `31`="Oyo",`32`="Plateau",`33`="Rivers",`34`="Sokoto",`35`="Taraba",
  `36`="Yobe",`37`="Zamfara"
)

etats_nigeria <- c(
  "1"  = "Abia",        "2"  = "Adamawa",    "3"  = "Akwa Ibom",
  "4"  = "Anambra",     "5"  = "Bauchi",      "6"  = "Bayelsa",
  "7"  = "Benue",       "8"  = "Borno",       "9"  = "Cross River",
  "10" = "Delta",       "11" = "Ebonyi",      "12" = "Edo",
  "13" = "Ekiti",       "14" = "Enugu",       "15" = "FCT Abuja",
  "16" = "Gombe",       "17" = "Imo",         "18" = "Jigawa",
  "19" = "Kaduna",      "20" = "Kano",        "21" = "Katsina",
  "22" = "Kebbi",       "23" = "Kogi",        "24" = "Kwara",
  "25" = "Lagos",       "26" = "Nasarawa",    "27" = "Niger",
  "28" = "Ogun",        "29" = "Ondo",        "30" = "Osun",
  "31" = "Oyo",         "32" = "Plateau",     "33" = "Rivers",
  "34" = "Sokoto",      "35" = "Taraba",      "36" = "Yobe",
  "37" = "Zamfara"
)

# ============================================================
# TÂCHE 1 : Construction de la variable superficie en hectares
# ============================================================

# Codes unités superficie (questionnaire PDF section 11A) :
# 1=Heaps, 2=Ridges, 3=Stands, 5=Acres, 6=Hectares, 7=Square meters
df_parcelles <- sect11a1_clean %>%
  zap_labels() %>%
  rename_with(tolower)

# Tenure foncière + caractéristiques parcelles (post-planting W4)
df_tenure <- read_dta(file.path(DATA_DIR, "sect11b1_plantingw4.dta")) %>%
  zap_labels() %>%
  rename_with(tolower)

# Identifiants ménage + rural/urbain (post-harvest W4)
df_menage <- read_dta(file.path(DATA_DIR, "secta_harvestw4.dta")) %>%
  zap_labels() %>%
  rename_with(tolower) %>%
  select(hhid, sector, state, zone, wt_wave4) %>%
  mutate(
    milieu   = case_when(sector == 1 ~ "Urbain", sector == 2 ~ "Rural",
                         TRUE ~ NA_character_),
    nom_etat = etats_nigeria[as.character(state)],
    nom_etat = if_else(is.na(nom_etat), paste0("État ", state), nom_etat)
  )

# Données GPS post-récolte W4 (prefilled_gps_area)
df_gps <- read_dta(here("data", "raw", "Base_vague", "NGA_2018_GHSP-W4_v03_M_Stata12", "secta1_harvestw4.dta")) %>%
  zap_labels() %>%
  rename_with(tolower) %>%
  select(hhid, plotid, prefilled_gps_area, sa1q9, sa1q11) %>%
  rename(
    gps_mesure_ph    = sa1q9,           # mesuré par GPS post-récolte ?
    superficie_ph    = sa1q11           # superficie déclarée post-récolte
  )
# Facteurs de conversion zone-spécifiques vers hectares (source : World Bank GHS)
# Les unités traditionnelles (Heaps, Ridges, Stands) varient selon la zone géographique
# zone : 1=North Central, 2=North East, 3=North West,
#         4=South East,   5=South South, 6=South West
facteurs_conversion <- tribble(
  ~zone, ~heaps,   ~ridges,  ~stands,
  1,     0.00012,  0.0027,   0.00006,   # North Central
  2,     0.00016,  0.0040,   0.00016,   # North East
  3,     0.00011,  0.00494,  0.00004,   # North West
  4,     0.00019,  0.0023,   0.00004,   # South East
  5,     0.00021,  0.0023,   0.00013,   # South South
  6,     0.00012,  0.00001,  0.00041    # South West
)

df_sup_clean <- df_parcelles %>%
  select(hhid, plotid, s11aq4a, s11aq4aa, s11aq4b, s11aq4c, s11b1q27,
         sector, state, zone) %>%
  rename(
    gps_mesure       = s11aq4a,      # mesuré par GPS ?
    sup_declaree     = s11aq4aa,     # superficie déclarée (nombre)
    unite_sup        = s11aq4b,      # unité
    sup_gps_m2       = s11aq4c,      # GPS en m²
    plot_cultive     = s11b1q27      # parcelle cultivée ?
  ) %>%
  # Joindre GPS post-récolte
  left_join(df_gps, by = c("hhid", "plotid")) %>%
  # Joindre les facteurs de conversion selon la zone
  left_join(facteurs_conversion, by = "zone") %>%
  mutate(
    # GPS W4 post-planting en hectares
    # Mesure physique faite par l'enquêteur lors de la visite post-planting
    # Priorité 1 : mesure directe, la plus fiable
    sup_gps_ha = sup_gps_m2 / 10000,
    
    # GPS W4 post-récolte en hectares (prefilled depuis visite précédente)
    # Priorité 2 : substitut si GPS post-planting manquant
    sup_gps_ph_ha = prefilled_gps_area / 10000,
    
    # Superficie déclarée convertie en hectares selon unité et zone
    # Priorité 3 : déclaration de l'agriculteur, moins fiable (biais de surestimation)
    sup_declaree_ha = case_when(
      unite_sup == 1 ~ sup_declaree * heaps,       # Heaps  -> ha (zone-spécifique)
      unite_sup == 2 ~ sup_declaree * ridges,      # Ridges -> ha (zone-spécifique)
      unite_sup == 3 ~ sup_declaree * stands,      # Stands -> ha (zone-spécifique)
      unite_sup == 5 ~ sup_declaree * 0.404686,    # Acres  -> ha (standard)
      unite_sup == 6 ~ sup_declaree,               # Hectares (pas de conversion)
      unite_sup == 7 ~ sup_declaree / 10000,       # m²     -> ha
      TRUE           ~ NA_real_                    # unité inconnue
    ),
    
    # Superficie finale : GPS post-planting > GPS post-récolte > déclarée
    superficie_ha = case_when(
      !is.na(sup_gps_ha)    & sup_gps_ha    > 0 ~ sup_gps_ha,
      !is.na(sup_gps_ph_ha) & sup_gps_ph_ha > 0 ~ sup_gps_ph_ha,
      !is.na(sup_declaree_ha)                    ~ sup_declaree_ha,
      TRUE                                        ~ NA_real_
    ),
    
    # Valeurs aberrantes -> NA
    superficie_ha = if_else(superficie_ha <= 0 | superficie_ha > 500,
                            NA_real_, superficie_ha),
    
    # Milieu et État
    milieu   = case_when(sector == 1 ~ "Urbain", sector == 2 ~ "Rural",
                         TRUE ~ NA_character_),
    nom_etat = etats_nigeria[as.character(state)],
    nom_etat = if_else(is.na(nom_etat), paste0("État ", state), nom_etat)
  )

saveRDS(df_sup_clean,  here("data", "processed", "df_sup_clean.dta"))

message("Valeurs manquantes superficie : ",
        sum(is.na(df_sup_clean$superficie_ha)))
message("Valeurs aberrantes écartées   : ",
        sum(df_parcelles$s11aq4aa > 500, na.rm = TRUE))


# ============================================================
# TÂCHE 2 : Superficie totale par ménage + statistiques pondérées
# ============================================================
sect11a1_clean <- sect11a1_clean %>%
  mutate(area_ha = s11aq4aa / 10000) %>%      
  filter(!is.na(area_ha))

# Agrégation au niveau ménage
superficie_menage <- sect11a1_clean %>%
  group_by(hhid) %>%
  summarise(
    area_totale_ha = sum(area_ha, na.rm = TRUE),
    nb_parcelles   = n(),
    wt_wave4       = first(wt_wave4),
    .groups        = "drop"
  )

# Intégration dans sect11a1_clean
sect11a1_clean <- sect11a1_clean %>%
  left_join(superficie_menage %>% select(hhid, area_totale_ha, nb_parcelles),
            by = "hhid")

# Plan de sondage au niveau ménage pour les statistiques pondérées
plan_superficie <- superficie_menage %>%
  filter(!is.na(wt_wave4)) %>%
  as_survey_design(
    weights = wt_wave4
  )


#=============================================================================
  # TÂCHE 19 : Description valeurs manquantes et aberrantes
  # =============================================================================

# Statistiques de base sur la superficie brute
stats_brutes <- df_sup_clean %>%
  summarise(
    N_total         = n(),
    N_GPS_dispo     = sum(!is.na(sup_gps_m2)),
    N_declaree_dispo = sum(!is.na(sup_declaree_ha)),
    N_finale_dispo  = sum(!is.na(superficie_ha)),
    N_manquants     = sum(is.na(superficie_ha)),
    pct_manquants   = round(mean(is.na(superficie_ha)) * 100, 1),
    N_aberrants_neg = sum(sup_gps_ha < 0, na.rm = TRUE),
    N_aberrants_500 = sum(sup_gps_ha > 500, na.rm = TRUE)
  )

print(stats_brutes)

stats_brutes %>%
  gt::gt() %>%
  gt::gtsave(here("outputs", "tables", "description_des_valeurs_manquante.rtf"))

# =============================================================================
# TÂCHE 20 : Analyse univariée de la superficie
# =============================================================================

# --- Statistiques descriptives par décile ------------------------------------
deciles_sup <- df_sup_clean  %>%
  filter(!is.na(superficie_ha)) %>%
  summarise(
    N         = n(),
    Min       = min(superficie_ha),
    D1        = quantile(superficie_ha, 0.10),
    D2        = quantile(superficie_ha, 0.20),
    D3        = quantile(superficie_ha, 0.30),
    Q1        = quantile(superficie_ha, 0.25),
    D5        = quantile(superficie_ha, 0.50),
    Moyenne   = mean(superficie_ha),
    D7        = quantile(superficie_ha, 0.70),
    Q3        = quantile(superficie_ha, 0.75),
    D9        = quantile(superficie_ha, 0.90),
    Max       = max(superficie_ha),
    EcartType = sd(superficie_ha),
    CV        = sd(superficie_ha) / mean(superficie_ha) * 100
  ) %>%
  mutate(across(where(is.numeric), ~ round(., 4)))

deciles_sup

deciles_sup %>%
  gt::gt() %>%
  gt::gtsave(here("outputs", "tables", "Decile_par_parceles.rtf"))




# ============================================================
# PACKAGES NÉCESSAIRES
# ============================================================
library(tidyverse)
library(srvyr)
library(gtsummary)
library(patchwork)
library(gt)
library(here)

# Couleurs organismes internationaux (Banque Mondiale / FAO)
col_wb   <- "#009FDA"   # Bleu Banque Mondiale
col_fao  <- "#4CAF50"   # Vert FAO
col_warn <- "#E63329"   # Rouge alerte
col_grey <- "#6C757D"   # Gris neutre

# ============================================================
# CONSTRUCTION DE area_ha ET superficie_menage
# ============================================================

sect11a1_clean <- sect11a1_clean %>%
  mutate(
    # Superficie GPS convertie de m² en hectares
    area_gps_ha = if_else(!is.na(s11aq4c) & s11aq4c > 0,
                          s11aq4c * 0.0001, NA_real_),
    
    # Superficie déclarée convertie en hectares selon l'unité (s11aq4b)
    area_declared_ha = case_when(
      s11aq4b == 6 ~ s11aq4aa * 1.0,        # Déjà en hectares
      s11aq4b == 5 ~ s11aq4aa * 0.404686,   # Acres -> ha
      s11aq4b == 7 ~ s11aq4aa * 0.0001,     # m² -> ha
      s11aq4b == 2 ~ s11aq4aa * 0.01,       # Ridges -> ha (approx.)
      s11aq4b == 1 ~ s11aq4aa * 0.00012,    # Heaps -> ha (approx.)
      s11aq4b == 3 ~ s11aq4aa * 0.01,       # Stands -> ha (approx.)
      TRUE         ~ NA_real_
    ),
    
    # Variable finale : priorité à la mesure GPS, sinon déclaration
    area_ha = if_else(!is.na(area_gps_ha), area_gps_ha, area_declared_ha)
  )

# Agrégation au niveau ménage : superficie totale et nombre de parcelles
superficie_menage <- sect11a1_clean %>%
  group_by(hhid) %>%
  summarise(
    area_totale_ha = sum(area_ha, na.rm = TRUE),
    nb_parcelles   = n(),
    wt_wave4       = first(wt_wave4),
    .groups        = "drop"
  )

# Intégration de la superficie totale dans la base parcelle
sect11a1_clean <- sect11a1_clean %>%
  left_join(superficie_menage %>% select(hhid, area_totale_ha, nb_parcelles),
            by = "hhid")


# ============================================================
# TÂCHE 1A : HISTOGRAMME EN ÉCHELLE LOG — PARCELLE ET MÉNAGE
# ============================================================

# Filtrage des valeurs aberrantes et manquantes pour les graphiques
dat_parc <- sect11a1_clean %>%
  filter(!is.na(area_ha), area_ha > 0, area_ha <= 500)

dat_men <- superficie_menage %>%
  filter(!is.na(area_totale_ha), area_totale_ha > 0, area_totale_ha <= 500)

# Histogramme pondéré de la superficie par parcelle (échelle log)
hist_parc <- ggplot(dat_parc, aes(x = area_ha, weight = wt_wave4)) +
  geom_histogram(bins = 50, fill = col_wb, color = "white", alpha = 0.85) +
  scale_x_log10(labels = scales::label_number(accuracy = 0.01)) +
  scale_y_continuous(labels = scales::label_comma()) +
  labs(
    title    = "Distribution de la superficie par parcelle",
    subtitle = "Échelle logarithmique — parcelles ≤ 500 ha",
    x        = "Superficie (ha, échelle log)",
    y        = "Fréquence (pondérée)",
    caption  = "Source : GHS Panel W4, Nigeria 2018-2019"
  ) +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold"))

# Histogramme pondéré de la superficie totale par ménage (échelle log)
hist_men <- ggplot(dat_men, aes(x = area_totale_ha, weight = wt_wave4)) +
  geom_histogram(bins = 50, fill = col_fao, color = "white", alpha = 0.85) +
  scale_x_log10(labels = scales::label_number(accuracy = 0.01)) +
  scale_y_continuous(labels = scales::label_comma()) +
  labs(
    title    = "Distribution de la superficie totale par ménage",
    subtitle = "Échelle logarithmique — ménages ≤ 500 ha",
    x        = "Superficie totale (ha, échelle log)",
    y        = "Fréquence (pondérée)",
    caption  = "Source : GHS Panel W4, Nigeria 2018-2019"
  ) +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold"))

# Assemblage des deux histogrammes en une seule figure (patchwork)
fig_hist <- hist_parc / hist_men
ggsave(here("outputs", "figures", "fig01_histogramme_superficie.png"),
       fig_hist, width = 10, height = 8, dpi = 300)
cat("fig01 exportée\n")


# ============================================================
# TÂCHE 1C : BOXPLOT SUPERFICIE PARCELLE ET MÉNAGE
# ============================================================

# Combinaison des données parcelle et ménage pour le boxplot
dat_box <- bind_rows(
  dat_parc %>% transmute(superficie = area_ha,        niveau = "Parcelle", wt_wave4),
  dat_men  %>% transmute(superficie = area_totale_ha, niveau = "Ménage",   wt_wave4)
)

# Boxplot pondéré en échelle logarithmique
fig_box <- ggplot(dat_box, aes(x = niveau, y = superficie,
                               fill = niveau, weight = wt_wave4)) +
  geom_boxplot(outlier.shape = 21, outlier.size = 1.2,
               outlier.alpha = 0.4, alpha = 0.8) +
  scale_y_log10(labels = scales::label_number(accuracy = 0.01)) +
  scale_fill_manual(values = c("Parcelle" = col_wb, "Ménage" = col_fao)) +
  labs(
    title    = "Boxplot de la superficie agricole",
    subtitle = "Échelle logarithmique — parcelles et ménages ≤ 500 ha",
    x        = NULL,
    y        = "Superficie (ha, échelle log)",
    caption  = "Source : GHS Panel W4, Nigeria 2018-2019"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title      = element_text(face = "bold"),
    legend.position = "none"
  )

ggsave(here("outputs", "figures", "fig02_boxplot_superficie.png"),
       fig_box, width = 7, height = 6, dpi = 300)
cat("fig02 exportée\n")


# ============================================================
# TÂCHE 2 : SCATTER PLOT GPS vs DÉCLARÉE + LIGNE 45° + SPEARMAN
# ============================================================

# Filtrage : uniquement les parcelles avec les deux mesures disponibles
# et sans valeurs aberrantes (> 500 ha)
dat_scatter <- sect11a1_clean %>%
  filter(
    !is.na(area_gps_ha),      !is.na(area_declared_ha),
    area_gps_ha      > 0,     area_declared_ha > 0,
    area_gps_ha      <= 500,  area_declared_ha <= 500,
    !is.na(wt_wave4)
  )

# Corrélation de Spearman entre mesure GPS et mesure déclarée
spearman_res <- cor.test(dat_scatter$area_gps_ha,
                         dat_scatter$area_declared_ha,
                         method = "spearman")
rho_val <- round(spearman_res$estimate, 4)
p_val   <- format.pval(spearman_res$p.value, digits = 3)
cat("Spearman rho =", rho_val, "| p-value =", p_val, "\n")

# Scatter plot GPS vs déclarée avec ligne d'égalité parfaite (45°)
fig_scatter <- ggplot(dat_scatter,
                      aes(x = area_declared_ha, y = area_gps_ha,
                          weight = wt_wave4)) +
  geom_point(alpha = 0.25, size = 1.2, color = col_wb) +
  # Ligne d'égalité parfaite (pente 45°)
  geom_abline(slope = 1, intercept = 0,
              color = col_warn, linewidth = 1, linetype = "dashed") +
  scale_x_log10(labels = scales::label_number(accuracy = 0.01)) +
  scale_y_log10(labels = scales::label_number(accuracy = 0.01)) +
  # Annotation du coefficient de Spearman
  annotate("text", x = 0.01, y = 200,
           label = paste0("Spearman ρ = ", rho_val, "\np < 0,001"),
           hjust = 0, size = 3.8, color = col_grey) +
  labs(
    title    = "Superficie GPS vs. superficie déclarée",
    subtitle = "Ligne rouge en pointillés : égalité parfaite (pente 45°)",
    x        = "Superficie déclarée (ha, échelle log)",
    y        = "Superficie GPS (ha, échelle log)",
    caption  = "Source : GHS Panel W4, Nigeria 2018-2019"
  ) +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold"))

ggsave(here("outputs", "figures", "fig03_scatter_gps_declared.png"),
       fig_scatter, width = 8, height = 6, dpi = 300)
cat("fig03 exportée\n")


# ============================================================
# ANALYSE DU RÉGIME DE TENURE FONCIÈRE=======================
# ============================================================

# Chargement de la base parcelle (tenure) et jointure avec les poids
sect11b1 <- read_dta(here("data", "raw", "sect11b1_plantingw4.dta"))

sect11b1 <- sect11b1 %>%
  left_join(poids, by = "hhid")

# Recodage de la variable tenure en 7 modalités
sect11b1 <- sect11b1 %>%
  mutate(
    tenure = case_when(
      s11b1q4 == 1 ~ "Propriété pleine",
      s11b1q4 == 2 ~ "Location",
      s11b1q4 == 3 ~ "Prêt gratuit",
      s11b1q4 == 4 ~ "Distribution communautaire",
      s11b1q4 == 5 ~ "Héritage",
      s11b1q4 == 6 ~ "Métayage",
      s11b1q4 == 7 ~ "Échange temporaire",
      TRUE         ~ NA_character_
    ) %>% factor(),
    
    # Recodage du milieu de résidence
    milieu = case_when(
      sector == 1 ~ "Urbain",
      sector == 2 ~ "Rural",
      TRUE        ~ NA_character_
    ) %>% factor(levels = c("Rural", "Urbain"))
  )


# ============================================================
# TÂCHE 1 : FRÉQUENCES ET PROPORTIONS PONDÉRÉES
# ============================================================

# Plan de sondage au niveau parcelle pour le régime de tenure
plan_tenure <- sect11b1 %>%
  filter(!is.na(wt_wave4), !is.na(tenure)) %>%
  as_survey_design(weights = wt_wave4)

# Tableau des fréquences et proportions pondérées par modalité de tenure
tab_tenure <- plan_tenure %>%
  group_by(tenure) %>%
  summarise(
    n_pondere  = survey_total(na.rm = TRUE),
    prop       = survey_mean(na.rm = TRUE)
  ) %>%
  arrange(desc(prop)) %>%
  mutate(
    prop_pct = round(prop * 100, 1),
    n_pondere = round(n_pondere, 0)
  ) %>%
  select(tenure, n_pondere, prop_pct)

# Mise en forme avec gt
tab_tenure %>%
  gt::gt() %>%
  gt::tab_header(
    title    = "Régime de tenure foncière des parcelles agricoles",
    subtitle = "GHS Panel W4, Nigeria 2018-2019"
  ) %>%
  gt::cols_label(
    tenure    = "Mode d'acquisition",
    n_pondere = "Effectif (pondéré)",
    prop_pct  = "Proportion (%)"
  ) %>%
  gt::fmt_number(columns = n_pondere, decimals = 0, sep_mark = " ") %>%
  gt::tab_source_note("Source : GHS Panel W4, Nigeria 2018-2019") %>%
  gt::gtsave(here("outputs", "tables", "tab_tenure.rtf"))


# ============================================================
# TÂCHE 2 : BARPLOT HORIZONTAL DU RÉGIME DE TENURE
# ============================================================

# Données pour le barplot : proportions pondérées triées
dat_bar <- plan_tenure %>%
  group_by(tenure) %>%
  summarise(prop = survey_mean(na.rm = TRUE)) %>%
  mutate(
    prop_pct = prop * 100,
    # Réordonnancement par proportion croissante pour lecture horizontale
    tenure   = fct_reorder(tenure, prop_pct)
  )

fig_tenure <- ggplot(dat_bar, aes(x = prop_pct, y = tenure)) +
  geom_col(fill = col_wb, alpha = 0.85) +
  # Ajout des étiquettes de proportion
  geom_text(aes(label = paste0(round(prop_pct, 1), "%")),
            hjust = -0.15, size = 3.5, color = col_grey) +
  scale_x_continuous(
    limits = c(0, 75),
    labels = scales::label_percent(scale = 1)
  ) +
  labs(
    title   = "Répartition des parcelles selon le régime de tenure",
    subtitle = "GHS Panel W4, Nigeria 2018-2019",
    x       = "Proportion (%)",
    y       = NULL,
    caption = "Source : GHS Panel W4, Nigeria 2018-2019"
  ) +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold"))

ggsave(here("outputs", "figures", "fig04_barplot_tenure.png"),
       fig_tenure, width = 9, height = 5, dpi = 300)
cat("fig04 exportée\n")


# ============================================================
# TÂCHE 3 : TEST DU CHI-DEUX — TENURE x MILIEU (RURAL/URBAIN)
# ============================================================

# Tableau croisé brut (non pondéré) pour le test du chi-deux
tab_chi2 <- table(sect11b1$tenure, sect11b1$milieu)

# Test d'indépendance du chi-deux
test_chi2 <- chisq.test(tab_chi2)
print(test_chi2)

# Affichage du tableau croisé avec proportions par ligne
tab_croise <- sect11b1 %>%
  filter(!is.na(tenure), !is.na(milieu)) %>%
  count(tenure, milieu) %>%
  group_by(tenure) %>%
  mutate(prop = round(100 * n / sum(n), 1)) %>%
  pivot_wider(names_from  = milieu,
              values_from = c(n, prop)) %>%
  gt::gt() %>%
  gt::tab_header(
    title    = "Régime de tenure selon le milieu de résidence",
    subtitle = paste0("Test du chi-deux : χ² = ", round(test_chi2$statistic, 2),
                      ", ddl = ", test_chi2$parameter,
                      ", p < 0,001")
  ) %>%
  gt::tab_source_note("Source : GHS Panel W4, Nigeria 2018-2019") %>%
  gt::gtsave(here("outputs", "tables", "tab_tenure_milieu.rtf"))


#=============

# ============================================================
# TÂCHE 23 : RELATION SUPERFICIE TOTALE x NOMBRE DE PARCELLES
# ============================================================

# Préparation des données au niveau ménage
# (superficie totale et nombre de parcelles par hhid)
dat_scatter_parc <- sect11a1_clean %>%
  filter(!is.na(area_ha), area_ha > 0, area_ha <= 500) %>%
  group_by(hhid) %>%
  summarise(
    area_totale_ha = sum(area_ha, na.rm = TRUE),
    nb_parcelles   = n(),
    wt_wave4       = first(wt_wave4),
    .groups        = "drop"
  ) %>%
  filter(!is.na(wt_wave4), area_totale_ha > 0)


# ============================================================
# CORRÉLATION DE SPEARMAN AVEC INTERVALLE DE CONFIANCE (95%)
# via transformation de Fisher z
# ============================================================

spearman_parc <- cor.test(
  dat_scatter_parc$nb_parcelles,
  dat_scatter_parc$area_totale_ha,
  method = "spearman"
)

# Calcul manuel de l'IC 95% via transformation de Fisher
n_men  <- nrow(dat_scatter_parc)
rho    <- spearman_parc$estimate
z_fish <- atanh(rho)                          # Transformation de Fisher
se_z   <- 1 / sqrt(n_men - 3)                # Erreur standard
ci_low  <- tanh(z_fish - 1.96 * se_z)        # Borne inférieure
ci_high <- tanh(z_fish + 1.96 * se_z)        # Borne supérieure

cat("=== Corrélation de Spearman ===\n")
cat("rho     =", round(rho, 4), "\n")
cat("IC 95%  = [", round(ci_low, 4), ";", round(ci_high, 4), "]\n")
cat("p-value =", format.pval(spearman_parc$p.value, digits = 3), "\n")
cat("n       =", n_men, "ménages\n")


# ============================================================
# SCATTER PLOT + COURBE LOESS + ANNOTATION SPEARMAN
# ============================================================

fig_scatter_parc <- ggplot(dat_scatter_parc,
                           aes(x = nb_parcelles, y = area_totale_ha,
                               weight = wt_wave4)) +
  # Points individuels semi-transparents
  geom_jitter(alpha = 0.25, size = 1.2, color = col_wb,
              width = 0.2, height = 0) +
  # Courbe de tendance LOESS
  geom_smooth(method = "loess", se = TRUE,
              color   = col_warn,
              fill    = col_warn,
              alpha   = 0.2,
              linewidth = 1) +
  # Échelle log sur l'axe y pour lisibilité
  scale_y_log10(labels = scales::label_number(accuracy = 0.01)) +
  # Axe x en entier (nombre de parcelles)
  scale_x_continuous(breaks = 1:13) +
  # Annotation du coefficient de Spearman
  annotate("text",
           x     = 9, y = 0.005,
           label = paste0("Spearman ρ = ", round(rho, 4),
                          "\nIC 95% [", round(ci_low, 4),
                          " ; ", round(ci_high, 4), "]",
                          "\np < 0,001"),
           hjust = 0, size = 3.5, color = col_grey) +
  labs(
    title    = "Superficie totale du ménage selon le nombre de parcelles",
    subtitle = "Courbe de tendance LOESS — échelle logarithmique sur l'axe des ordonnées",
    x        = "Nombre de parcelles",
    y        = "Superficie totale (ha, échelle log)",
    caption  = "Source : GHS Panel W4, Nigeria 2018-2019"
  ) +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold"))

ggsave(here("outputs", "figures", "fig06_scatter_superficie_parcelles.png"),
       fig_scatter_parc, width = 9, height = 6, dpi = 300)
cat("fig06 exportée\n")

#====================================================================================================

# ============================================================
# TÂCHE 24 : HEATMAP ÉTAT × VAGUE — SUPERFICIE MÉDIANE
# ============================================================

w4 <- sect11a1_clean %>%
  mutate(wt_wave = wt_wave4, vague = "W4 (2018)",
         state_name = state_labels[as.character(as.numeric(state))]) %>%
  filter(!is.na(area_ha), area_ha > 0, area_ha <= 500, !is.na(wt_wave))

# Fonction de médiane pondérée

dat_vagues <- w4 %>%
  select(hhid, vague, area_ha, wt_wave) %>%
  mutate(vague = factor("W4 (2018)"))

weighted_median <- function(x, w) {
  ord  <- order(x)
  x    <- x[ord]
  w    <- w[ord]
  cumw <- cumsum(w) / sum(w)
  x[which(cumw >= 0.5)[1]]
}

# Calcul de la médiane pondérée par état et par vague
# en utilisant les 4 bases déjà préparées dans la tâche précédente
dat_heatmap <- dat_vagues %>%
  # Récupération du numéro d'état depuis sect11a1_clean pour W4
  # et depuis les bases brutes pour W1-W3 via left_join
  left_join(
    bind_rows(
      w4 %>% select(hhid, state) %>% mutate(vague = "W4 (2018)")
    ),
    by = c("hhid", "vague")
  ) %>%
  # Harmonisation des labels d'états
  mutate(
    state_name = case_when(
      state ==  1 ~ "Abia",        state ==  2 ~ "Adamawa",
      state ==  3 ~ "Akwa Ibom",   state ==  4 ~ "Anambra",
      state ==  5 ~ "Bauchi",      state ==  6 ~ "Bayelsa",
      state ==  7 ~ "Benue",       state ==  8 ~ "Borno",
      state ==  9 ~ "Cross River", state == 10 ~ "Delta",
      state == 11 ~ "Ebonyi",      state == 12 ~ "Edo",
      state == 13 ~ "Ekiti",       state == 14 ~ "Enugu",
      state == 15 ~ "FCT",         state == 16 ~ "Gombe",
      state == 17 ~ "Imo",         state == 18 ~ "Jigawa",
      state == 19 ~ "Kaduna",      state == 20 ~ "Kano",
      state == 21 ~ "Katsina",     state == 22 ~ "Kebbi",
      state == 23 ~ "Kogi",        state == 24 ~ "Kwara",
      state == 25 ~ "Lagos",       state == 26 ~ "Nasarawa",
      state == 27 ~ "Niger",       state == 28 ~ "Ogun",
      state == 29 ~ "Ondo",        state == 30 ~ "Osun",
      state == 31 ~ "Oyo",         state == 32 ~ "Plateau",
      state == 33 ~ "Rivers",      state == 34 ~ "Sokoto",
      state == 35 ~ "Taraba",      state == 36 ~ "Yobe",
      state == 37 ~ "Zamfara",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(state_name), !is.na(area_ha), !is.na(wt_wave))

# Calcul de la médiane pondérée par état et vague
heatmap_data <- dat_heatmap %>%
  group_by(state_name, vague) %>%
  filter(n() >= 5) %>%                  # Au moins 5 observations par cellule
  summarise(
    med_ha = weighted_median(area_ha, wt_wave),
    .groups = "drop"
  )

# Ordonnancement des états par médiane globale décroissante
ordre_states <- heatmap_data %>%
  group_by(state_name) %>%
  summarise(med_globale = median(med_ha, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(med_globale)) %>%
  pull(state_name)

heatmap_data <- heatmap_data %>%
  mutate(state_name = factor(state_name, levels = ordre_states))

# ============================================================
# HEATMAP ÉTAT × W4 UNIQUEMENT
# ============================================================
# Redémarrer la session R puis exécuter :
library(tidyverse)
library(haven)
library(here)
library(srvyr)

# Récupération de la variable état depuis sect11a1_clean
dat_heatmap_w4 <- sect11a1_clean %>%
  filter(!is.na(area_ha), area_ha > 0, area_ha <= 500, !is.na(wt_wave4)) %>%
  mutate(
    state_name = case_when(
      state ==  1 ~ "Abia",        state ==  2 ~ "Adamawa",
      state ==  3 ~ "Akwa Ibom",   state ==  4 ~ "Anambra",
      state ==  5 ~ "Bauchi",      state ==  6 ~ "Bayelsa",
      state ==  7 ~ "Benue",       state ==  8 ~ "Borno",
      state ==  9 ~ "Cross River", state == 10 ~ "Delta",
      state == 11 ~ "Ebonyi",      state == 12 ~ "Edo",
      state == 13 ~ "Ekiti",       state == 14 ~ "Enugu",
      state == 15 ~ "FCT",         state == 16 ~ "Gombe",
      state == 17 ~ "Imo",         state == 18 ~ "Jigawa",
      state == 19 ~ "Kaduna",      state == 20 ~ "Kano",
      state == 21 ~ "Katsina",     state == 22 ~ "Kebbi",
      state == 23 ~ "Kogi",        state == 24 ~ "Kwara",
      state == 25 ~ "Lagos",       state == 26 ~ "Nasarawa",
      state == 27 ~ "Niger",       state == 28 ~ "Ogun",
      state == 29 ~ "Ondo",        state == 30 ~ "Osun",
      state == 31 ~ "Oyo",         state == 32 ~ "Plateau",
      state == 33 ~ "Rivers",      state == 34 ~ "Sokoto",
      state == 35 ~ "Taraba",      state == 36 ~ "Yobe",
      state == 37 ~ "Zamfara",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(state_name))

# Médiane pondérée par état
heatmap_w4 <- dat_heatmap_w4 %>%
  group_by(state_name) %>%
  filter(n() >= 5) %>%
  summarise(
    med_ha = weighted_median(area_ha, wt_wave4),
    .groups = "drop"
  ) %>%
  # Ordonnancement par médiane décroissante
  mutate(state_name = fct_reorder(state_name, med_ha))

# Heatmap à une seule colonne (W4)
fig_heatmap_w4 <- ggplot(heatmap_w4,
                         aes(x = "W4 (2018)", y = state_name, fill = med_ha)) +
  geom_tile(color = "white", linewidth = 0.5) +
  geom_text(aes(label = round(med_ha, 2)),
            size = 3, fontface = "bold",
            color = if_else(heatmap_w4$med_ha > 1.5, "white", "black")) +
  scale_fill_gradientn(
    colours  = c("#EAF4FB", "#66C2E8", "#009FDA", "#006BA6", "#003A5C"),
    name     = "Médiane\n(ha)",
    na.value = "grey88"
  ) +
  scale_x_discrete(position = "top") +
  labs(
    title    = "Superficie médiane des exploitations par État — W4 (2018–2019)",
    subtitle = "GHS Panel Nigeria — parcelles ≤ 500 ha",
    x        = NULL,
    y        = NULL,
    caption  = "Source : GHS Panel W4, Nigeria 2018-2019 | Médiane pondérée (wt_wave4)"
  ) +
  theme_minimal(base_size = 10) +
  theme(
    plot.title      = element_text(face = "bold", color = "#003A5C", size = 12),
    plot.subtitle   = element_text(size = 9, color = col_grey),
    plot.caption    = element_text(size = 8, color = col_grey),
    axis.text.y     = element_text(size = 8.5),
    axis.text.x     = element_text(size = 10, face = "bold"),
    legend.position = "right",
    panel.grid      = element_blank()
  )

ggsave(here("outputs", "figures", "fig_heatmap_w4.png"),
       fig_heatmap_w4, width = 6, height = 5, dpi = 300)
cat("Heatmap W4 exportée\n")