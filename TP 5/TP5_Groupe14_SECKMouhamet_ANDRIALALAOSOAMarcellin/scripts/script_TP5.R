# ==============================================================================
# TP5 : Cultures pratiquées, intrants utilisés et rendements agricoles
#        Nigeria GHS Panel — Vague 4 (2018-2019)
# ==============================================================================




# ==============================================================================
# ÉTAPE 0 : INITIALISATION
# ==============================================================================

if (!require("pacman")) install.packages("pacman")
library(pacman)
p_load(haven, dplyr, ggplot2, tidyr, forcats, scales, sf, viridis,
       rstatix, patchwork, stringr)

chemin <- "C:/Users/HP/Desktop/ISE1 CL/Semestre2/Projet statistique avec R ou python/Dossier R/TP/TP5"

#dir.create(paste0(chemin, "/outputs/tables"), recursive = TRUE, showWarnings = FALSE)
#dir.create(paste0(chemin, "/outputs/graphs"), recursive = TRUE, showWarnings = FALSE)

cat("\n=== TP5 : Analyses agricoles GHS Panel vague 4 ===\n")





# ==============================================================================
# ÉTAPE 1 : CHARGEMENT DES DONNÉES
# ==============================================================================

secta     <- read_dta(paste0(chemin, "/data/raw/secta_harvestw4.dta"))
secta1    <- read_dta(paste0(chemin, "/data/raw/secta1_harvestw4.dta"))
sect11a1  <- read_dta(paste0(chemin, "/data/raw/sect11a1_plantingw4.dta"))
sect11f   <- read_dta(paste0(chemin, "/data/raw/sect11f_plantingw4.dta"))
secta3i   <- read_dta(paste0(chemin, "/data/raw/secta3i_harvestw4.dta"))
secta11c2 <- read_dta(paste0(chemin, "/data/raw/secta11c2_harvestw4.dta"))





# ==============================================================================
# ÉTAPE 2 : CONSTRUCTION DES BASES ANALYTIQUES
# ==============================================================================

# 2.1 Table de référence des poids (niveau ménage) 

poids <- secta %>% select(hhid, wt_wave4)

na_poids <- sum(is.na(poids$wt_wave4))
cat("\nNA sur wt_wave4 :", na_poids,
    "(", round(na_poids / nrow(poids) * 100, 2), "%)\n")

# 2.2 Table de référence milieu + état (niveau ménage) 

hhid_geo <- sect11a1 %>%
  select(hhid, state, sector) %>%
  distinct(hhid, .keep_all = TRUE) %>%
  mutate(milieu_lib = ifelse(as.numeric(sector) == 1, "Urbain", "Rural"))

state_labels <- attr(sect11a1$state, "labels")
state_names  <- gsub("^[0-9]+\\.\\s*", "", names(state_labels))
code_to_state <- tibble(
  state      = as.numeric(state_labels),
  state_name = state_names
)

hhid_geo <- hhid_geo %>%
  left_join(code_to_state, by = "state")

cat("Ménages avec géolocalisation :", nrow(hhid_geo), "\n")

# 2.3 Table des superficies par parcelle 

superficie_parc <- sect11a1 %>%
  mutate(
    s11aq4b_num   = as.numeric(s11aq4b),
    gps_dispo     = as.numeric(s11aq4a) == 1 & !is.na(s11aq4c),
    superficie_ha = case_when(
      gps_dispo                       ~ s11aq4c / 10000,
      !gps_dispo & s11aq4b_num == 5   ~ s11aq4aa * 0.404686,
      !gps_dispo & s11aq4b_num == 6   ~ s11aq4aa,
      !gps_dispo & s11aq4b_num == 7   ~ s11aq4aa / 10000,
      TRUE ~ NA_real_
    )
  ) %>%
  filter(!is.na(superficie_ha), superficie_ha > 0) %>%
  select(hhid, plotid, superficie_ha, gps_dispo)

cat("Parcelles avec superficie valide :", nrow(superficie_parc), "\n")
cat("  dont mesurées GPS :", sum(superficie_parc$gps_dispo), "\n")

# 2.4 Table des intrants par parcelle

intrants_parc <- secta11c2 %>%
  mutate(
    engrais_inorg = case_when(
      as.numeric(s11dq1a) == 1 ~ 1L,
      as.numeric(s11dq1a) == 2 ~ 0L,
      TRUE ~ NA_integer_
    ),
    engrais_npk   = ifelse(!is.na(s11c2q36_1), as.integer(s11c2q36_1), NA_integer_),
    engrais_uree  = ifelse(!is.na(s11c2q36_2), as.integer(s11c2q36_2), NA_integer_),
    engrais_org   = case_when(
      as.numeric(s11dq36) == 1 ~ 1L,
      as.numeric(s11dq36) == 2 ~ 0L,
      TRUE ~ NA_integer_
    ),
    pesticide     = case_when(
      as.numeric(s11c2q1) == 1 ~ 1L,
      as.numeric(s11c2q1) == 2 ~ 0L,
      TRUE ~ NA_integer_
    )
  ) %>%
  select(hhid, plotid, engrais_inorg, engrais_npk,
         engrais_uree, engrais_org, pesticide)

# 2.5 Table de production en kg (culture×parcelle)

production_kg <- secta3i %>%
  filter(as.numeric(sa3iq3) == 1) %>%
  mutate(
    cropcode_num = as.numeric(cropcode),
    conv_final  = case_when(
      !is.na(sa3iq6_conv) & sa3iq6_conv > 0      ~ sa3iq6_conv,
      is.na(sa3iq6_conv) & as.numeric(sa3iq6ii) == 1 ~ 1,
      TRUE ~ NA_real_
    ),
    quantite_kg = sa3iq6i * conv_final
  ) %>%
  filter(!is.na(quantite_kg), quantite_kg > 0,
         cropcode_num %in% c(1080, 1090)) %>%
  mutate(culture = ifelse(cropcode_num == 1080, "Maïs", "Millet")) %>%
  select(hhid, plotid, cropcode_num, culture, quantite_kg)

# 2.6 Base analytique principale : rendement

base_rendement <- production_kg %>%
  left_join(superficie_parc, by = c("hhid", "plotid")) %>%
  left_join(intrants_parc,   by = c("hhid", "plotid")) %>%
  left_join(hhid_geo,        by = "hhid") %>%
  left_join(poids,           by = "hhid") %>%
  filter(!is.na(superficie_ha)) %>%
  mutate(rendement_ha = quantite_kg / superficie_ha)

cat("Obs pour calcul rendement :", nrow(base_rendement), "\n")
cat("  dont avec info intrants  :",
    sum(!is.na(base_rendement$engrais_inorg)), "\n")

# 2.7 Base cultures (Q25, Q26)

base_cultures <- sect11f %>%
  left_join(hhid_geo %>% select(hhid, milieu_lib, state_name), by = "hhid") %>%
  left_join(poids, by = "hhid")

# 2.8 Base intrants (Q27)

base_intrants <- intrants_parc %>%
  left_join(hhid_geo, by = "hhid") %>%
  left_join(poids,    by = "hhid")

cat("\nToutes les bases analytiques construites.\n")






# ==============================================================================
# QUESTION 25 : TOP 15 CULTURES LES PLUS FRÉQUENTES
# ==============================================================================

cat("\n=== Q25 : Top 15 cultures les plus fréquentes ===\n")

crop_labels_df <- tibble(
  cropcode_num = as.numeric(attr(sect11f$cropcode, "labels")),
  crop_name    = gsub("^[0-9]+\\.\\s*", "",
                      names(attr(sect11f$cropcode, "labels")))
)

freq_cultures <- base_cultures %>%
  mutate(cropcode_num = as.numeric(cropcode)) %>%
  count(cropcode_num, name = "n_obs") %>%
  left_join(crop_labels_df, by = "cropcode_num") %>%
  mutate(
    crop_name = if_else(is.na(crop_name),
                        paste0("Code ", cropcode_num), crop_name),
    crop_type = case_when(
      cropcode_num %in% c(1080, 1090, 1100, 1110, 1124) ~ "Céréale",
      cropcode_num %in% c(1010, 1060, 1130, 2220)        ~ "Légumineuse",
      cropcode_num %in% c(1020, 1040, 1100, 1121, 1123, 1140, 1150) ~ "Tubercule",
      cropcode_num %in% c(1050, 1120, 3180, 2190)        ~ "Culture de rente",
      TRUE ~ "Autre"
    )
  ) %>%
  arrange(desc(n_obs)) %>%
  slice_head(n = 15)

write.csv(freq_cultures,
          paste0(chemin, "/outputs/tables/q25_top15_cultures.csv"),
          row.names = FALSE)

pal_type <- c(
  "Céréale"          = "#2E86AB",
  "Légumineuse"      = "#A23B72",
  "Tubercule"        = "#F18F01",
  "Culture de rente" = "#2D6A4F",
  "Autre"            = "#999999"
)

p_q25 <- freq_cultures %>%
  mutate(crop_name = fct_reorder(crop_name, n_obs)) %>%
  ggplot(aes(x = n_obs, y = crop_name, fill = crop_type)) +
  geom_col(width = 0.65) +
  geom_text(aes(label = scales::comma(n_obs)), hjust = -0.1, size = 3) +
  scale_fill_manual(values = pal_type, name = "Type de culture") +
  scale_x_continuous(expand = expansion(mult = c(0, 0.18)),
                     labels = scales::comma) +
  labs(x = "Nombre d'observations (parcelle × culture)", y = NULL) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom")

ggsave(paste0(chemin, "/outputs/graphs/q25_top15_cultures.png"),
       p_q25, width = 8, height = 5.5, dpi = 300)

print(p_q25)






# ==============================================================================
# QUESTION 26 : DIVERSIFICATION CULTURALE PAR MÉNAGE (améliorée)
# ==============================================================================

cat("\n=== Q26 : Indice de diversification culturale ===\n")

diversif <- base_cultures %>%
  group_by(hhid, milieu_lib) %>%
  summarise(
    n_cultures = n_distinct(cropcode),
    wt_wave4   = first(wt_wave4),
    .groups    = "drop"
  ) %>%
  filter(!is.na(milieu_lib))

stats_diversif <- diversif %>%
  group_by(milieu_lib) %>%
  summarise(
    n      = n(),
    mean   = round(mean(n_cultures), 2),
    median = median(n_cultures),
    sd     = round(sd(n_cultures), 2),
    Q1     = quantile(n_cultures, 0.25),
    Q3     = quantile(n_cultures, 0.75),
    .groups = "drop"
  )
write.csv(stats_diversif,
          paste0(chemin, "/outputs/tables/q26_diversif_stats.csv"),
          row.names = FALSE)

wilcox_div <- wilcox.test(n_cultures ~ milieu_lib,
                          data = diversif, exact = FALSE)
z_div <- qnorm(wilcox_div$p.value / 2)
r_div <- abs(z_div) / sqrt(nrow(diversif))
capture.output(
  list(wilcox = wilcox_div, taille_effet_r = r_div),
  file = paste0(chemin, "/outputs/tables/q26_wilcoxon_diversif.txt")
)

# Graphique 1 : Histogramme global

p_q26_hist <- ggplot(diversif, aes(x = n_cultures)) +
  geom_histogram(binwidth = 1, fill = "#2E86AB", color = "white") +
  scale_x_continuous(breaks = 1:max(diversif$n_cultures)) +
  labs(x = "Nombre de cultures distinctes par ménage", y = "Effectif") +
  theme_minimal(base_size = 11)

ggsave(paste0(chemin, "/outputs/graphs/q26_histogramme_diversif.png"),
       p_q26_hist, width = 6, height = 4, dpi = 300)

# Graphique 2 : Violin + boxplot par milieu

p_q26_violin <- ggplot(diversif,
                       aes(x = milieu_lib, y = n_cultures, fill = milieu_lib)) +
  geom_violin(alpha = 0.7, width = 0.8) +
  geom_boxplot(width = 0.15, outlier.shape = NA, fill = "white") +
  scale_fill_manual(values = c("Rural" = "#2E86AB", "Urbain" = "#F18F01"),
                    guide = "none") +
  annotate("text", x = 1.5, y = max(diversif$n_cultures) * 0.95,
           label = paste0("Wilcoxon p = ",
                          format.pval(wilcox_div$p.value, digits = 3),
                          "\nr = ", round(r_div, 3)),
           size = 3.5) +
  labs(x = NULL, y = "Nombre de cultures distinctes") +
  theme_minimal(base_size = 11)

ggsave(paste0(chemin, "/outputs/graphs/q26_violin_diversif.png"),
       p_q26_violin, width = 6, height = 5, dpi = 300)

print(p_q26_hist)
print(p_q26_violin)







# ==============================================================================
# QUESTION 27 : UTILISATION DES ENGRAIS PAR ZONE
# ==============================================================================

cat("\n=== Q27 : Taux d'utilisation des engrais ===\n")

calc_taux_ic <- function(data, var_name, groupe) {
  data %>%
    filter(!is.na(.data[[var_name]]), !is.na(.data[[groupe]])) %>%
    group_by(g = .data[[groupe]]) %>%
    summarise(
      n       = n(),
      n_oui   = sum(.data[[var_name]], na.rm = TRUE),
      taux    = n_oui / n,
      se      = sqrt(taux * (1 - taux) / n),
      ic_low  = pmax(0, taux - 1.96 * se),
      ic_high = pmin(1, taux + 1.96 * se),
      .groups = "drop"
    ) %>%
    mutate(intrant = var_name)
}

taux_milieu <- bind_rows(
  calc_taux_ic(base_intrants, "engrais_inorg", "milieu_lib"),
  calc_taux_ic(base_intrants, "engrais_npk",   "milieu_lib"),
  calc_taux_ic(base_intrants, "engrais_uree",  "milieu_lib"),
  calc_taux_ic(base_intrants, "engrais_org",   "milieu_lib"),
  calc_taux_ic(base_intrants, "pesticide",     "milieu_lib")
) %>%
  mutate(
    intrant_lib = case_when(
      intrant == "engrais_inorg" ~ "Engrais chimique (total)",
      intrant == "engrais_npk"   ~ "NPK",
      intrant == "engrais_uree"  ~ "Urée",
      intrant == "engrais_org"   ~ "Engrais organique",
      intrant == "pesticide"     ~ "Pesticide"
    )
  )

write.csv(taux_milieu,
          paste0(chemin, "/outputs/tables/q27_taux_intrants_milieu.csv"),
          row.names = FALSE)

tab_chi2 <- base_intrants %>%
  filter(!is.na(engrais_inorg), !is.na(milieu_lib)) %>%
  { table(.$milieu_lib, .$engrais_inorg) }
chi2_engrais   <- chisq.test(tab_chi2)
cramer_engrais <- cramer_v(tab_chi2)
capture.output(
  list(chi2 = chi2_engrais, cramer_v = cramer_engrais),
  file = paste0(chemin, "/outputs/tables/q27_chi2_engrais.txt")
)

# Graphique en facettes horizontales
p_q27 <- taux_milieu %>%
  mutate(intrant_lib = factor(intrant_lib,
                              levels = c("Engrais chimique (total)",
                                         "NPK", "Urée",
                                         "Engrais organique",
                                         "Pesticide"))) %>%
  ggplot(aes(x = taux, y = intrant_lib, fill = g)) +
  geom_col(position = position_dodge(0.7), width = 0.6) +
  geom_errorbar(aes(xmin = ic_low, xmax = ic_high),
                position = position_dodge(0.7), width = 0.2) +
  geom_text(aes(label = paste0(round(taux * 100, 1), "%")),
            position = position_dodge(0.7), hjust = -0.2, size = 3) +
  scale_fill_manual(values = c("Rural" = "#2E86AB", "Urbain" = "#F18F01"),
                    name = "Milieu") +
  scale_x_continuous(labels = percent_format(),
                     expand = expansion(mult = c(0, 0.2))) +
  facet_wrap(~intrant_lib, scales = "free_y", ncol = 2) +
  labs(x = "Taux d'utilisation (% parcelles)", y = NULL) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom",
        strip.text = element_text(face = "bold", size = 9),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())

ggsave(paste0(chemin, "/outputs/graphs/q27_taux_intrants_facet.png"),
       p_q27, width = 9, height = 6, dpi = 300)

print(p_q27)





# ==============================================================================
# CHARGEMENT DU SHAPEFILE NIGÉRIA (ÉTATS)
# ==============================================================================

library(sf)

shapefile_path <- "data/raw/gadm41_NGA_1.shp"
if (!file.exists(shapefile_path)) {
  stop("Shapefile non trouvé. Vérifiez le chemin.")
}
nigeria_states <- st_read(shapefile_path)

# Table de correspondance codes -> noms d'États (depuis secta1)
state_labels      <- attr(secta1$state, "labels")
state_codes       <- as.numeric(state_labels)
state_names_raw   <- names(state_labels)
state_names_clean <- gsub("^[0-9]+\\.\\s*", "", state_names_raw)
code_to_name      <- data.frame(
  state_code = state_codes,
  state_name = state_names_clean,
  stringsAsFactors = FALSE
)


# ==============================================================================
# QUESTION 27 : TAUX D'UTILISATION DES ENGRAIS PAR ÉTAT (CARTE)
# ==============================================================================

# 27a. Calcul du taux par État (base_intrants doit contenir state_name)

taux_engrais_etat <- base_intrants %>%
  filter(!is.na(engrais_inorg), !is.na(state_name)) %>%
  group_by(state_name) %>%
  summarise(
    n_parcelles = n(),
    n_engrais   = sum(engrais_inorg == 1, na.rm = TRUE),
    taux        = n_engrais / n_parcelles,
    .groups     = "drop"
  ) %>%
  mutate(
    state_name = case_when(
      state_name == "Federal Capital Territory" ~ "FCT",
      state_name == "Akwa Ibom"                ~ "Akwa Ibom",
      TRUE                                     ~ state_name
    )
  )

# 27b. Jointure avec le shapefile

map_data_engrais <- nigeria_states %>%
  left_join(taux_engrais_etat, by = c("NAME_1" = "state_name"))

# 27c. Centroïdes pour les étiquettes
centroids_engrais <- st_centroid(map_data_engrais)

# 27d. Carte

p_carte_engrais <- ggplot(data = map_data_engrais) +
  geom_sf(aes(fill = taux), color = "white", size = 0.2) +
  geom_sf_text(data = centroids_engrais, aes(label = NAME_1),
               size = 2.5, color = "white", fontface = "bold") +
  scale_fill_viridis_c(option = "plasma", na.value = "grey50",
                       name = "Taux d'utilisation\nengrais chimique",
                       labels = scales::percent_format()) +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    legend.position = "right"
  )

ggsave(paste0(chemin, "/outputs/graphs/q27_carte_taux_engrais_etat.png"),
       p_carte_engrais, width = 8, height = 7, dpi = 300)

print(p_carte_engrais)





# ==============================================================================
# QUESTION 28 : RENDEMENT À L'HECTARE — MAÏS ET MILLET (améliorée)
# ==============================================================================

cat("\n=== Q28 : Rendements à l'hectare maïs et millet ===\n")

# Élimination des outliers (IQR×3)
rendement_clean <- base_rendement %>%
  group_by(culture) %>%
  mutate(
    Q1_r    = quantile(rendement_ha, 0.25, na.rm = TRUE),
    Q3_r    = quantile(rendement_ha, 0.75, na.rm = TRUE),
    IQR_r   = Q3_r - Q1_r,
    outlier = rendement_ha < (Q1_r - 3 * IQR_r) |
      rendement_ha > (Q3_r + 3 * IQR_r)
  ) %>%
  filter(!outlier) %>%
  ungroup()

n_out <- nrow(base_rendement) - nrow(rendement_clean)
cat("Obs avant élimination outliers :", nrow(base_rendement), "\n")
cat("Outliers écartés (IQR×3)        :", n_out, "\n")
cat("Obs finales                     :", nrow(rendement_clean), "\n")

stats_rend <- rendement_clean %>%
  group_by(culture) %>%
  summarise(
    n      = n(),
    mean   = round(mean(rendement_ha), 0),
    median = round(median(rendement_ha), 0),
    sd     = round(sd(rendement_ha), 0),
    Q1     = round(quantile(rendement_ha, 0.25), 0),
    Q3     = round(quantile(rendement_ha, 0.75), 0),
    .groups = "drop"
  )
write.csv(stats_rend,
          paste0(chemin, "/outputs/tables/q28_stats_rendement.csv"),
          row.names = FALSE)

# États avec au moins 10 observations
etats_valides <- rendement_clean %>%
  filter(!is.na(state_name)) %>%
  count(state_name, culture) %>%
  filter(n >= 10) %>%
  pull(state_name) %>%
  unique()

# Violin + boxplot par État, ordonné par médiane
p_q28 <- rendement_clean %>%
  filter(!is.na(state_name), state_name %in% etats_valides) %>%
  mutate(state_name = fct_reorder(state_name, rendement_ha, median, .desc = TRUE)) %>%
  ggplot(aes(x = state_name, y = rendement_ha, fill = culture)) +
  geom_violin(alpha = 0.6, width = 0.8, position = position_dodge(0.8)) +
  geom_boxplot(width = 0.15, position = position_dodge(0.8),
               outlier.shape = NA, fill = "white") +
  facet_wrap(~culture, scales = "free_y", ncol = 2) +
  scale_fill_manual(values = c("Maïs" = "#2E86AB", "Millet" = "#F18F01"),
                    guide = "none") +
  scale_y_continuous(labels = scales::comma) +
  labs(x = "État", y = "Rendement (kg/ha)") +
  coord_flip() +
  theme_minimal(base_size = 10) +
  theme(strip.text = element_text(face = "bold"),
        axis.text.y = element_text(size = 7))

ggsave(paste0(chemin, "/outputs/graphs/q28_rendement_par_etat_violin.png"),
       p_q28, width = 12, height = 8, dpi = 300)

print(p_q28)





# ==============================================================================
# QUESTION 28 (suite) : CARTES DES RENDEMENTS MÉDIANS PAR ÉTAT (MAÏS ET MILLET séparés)
# ==============================================================================

# Calcul du rendement médian par État et culture
rend_median_etat <- rendement_clean %>%
  filter(!is.na(state_name)) %>%
  group_by(state_name, culture) %>%
  summarise(
    rendement_median = median(rendement_ha, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    state_name = case_when(
      state_name == "Federal Capital Territory" ~ "FCT",
      state_name == "Akwa Ibom"                ~ "Akwa Ibom",
      TRUE                                     ~ state_name
    )
  )

# Séparer maïs et millet
rend_mais <- rend_median_etat %>% filter(culture == "Maïs")
rend_millet <- rend_median_etat %>% filter(culture == "Millet")

# Jointures
map_data_mais <- nigeria_states %>%
  left_join(rend_mais, by = c("NAME_1" = "state_name"))
map_data_millet <- nigeria_states %>%
  left_join(rend_millet, by = c("NAME_1" = "state_name"))

# Centroïdes (un seul jeu)
centroids <- st_centroid(nigeria_states)

# Carte maïs
p_carte_mais <- ggplot(data = map_data_mais) +
  geom_sf(aes(fill = rendement_median), color = "white", size = 0.2) +
  geom_sf_text(data = centroids, aes(label = NAME_1),
               size = 2.5, color = "white", fontface = "bold") +
  scale_fill_viridis_c(option = "plasma", na.value = "grey50",
                       name = "Rendement médian\n(kg/ha)",
                       labels = scales::comma_format()) +
  theme_minimal() +
  theme(axis.text = element_blank(), axis.title = element_blank(),
        panel.grid = element_blank(), legend.position = "right")

# Carte millet
p_carte_millet <- ggplot(data = map_data_millet) +
  geom_sf(aes(fill = rendement_median), color = "white", size = 0.2) +
  geom_sf_text(data = centroids, aes(label = NAME_1),
               size = 2.5, color = "white", fontface = "bold") +
  scale_fill_viridis_c(option = "plasma", na.value = "grey50",
                       name = "Rendement médian\n(kg/ha)",
                       labels = scales::comma_format()) +
  labs(title = "Rendement médian du millet à l'hectare par État",
       subtitle = "Vague 4 – GHS Panel Nigeria") +
  theme_minimal() +
  theme(axis.text = element_blank(), axis.title = element_blank(),
        panel.grid = element_blank(), legend.position = "right")

# Sauvegarde
ggsave(paste0(chemin, "/outputs/graphs/q28_carte_rendement_mais.png"),
       p_carte_mais, width = 8, height = 7, dpi = 300)
ggsave(paste0(chemin, "/outputs/graphs/q28_carte_rendement_millet.png"),
       p_carte_millet, width = 8, height = 7, dpi = 300)

# Affichage
print(p_carte_mais)
print(p_carte_millet)





# ==============================================================================
# QUESTION 29 : RELATION ENGRAIS × RENDEMENT – BOXPLOT SANS POINTS
# ==============================================================================

# Préparation des données et tests
rend_engrais <- rendement_clean %>%
  filter(!is.na(engrais_inorg)) %>%
  mutate(engrais_lib = ifelse(engrais_inorg == 1,
                              "Avec engrais chimique",
                              "Sans engrais chimique"))

stats_rend_eng <- rend_engrais %>%
  group_by(culture, engrais_lib) %>%
  summarise(
    n = n(),
    median = round(median(rendement_ha), 0),
    mean = round(mean(rendement_ha), 0),
    .groups = "drop"
  )
write.csv(stats_rend_eng,
          paste0(chemin, "/outputs/tables/q29_rendement_engrais.csv"),
          row.names = FALSE)

# Tests de Wilcoxon
wilcox_mais <- wilcox.test(rendement_ha ~ engrais_inorg,
                           data = filter(rend_engrais, culture == "Maïs"),
                           exact = FALSE)
n_mais <- nrow(filter(rend_engrais, culture == "Maïs"))
r_mais <- abs(qnorm(wilcox_mais$p.value / 2)) / sqrt(n_mais)

if(nrow(filter(rend_engrais, culture == "Millet")) >= 20) {
  wilcox_millet <- wilcox.test(rendement_ha ~ engrais_inorg,
                               data = filter(rend_engrais, culture == "Millet"),
                               exact = FALSE)
  r_millet <- abs(qnorm(wilcox_millet$p.value / 2)) / sqrt(nrow(filter(rend_engrais, culture == "Millet")))
} else {
  wilcox_millet <- NULL
  r_millet <- NA
}

capture.output(
  list(mais = wilcox_mais, r_mais = r_mais,
       millet = wilcox_millet, r_millet = r_millet),
  file = paste0(chemin, "/outputs/tables/q29_wilcoxon_engrais.txt")
)

# Graphique : boxplot uniquement (sans jitter)
p_q29 <- rend_engrais %>%
  mutate(engrais_lib = factor(engrais_lib,
                              levels = c("Sans engrais chimique",
                                         "Avec engrais chimique"))) %>%
  ggplot(aes(x = engrais_lib, y = rendement_ha, fill = engrais_lib)) +
  geom_boxplot(alpha = 0.7, width = 0.5) +
  facet_wrap(~culture, scales = "free_y", ncol = 2) +
  scale_fill_manual(values = c("Avec engrais chimique" = "#2D6A4F",
                               "Sans engrais chimique" = "#C94040"),
                    guide = "none") +
  scale_y_continuous(labels = scales::comma) +
  labs(x = NULL, y = "Rendement (kg/ha)") +
  theme_minimal(base_size = 11) +
  theme(strip.text = element_text(face = "bold"))

# Ajout des annotations (p-value et taille d'effet)
if(exists("wilcox_mais")) {
  ymax_mais <- max(rend_engrais$rendement_ha[rend_engrais$culture == "Maïs"], na.rm = TRUE)
  p_q29 <- p_q29 +
    annotate("text", x = 1.5, y = ymax_mais * 0.9,
             label = paste0("p = ", format.pval(wilcox_mais$p.value, digits = 3),
                            "\nr = ", round(r_mais, 3)),
             size = 3, hjust = 0.5)
}
if(exists("wilcox_millet") && !is.null(wilcox_millet)) {
  ymax_millet <- max(rend_engrais$rendement_ha[rend_engrais$culture == "Millet"], na.rm = TRUE)
  p_q29 <- p_q29 +
    annotate("text", x = 1.5, y = ymax_millet * 0.9,
             label = paste0("p = ", format.pval(wilcox_millet$p.value, digits = 3),
                            "\nr = ", round(r_millet, 3)),
             size = 3, hjust = 0.5)
}

ggsave(paste0(chemin, "/outputs/graphs/q29_rendement_engrais_boxplot.png"),
       p_q29, width = 9, height = 5, dpi = 300)

print(p_q29)


cat("Q29 : Graphique avec boxplots seuls (sans points).\n")






# ==============================================================================
# AFFICHAGE DES RÉSULTATS DES TESTS STATISTIQUES
# ==============================================================================

cat("\n\n================== RÉSULTATS DES TESTS STATISTIQUES ==================\n")

# Q26 : Test de Wilcoxon (diversification rurale/urbaine)
cat("\n--- Q26 : Wilcoxon (diversification culturale) ---\n")
print(wilcox_div)
cat("\nTaille d'effet r :", round(r_div, 3), "\n")

# Q27 : Chi-deux (milieu × engrais inorganique)
cat("\n--- Q27 : Chi-deux (milieu × engrais chimique) ---\n")
print(chi2_engrais)
cat("\nV de Cramér :", round(cramer_engrais, 3), "\n")

# Q28 : (Pas de test supplémentaire, les statistiques descriptives sont déjà affichées)

# Q29 : Wilcoxon (engrais × rendement)
cat("\n--- Q29 : Wilcoxon (engrais × rendement) ---\n")
cat("\n*** Maïs ***\n")
print(wilcox_mais)
cat("Taille d'effet r :", round(r_mais, 3), "\n")
if(exists("wilcox_millet") && !is.null(wilcox_millet)) {
  cat("\n*** Millet ***\n")
  print(wilcox_millet)
  cat("Taille d'effet r :", round(r_millet, 3), "\n")
} else {
  cat("\nMillet : effectif insuffisant pour le test.\n")
}

# Affichage des tableaux de statistiques descriptives (optionnel)
cat("\n\n--- Tableaux récapitulatifs ---\n")
cat("\nQ26 : Diversification par milieu\n")
print(stats_diversif)
cat("\nQ27 : Taux d'intrants par milieu\n")
print(taux_milieu %>% select(g, intrant_lib, taux, ic_low, ic_high) %>% mutate(across(c(taux, ic_low, ic_high), ~ round(.x*100,1))))
cat("\nQ28 : Rendements maïs/millet (après outliers)\n")
print(stats_rend)
cat("\nQ29 : Rendements par culture et usage d'engrais\n")
print(stats_rend_eng)

cat("\n================== FIN DES RÉSULTATS ==================\n")
