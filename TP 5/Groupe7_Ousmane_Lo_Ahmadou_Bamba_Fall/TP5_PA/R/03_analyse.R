# =============================================================
# 03_analyse.R — TP5 Cultures, intrants et rendements agricoles
# Nigeria GHS Panel W1 / W3 / W4
# Questions 25 à 30
# =============================================================

library(here)
library(haven)
library(dplyr)
library(ggplot2)
library(forcats)
library(scales)
library(rstatix)
library(survey)
library(srvyr)
library(patchwork)
library(viridis)


# =============================================================
# CHARGEMENT DES DONNÉES
# =============================================================

secta3i_w4   <- read_dta(here("data","raw","w4","secta3i_harvestw4.dta"))
secta3ii_w4  <- read_dta(here("data","raw","w4","secta3ii_harvestw4.dta"))
secta_w4     <- read_dta(here("data","raw","w4","secta_harvestw4.dta"))
secta11c2_w4 <- read_dta(here("data","raw","w4","secta11c2_harvestw4.dta"))
sect11f_w4   <- read_dta(here("data","raw","w4","sect11f_plantingw4.dta"))

secta3i_w3   <- read_dta(here("data","raw","w3","secta3i_harvestw3.dta"))
secta3ii_w3  <- read_dta(here("data","raw","w3","secta3ii_harvestw3.dta"))
secta11d_w3  <- read_dta(here("data","raw","w3","secta11d_harvestw3.dta"))
secta_w3     <- read_dta(here("data","raw","w3","secta_harvestw3.dta"))

secta3_w1    <- read_dta(here("data","raw","w1","secta3_harvestw1.dta"))
sect11d_w1   <- read_dta(here("data","raw","w1","sect11d_plantingw1.dta"))
sect11f_w1   <- read_dta(here("data","raw","w1","sect11f_plantingw1.dta"))
secta_w1     <- read_dta(here("data","raw","w1","secta_harvestw1.dta"))

# Table des codes cultures avec type agronomique
crop_types <- tribble(
  ~cropcode, ~crop_name,          ~type_culture,
  1010,      "Sorghum blanc",     "Céréale",
  1020,      "Sorghum rouge",     "Céréale",
  1050,      "Mil pénicillaire",  "Céréale",
  1060,      "Riz",               "Céréale",
  1070,      "Sorgho",            "Céréale",
  1080,      "Maïs",              "Céréale",
  1100,      "Millet",            "Céréale",
  1110,      "Blé",               "Céréale",
  2010,      "Niébé",             "Légumineuse",
  2020,      "Arachide",          "Légumineuse",
  2060,      "Soja",              "Légumineuse",
  2080,      "Haricot mungo",     "Légumineuse",
  2100,      "Pois cajan",        "Légumineuse",
  3010,      "Igname",            "Tubercule",
  3020,      "Manioc",            "Tubercule",
  3040,      "Patate douce",      "Tubercule",
  5010,      "Coton",             "Culture de rente",
  5020,      "Cacao",             "Culture de rente",
  5030,      "Café",              "Culture de rente",
  5040,      "Palmier à huile",   "Culture de rente",
  5061,      "Sésame",            "Culture de rente"
)

# Poids et zone depuis secta_w4
poids_zone_w4 <- secta_w4 %>%
  select(hhid, sector, state, zone, wt_wave4) %>%
  distinct(hhid, .keep_all = TRUE)


# =============================================================
# QUESTION 25 — 15 cultures les plus fréquentes en W4
# =============================================================

# Combiner secta3i (niveau parcelle-culture) et secta3ii (niveau culture-ménage)
# pour avoir la liste complète des cultures pratiquées en W4
cultures_w4 <- bind_rows(
  secta3i_w4  %>% select(hhid, cropcode),
  secta3ii_w4 %>% select(hhid, cropcode)
) %>%
  distinct(hhid, cropcode) %>%
  filter(!is.na(cropcode))

top15 <- cultures_w4 %>%
  count(cropcode, name = "nb_menages") %>%
  left_join(crop_types, by = "cropcode") %>%
  mutate(
    crop_name    = ifelse(is.na(crop_name), paste("Code", cropcode), crop_name),
    type_culture = ifelse(is.na(type_culture), "Autre", type_culture),
    proportion   = nb_menages / n_distinct(cultures_w4$hhid) * 100
  ) %>%
  slice_max(nb_menages, n = 15) %>%
  mutate(crop_name = fct_reorder(crop_name, nb_menages))

sauvegarder_tab(top15, "Q25_top15_cultures.csv")

p_q25 <- ggplot(top15, aes(x = nb_menages, y = crop_name, fill = type_culture)) +
  geom_col(alpha = 0.85) +
  geom_text(aes(label = paste0(round(proportion, 1), "%")),
            hjust = -0.15, size = 3.5) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.12))) +
  scale_fill_viridis_d(option = "plasma", name = "Type de culture") +
  labs(title   = "15 cultures les plus fréquentes – Nigeria GHS W4",
       x       = "Nombre de ménages",
       y       = NULL,
       caption = "Source : Nigeria GHS Panel W4 | secta3i + secta3ii") +
  theme_minimal(base_size = 12)

sauvegarder_fig(p_q25, "Q25_top15_cultures.png", largeur = 11, hauteur = 8)
message("Q25 done")


# =============================================================
# QUESTION 26 — Indice de diversification culturale par ménage
# =============================================================

diversif <- cultures_w4 %>%
  group_by(hhid) %>%
  summarise(nb_cultures = n_distinct(cropcode), .groups = "drop") %>%
  left_join(poids_zone_w4 %>% select(hhid, sector, wt_wave4), by = "hhid") %>%
  mutate(zone_label = case_when(
    as.numeric(sector) == 1 ~ "Urbain",
    as.numeric(sector) == 2 ~ "Rural"
  )) %>%
  filter(!is.na(zone_label), !is.na(wt_wave4))

svy_diversif <- svydesign(ids = ~1, weights = ~wt_wave4, data = diversif)

# Test Wilcoxon pondéré
wilcox_d <- svyranktest(nb_cultures ~ zone_label, design = svy_diversif)
message("Q26 - Wilcoxon p = ", round(wilcox_d$p.value, 4))

p_violin_q26 <- ggplot(diversif,
                       aes(x = zone_label, y = nb_cultures,
                           fill = zone_label, weight = wt_wave4)) +
  geom_violin(alpha = 0.6, trim = FALSE) +
  geom_boxplot(width = 0.15, alpha = 0.9, outlier.size = 0.5) +
  scale_fill_manual(values = c("Rural" = "#66BB6A", "Urbain" = "#42A5F5")) +
  annotate("text", x = 1.5, y = max(diversif$nb_cultures) * 0.95,
           label = paste0("p = ", round(wilcox_d$p.value, 4)), size = 4) +
  labs(title   = "Diversification culturale par zone – W4 (pondéré)",
       x       = NULL,
       y       = "Nombre de cultures différentes",
       caption = "Source : Nigeria GHS Panel W4 | secta3i + secta3ii") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "none")

p_hist_q26 <- ggplot(diversif, aes(x = nb_cultures, weight = wt_wave4)) +
  geom_histogram(binwidth = 1, fill = "#5C6BC0", color = "white", alpha = 0.85) +
  labs(title   = "Distribution de l'indice de diversification – W4 (pondéré)",
       x       = "Nombre de cultures",
       y       = "Effectif pondéré",
       caption = "Source : Nigeria GHS Panel W4") +
  theme_minimal(base_size = 12)

p_q26 <- p_hist_q26 / p_violin_q26
sauvegarder_fig(p_q26, "Q26_diversification_culturale.png",
                largeur = 10, hauteur = 10)
message("Q26 done")


# =============================================================
# QUESTION 27 — Utilisation des engrais par zone et État (W4)
# =============================================================

# secta11c2_harvestw4 : niveau parcelle
# s11dq1a = engrais inorganique (1=oui), s11dq36 = engrais organique (1=oui)
# s11c2q36_1 = NPK, s11c2q36_2 = UREA

engrais_w4 <- secta11c2_w4 %>%
  select(hhid, plotid,
         inorganique = s11dq1a,
         organique   = s11dq36,
         npk         = s11c2q36_1,
         urea        = s11c2q36_2) %>%
  mutate(
    inorganique = as.numeric(inorganique) == 1,
    organique   = as.numeric(organique)   == 1,
    npk         = as.numeric(npk)         == 1,
    urea        = as.numeric(urea)        == 1
  ) %>%
  group_by(hhid) %>%
  summarise(
    utilise_inorg = any(inorganique, na.rm = TRUE),
    utilise_org   = any(organique,   na.rm = TRUE),
    utilise_npk   = any(npk,         na.rm = TRUE),
    utilise_urea  = any(urea,        na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(poids_zone_w4, by = "hhid") %>%
  filter(!is.na(wt_wave4)) %>%
  mutate(zone_label = case_when(
    as.numeric(sector) == 1 ~ "Urbain",
    as.numeric(sector) == 2 ~ "Rural"
  ))

# Taux pondérés par type × zone
taux_engrais <- engrais_w4 %>%
  group_by(zone_label) %>%
  summarise(
    taux_inorganique = sum(wt_wave4[utilise_inorg]) / sum(wt_wave4) * 100,
    taux_organique   = sum(wt_wave4[utilise_org])   / sum(wt_wave4) * 100,
    taux_npk         = sum(wt_wave4[utilise_npk])   / sum(wt_wave4) * 100,
    taux_urea        = sum(wt_wave4[utilise_urea])  / sum(wt_wave4) * 100,
    n_pondere        = sum(wt_wave4),
    .groups = "drop"
  )

sauvegarder_tab(taux_engrais, "Q27_taux_engrais_zone.csv")

# Test chi-deux pondéré zone × utilisation inorganique
svy_engrais <- svydesign(ids = ~1, weights = ~wt_wave4, data = engrais_w4)
chi2_engrais <- svychisq(~ utilise_inorg + zone_label, design = svy_engrais)
message("Q27 - chi2 zone x inorganique p = ", round(chi2_engrais$p.value, 4))

# Barplot groupé
taux_long <- taux_engrais %>%
  tidyr::pivot_longer(cols = starts_with("taux_"),
                      names_to  = "type_engrais",
                      values_to = "taux") %>%
  mutate(type_engrais = recode(type_engrais,
                               "taux_inorganique" = "Inorganique",
                               "taux_organique"   = "Organique",
                               "taux_npk"         = "NPK",
                               "taux_urea"        = "Urée"
  ))

p_q27 <- ggplot(taux_long,
                aes(x = type_engrais, y = taux, fill = zone_label)) +
  geom_col(position = "dodge", alpha = 0.85) +
  scale_y_continuous(labels = percent_format(scale = 1)) +
  scale_fill_manual(values = c("Rural" = "#66BB6A", "Urbain" = "#42A5F5")) +
  labs(title   = "Taux d'utilisation des engrais par type et zone – W4 (pondéré)",
       x       = "Type d'engrais",
       y       = "Taux pondéré (%)",
       fill    = "Zone",
       caption = "Source : Nigeria GHS Panel W4 | secta11c2_harvestw4") +
  theme_minimal(base_size = 12)

sauvegarder_fig(p_q27, "Q27_engrais_zone.png")
message("Q27 done")


# =============================================================
# QUESTION 28 — Rendement à l'hectare (maïs et millet) – W4
# =============================================================

# Maïs = 1080 | Millet = 1100
CODES_CIBLES <- c(1080, 1100)

# Superficie par parcelle-culture depuis secta3i (en ha via conversion)
superficie <- secta3i_w4 %>%
  filter(cropcode %in% CODES_CIBLES) %>%
  select(hhid, plotid, cropcode,
         superficie_m   = sa3iq6i,
         conv_factor    = sa3iq6_conv) %>%
  mutate(superficie_ha = superficie_m * conv_factor / 10000) %>%
  group_by(hhid, cropcode) %>%
  summarise(superficie_ha = sum(superficie_ha, na.rm = TRUE), .groups = "drop") %>%
  filter(superficie_ha > 0)

# Production depuis secta3ii (en kg via conversion)
production <- secta3ii_w4 %>%
  filter(cropcode %in% CODES_CIBLES) %>%
  select(hhid, cropcode,
         quantite     = sa3iiq1a,
         conv_factor  = sa3iiq1_conv) %>%
  mutate(prod_kg = quantite * conv_factor) %>%
  group_by(hhid, cropcode) %>%
  summarise(prod_kg = sum(prod_kg, na.rm = TRUE), .groups = "drop") %>%
  filter(prod_kg > 0)

# Calcul du rendement
rendement <- superficie %>%
  inner_join(production, by = c("hhid", "cropcode")) %>%
  mutate(rendement_kg_ha = prod_kg / superficie_ha) %>%
  left_join(poids_zone_w4 %>% select(hhid, state, wt_wave4), by = "hhid") %>%
  filter(!is.na(wt_wave4)) %>%
  left_join(crop_types %>% select(cropcode, crop_name), by = "cropcode")

# Supprimer outliers IQR × 3
rendement_clean <- rendement %>%
  group_by(cropcode) %>%
  mutate(
    q1   = quantile(rendement_kg_ha, 0.25, na.rm = TRUE),
    q3   = quantile(rendement_kg_ha, 0.75, na.rm = TRUE),
    iqr  = q3 - q1,
    flag = rendement_kg_ha < (q1 - 3 * iqr) |
      rendement_kg_ha > (q3 + 3 * iqr)
  ) %>%
  filter(!flag) %>%
  ungroup()

message("Q28 - Obs avant/après outliers : ", nrow(rendement), " / ", nrow(rendement_clean))
sauvegarder_tab(rendement_clean %>%
                  select(hhid, cropcode, crop_name, superficie_ha,
                         prod_kg, rendement_kg_ha, state),
                "Q28_rendements.csv")

p_q28 <- ggplot(rendement_clean,
                aes(x = reorder(as.factor(state), rendement_kg_ha, median),
                    y = rendement_kg_ha,
                    fill = crop_name,
                    weight = wt_wave4)) +
  geom_boxplot(alpha = 0.75, outlier.size = 0.4) +
  facet_wrap(~ crop_name, scales = "free_y") +
  scale_fill_manual(values = c("Maïs" = "#FFA726", "Millet" = "#AB47BC")) +
  labs(title   = "Distribution des rendements par État – W4",
       x       = "État",
       y       = "Rendement (kg/ha)",
       caption = "Source : Nigeria GHS Panel W4 | secta3i + secta3ii") +
  theme_minimal(base_size = 11) +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 60, hjust = 1))

sauvegarder_fig(p_q28, "Q28_rendements_etat.png", largeur = 14, hauteur = 8)
message("Q28 done")


# =============================================================
# QUESTION 29 — Engrais chimique vs rendement (maïs + millet)
# =============================================================

# Utilisation engrais inorganique par ménage (déjà calculé)
engrais_hh <- engrais_w4 %>%
  select(hhid, utilise_inorg)

df_q29 <- rendement_clean %>%
  left_join(engrais_hh, by = "hhid") %>%
  filter(!is.na(utilise_inorg)) %>%
  mutate(engrais_label = ifelse(utilise_inorg,
                                "Avec engrais chimique",
                                "Sans engrais chimique"))

# Test Wilcoxon pondéré
svy_q29 <- svydesign(ids = ~1, weights = ~wt_wave4, data = df_q29)
wilcox_q29 <- svyranktest(rendement_kg_ha ~ engrais_label, design = svy_q29)
r_effet <- abs(qnorm(wilcox_q29$p.value / 2)) / sqrt(nrow(df_q29))

message("Q29 - Wilcoxon p = ", round(wilcox_q29$p.value, 4),
        " | r = ", round(r_effet, 3))

p_q29 <- ggplot(df_q29,
                aes(x = engrais_label, y = rendement_kg_ha,
                    fill = engrais_label, weight = wt_wave4)) +
  geom_boxplot(alpha = 0.75) +
  facet_wrap(~ crop_name, scales = "free_y") +
  annotate("text",
           x     = 1.5,
           y     = max(df_q29$rendement_kg_ha) * 0.9,
           label = paste0("p = ", round(wilcox_q29$p.value, 4),
                          "\nr = ", round(r_effet, 3)),
           size = 3.5) +
  scale_fill_manual(values = c("Avec engrais chimique"  = "#EF5350",
                               "Sans engrais chimique"  = "#78909C")) +
  labs(title   = "Rendement selon l'utilisation d'engrais chimique – W4 (pondéré)",
       x       = NULL,
       y       = "Rendement (kg/ha)",
       caption = "Source : Nigeria GHS Panel W4") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "none")

sauvegarder_fig(p_q29, "Q29_engrais_rendement.png", largeur = 11, hauteur = 7)
message("Q29 done")


# =============================================================
# QUESTION 30 — Adoption intrants W1 vs W4
# =============================================================

# ── Engrais ──────────────────────────────────────────────────
# W1 : sect11d_plantingw1, s11dq1 = 1 yes / 2 no, niveau parcelle
engrais_hh_w1 <- sect11d_w1 %>%
  mutate(utilise_engrais = as.numeric(s11dq1) == 1) %>%
  group_by(hhid) %>%
  summarise(utilise_engrais = any(utilise_engrais, na.rm = TRUE),
            .groups = "drop")

poids_w1 <- secta_w1 %>%
  select(hhid, wt_wave1) %>%
  distinct(hhid, .keep_all = TRUE)

taux_engrais_w1 <- engrais_hh_w1 %>%
  left_join(poids_w1, by = "hhid") %>%
  filter(!is.na(wt_wave1)) %>%
  summarise(taux = sum(wt_wave1[utilise_engrais]) / sum(wt_wave1) * 100,
            vague = "W1", intrant = "Engrais")

# W4 : déjà calculé (utilise_inorg)
taux_engrais_w4 <- engrais_w4 %>%
  summarise(taux = sum(wt_wave4[utilise_inorg]) / sum(wt_wave4) * 100,
            vague = "W4", intrant = "Engrais")

# ── Semences améliorées ───────────────────────────────────────
# W1 : sect11f_plantingw1 — pas de variable directe semences améliorées
# → on utilise le taux de ménages ayant des cultures (proxy)
# W4 : sect11f_plantingw4, s11fq3b = 1 improved / 2 traditional

semences_hh_w4 <- sect11f_w4 %>%
  mutate(seed_amelioree = as.numeric(s11fq3b) == 1) %>%
  group_by(hhid) %>%
  summarise(seed_amelioree = any(seed_amelioree, na.rm = TRUE),
            .groups = "drop")

taux_seed_w4 <- semences_hh_w4 %>%
  left_join(poids_zone_w4 %>% select(hhid, wt_wave4), by = "hhid") %>%
  filter(!is.na(wt_wave4)) %>%
  summarise(taux = sum(wt_wave4[seed_amelioree]) / sum(wt_wave4) * 100,
            vague = "W4", intrant = "Semences améliorées")

# W1 : sect11f_plantingw1 n'a pas de variable improved/traditional
# On indique NA avec une note
taux_seed_w1 <- tibble(taux = NA_real_, vague = "W1",
                       intrant = "Semences améliorées")

# ── Assembler et visualiser ───────────────────────────────────
df_q30 <- bind_rows(
  taux_engrais_w1,
  taux_engrais_w4,
  taux_seed_w1,
  taux_seed_w4
)

# Évolution uniquement pour les intrants avec données W1 ET W4
evol <- df_q30 %>%
  filter(!is.na(taux)) %>%
  group_by(intrant) %>%
  filter(n() == 2) %>%
  summarise(
    taux_w1   = taux[vague == "W1"],
    taux_w4   = taux[vague == "W4"],
    evolution = taux_w4 - taux_w1,
    .groups   = "drop"
  )

sauvegarder_tab(df_q30 %>% filter(!is.na(taux)), "Q30_adoption_intrants.csv")
sauvegarder_tab(evol,                             "Q30_evolution_adoption.csv")

# Barplot uniquement sur les données disponibles
p_q30 <- ggplot(df_q30 %>% filter(!is.na(taux)),
                aes(x = vague, y = taux, fill = intrant)) +
  geom_col(position = "dodge", alpha = 0.85) +
  geom_text(aes(label = paste0(round(taux, 1), "%")),
            position = position_dodge(0.9),
            vjust = -0.4, size = 3.8) +
  scale_y_continuous(labels = percent_format(scale = 1),
                     expand = expansion(mult = c(0, 0.1))) +
  scale_fill_manual(values = c("Engrais"             = "#EF5350",
                               "Semences améliorées" = "#42A5F5")) +
  labs(title   = "Taux d'adoption des intrants W1 vs W4 (pondéré)",
       x       = "Vague",
       y       = "Taux pondéré (%)",
       fill    = "Intrant",
       caption = "Source : Nigeria GHS Panel W1 & W4\nNote : semences améliorées non disponibles en W1") +
  theme_minimal(base_size = 12)

sauvegarder_fig(p_q30, "Q30_adoption_W1_W4.png")
message("Q30 done")

message("\n=== Toutes les analyses TP5 terminées ===")