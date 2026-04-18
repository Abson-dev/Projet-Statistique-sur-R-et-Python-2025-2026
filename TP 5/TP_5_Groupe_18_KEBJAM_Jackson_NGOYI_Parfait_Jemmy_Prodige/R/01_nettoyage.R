# ============================================================================
# SCRIPT DE NETTOYAGE - VAGUE 4
# Objectif : Traiter les bases nécessaires pour les analyses
# ============================================================================

# Chargement des bibliothèques
library(haven)
library(dplyr)
library(stringr)
library(tidyr)

# Chemins (avec vérification)
data_path <- "data/raw"
processed_path <- "data/processed"

if (!dir.exists(data_path)) stop("Dossier data/raw introuvable")
if (!dir.exists(processed_path)) dir.create(processed_path, recursive = TRUE)

# Les estimations sont pondérées par wt_wave4 afin de garantir la représentativité
# nationale des ménages agricoles dans LSMS-ISA.

# ============================================================================
# 1. CHARGEMENT DES DONNÉES COMMUNES
# ============================================================================

secta_w4 <- read_dta(file.path(data_path, "secta_harvestw4.dta"))

# ============================================================================
# 2. CLASSIFICATION DES CULTURES
# ============================================================================

secta3i_w4 <- read_dta(file.path(data_path, "secta3i_harvestw4.dta"))

crop_labels <- attr(secta3i_w4$cropcode, "labels")
crop_mapping <- data.frame(
  cropcode = as.numeric(crop_labels),
  crop_name = tolower(str_trim(str_remove(names(crop_labels), "^[0-9]+\\.\\s*")))
)

crop_mapping <- crop_mapping %>%
  mutate(crop_type = case_when(
    str_detect(crop_name, "maize|rice|sorghum|guinea corn|millet|maiwa|wheat|acha") ~ "céréale",
    str_detect(crop_name, "bean|cowpea|ground nut|peanut|soya|bambara|pigeon pea|locust") ~ "légumineuse",
    str_detect(crop_name, "cassava|yam|cocoyam|potato|sweet potato") ~ "tubercule",
    str_detect(crop_name, "cotton|cocoa|oil palm|palm fruit|kola|cashew|ginger|sesame|beeni|tobacco|rubber|coffee|shea") ~ "culture de rente",
    TRUE ~ "autres"
  ))

cat("✅ Classification créée pour", nrow(crop_mapping), "cultures\n")

# ============================================================================
# 3. BASE SECTA3II (CULTURES AGRÉGÉES PAR MÉNAGE) - Question 25
# ============================================================================

secta3ii_w4 <- read_dta(file.path(data_path, "secta3ii_harvestw4.dta"))

secta3ii <- secta3ii_w4 %>%
  select(hhid, zone, state, lga, sector, ea, cropcode) %>%
  left_join(crop_mapping, by = "cropcode") %>%
  left_join(secta_w4 %>% select(hhid, wt_wave4), by = "hhid") %>%
  mutate(area_type = ifelse(sector == 1, "urban", ifelse(sector == 2, "rural", NA))) %>%
  distinct()

saveRDS(secta3ii, file.path(processed_path, "secta3ii.rds"))
cat("✅ secta3ii.rds :", nrow(secta3ii), "observations,", n_distinct(secta3ii$hhid), "ménages\n")

# ============================================================================
# 4. BASE SECTA3 (CULTURES PAR PARCELLE) - Question 26
# ============================================================================

secta3 <- secta3i_w4 %>%
  filter(sa3iq3 == 1) %>%
  select(hhid, zone, state, lga, sector, ea, plotid, cropcode) %>%
  left_join(crop_mapping, by = "cropcode") %>%
  left_join(secta_w4 %>% select(hhid, wt_wave4), by = "hhid") %>%
  mutate(area_type = ifelse(sector == 1, "urban", ifelse(sector == 2, "rural", NA))) %>%
  distinct()

saveRDS(secta3, file.path(processed_path, "secta3.rds"))
cat("✅ secta3.rds :", nrow(secta3), "observations,", n_distinct(secta3$hhid), "ménages\n")

# ============================================================================
# 5. BASE SECTA11C2 (ENGRAIS PAR PARCELLE) - Question 27
# ============================================================================
# Justification : L'énoncé mentionne secta5a, mais cette base ne contient pas
# les détails NPK/Urée. secta11c2 est plus complète et permet de distinguer
# les types d'engrais demandés : organique, NPK, urée.
# Définition agronomique standard : engrais chimique = NPK ou Urée

secta11c2_w4 <- read_dta(file.path(data_path, "secta11c2_harvestw4.dta"))

# Conservation au niveau parcelle avec toutes les variables géographiques
secta11c2_parcelle <- secta11c2_w4 %>%
  select(hhid, plotid, sector, zone, state, lga, ea,
         s11dq1a, s11c2q36_1, s11c2q36_2, s11dq36) %>%
  group_by(hhid, plotid, sector, zone, state, lga, ea) %>%
  summarise(
    used_inorganic = ifelse(all(is.na(s11dq1a)), 0, max(s11dq1a, na.rm = TRUE)),
    used_npk = ifelse(all(is.na(s11c2q36_1)), 0, max(s11c2q36_1, na.rm = TRUE)),
    used_urea = ifelse(all(is.na(s11c2q36_2)), 0, max(s11c2q36_2, na.rm = TRUE)),
    used_organic = ifelse(all(is.na(s11dq36)), 0, max(s11dq36, na.rm = TRUE)),
    used_chemical = ifelse(used_npk == 1 | used_urea == 1, 1, 0),
    .groups = "drop"
  )

# Agrégation au niveau ménage pour Q27 (taux par ménage)
secta11c2_menage <- secta11c2_parcelle %>%
  group_by(hhid) %>%
  summarise(
    used_inorganic = max(used_inorganic, na.rm = TRUE),
    used_npk = max(used_npk, na.rm = TRUE),
    used_urea = max(used_urea, na.rm = TRUE),
    used_organic = max(used_organic, na.rm = TRUE),
    used_chemical = max(used_chemical, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(secta_w4 %>% select(hhid, wt_wave4, zone, state, lga, sector, ea), by = "hhid") %>%
  mutate(area_type = ifelse(sector == 1, "urban", ifelse(sector == 2, "rural", NA)))

saveRDS(secta11c2_menage, file.path(processed_path, "secta11c2.rds"))
cat("✅ secta11c2.rds :", nrow(secta11c2_menage), "ménages\n")
cat("   - Engrais chimique (NPK/Urée):", sum(secta11c2_menage$used_chemical == 1), "\n")

# ============================================================================
# 6. BASE SECTA_RENDEMENT (MAÏS ET MILLET) - Questions 28 et 29
# ============================================================================

secta1_w4 <- read_dta(file.path(data_path, "secta1_harvestw4.dta"))

# ---------------------------------------------------------------------------
# 1. Base parcelle avec production et superficie
# ---------------------------------------------------------------------------
base_plot <- secta3i_w4 %>%
  filter(cropcode %in% c(1080, 1100), sa3iq3 == 1) %>%
  select(hhid, zone, state, lga, sector, ea, plotid, cropcode,
         sa3iq6i, sa3iq6_conv) %>%
  mutate(
    culture = case_when(
      cropcode == 1080 ~ "mais",
      cropcode == 1100 ~ "millet"
    ),
    production_kg = ifelse(!is.na(sa3iq6_conv) & sa3iq6_conv > 0,
                           sa3iq6i * sa3iq6_conv, NA_real_)
  ) %>%
  left_join(
    secta1_w4 %>%
      select(hhid, plotid, prefilled_gps_area, sa1q11, sa1q9),
    by = c("hhid", "plotid")
  ) %>%
  mutate(
    superficie_ha = case_when(
      !is.na(prefilled_gps_area) & prefilled_gps_area > 0 ~ prefilled_gps_area / 10000,
      !is.na(sa1q11) & sa1q11 > 0 ~ sa1q11,
      TRUE ~ NA_real_
    ),
    gps_measured = ifelse(sa1q9 == 1, 1, 0),
    rendement_kg_ha = production_kg / superficie_ha
  ) %>%
  filter(!is.na(rendement_kg_ha), rendement_kg_ha > 0, rendement_kg_ha < 20000)

# ---------------------------------------------------------------------------
# 2. Identifier la culture principale du ménage
# (basée sur production totale)
# ---------------------------------------------------------------------------
culture_principale <- base_plot %>%
  group_by(hhid, culture) %>%
  summarise(prod_tot = sum(production_kg, na.rm = TRUE), .groups = "drop") %>%
  group_by(hhid) %>%
  slice_max(prod_tot, n = 1, with_ties = FALSE) %>%
  ungroup()

# ---------------------------------------------------------------------------
# 3. Garder uniquement la culture principale
# ---------------------------------------------------------------------------
secta_rendement <- base_plot %>%
  inner_join(culture_principale, by = c("hhid", "culture"))

# ---------------------------------------------------------------------------
# 4. Ajouter fertilisants + poids + variable used_chemical
# ---------------------------------------------------------------------------
secta_rendement <- secta_rendement %>%
  left_join(
    secta11c2_w4 %>%
      group_by(hhid, plotid) %>%
      summarise(
        used_inorganic = ifelse(all(is.na(s11dq1a)), 0, max(s11dq1a, na.rm = TRUE)),
        used_npk = ifelse(all(is.na(s11c2q36_1)), 0, max(s11c2q36_1, na.rm = TRUE)),
        used_urea = ifelse(all(is.na(s11c2q36_2)), 0, max(s11c2q36_2, na.rm = TRUE)),
        used_organic = ifelse(all(is.na(s11dq36)), 0, max(s11dq36, na.rm = TRUE)),
        # Définition agronomique : engrais chimique = NPK ou Urée
        used_chemical = ifelse(used_npk == 1 | used_urea == 1, 1, 0),
        .groups = "drop"
      ),
    by = c("hhid", "plotid")
  ) %>%
  left_join(
    secta_w4 %>% select(hhid, wt_wave4),
    by = "hhid"
  ) %>%
  mutate(
    area_type = ifelse(sector == 1, "urban",
                       ifelse(sector == 2, "rural", NA))
  )

# ---------------------------------------------------------------------------
# 5. Détection des outliers (IQR × 3)
# ---------------------------------------------------------------------------
secta_rendement <- secta_rendement %>%
  group_by(culture) %>%
  mutate(
    q1 = quantile(rendement_kg_ha, 0.25, na.rm = TRUE),
    q3 = quantile(rendement_kg_ha, 0.75, na.rm = TRUE),
    iqr = q3 - q1,
    outlier = rendement_kg_ha < (q1 - 3 * iqr) |
      rendement_kg_ha > (q3 + 3 * iqr)
  ) %>%
  ungroup()

# ---------------------------------------------------------------------------
# 6. Sélection finale (inclure used_chemical)
# ---------------------------------------------------------------------------
secta_rendement <- secta_rendement %>%
  select(hhid, plotid, zone, state, lga, sector, ea,
         cropcode, culture,
         production_kg, superficie_ha, rendement_kg_ha,
         used_inorganic, used_npk, used_urea, used_organic, used_chemical,
         wt_wave4, area_type, outlier, gps_measured)

# ---------------------------------------------------------------------------
# 7. Sauvegarde
# ---------------------------------------------------------------------------
saveRDS(secta_rendement, file.path(processed_path, "secta_rendement.rds"))
write.csv(secta_rendement, file.path(processed_path, "secta_rendement.csv"), row.names = FALSE)

cat("  - Observations:", nrow(secta_rendement), "\n")
cat("  - Ménages:", n_distinct(secta_rendement$hhid), "\n")
cat("  - Maïs:", sum(secta_rendement$culture == "mais"), "\n")
cat("  - Millet:", sum(secta_rendement$culture == "millet"), "\n")
cat("  - Outliers:", sum(secta_rendement$outlier, na.rm = TRUE), "\n")
cat("  - Avec engrais chimique:", sum(secta_rendement$used_chemical == 1, na.rm = TRUE), "\n")
cat("  → Sauvegardé: secta_rendement.rds\n")