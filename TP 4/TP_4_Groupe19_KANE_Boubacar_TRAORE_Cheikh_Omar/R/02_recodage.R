# ==============================================================================
# TP4 — Analyse des parcelles agricoles
# Fichier : 02_recodage.R
# Objectif : Transformation et création des variables analytiques
# Auteurs  : Boubacar KANE / Cheikh Omar TRAORE
# Date     : 2025 - 2026
# ==============================================================================
# Prérequis : avoir exécuté 01_import.R

# ---------------------------------------------------------------------------- #
# 1. Recodage de sect11a1_p4
# ---------------------------------------------------------------------------- #

sect11a1_p4 <- sect11a1_p4 |>
  mutate(
    zone             = as_factor(zone),
    unite_mesure     = as_factor(s11aq4b),
    superficie_agri  = s11aq4aa,
    superficie_gps_m2 = s11aq4c,
    secteur          = factor(sector, levels = 1:2,
                              labels = c("Urbain", "Rural")),
    state_label      = as_factor(state)
  )

# ---------------------------------------------------------------------------- #
# 2. Construction de la superficie en hectares (avec facteurs de conversion)
# ---------------------------------------------------------------------------- #
# Sources : Nigeria GHS 2018/19 Basic Information Document
# Les unités Plots, Acres, Hectares, Square Meters sont zones-invariantes.
# Les unités Heaps, Ridges, Stands dépendent de la zone géopolitique.

sect11a1_p4 <- sect11a1_p4 |>
  mutate(
    superficie_ha = case_when(
      # Unités indépendantes de la zone
      unite_mesure == "4. Plots"         ~ superficie_agri * 0.0667,
      unite_mesure == "5. ACRES"         ~ superficie_agri * 0.4000,
      unite_mesure == "6. HECTARES"      ~ superficie_agri * 1.0000,
      unite_mesure == "7. SQUARE METERS" ~ superficie_agri * 0.0001,
      
      # STANDS par zone géopolitique
      unite_mesure == "3. STANDS" & zone == "1. North Central" ~ superficie_agri * 0.00006,
      unite_mesure == "3. STANDS" & zone == "2. North East"    ~ superficie_agri * 0.00016,
      unite_mesure == "3. STANDS" & zone == "3. North West"    ~ superficie_agri * 0.00004,
      unite_mesure == "3. STANDS" & zone == "4. South East"    ~ superficie_agri * 0.00004,
      unite_mesure == "3. STANDS" & zone == "5. South South"   ~ superficie_agri * 0.00013,
      unite_mesure == "3. STANDS" & zone == "6. South West"    ~ superficie_agri * 0.00041,
      
      # RIDGES par zone géopolitique
      unite_mesure == "2. RIDGES" & zone == "1. North Central" ~ superficie_agri * 0.00270,
      unite_mesure == "2. RIDGES" & zone == "2. North East"    ~ superficie_agri * 0.00400,
      unite_mesure == "2. RIDGES" & zone == "3. North West"    ~ superficie_agri * 0.00494,
      unite_mesure == "2. RIDGES" & zone == "4. South East"    ~ superficie_agri * 0.00230,
      unite_mesure == "2. RIDGES" & zone == "5. South South"   ~ superficie_agri * 0.00230,
      unite_mesure == "2. RIDGES" & zone == "6. South West"    ~ superficie_agri * 0.00001,
      
      # HEAPS par zone géopolitique
      unite_mesure == "1. HEAPS" & zone == "1. North Central"  ~ superficie_agri * 0.00012,
      unite_mesure == "1. HEAPS" & zone == "2. North East"     ~ superficie_agri * 0.00016,
      unite_mesure == "1. HEAPS" & zone == "3. North West"     ~ superficie_agri * 0.00011,
      unite_mesure == "1. HEAPS" & zone == "4. South East"     ~ superficie_agri * 0.00019,
      unite_mesure == "1. HEAPS" & zone == "5. South South"    ~ superficie_agri * 0.00021,
      unite_mesure == "1. HEAPS" & zone == "6. South West"     ~ superficie_agri * 0.00012,
      
      TRUE ~ NA_real_
    ),
    # Superficie GPS en hectares
    superficie_gps_ha = superficie_gps_m2 / 10000
  )

# ---------------------------------------------------------------------------- #
# 3. Recodage de sect11b1_p4
# ---------------------------------------------------------------------------- #

sect11b1_p4 <- sect11b1_p4 |>
  mutate(
    tenure = factor(
      s11b1q4,
      levels = 1:7,
      labels = c(
        "Propriete pleine (achat direct)",
        "Location (especes/nature)",
        "Usage gratuit",
        "Distribution par la communaute",
        "Heritage familial",
        "Metayage",
        "Echange temporaire de terres"
      )
    ),
    secteur = factor(sector, levels = 1:2,
                     labels = c("Urbain", "Rural"))
  )

# ---------------------------------------------------------------------------- #
# 4. Intégration des poids de sondage (wt_wave4 depuis secta_w4)
# ---------------------------------------------------------------------------- #

sect11a1_p4 <- sect11a1_p4 |>
  left_join(
    secta_w4 |> select(hhid, wt_wave4),
    by = "hhid"
  )

sect11b1_p4 <- sect11b1_p4 |>
  left_join(
    secta_w4 |> select(hhid, wt_wave4),
    by = "hhid"
  )

cat("\n=== Recodage sect11a1_p4 ===\n")
cat("Distribution de unite_mesure :\n")
print(table(sect11a1_p4$unite_mesure, useNA = "ifany"))
cat("\nNA sur superficie_ha :", sum(is.na(sect11a1_p4$superficie_ha)), "\n")
cat("NA sur wt_wave4 (a1)  :", sum(is.na(sect11a1_p4$wt_wave4)), "\n")

cat("\n=== Recodage sect11b1_p4 ===\n")
cat("Distribution de tenure :\n")
print(table(sect11b1_p4$tenure, useNA = "ifany"))
cat("NA sur wt_wave4 (b1)  :", sum(is.na(sect11b1_p4$wt_wave4)), "\n")

