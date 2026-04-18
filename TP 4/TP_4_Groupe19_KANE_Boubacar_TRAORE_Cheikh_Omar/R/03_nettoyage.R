# ==============================================================================
# TP4 — Analyse des parcelles agricoles
# Fichier : 03_nettoyage.R
# Objectif : Contrôle qualité, suppression des aberrants et doublons,
#            agrégation au niveau ménage
# ==============================================================================
# Prérequis : avoir exécuté 01_import.R puis 02_recodage.R

# ---------------------------------------------------------------------------- #
# 1. Tableau de qualité des données AVANT nettoyage
# ---------------------------------------------------------------------------- #

n_total_a1 <- nrow(sect11a1_p4)
n_total_b1 <- nrow(sect11b1_p4)

na_superf_ha  <- sum(is.na(sect11a1_p4$superficie_ha))
na_superf_gps <- sum(is.na(sect11a1_p4$superficie_gps_ha))
na_poids_a1   <- sum(is.na(sect11a1_p4$wt_wave4))
aberrants_neg <- sum(sect11a1_p4$superficie_ha < 0,   na.rm = TRUE)
aberrants_sup <- sum(sect11a1_p4$superficie_ha > 500, na.rm = TRUE)

qa_table <- data.frame(
  Variable  = c("Superficie ha (declaree)", "Superficie GPS (ha)", "Poids wt_wave4"),
  N_total   = rep(n_total_a1, 3),
  NA_n      = c(na_superf_ha, na_superf_gps, na_poids_a1),
  NA_pct    = round(c(na_superf_ha, na_superf_gps, na_poids_a1) / n_total_a1 * 100, 1),
  Aberrants = c(aberrants_neg + aberrants_sup, NA, NA),
  Seuil     = c("< 0 ou > 500 ha", "--", "--")
)

cat("\n=== Qualité des données (avant nettoyage) ===\n")
print(qa_table)

# Sauvegarde
write.csv(qa_table,
          file.path(proj_root, "output/tables/qa_donnees_tp4.csv"),
          row.names = FALSE)

# ---------------------------------------------------------------------------- #
# 2. Nettoyage de sect11a1_p4
# ---------------------------------------------------------------------------- #

sect11a1_p4 <- sect11a1_p4 |>
  distinct(hhid, plotid, .keep_all = TRUE) |>          # supprimer doublons
  filter(!is.na(hhid)) |>                               # hhid manquants
  filter(!is.na(superficie_ha)) |>                      # superficie manquante
  filter(superficie_ha >= 0, superficie_ha <= 500)      # valeurs aberrantes

cat("\n=== Après nettoyage sect11a1_p4 ===\n")
cat("Parcelles retenues :", nrow(sect11a1_p4), "/", n_total_a1, "\n")

# ---------------------------------------------------------------------------- #
# 3. Nettoyage de sect11b1_p4
# ---------------------------------------------------------------------------- #

sect11b1_p4 <- sect11b1_p4 |>
  distinct(hhid, plotid, .keep_all = TRUE) |>
  filter(!is.na(hhid))

cat("Parcelles retenues b1 :", nrow(sect11b1_p4), "/", n_total_b1, "\n")

# ---------------------------------------------------------------------------- #
# 4. Agrégation au niveau ménage (superficie totale + nombre de parcelles)
# ---------------------------------------------------------------------------- #

menage_parcelle <- sect11a1_p4 |>
  group_by(hhid) |>
  summarise(
    nombre_parcelle          = n_distinct(plotid, na.rm = TRUE),
    superficie_totale_menage = sum(superficie_ha, na.rm = TRUE),
    wt_wave4                 = first(wt_wave4),
    secteur                  = first(secteur),
    state                    = first(state),
    state_label              = first(state_label),
    zone                     = first(zone),
    .groups = "drop"
  )

# Rejoindre les agrégats ménage dans la base parcelle
sect11a1_p4 <- sect11a1_p4 |>
  left_join(
    menage_parcelle |>
      select(hhid, nombre_parcelle, superficie_totale_menage),
    by = "hhid"
  )

cat("\nMénages agriculteurs (>= 1 parcelle valide) :", nrow(menage_parcelle), "\n")

# Sauvegarde des bases propres
saveRDS(sect11a1_p4,   file.path(proj_root, "data/processed/sect11a1_clean.rds"))
saveRDS(sect11b1_p4,   file.path(proj_root, "data/processed/sect11b1_clean.rds"))
saveRDS(menage_parcelle, file.path(proj_root, "data/processed/menage_parcelle.rds"))

write.csv(menage_parcelle,
          file.path(proj_root, "output/tables/superficie_par_menage.csv"),
          row.names = FALSE)

