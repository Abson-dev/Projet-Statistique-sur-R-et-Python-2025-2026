###############################################################################
# 01_import.R — Chargement et exploration des données (Tâche 1)
# Projet : TP1 — Profil démographique des ménages nigérians
###############################################################################

cat("\n========== 01_import : Chargement des données ==========\n")

# Chargement des fichiers principaux (vague 4, Post-Harvest)
sect1_hw4 <- read_dta(file.path(chemin_data, "sect1_harvestw4.dta"))
secta_hw4 <- read_dta(file.path(chemin_data, "secta_harvestw4.dta"))

cat("sect1_harvestw4 :", nrow(sect1_hw4), "observations,",
    ncol(sect1_hw4), "variables\n")

# Structure
cat("\n--- Aperçu (glimpse) ---\n")
glimpse(sect1_hw4[, c("hhid","indiv","s1q2","s1q3","s1q4","s1q7",
                       "zone","state","sector")])

# Vérification des doublons
n_doublons <- sect1_hw4 %>%
  group_by(hhid, indiv) %>%
  filter(n() > 1) %>%
  nrow()
cat("\nDoublons sur (hhid, indiv) :", n_doublons, "\n")

# Valeurs manquantes
vars_cles <- sect1_hw4 %>%
  select(hhid, indiv, s1q2, s1q3, s1q4, s1q7, zone, state, sector)

cat("\nPourcentage de NA par variable clé :\n")
print(
  vars_cles %>%
    summarise(across(everything(), ~ round(mean(is.na(.)) * 100, 2))) %>%
    pivot_longer(everything(), names_to = "Variable", values_to = "Pct_NA") %>%
    arrange(desc(Pct_NA))
)

cat("[01_import] Chargement terminé.\n")
