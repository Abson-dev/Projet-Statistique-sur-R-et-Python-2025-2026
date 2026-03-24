###############################################################################
# 01_import.R -- Chargement et exploration des donnees (Tache 1)
# Projet : TP1 -- Profil demographique des menages nigerians
###############################################################################

cat("\n========== 01_import : Chargement des donnees ==========\n")

sect1_hw4 <- read_dta(file.path(chemin_data, "sect1_harvestw4.dta"))
secta_hw4 <- read_dta(file.path(chemin_data, "secta_harvestw4.dta"))

cat("sect1_harvestw4 :", nrow(sect1_hw4), "observations,",
    ncol(sect1_hw4), "variables\n")
cat("secta_harvestw4 :", nrow(secta_hw4), "observations\n")

# Verification des doublons
n_doublons <- sect1_hw4 %>%
  group_by(hhid, indiv) %>%
  filter(n() > 1) %>%
  nrow()
cat("Doublons sur (hhid, indiv) :", n_doublons, "\n")

# Valeurs manquantes
cat("\nPourcentage de NA par variable cle :\n")
vars_cles <- sect1_hw4 %>%
  select(hhid, indiv, s1q2, s1q3, s1q4, s1q7, zone, state, sector)
print(
  vars_cles %>%
    summarise(across(everything(), ~ round(mean(is.na(.)) * 100, 2))) %>%
    pivot_longer(everything(), names_to = "Variable", values_to = "Pct_NA") %>%
    arrange(desc(Pct_NA))
)

cat("[01_import] Chargement termine.\n")
