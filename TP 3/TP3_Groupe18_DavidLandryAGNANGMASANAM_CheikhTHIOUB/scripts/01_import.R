###############################################################################
# 01_import.R -- Chargement et exploration des donnees sante (Tache 13)
# Projet : TP3 -- Acces aux soins et depenses de sante (GHS Panel W4)
###############################################################################

cat("\n========== 01_import : Chargement des donnees ==========\n")

# ---- Chargement des fichiers ----
# sect4a : section sante individuelle (morbidite, consultations, depenses)
sect4a <- read_dta(file.path(chemin_data, "sect4a_harvestw4.dta"))

# sect1 : composition du menage (sexe, age pour jointure)
sect1  <- read_dta(file.path(chemin_data, "sect1_harvestw4.dta"))

# totcons_final : agregats de consommation (pour quintiles de richesse)
totcons <- read_dta(file.path(chemin_data, "totcons_final.dta"))

cat("sect4a_harvestw4 :", nrow(sect4a), "observations,",
    ncol(sect4a), "variables\n")
cat("sect1_harvestw4  :", nrow(sect1), "observations,",
    ncol(sect1), "variables\n")
cat("totcons_final    :", nrow(totcons), "observations,",
    ncol(totcons), "variables\n")

# ---- Apercu de la section sante ----
cat("\n--- Apercu sect4a (variables sante) ---\n")
glimpse(sect4a[, c("hhid", "indiv", "sector", "s4aq1", "s4aq3",
                    "s4aq3b_1", "s4aq6a", "s4aq7", "s4aq8",
                    "s4aq9", "s4aq14", "s4aq17")])

# ---- Valeurs manquantes sur les variables cles ----
cat("\n--- Valeurs manquantes (variables sante) ---\n")
vars_sante <- sect4a %>%
  select(hhid, indiv, s4aq1, s4aq3, s4aq3b_1, s4aq6a, s4aq9, s4aq14, s4aq17)
print(
  vars_sante %>%
    summarise(across(everything(), ~ round(mean(is.na(.)) * 100, 1))) %>%
    pivot_longer(everything(), names_to = "Variable", values_to = "NA_%") %>%
    arrange(desc(`NA_%`))
)

cat("[01_import] Chargement termine.\n")
