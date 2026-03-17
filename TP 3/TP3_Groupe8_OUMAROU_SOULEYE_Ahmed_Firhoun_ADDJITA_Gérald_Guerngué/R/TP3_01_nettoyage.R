rm(list = ls())

library(haven)
library(labelled)
library(dplyr)

# ─────────────────────────────────────────────────────────────────
# IMPORTATION DES DONNÉES BRUTES (OBJETS DÉJÀ DANS LE PROJET R)
# ─────────────────────────────────────────────────────────────────

# Les bases sont supposées déjà chargées dans l'environnement R
# (sect4a_harvestw4, sect1_harvestw4, totcons_final)

# ─────────────────────────────────────────────────────────────────
# TRAITEMENT : SÉLECTION, RECODAGE, FUSION, SAUVEGARDE
# ─────────────────────────────────────────────────────────────────

w <- 4

# Récupérer les data.frames
sect4a_h <- read_dta("data/raw/sect4a_harvestw4.dta")
sect1_h  <- read_dta("data/raw/sect1_harvestw4.dta")
totcons_final <- read_dta("data/raw/totcons_final.dta")
# ── Sélection des variables santé dans sect4a ───────────────────
sect4a_h <- sect4a_h %>%
  select(hhid, indiv,
         any_of(c("s4aq1","s4aq3","s4aq3b_1",
                  "s4aq5","s4aq6a",
                  "s4aq9","s4aq10","s4aq14","s4aq15")))

# ── Sélection des variables demographiques dans sect1 ───────────
sect1_h <- sect1_h %>%
  select(hhid, indiv,
         s1q2, s1q4,
         any_of(c("sector","state","zone","lga")))

# ── Fusion sect4a + sect1 ───────────────────────────────────────
merged <- left_join(sect4a_h, sect1_h, by = c("hhid", "indiv"))

# ── Ajout des quintiles de consommation ─────────────────────────
cons_propre <- totcons_final %>%
  select(hhid,
         any_of(c("totcons_adj","totcons_pc","hhsize"))) %>%
  filter(!is.na(totcons_adj), totcons_adj > 0) %>%
  mutate(
    quintile_cons  = ntile(totcons_adj, 5),
    quintile_label = factor(quintile_cons,
                            levels = 1:5,
                            labels = c("Q1 (Pauvres)","Q2","Q3","Q4","Q5 (Riches)")
    )
  )

merged <- left_join(merged, cons_propre, by = "hhid")

# ── Calcul de la depense totale de sante ────────────────────────
if (all(c("s4aq9","s4aq10","s4aq14") %in% names(merged))) {
  merged <- merged %>%
    mutate(
      depense_totale = rowSums(
        cbind(ifelse(is.na(s4aq9),  0, s4aq9),
              ifelse(is.na(s4aq10), 0, s4aq10),
              ifelse(is.na(s4aq14), 0, s4aq14)),
        na.rm = TRUE
      ),
      depense_totale = ifelse(depense_totale == 0,
                              NA, depense_totale)
    )
}

# ── Ajout du préfixe "sante_" aux variables de sect4a ───────────
sante_vars <- c("s4aq1","s4aq3","s4aq3b_1","s4aq5","s4aq6a",
                "s4aq9","s4aq10","s4aq14","s4aq15","depense_totale")

merged <- merged %>%
  rename_with(
    ~ paste0("sante_", .x),
    any_of(sante_vars)
  )

# ── Conversion en factor des variables catégorielles ────────────
merged <- merged %>%
  mutate(
    across(
      -c(hhid, indiv,
         starts_with("wt_"),
         any_of(c("s1q4","sante_s4aq9","sante_s4aq10",
                  "sante_s4aq14","sante_s4aq5",
                  "sante_depense_totale",
                  "totcons_adj","totcons_pc","hhsize",
                  "quintile_cons"))),
      ~ tryCatch(as_factor(.x), error = function(e) .x)
    )
  )

# ── Sauvegarder ─────────────────────────────────────────────────
write_dta(merged, "data/processed/processed_sante_w4.dta")

# ── Conserver dans l'environnement R ────────────────────────────
processed_sante_w4 <- merged

cat("Vague 4 traitee :", nrow(merged), "observations,",
    ncol(merged), "variables\n")