rm(list = ls())
packages <- c(
  "haven", "labelled", "dplyr", "Hmisc", "stringr"
)

packages_manquants <- packages[!packages %in% installed.packages()[, "Package"]]

if (length(packages_manquants) > 0) {
  cat("Installation des packages manquants :",
      paste(packages_manquants, collapse = ", "), "\n")
  install.packages(packages_manquants)
} else {
  cat("Tout est déjà installé")
}


# Charger tous les packages
invisible(lapply(packages, library, character.only = TRUE))
# Chargement des données
sections <- c("sect1_harvest", "sect1_planting", "secta_harvest")
for (sec in sections) {
  file_path <- paste0("data/raw/", sec, "w4.dta")
  assign(paste0(sec, "w4"), read_dta(file_path))
}
rm(file_path)

# Fonction de winsorisation pondérée
winsorize_weighted <- function(x, w) {
  q <- as.numeric(wtd.quantile(x, weights = w, probs = c(0.25, 0.75), na.rm = TRUE))
  Q1  <- q[1]
  Q3  <- q[2]
  IQR_pond <- Q3 - Q1
  borne_inf <- Q1 - 1.5 * IQR_pond
  borne_sup <- Q3 + 1.5 * IQR_pond
  x <- ifelse(x < borne_inf, borne_inf, x)
  x <- ifelse(x > borne_sup, borne_sup, x)
  return(x)
}

# Sélection des variables
sect1_harvest  <- sect1_harvestw4 %>%
  select(hhid, indiv, s1q2, s1q3, s1q4, sector)

sect1_planting <- sect1_plantingw4 %>%
  select(hhid, indiv, s1q2, s1q3, s1q4, s1q6) %>%
  rename(habite_toujours = s1q4, s1q4 = s1q6)

secta_harvest  <- secta_harvestw4 %>%
  select(any_of(c("hhid", "zone", "state", "lga", "strata", "ea")),
         starts_with("wt_wave"))

# Fusion
merge_harvest <- merge(sect1_harvest, secta_harvest, by = "hhid")

merge_harvest <- merge_harvest %>%
  rename_with(~ paste0("harvest_", .x),
              -any_of(c("hhid", "indiv", "zone", "sector", "state", "lga", "strata", "ea")) &
                -starts_with("wt_")) %>%
  mutate(across(-c(hhid, indiv, starts_with("wt_wave"), ends_with("s1q4")), ~ as_factor(.x)))

sect1_planting <- sect1_planting %>%
  rename_with(~ paste0("planting_", .x), -c(hhid, indiv)) %>%
  mutate(across(-c(hhid, indiv, ends_with("s1q4")), ~ as_factor(.x)))

processed_w4 <- merge(x = merge_harvest, y = sect1_planting, by = c("hhid", "indiv"))

# Winsorisation pondérée sur s1q4 (âge)
processed_w4 <- processed_w4 %>%
  mutate(across(
    ends_with("s1q4"),
    ~ winsorize_weighted(.x, w = processed_w4$wt_wave4)
  ))

# Suppression des non-répondants et repondération
processed_w4 <- processed_w4 %>%
  mutate(
    non_repondant = is.na(harvest_s1q2) & is.na(harvest_s1q3) & is.na(harvest_s1q4)
  )
  processed_w4 <- processed_w4 %>%
    group_by(strata) %>%
    mutate(
      n_h      = n(),
      r_h      = sum(!non_repondant),
      w2h      = n_h / r_h,
      wt_wave4 = ifelse(!non_repondant, wt_wave4 * w2h, wt_wave4)
    ) %>%
    ungroup() %>%
    select(-n_h, -r_h, -w2h)

processed_w4 <- processed_w4 %>%
  filter(!non_repondant) %>%
  select(-non_repondant)

# Re-labellisation en français
processed_w4 <- processed_w4 %>%
  mutate(
    across(ends_with("s1q2"), ~ recode(.x,
                                       "1. MALE"   = "Homme",
                                       "2. FEMALE" = "Femme"
    )),
    across(ends_with("s1q3"), ~ recode(.x,
                                       "1. HEAD"                          = "Chef de ménage",
                                       "2. SPOUSE"                        = "Conjoint(e)",
                                       "3. OWN CHILD"                     = "Enfant",
                                       "4. STEP CHILD"                    = "Beau-fils/Belle-fille",
                                       "5. ADOPTED CHILD"                 = "Enfant adopté(e)",
                                       "6. GRANDCHILD"                    = "Petit-enfant",
                                       "7. BROTHER/SISTER"                = "Frère/Sœur",
                                       "8. NIECE/NEPHEW"                  = "Neveu/Nièce",
                                       "9. BROTHER/SISTER-IN-LAW"         = "Beau-frère/Belle-sœur",
                                       "10. PARENT"                       = "Père/Mère",
                                       "11. PARENT-IN-LAW"                = "Beau-père/Belle-mère",
                                       "12. DOMESTIC HELP"                = "Employé(e) domestique",
                                       "12. DOMESTIC HELP (RESIDENT)"     = "Employé(e) domestique (résident)",
                                       "13. DOMESTIC HELP (NON RESIDENT)" = "Employé(e) domestique (non résident)",
                                       "14. OTHER RELATION (SPECIFY)"     = "Autre membre de la famille",
                                       "15. OTHER NON-RELATION (SPECIFY)" = "Autre personne non apparentée"
    )),
    sector = recode(sector,
                    "1. Urban" = "Urbain",
                    "2. Rural" = "Rural"
    ),
    zone = recode(zone,
                  "1. North Central" = "Nord-Centre",
                  "2. North East"    = "Nord-Est",
                  "3. North West"    = "Nord-Ouest",
                  "4. South East"    = "Sud-Est",
                  "5. South South"   = "Sud-Sud",
                  "6. South West"    = "Sud-Ouest"
    ),
    across(ends_with("habite_toujours"), ~ recode(.x,
                                                  "1. YES" = "Oui",
                                                  "2. NO"  = "Non"
    )),
    lga = str_remove(lga, "^\\d+\\.\\s*") %>%
      str_to_title() %>%
      as_factor()
  ) %>%
  filter(
    !is.na(wt_wave4)
  )

# Sauvegarde
write_dta(processed_w4, "data/processed/processed_w4.dta")