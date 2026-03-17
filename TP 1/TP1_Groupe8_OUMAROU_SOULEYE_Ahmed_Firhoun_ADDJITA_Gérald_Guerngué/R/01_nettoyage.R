rm(list = ls())

library(haven)
library(labelled)
library(dplyr)

#Importer les données

# listes des sections et vagues
sections <- c("sect1_harvest", "sect1_planting", "secta_harvest")
waves <- 1:4


##Pour chaque section ("sect1_harvest", "sect1_planting", "secta_harvest")
for (sec in sections) {
  ##Pour chaque vague
  for (w in waves) {
    #On concatène le chemin du fichier
    file_path <- paste0("data/raw/", sec, "w", w, ".dta")
    #Le nom de la variable dans R, seral le concaténé de la section et de la vague
    df_name <- paste0(sec, "w", w)
    # lire le fichier et l'assigner à la variable
    assign(df_name, read_dta(file_path))
  }
}
rm(df_name, file_path)


for (w in waves) {
  
  # noms des fichiers / data.frames
  sect1_harvest <- get(paste0("sect1_harvestw", w))
  sect1_planting <- get(paste0("sect1_plantingw", w))
  secta_harvest  <- get(paste0("secta_harvestw", w))
  
  # Sélection des variables clés
  sect1_harvest <- sect1_harvest %>%
    select(hhid, indiv, s1q2, s1q3, s1q4,sector)
  if (w==1) {
    sect1_planting <- sect1_planting %>%
      select(hhid, indiv, s1q2, s1q3, s1q4)
  } else {
    sect1_planting <- sect1_planting %>%
      select(hhid, indiv, s1q2, s1q3, s1q4, s1q6) %>%
      rename(habite_toujours=s1q4,s1q4=s1q6)
  }
  
  secta_harvest <- secta_harvest %>%
    select(any_of(c("hhid","zone","state","lga")),
           starts_with("wt_"))
  #  Fusionner sect1_harvest et secta_harvest
  merge_harvest <- merge(sect1_harvest, secta_harvest, by = "hhid")
  
  #  Ajouter préfixes (harvest_ et planting_) et convertir en factor
  merge_harvest <- merge_harvest %>%
    rename_with(~ paste0("harvest_", .x), -any_of(c("hhid","indiv","zone","sector","state","lga")) & -starts_with("wt_")) %>%
    mutate(across(-c(hhid, indiv, starts_with("wt_"),ends_with("s1q4")), ~ as_factor(.x)))
  
  sect1_planting <- sect1_planting %>%
    rename_with(~ paste0("planting_", .x), -c(hhid, indiv)) %>%
    mutate(across(-c(hhid, indiv,ends_with("s1q4")), ~ as_factor(.x)))
  
  #  Fusionner harvest et planting
  processed <- merge(x = merge_harvest, y = sect1_planting, by = c("hhid", "indiv"))
  
  # Sauvegarder
  write_dta(processed, paste0("data/processed/processed_w", w, ".dta"))
  
  # Assigner le dataframe dans l'environnement
  assign(paste0("processed_w", w), processed)
}

