
library(rlang)
library(dplyr)
library(here)
library(haven)

# Charger les données brutes

data_clean <- readRDS(here("data", "processed", "data_clean.rds"))

# Construction de la variable "niveau_educ"

data_clean <- data_clean %>%
  mutate(
    s2aq9 = as.numeric(s2aq9),  # conversion
    niveau_educ = case_when(
    s2aq9 %in% c(0, 1, 2, 3) ~ "Aucun",
    s2aq9 %in% 11:16 ~ "Primaire",
    s2aq9 %in% 21:23 ~ "Junior Secondary",
    s2aq9 %in% 24:26 ~ "Senior Secondary",
    s2aq9 >= 27 ~ "Tertiaire",
  ))

# Sauvegarder les données brutes dans processed

saveRDS(data_clean, here("data", "processed", "data_var.rds"))




