# =========================================================
# 04_utils.R
# Fonctions utiles
# =========================================================

clean_label_text <- function(x) {
  x <- as.character(x)
  stringr::str_trim(x)
}

to_yes_no <- function(x) {
  x_chr <- tolower(trimws(as.character(x)))
  dplyr::case_when(
    stringr::str_detect(x_chr, "yes|oui") ~ "Oui",
    stringr::str_detect(x_chr, "no|non") ~ "Non",
    TRUE ~ NA_character_
  )
}

recode_sex <- function(x) {
  x_chr <- tolower(trimws(as.character(x)))
  dplyr::case_when(
    stringr::str_detect(x_chr, "male|homme") ~ "Homme",
    stringr::str_detect(x_chr, "female|femme") ~ "Femme",
    TRUE ~ NA_character_
  )
}

recode_sector <- function(x) {
  x_chr <- tolower(trimws(as.character(x)))
  dplyr::case_when(
    stringr::str_detect(x_chr, "urban|urbain") ~ "Urbain",
    stringr::str_detect(x_chr, "rural") ~ "Rural",
    TRUE ~ NA_character_
  )
}

make_age_group <- function(age) {
  dplyr::case_when(
    is.na(age) ~ NA_character_,
    age < 5 ~ "0-4",
    age <= 14 ~ "5-14",
    age <= 24 ~ "15-24",
    age <= 44 ~ "25-44",
    age <= 64 ~ "45-64",
    age >= 65 ~ "65+",
    TRUE ~ NA_character_
  )
}
