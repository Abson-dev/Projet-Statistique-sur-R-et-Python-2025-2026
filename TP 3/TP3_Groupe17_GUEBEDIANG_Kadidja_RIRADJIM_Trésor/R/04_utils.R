# =========================================================
# 04_utils.R
# Fonctions utiles
# =========================================================

clean_label_text <- function(x) {
  x <- as.character(x)
  x <- stringr::str_trim(x)
  x
}

to_yes_no <- function(x) {
  x <- as.character(x)
  x <- stringr::str_to_lower(x)
  
  dplyr::case_when(
    stringr::str_detect(x, "yes|1\\. yes|yes, illness|yes, injury") ~ "Oui",
    stringr::str_detect(x, "no|2\\. no|0\\. no one") ~ "Non",
    TRUE ~ NA_character_
  )
}

recode_sex <- function(x) {
  x <- as.character(x)
  x <- stringr::str_to_lower(x)
  
  dplyr::case_when(
    stringr::str_detect(x, "male") ~ "Homme",
    stringr::str_detect(x, "female") ~ "Femme",
    TRUE ~ NA_character_
  )
}

recode_sector <- function(x) {
  x <- as.character(x)
  x <- stringr::str_to_lower(x)
  
  dplyr::case_when(
    stringr::str_detect(x, "urban") ~ "Urbain",
    stringr::str_detect(x, "rural") ~ "Rural",
    TRUE ~ NA_character_
  )
}

make_age_group <- function(age) {
  dplyr::case_when(
    is.na(age) ~ NA_character_,
    age < 5 ~ "0-4",
    age >= 5  & age <= 14 ~ "5-14",
    age >= 15 & age <= 24 ~ "15-24",
    age >= 25 & age <= 44 ~ "25-44",
    age >= 45 & age <= 64 ~ "45-64",
    age >= 65 ~ "65+",
    TRUE ~ NA_character_
  )
}

safe_numeric <- function(x) {
  suppressWarnings(as.numeric(as.character(x)))
}

ci_prop_95 <- function(success, total) {
  if (is.na(success) || is.na(total) || total == 0) {
    return(data.frame(prop = NA_real_, low = NA_real_, high = NA_real_))
  }
  
  test <- prop.test(success, total)
  data.frame(
    prop = success / total,
    low  = test$conf.int[1],
    high = test$conf.int[2]
  )
}

make_quintile <- function(x) {
  if (all(is.na(x))) return(rep(NA_character_, length(x)))
  
  cuts <- stats::quantile(x, probs = seq(0, 1, 0.2), na.rm = TRUE, type = 7)
  cuts <- unique(cuts)
  
  if (length(cuts) < 6) {
    return(rep(NA_character_, length(x)))
  }
  
  cut(
    x,
    breaks = cuts,
    include.lowest = TRUE,
    labels = c("Q1", "Q2", "Q3", "Q4", "Q5")
  ) %>% as.character()
}

top_n_with_other <- function(df, var, n = 10) {
  var <- rlang::ensym(var)
  
  top_levels <- df %>%
    count(!!var, sort = TRUE) %>%
    slice_head(n = n) %>%
    pull(!!var) %>%
    as.character()
  
  df %>%
    mutate(
      !!var := ifelse(as.character(!!var) %in% top_levels,
                      as.character(!!var),
                      "Autres")
    )
}