source("R/01_packages.R")
source("R/02_paths.R")

read_dta_quick <- function(path) {
  haven::read_dta(path)
}

safe_var_label <- function(x) {
  lab <- attr(x, "label")
  
  if (is.null(lab)) {
    return(NA_character_)
  }
  
  lab_chr <- paste(as.character(lab), collapse = " | ")
  
  if (length(lab_chr) == 0 || is.na(lab_chr) || lab_chr == "") {
    return(NA_character_)
  }
  
  lab_chr
}

extract_var_dictionary <- function(df, file_path, wave) {
  tibble(
    wave = wave,
    file_name = fs::path_file(file_path),
    variable = names(df),
    class = purrr::map_chr(df, ~ paste(class(.x), collapse = ", ")),
    labelled = purrr::map_lgl(df, haven::is.labelled),
    var_label = purrr::map_chr(df, safe_var_label),
    n_missing = purrr::map_int(df, ~ sum(is.na(.x))),
    pct_missing = purrr::map_dbl(df, ~ mean(is.na(.x)) * 100)
  )
}

extract_file_summary <- function(df, file_path, wave) {
  tibble(
    wave = wave,
    file_name = fs::path_file(file_path),
    full_path = as.character(file_path),
    n_rows = nrow(df),
    n_cols = ncol(df),
    has_hhid = "hhid" %in% names(df),
    has_indiv = "indiv" %in% names(df),
    has_indiv_id = "indiv_id" %in% names(df),
    has_personid = "personid" %in% names(df),
    has_weight = any(stringr::str_detect(names(df), "weight|wt|wgt|hhweight")),
    has_sector = "sector" %in% names(df),
    has_state = "state" %in% names(df),
    has_lga = "lga" %in% names(df),
    has_ea = "ea" %in% names(df)
  )
}

audit_one_file <- function(path, wave) {
  message("Audit : ", fs::path_file(path))
  df <- read_dta_quick(path)
  
  list(
    summary = extract_file_summary(df, path, wave),
    dictionary = extract_var_dictionary(df, path, wave)
  )
}

audit_wave <- function(paths, wave) {
  results <- purrr::map(paths, ~ audit_one_file(.x, wave))
  
  file_summary <- purrr::map_dfr(results, "summary")
  var_dictionary <- purrr::map_dfr(results, "dictionary")
  
  list(
    file_summary = file_summary,
    var_dictionary = var_dictionary
  )
}

catalogue <- readr::read_csv(
  here::here("data", "processed", "final", "catalogue_fichiers_dta.csv"),
  show_col_types = FALSE
)

paths_w1 <- catalogue |>
  dplyr::filter(wave == "W1_2010") |>
  dplyr::pull(full_path)

paths_w2 <- catalogue |>
  dplyr::filter(wave == "W2_2012") |>
  dplyr::pull(full_path)

paths_w3 <- catalogue |>
  dplyr::filter(wave == "W3_2015") |>
  dplyr::pull(full_path)

paths_w4 <- catalogue |>
  dplyr::filter(wave == "W4_2018") |>
  dplyr::pull(full_path)

audit_w1 <- audit_wave(paths_w1, "W1_2010")
audit_w2 <- audit_wave(paths_w2, "W2_2012")
audit_w3 <- audit_wave(paths_w3, "W3_2015")
audit_w4 <- audit_wave(paths_w4, "W4_2018")

file_summary_all <- dplyr::bind_rows(
  audit_w1$file_summary,
  audit_w2$file_summary,
  audit_w3$file_summary,
  audit_w4$file_summary
)

var_dictionary_all <- dplyr::bind_rows(
  audit_w1$dictionary,
  audit_w2$dictionary,
  audit_w3$dictionary,
  audit_w4$dictionary
)

readr::write_csv(
  file_summary_all,
  here::here("data", "processed", "final", "audit_file_summary.csv")
)

readr::write_csv(
  var_dictionary_all,
  here::here("data", "processed", "final", "audit_variable_dictionary.csv")
)

message("Audit terminé.")