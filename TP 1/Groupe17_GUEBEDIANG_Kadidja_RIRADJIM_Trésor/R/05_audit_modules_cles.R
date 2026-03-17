source("R/01_packages.R")
source("R/02_paths.R")

read_dta_clean <- function(path) {
  haven::read_dta(path) |>
    janitor::clean_names()
}

inspect_file <- function(path) {
  df <- read_dta_clean(path)
  
  cat("\n============================\n")
  cat("Fichier :", path, "\n")
  cat("============================\n")
  cat("Dimensions :", nrow(df), "lignes x", ncol(df), "colonnes\n\n")
  
  cat("Noms des variables :\n")
  print(names(df))
  
  cat("\nAperçu structure :\n")
  str(df)
  
  cat("\nRésumé des valeurs manquantes :\n")
  miss <- naniar::miss_var_summary(df)
  miss <- miss[order(-miss$pct_miss), ]
  print(miss)
  
  invisible(df)
}

files_to_inspect <- c(
  fs::dir_ls(path_wave1, recurse = TRUE, regexp = "sect1_harvestw1\\.dta$"),
  fs::dir_ls(path_wave1, recurse = TRUE, regexp = "secta_harvestw1\\.dta$"),
  fs::dir_ls(path_wave1, recurse = TRUE, regexp = "sect2a_harvestw1\\.dta$"),
  fs::dir_ls(path_wave1, recurse = TRUE, regexp = "sect2b_harvestw1\\.dta$"),
  fs::dir_ls(path_wave1, recurse = TRUE, regexp = "sect3a_harvestw1\\.dta$"),
  fs::dir_ls(path_wave1, recurse = TRUE, regexp = "sect3b_harvestw1\\.dta$"),
  fs::dir_ls(path_wave4, recurse = TRUE, regexp = "sect1_harvestw4\\.dta$"),
  fs::dir_ls(path_wave4, recurse = TRUE, regexp = "secta_harvestw4\\.dta$"),
  fs::dir_ls(path_wave4, recurse = TRUE, regexp = "sect2_harvestw4\\.dta$"),
  fs::dir_ls(path_wave4, recurse = TRUE, regexp = "sect3a_harvestw4\\.dta$"),
  fs::dir_ls(path_wave4, recurse = TRUE, regexp = "sect3b_harvestw4\\.dta$")
)

purrr::walk(files_to_inspect, inspect_file)