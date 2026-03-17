# Avant toute analyse, il faut construire un catalogue des données.
source("R/01_packages.R")
source("R/02_paths.R")

catalogue_wave1 <- fs::dir_ls(path_wave1, recurse = TRUE, regexp = "\\.dta$")
catalogue_wave2 <- fs::dir_ls(path_wave2, recurse = TRUE, regexp = "\\.dta$")
catalogue_wave3 <- fs::dir_ls(path_wave3, recurse = TRUE, regexp = "\\.dta$")
catalogue_wave4 <- fs::dir_ls(path_wave4, recurse = TRUE, regexp = "\\.dta$")

catalogue <- tibble(
  wave = c(
    rep("W1_2010", length(catalogue_wave1)),
    rep("W2_2012", length(catalogue_wave2)),
    rep("W3_2015", length(catalogue_wave3)),
    rep("W4_2018", length(catalogue_wave4))
  ),
  full_path = c(catalogue_wave1, catalogue_wave2, catalogue_wave3, catalogue_wave4)
) |>
  mutate(
    file_name = fs::path_file(full_path),
    folder = fs::path_dir(full_path),
    size_mb = round(file.info(full_path)$size / 1024^2, 3)
  ) |>
  arrange(wave, file_name)

readr::write_csv(
  catalogue,
  here::here("data", "processed", "final", "catalogue_fichiers_dta.csv")
)

print(catalogue, n = 20)

# VVérification
file.exists(here::here("data", "processed", "final", "catalogue_fichiers_dta.csv"))

# Bref apperçu
dplyr::count(catalogue, wave)
head(catalogue, 10)

