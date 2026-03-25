# =========================================================
# 00_packages.R
# Installe et charge les packages nécessaires
# =========================================================

required_packages <- c(
  "haven",
  "dplyr",
  "tidyr",
  "stringr",
  "purrr",
  "ggplot2",
  "readr",
  "forcats",
  "janitor",
  "fs",
  "glue",
  "scales",
  "survey",
  "rmarkdown",
  "knitr",
  "flextable",
  "officer"
)

install_if_missing <- function(pkgs) {
  for (p in pkgs) {
    if (!requireNamespace(p, quietly = TRUE)) {
      install.packages(p, dependencies = TRUE)
    }
  }
}

install_if_missing(required_packages)
invisible(lapply(required_packages, library, character.only = TRUE))

options(survey.lonely.psu = "adjust")
