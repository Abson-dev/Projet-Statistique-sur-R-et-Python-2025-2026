required_packages <- c(
  "tidyverse",
  "haven",
  "readxl",
  "janitor",
  "gtsummary",
  "survey",
  "sf",
  "renv",
  "fs",
  "here",
  "glue",
  "stringr",
  "purrr",
  "labelled",
  "skimr",
  "naniar",
  "patchwork",
  "gt",
  "DescTools",
  "rstatix"
)

installed <- rownames(installed.packages())
to_install <- setdiff(required_packages, installed)

if (length(to_install) > 0) {
  install.packages(to_install)
}

invisible(lapply(required_packages, library, character.only = TRUE))