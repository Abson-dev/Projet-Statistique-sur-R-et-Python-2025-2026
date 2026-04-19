# TP4 : Parcelles agricoles, superficie et tenure foncière
# Par : Nkwa Tsamo Leslye & Ouattara Ousmane, 
# Supervisé par : M. Aboubacar HEMA, Research Scientist  at IFPRI

rm(list = ls())

# Packages requis
pkgs <- c(
  "tidyverse", "haven", "labelled", "here", "scales",
  "patchwork", "naniar", "moments",
  "rstatix", "gtsummary", "gt",
  "srvyr", "survey", "flextable", "officer", "knitr",
  "sf", "rnaturalearth", "rnaturalearthdata", "boot"
)

manquants <- pkgs[!pkgs %in% installed.packages()[, 1]]
if (length(manquants) > 0) {
  cat("Packages manquants :", paste(manquants, collapse = ", "), "\n")
  cat("Lancez renv::restore() puis relancez main.R\n")
  stop("Environnement non initialisé.")
}

invisible(lapply(pkgs, library, character.only = TRUE))
cat("Packages chargés :", length(pkgs), "\n")

set.seed(2070)
source_ghs <- "Source : GHS Panel W4 (2018-2019), NBS Nigeria / World Bank LSMS-ISA. Calculs des auteurs."

source(here("scripts", "01_preparation.R"), encoding = "UTF-8")
source(here("scripts", "02_analyses.R"),    encoding = "UTF-8")

cat(sprintf("\nPipeline terminé\n"))
cat("Les figures sont dans : outputs/figures/\n")
cat("Les tableaux sont dans : outputs/tables/\n")

rmarkdown::render(
  here("docs", "rapport.Rmd"),
  output_format = "word_document",
  output_file   = here("docs", "rapport.docx")
)
cat("Rapport generé. Ouvrez docs/rapport.docx\n")
