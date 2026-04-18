# TP4 :  Analyse des parcelles agricoles: superficie, tenure foncière et utilisation des terres
# Par :  Astou DIOP ET YEMELI SAAH Eugène Crespo, ISE1
# Supervisé par : M. Aboubacar HEMA, IFPRI

rm(list = ls())

# Packages requis
pkgs <- c(
  "tidyverse", "haven", "labelled", "here", "scales",
  "apyramid", "patchwork", "naniar", "moments",
  "rstatix", "coin", "PropCIs", "gtsummary", "gt",
  "srvyr", "survey", "flextable", "officer", "knitr"
)

manquants <- pkgs[!pkgs %in% installed.packages()[, 1]]
if (length(manquants) > 0) {
  cat("Packages manquants :", paste(manquants, collapse = ", "), "\n")
  cat("Lancez renv::restore() puis relancez main.R\n")
  stop("Environnement non initialisé.")
}

invisible(lapply(pkgs, library, character.only = TRUE)) 
cat("Packages chargés :", length(pkgs), "\n")

set.seed(2026)
source_ghs <- "Source : GHS Panel W4 (2018-2019), NBS Nigeria / World Bank LSMS-ISA. Calculs des auteurs."

source(here("scripts", "nettoyage.R"),  encoding = "UTF-8")
source(here("scripts", "setup.R"),  encoding = "UTF-8")
source(here("scripts", "donnee.R"),  encoding = "UTF-8")
source(here("scripts", "analyses.R"),   encoding = "UTF-8")


cat(sprintf("\nPipeline terminé"))
cat("Les figures sont dans : outputs/figures/\n")
cat("Les tableaux sont dans : outputs/tables/\n")

# Rapport principal
rmarkdown::render(
  input         = here("docs", "rapport.Rmd"),
  output_format = "word_document",
  output_file   = here("docs", "rapport.docx")
)


cat("Tout foncionne bien. Pour le rapport  , ouvrez docs\n")