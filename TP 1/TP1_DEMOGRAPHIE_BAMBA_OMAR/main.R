packages <- c("naniar", "haven", "dplyr", "ggplot2", "apyramid",
              "gtsummary", "rstatix", "PropCIs", "patchwork", "scales",
              "forcats", "moments", "survey", "srvyr", "Hmisc", "here")

a_installer <- packages[!packages %in% rownames(installed.packages())]
if (length(a_installer) > 0) install.packages(a_installer)
lapply(packages, library, character.only = TRUE)

library(here)

source(here("R", "fonctions.R"))
source(here("R", "01_import.R"))
source(here("R", "02_nettoyage.R"))
source(here("R", "03_analyse.R"))
