library(here)
library(fs)

dir_create(here("data", "processed", "temp"))
dir_create(here("data", "processed", "clean"))
dir_create(here("data", "processed", "final"))

dir_create(here("output", "figures", "analyse1"))
dir_create(here("output", "figures", "analyse2"))
dir_create(here("output", "figures", "analyse3"))

dir_create(here("output", "tables", "analyse1"))
dir_create(here("output", "tables", "analyse2"))
dir_create(here("output", "tables", "analyse3"))

dir_create(here("output", "logs"))

path_wave1_zip <- here("data", "raw", "wave1", "NGA_2010_GHSP-W1_v03_M_STATA.zip")
path_wave2_zip <- here("data", "raw", "wave2", "NGA_2012_GHSP-W2_v02_M_STATA.zip")
path_wave3_zip <- here("data", "raw", "wave3", "NGA_2015_GHSP-W3_v02_M_Stata.zip")
path_wave4_zip <- here("data", "raw", "wave4", "NGA_2018_GHSP-W4_v03_M_Stata12.zip")

path_wave1 <- here("data", "raw", "wave1", "extracted")
path_wave2 <- here("data", "raw", "wave2", "extracted")
path_wave3 <- here("data", "raw", "wave3", "extracted")
path_wave4 <- here("data", "raw", "wave4", "extracted")