rm(list=ls())
# Chargement de sect1_harvestw4 avec haven
library(haven)
data <- read_stata("data/raw/sect1_harvestw4.dta")

# Exploration de la structure
str(data)
glimpse(data)
summary(data)

